""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" VI interface to crackNum
"
" Copyright   :  (c) Levent Erkok
" License     :  BSD3
" Maintainer  :  erkokl@gmail.com
"
" INSTALLATION: Put this file in a convenient location (typically your .vim directory),
" and put "so crackNum.vim" in your .vimrc file. (With the appropriate path.)
"
" Once you restart vim, locate your cursor over a stream of binary/hex digits, and
" enter the command :CrackNum to see further options.
"
" Arguments are passed to crackNum verbatim, so anything the executable accepts works:
"
"     :CrackNum -i4              " decode as a 4-bit signed integer
"     :CrackNum -fhp             " decode as a half-precision float
"     :CrackNum -f3+4            " decode as a float with 3 exponent, 4 significand bits
"     :CrackNum -l4 -fhp         " decode 4 lanes of half-precision floats
"
" Use TAB to complete the common formats. With no arguments, :CrackNum prompts for them.
" The formats offered come from "crackNum --list-formats", so they track the executable.
"
" Set g:crackNumProgram to run a crackNum that is not on your PATH, and
" g:crackNumPrecisions to replace the completion list outright.
"
" See https://github.com/LeventErkok/CrackNum for details.

" Which executable to run; override if crackNum is not on your PATH.
if !exists("g:crackNumProgram")
    let g:crackNumProgram = "crackNum"
endif

" Bare -i/-w flags offered by TAB completion; picking one prompts for the bit-width
" separately, since crackNum takes any -iN/-wN and there's no fixed list to offer.
" Lane counts (-lN) are left out of completion entirely: they're not a format on
" their own and always need to be paired with a -f/-i/-w flag, so type -lN directly.
let s:crackNumIntFlags = ["-i", "-w"]

" Used only when crackNum is too old to know --list-formats, or is not on the PATH.
" Anything crackNum has learned since is picked up from the executable, not from here.
let s:crackNumFallbackFormats = [ "hp", "bp", "tf32", "sp", "dp", "qp"
                              \ , "e5m2", "e4m3", "fp4", "fp4e0m3", "e8m0"
                              \ ]

" The formats the executable reports, as -f flags. Asking it keeps this list from
" drifting out of date as crackNum grows new formats. Cached per executable, since
" completion is on a keystroke path and this shells out.
let s:crackNumFormatCache = {}
function! s:CrackNumFormats()
    if has_key(s:crackNumFormatCache, g:crackNumProgram)
        return s:crackNumFormatCache[g:crackNumProgram]
    endif
    let l:names = []
    " 2>&1 so that an older crackNum, which errors out here, cannot scribble on the screen.
    let l:out = systemlist(shellescape(g:crackNumProgram) . " --list-formats 2>&1")
    if v:shell_error == 0
        let l:names = filter(l:out, 'v:val =~# "^[a-z0-9]\\+$"')
    endif
    if empty(l:names)
        let l:names = s:crackNumFallbackFormats
    endif
    let s:crackNumFormatCache[g:crackNumProgram] = map(copy(l:names), '"-f" . v:val')
    return s:crackNumFormatCache[g:crackNumProgram]
endfunction

" What TAB offers. Set g:crackNumPrecisions yourself to override the whole list.
function! s:CrackNumChoices()
    if exists("g:crackNumPrecisions")
        return g:crackNumPrecisions
    endif
    return s:crackNumIntFlags + s:CrackNumFormats()
endfunction

function! CrackNumComplete(A, L, P)
    let l:all = s:CrackNumChoices()
    if empty(a:A)
        return l:all
    endif
    " Plain prefix match: the arguments contain '-' and '+', so avoid regex matching.
    let l:out = filter(copy(l:all), 'stridx(v:val, a:A) == 0')
    if empty(l:out)
        return l:all
    endif
    return l:out
endfunction

" Patterns crackNum accepts as input, most specific first. Note that <cword> is not
" good enough on its own: it stops at the quote in verilog notation (64'hdeadbeef),
" and <cWORD> is too greedy, picking up trailing punctuation such as "0x8000;".
let s:crackNumPatterns = [ "\\d\\+'[bBoOdDhH][0-9a-fA-F_]\\+"
                       \ , "0[xX][0-9a-fA-F_]\\+"
                       \ , "0[bB][01_]\\+"
                       \ ]

" The bit-pattern under the cursor, falling back on <cword> if nothing matches.
function! s:CrackNumWord()
    let l:line = getline('.')
    let l:idx  = col('.') - 1
    for l:pat in s:crackNumPatterns
        let l:from = 0
        while 1
            let l:m = matchstrpos(l:line, l:pat, l:from)
            if l:m[1] < 0
                break
            endif
            if l:idx >= l:m[1] && l:idx < l:m[2]
                return l:m[0]
            endif
            let l:from = l:m[2]
        endwhile
    endfor
    return expand("<cword>")
endfunction

function! CrackNum(...)
    redraw
    let l:curWord = s:CrackNumWord()
    if empty(l:curWord)
        echoerr "Place the cursor on a bin/hex number to crack!"
        return
    endif
    if empty(a:000)
        echo "Cracking \"" . l:curWord . "\".. Use TAB to see the formats supported."
        call inputsave()
        let l:args = [input("Format> ", "", "customlist,CrackNumComplete")]
        call inputrestore()
        redraw
        if empty(join(l:args, ''))
            echoerr "No format given; use e.g. -i4, -w8, or -fhp."
            return
        endif
        " -i/-w need a bit-width; ask for it separately rather than baking a fixed
        " set of widths into TAB completion, since crackNum takes any -iN/-wN.
        if l:args[0] ==# "-i" || l:args[0] ==# "-w"
            call inputsave()
            let l:width = input(l:args[0] . " width (bits)> ")
            call inputrestore()
            redraw
            if empty(l:width)
                echoerr "No width given; use e.g. " . l:args[0] . "4."
                return
            endif
            let l:args = [l:args[0] . l:width]
        endif
    else
        echo "Cracking \""  . l:curWord . "\".."
        let l:args = copy(a:000)
    endif

    " Quote the value: verilog notation (64'hdeadbeef) is not shell-safe as-is.
    let l:grepargs = join(l:args + [shellescape(l:curWord)], ' ')
    let l:grepprg_bak=&grepprg
    let l:grepformat_bak=&grepformat
    try
        let &grepprg=g:crackNumProgram
        " crackNum reports plain text, not file:line diagnostics, so take each line as is.
        let &grepformat="%m"
        silent execute "grep" . " " . l:grepargs
    finally
        let &grepprg=l:grepprg_bak
        let &grepformat=l:grepformat_bak
    endtry

    call setqflist([], 'a', {'title': g:crackNumProgram . ' ' . l:grepargs})

    botright copen

    redraw!
endfunction
command! -nargs=* -complete=customlist,CrackNumComplete CrackNum call CrackNum(<f-args>)
" end crackNum interface
