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
" See https://github.com/LeventErkok/CrackNum for details.

let g:crackNumPrecisions = ["hp","sp","dp","sb","sw","sd","sq","ub","uw","ud","uq"]
function! CrackNumComplete(A, L, P)
    if empty(a:A)
	return g:crackNumPrecisions
    else
    	let out = filter(copy(g:crackNumPrecisions), 'v:val =~ "^' . a:A . '.*"')
	if empty(out)
	    return g:crackNumPrecisions
	else
	    return out
    endif
endfunction
function! CrackNum(...)
    redraw
    let curWord = expand("<cword>")
    if empty(curWord)
	echoerr "Place the cursor on a bin/hex number to crack!"
	return
    endif
    if empty(a:000)
	echo "Cracking \"" . curWord . "\".. Use TAB to see precisions supported."
	call inputsave()
	let prec = input("Precision> ", "", "customlist,CrackNumComplete")
	call inputrestore()
	let args = [prec] + copy(a:000)
    else
	echo "Cracking \""  . curWord . "\".."
	let prec = a:1
	let args = copy(a:000)
    endif

    if index(g:crackNumPrecisions, prec) < 0
	echoerr "Unknown precision: \"" . prec . "\"" . ". Must be one of: " . join(g:crackNumPrecisions, ' ')
	return
    endif

    let l:grepargs = join(['--vim'] + copy(args) + ['--bv', curWord], ' ')
    let grepprg_bak=&grepprg
    let grepformat_bak=&grepformat
    try
        let &grepprg="crackNum"
        let &grepformat="VIM %m"
        silent execute "grep" . " " . l:grepargs
    finally
        let &grepprg=grepprg_bak
        let &grepformat=grepformat_bak
    endtry

    botright copen

    redraw!
endfunction
command! -nargs=* -complete=customlist,CrackNumComplete CrackNum call CrackNum(<f-args>)
map @nhp   :silent call CrackNum('hp')<CR>
map @nsp   :silent call CrackNum('sp')<CR>
map @ndp   :silent call CrackNum('dp')<CR>
map @nsb   :silent call CrackNum('sb')<CR>
map @nsw   :silent call CrackNum('sw')<CR>
map @nsd   :silent call CrackNum('sd')<CR>
map @nsq   :silent call CrackNum('sq')<CR>
map @nub   :silent call CrackNum('ub')<CR>
map @nuw   :silent call CrackNum('uw')<CR>
map @nud   :silent call CrackNum('ud')<CR>
map @nuq   :silent call CrackNum('uq')<CR>
" end crackNum interface
