#!/bin/sh
#
# Builds a fully static crackNum and z3 under Alpine/musl. Runs INSIDE the
# container (see linux-dist.yml); /work is the checkout, bind-mounted from the
# host. Everything it produces is static, so the results run on any x86_64
# Linux regardless of distribution or glibc version.
set -eux

GHC_VERSION=${GHC_VERSION:?}
Z3_TAG=${Z3_TAG:?}

# ncurses/zlib/gmp static archives are what make -optl-static resolvable; the
# GHC library (a crackNum dependency) drags in terminfo, hence ncurses-static.
apk add --no-cache \
    bash binutils build-base cmake curl findutils g++ gcc git gmp-dev \
    libffi-dev linux-headers make musl-dev ncurses-dev ncurses-static \
    perl python3 tar xz zlib-dev zlib-static

# ---------------------------------------------------------------- GHC & cabal
# Kept under /work so the host cache step can preserve them between runs.
export GHCUP_INSTALL_BASE_PREFIX=/work/.ghcup-base
export CABAL_DIR=/work/.cabal-home
export PATH="$GHCUP_INSTALL_BASE_PREFIX/.ghcup/bin:$PATH"

if [ ! -x "$GHCUP_INSTALL_BASE_PREFIX/.ghcup/bin/ghc" ]; then
    mkdir -p "$GHCUP_INSTALL_BASE_PREFIX"
    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org \
        | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
          BOOTSTRAP_HASKELL_MINIMAL=1 \
          sh
    ghcup install ghc   "$GHC_VERSION" --set
    ghcup install cabal latest         --set
fi

ghc   --version
cabal --version

# ------------------------------------------------------------------ crackNum
cd /work
cabal update
cabal build --allow-newer --enable-executable-static exe:crackNum
cp "$(cabal list-bin --allow-newer --enable-executable-static exe:crackNum)" /work/out/crackNum
chmod +x /work/out/crackNum
strip /work/out/crackNum

# ------------------------------------------------------------------------ z3
# No usable prebuilt exists: upstream's x64 tarballs are dynamically linked
# against glibc 2.39. Build it here so it is static like everything else.
if [ ! -x /work/z3-out/z3 ]; then
    rm -rf /tmp/z3
    git clone --depth 1 --branch "$Z3_TAG" https://github.com/Z3Prover/z3 /tmp/z3
    cmake -S /tmp/z3 -B /tmp/z3/build \
        -DCMAKE_BUILD_TYPE=Release \
        -DZ3_BUILD_LIBZ3_SHARED=OFF \
        -DZ3_BUILD_PYTHON_BINDINGS=OFF \
        -DZ3_BUILD_JAVA_BINDINGS=OFF \
        -DZ3_BUILD_DOTNET_BINDINGS=OFF \
        -DZ3_BUILD_TEST_EXECUTABLES=OFF \
        -DCMAKE_EXE_LINKER_FLAGS="-static"
    # NB. The CMake target is "shell"; it is the OUTPUT_NAME that is "z3".
    cmake --build /tmp/z3/build --target shell -j "$(nproc)"
    test -x /tmp/z3/build/z3
    mkdir -p /work/z3-out
    cp /tmp/z3/build/z3 /work/z3-out/z3
    strip /work/z3-out/z3
fi
cp /work/z3-out/z3 /work/out/z3
chmod +x /work/out/z3

# Hand everything back to the host user so later, non-root steps can touch it.
chown -R "${HOST_UID:-0}:${HOST_GID:-0}" /work
