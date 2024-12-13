# Installation Instructions

## Install core dependencies

opam install dune
opam install ounit2
opam install lwt
opam install lwt_ppx
opam install bogue

# If you're on macOS, you'll also need:

brew install pkg-config
brew install sdl2
brew install sdl2_image
brew install sdl2_ttf

Run 'dune exec bin/main.exe' and you're all set!
