# bridge-practice

This is definitely not consumer ready! but if you're comfortable writing code,
it should be straightforward to set everything up. You will need the following
external tools installed:
- Haskell and Stack
  - On a Mac, do this with `brew install haskell-stack`
  - On Linux, this is `sudo apt-get install haskell-platform`
- XeLaTeX
  - On a Mac, run `brew cask install mactex`
  - On Ubuntu, run `sudo apt-get install texlive-xetex`
- Dealer, a domain-specific language to generate bridge hands
  - On Ubuntu, run `sudo apt-get install dealer`
  - Admittedly, this is increasingly hard to find in a ready-to-install way. A
    copy of the source code is at https://github.com/penguinland/dealer, and
    once you've made a git clone of that, running `make` should compile it
    (you'll need `make`, GCC, flex, and yacc installed). Then, copy the binary
    (`dealer`) to somewhere in your shell path.

Once that's all installed, `run.sh` should be able to display 2 PDFs (one of
problems and one of solutions). If something has gone wrong but you figure out
what it was, please send a pull request to update these instructions!
