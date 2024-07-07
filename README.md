# bridge-practice

This is definitely not consumer ready! but if you're comfortable writing code,
it should be straightforward to set everything up.

There are two ways to use this: you can generate pairs of PDFs (one of problems
and one of solutions), or you can run a webserver (which shows a problem and a
button to show the solution, and which lets you get a new problem).

## Format

You select the topics you want to practice, and then you will be shown problems.
They are formed by picking a random topic, a random situation within that topic,
and then a random deal of cards consistent with that situation.

In the problem, you will be shown your hand and the bidding so far, and be asked
what to bid next. Opponents' bids will have their alerts displayed.

In the solution, you will be able to see all 4 hands, the bidding so far, all
alerts on all bids (both the opponents' and our own), the intended next bid, and
an explanation of why that bid is the right choice. There is also a "debug
string," which will help track down problems in the code if you encounter a
solution that appears incorrect.

## Installation

You will need the following external tools installed:
- Haskell and Stack
  - On Ubuntu, this is `sudo apt-get install haskell-platform`
  - On a Mac, do this with `brew install haskell-stack`
- Dealer, a domain-specific language to generate bridge hands
  - On Ubuntu, run `sudo apt-get install dealer`
  - Admittedly, this is increasingly hard to find in a ready-to-install way. A
    copy of the source code is at https://github.com/penguinland/dealer, and
    once you've made a git clone of that, running `make` should compile it
    (you'll need `make`, GCC, flex, and yacc installed). Then, copy the binary
    (`dealer`) to somewhere in your shell path.
- XeLaTeX (only used when generating PDFs, not the webserver)
  - On Ubuntu, run `sudo apt-get install texlive-xetex`
  - On a Mac, run `brew cask install mactex`

Disclaimer: I haven't had a Mac to run this on in several years, and it's
possible these instructions no longer work. All recent development has been done
on Ubuntu.

## Generating PDFs (webserver not required!)

To generate PDFs, edit `app/Main.hs` to list the topics you want to practice,
and then `run.sh` should be able to display 2 PDFs (one of problems and one of
solutions).

## Running the webserver (XeLaTeX not required!)

To run the server, go to the **root directory of this repo** and run `stack run
server`. Then, in your browser, head to http://localhost:8765. At the bottom,
you can check the topics you want to practice, you can hit "Next" to get a new
problem, and hit "Show Answer" to see the solution to the current problem.

To repeat: `stack run server` must be run from the root directory of this
repository, and will not do the right thing if you run it from another
directory instead.

### I'm bad at web technologies

The web interface is extremely basic. I'm not a web developer, and have no real
interest in improving it: it displays everything I need, and doesn't have any
needlessly complicated frills. If you don't like the web interface, you can make
your own.

## Bugfixes

If something has gone wrong but you figure out what it was, please send a pull
request to update these instructions!

Sometimes, the problems will involve marginal decisions that different players
might bid differently (for example, the intended solution is to make a limit
raise but you think you're shapely enough and have good enough intermediates to
be game forcing). The system as currently written doesn't have much nuance coded
into it, and does not take those things into account. Sorry, but the nuance of
proper hand evaluation is _hard._

Sometimes (hopefully rarely!), there will be a bug in the business logic, and
the "intended" solution is flat-out wrong (for example, the stated solution is
to raise partner's minor even though you've got a 7-card major). Those egregious
mistakes are easier to fix: please let me know if you find any.
