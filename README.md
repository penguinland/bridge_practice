# bridge-practice

There are two ways to use this: you can generate pairs of PDFs (one with a list
of problems and one with a list of solutions), or you can run a webserver (which
shows a single problem and a button to show the solution, and another button
which lets you get a new problem).

Sample PDFs are in the `samples/` directory, and there is probably a demo of
this running at https://bidding.coolthingoftheday.com (uptime not guaranteed!).
If you want to generate PDFs or run your own web interface and you're
comfortable writing code, it should hopefully be straightforward to set
everything up.

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
server`. Then, in your browser, head to http://localhost:8765. You can check
the topics you want to practice, hit "Next" to get a new problem, and hit "Show
Answer" to see the solution to the current problem.

To reiterate an important part: `stack run server` must be run from the root
directory of this repository, and will not do the right thing if you run it from
another directory instead.

### I'm bad at web technologies

The web interface is extremely basic. I'm not a web developer, and have no real
interest in improving it: it displays everything I need, and doesn't have any
needlessly complicated frills. If you don't like the web interface, you can make
your own.

## Code organization

If you're interested in modifying any of the code, here's the rough layout:
- All the interesting parts are in `src/`:
  - Files directly in `src/` are the plumbing of how everything works together.
    The most interesting file is likely `EDSL.hs`, which defines the embedded
    domain-specific language used to describe bridge hands.
  - Files in `src/Bids/` define bids using the EDSL.
  - Files in `src/Topics/` describe `Situations` to practice. Each of these
    files typically has a corresponding file in `src/Bids/` that goes with it,
    though some topics involve bids imported from multiple other files.
- `make_pdf/` and `server/` contain the executables. If you create a new
  `Topic`, you'll want to add it to one (both?) of those.
- `static/` contains frontend stuff for the webserver (HTML, JavaScript, and
  CSS).
- `bridge.tex` needs to be in the same directory as the `.tex` files created by
  `make_pdf`, or else they won't generate PDFs properly.

### Complexity of different parts

As of September 2024, this repo is over 10,000 lines of code. It sounds like a
lot, but most of it is in the business logic of defining conventions to practice
and paragraphs of text explaining the answers. The approximate line counts are:
- 1200 lines of code in `src/` directly, implementing the infrastructure of the
  system
- 3600 lines in `src/Bids/`, defining various bidding conventions
- 4800 lines in `src/Topics/`, defining situations to practice
- 800 lines of frontend stuff (LaTeX, JavaScript, CSS, HTML, `Main` functions)

It can be intimidating trying to learn your way around a new repo this large,
but you can ignore all the bids and topics you're not interested in, at which
point the code is a manageable size.

### Data Types and Terminology

Here is a rough summary of some of the data types in the code:
- A `SituationInstance` is a problem and solution to be shown to the user. It
  contains an entire deal of the cards, the bidding, the answer, the explanation
  of why the answer is correct, etc.
- A `Situation` contains a `dealer` program to generate a deal, plus everything
  else needed to make a `SituationInstance`.
- `Situations` take in a random number generator and give back a `Situation`.
  It might be a collection of multiple `Situation`s to randomly choose from, or
  a single `Situation` that can be parameterized (e.g., you can randomly set the
  vulnerability and dealer, while the rest is unchanged). It is often a
  collection of collections of parameterizable `Situation`s.
- A `Topic` is a named `Situations`. When you use this system as a whole, you
  select which `Topic`s you want to practice.
- A `Description` is a string-like data type that can render certain things
  (e.g., bids) in a fancy way. Most string-like things in here are actually
  `Description`s (`Topic` names, bidding alerts, explanations of why the answer
  is correct, etc.).
- An `Action` is a way to modify either the `dealer` program or the bidding.
  The embedded domain-specific language I've built is entirely made out of
  `Action`s.
- A `Call` is what you'd expect (pass, bid, double, or redouble). A
  `CompleteCall` is a `Call` with an optional `Description` of an alert.
- Hopefully other things are straightforward (e.g., you can figure out on your
  own what `Direction` and `Hand` represent).

## Bugfixes

If something has gone wrong but you figure out what it was, please send a pull
request to update these instructions!

Sometimes, the problems will involve marginal decisions that different players
might bid differently (for example, the intended solution is to make a limit
raise but you think you're shapely enough and have good enough intermediates to
be game forcing). The system as currently written doesn't have much nuance coded
into it, and does not take those things into account. Sorry, but the nuance of
proper hand evaluation is _hard._ In these cases, it's fine to disagree with the
intended solution, as long as you understand why a less sophisticated player
might think the intended solution is correct.

Sometimes (hopefully rarely!), there will be a bug in the business logic, and
the "intended" solution is flat-out wrong (for example, the stated solution is
to raise partner's minor even though you've got a 7-card major). Those egregious
mistakes are easier to fix: please let me know if you find any.
