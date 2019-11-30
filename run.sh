#!/bin/bash

set -e

CWD=$(cd "$(dirname "${BASH_SOURCE[0]}" )" && pwd)
pushd $CWD

stack build
# This next line is platform- and version-agnostic, assuming you've only set
# things up for a single one of each.
$CWD/.stack-work/dist/*/*/build/bridge-practice-exe/bridge-practice-exe

mv test.tex /tmp
cp bridge.tex /tmp
pushd /tmp

xelatex test.tex
mv test.pdf questions.pdf

xelatex "\let\showsolutions\relax\input{test}"
mv test.pdf solutions.pdf

# Open the questions last so it's the topmost window.
evince solutions.pdf&
evince questions.pdf&
