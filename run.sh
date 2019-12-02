#!/bin/bash

set -e

CWD=$(cd "$(dirname "${BASH_SOURCE[0]}" )" && pwd)
pushd $CWD

echo ""
echo "Compiling..."
stack build

echo ""
echo "Running..."
# This next line is platform- and version-agnostic, assuming you've only set
# things up for a single one of each.
$CWD/.stack-work/dist/*/*/build/bridge-practice-exe/bridge-practice-exe

mv test.tex /tmp
cp bridge.tex /tmp
pushd /tmp > /dev/null

echo ""
echo "Generating PDFs..."
xelatex test.tex
mv test.pdf questions.pdf
xelatex "\let\showsolutions\relax\input{test}"
mv test.pdf solutions.pdf

# Open the questions last so it's the topmost window. Suppress both stdout and
# stderr so they don't gum up the command prompt.
evince solutions.pdf &> /dev/null &
evince questions.pdf &> /dev/null &
