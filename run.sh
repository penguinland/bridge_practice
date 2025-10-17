#!/bin/bash

set -e

CWD=$(cd "$(dirname "${BASH_SOURCE[0]}" )" && pwd)
pushd $CWD

echo ""
echo "Compiling..."
stack build bridge-practice:make_pdf

echo ""
echo "Running..."
stack run make_pdf

mv test.tex /tmp
cp bridge.tex /tmp
pushd /tmp > /dev/null

echo ""
echo "Generating PDFs..."
# We rerun LaTeX twice each time, because the table widths might have changed.
# It would be great to suppress all output from the first run, but if something
# in it fails and it drops you into interactive mode, you need to see that!
xelatex test.tex
xelatex test.tex
mv test.pdf questions.pdf
xelatex "\let\showsolutions\relax\input{test}"
xelatex "\let\showsolutions\relax\input{test}"
mv test.pdf solutions.pdf

# Different OSes have different PDF viewers.
UNAME=$(uname)
case "$UNAME" in
    (*Linux*) PDF_VIEWER='evince'; ;;
    (*Darwin*) PDF_VIEWER='open'; ;;
esac;

# Open the questions last so it's the topmost window. Suppress both stdout and
# stderr so they don't gum up the command prompt.
"$PDF_VIEWER" solutions.pdf &> /dev/null &
sleep 0.1
"$PDF_VIEWER" questions.pdf &> /dev/null &
