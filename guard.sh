#!/bin/bash -euf

FSWATCH=$(which fswatch)
if [ ! -x "$FSWATCH" ]; then
  echo "$FSWATCH not found! On OS X: brew install fswatch"
  exit 2
fi

if [ -z "$1" ]; then
  echo "Usage: $0 <test-suite>"
  echo "   eg. $0 spec"
  echo "   ... in a Haskell project with a 'test-suite spec' line in its <project>.cabal file."
  exit 1
fi
SUITE="$1"

echo "Guard started; now waiting for changes."
fswatch --one-per-batch --latency 10 --recursive \
       --insensitive --extended --include="^[A-Za-z0-9_/]+.hs$" --exclude="[0-9~]$" \
       src/ test/ | xargs -n1 -I{} env NOTIFY=1 ./tests.sh "$SUITE"
