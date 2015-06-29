#!/bin/bash -e

NOTIFIER=$(which terminal-notifier)
if [ ! -x "$NOTIFIER" ]; then
  echo "$NOTIFIER not found! On OS X: brew install terminal-notifier"
  exit 2
fi

if [ -z "$1" ]; then
  echo "Usage: $0 <test-suite>"
  echo "   eg. $0 spec"
  echo "   ... in a Haskell project with a 'test-suite spec' line in its <project>.cabal file."
  exit 1
fi
SUITE="$1"

echo "Starting test run '$SUITE'..."
TEMPFILE=$(mktemp -t "guard-tests")

  set -o pipefail
  set +e
    cabal build "$SUITE" && ./dist/build/"$SUITE"/"$SUITE" --color | tee "$TEMPFILE"
    RES=$?
  set -e

  if [ "$NOTIFY" ]; then
    if [ "$RES" == "0" ]; then
      TITLE="Spec Success"
      SOUND="Tink"
    else
      TITLE="Spec Failure"
      SOUND="Glass"
    fi
    # De-colorise the output, strip blank lines, and grab the
    # final "4 examples, 1 failure" (or whatever) line.
    LASTLINE=$(cat "$TEMPFILE" | sed "s/$(printf '\033')\\[[?0-9;]*[a-zA-Z]//g" | sed '/^$/d' | tail -n 1)
    $NOTIFIER -sound "$SOUND" -title "$TITLE" -message "$LASTLINE"
  fi

rm "$TEMPFILE"
