#!/usr/bin/bash -eE

day="$1"

cd "$day"
dune exec --display quiet --no-print-directory "$day"
