#!/usr/bin/env sh
files=$(ls R)
for file in $files; do
  testfile="tests/testthat/test-$file"
  if [[ -e "$testfile" ]]; then
    echo "Test file for $file already exists"
  else
    echo "context(\"$file\")" >> $testfile
  fi
done
