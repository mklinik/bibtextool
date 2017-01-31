#!/bin/bash

# This script requires bibtextool to be installed somewhere in PATH
# We have a number of input bib files and expected output bib files. The tests
# run bibtex and compare the output.

ls input*.bib |\
  fromto 's/input/> output/' |\
  sed 's/^/bibtextool -p /' |\
  sh

ls output*.bib |\
  fromto 's/output/expected/' |\
  map diff -s
