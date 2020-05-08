#!/usr/bin/env bash

set -e

for d in $*; do
   cmp <(./hszpipe < $d) <(./zpipe < $d)
   cmp $d <(./zpipe < $d | ./zpipe -d) # just in case..
   cmp $d <(./zpipe < $d | ./hszpipe -d)
   echo -n .
done
echo
