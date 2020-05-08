#!/usr/bin/env bash

set -e

for d in $*; do
   # they don't actually produce the same output
   # cmp <(./hsgzip < $d) <(gzip -c < $d)
   cmp $d <(gzip -c < $d | gunzip -c) # just in case..
   cmp $d <(gzip -c < $d | ./hsgunzip )
   cmp $d <(./hsgzip < $d | gunzip -c)
   cmp $d <(./hsgzip < $d | ./hsgunzip)
   echo -n '.'
done
echo
