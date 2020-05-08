#!/usr/bin/env bash

for d in $*; do
   if gunzip -c < $d &> /dev/null; then
     if cmp <(./hsgunzip < $d) <(gunzip -c < $d) > /dev/null; then
       echo -n '.'
     else
       echo '#'
       echo "FAILED: $d"
       exit 1
     fi
   else
     echo -n '_'
  fi
done
echo
