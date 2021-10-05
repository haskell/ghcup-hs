#!/bin/sh

graphmod | tred | dot -Tsvg -Gsize=10,9\! > docs/modules_small.svg
graphmod | tred | dot -Tsvg -Gsize=15,9\! > docs/modules_wide.svg
