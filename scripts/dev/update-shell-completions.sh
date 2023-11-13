#!/bin/sh

set -xue

cabal --verbose=0 run ghcup:exe:ghcup -- --bash-completion-script ghcup > scripts/shell-completions/bash
cabal --verbose=0 run ghcup:exe:ghcup -- --zsh-completion-script ghcup > scripts/shell-completions/zsh
cabal --verbose=0 run ghcup:exe:ghcup -- --fish-completion-script ghcup > scripts/shell-completions/fish

