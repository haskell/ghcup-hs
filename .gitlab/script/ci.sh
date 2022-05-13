#!/usr/bin/env bash

set -Eeuo pipefail

TOP="$( cd "$(dirname "$0")" ; pwd -P )"
. "${TOP}/../ghcup_env"

function save_cabal_cache () {
  echo "Storing cabal cache from $CABAL_DIR to $CABAL_CACHE..."
  rm -Rf "$CABAL_CACHE"
  mkdir -p "$CABAL_CACHE"
  if [ -d "$CABAL_DIR" ]; then
	  cp -Rf "$CABAL_DIR" "$CABAL_CACHE/"
  fi
}


function extract_cabal_cache () {
  if [ -d "$CABAL_CACHE" ]; then
      echo "Extracting cabal cache from $CABAL_CACHE to $CABAL_DIR..."
      mkdir -p "$CABAL_DIR"
      cp -Rf "$CABAL_CACHE"/* "$CABAL_DIR"
  fi
}

function save_stack_cache () {
  echo "Storing stack cache from $STACK_ROOT to $STACK_CACHE..."
  rm -Rf "$STACK_CACHE"
  mkdir -p "$STACK_CACHE"
  if [ -d "$STACK_ROOT" ]; then
	  cp -Rf "$STACK_DIR" "$STACK_CACHE"
  fi
}


function extract_stack_cache () {
  if [ -d "$STACK_CACHE" ]; then
      echo "Extracting stack cache from $STACK_CACHE to $STACK_ROOT..."
      mkdir -p "$STACK_ROOT"
      cp -Rf "$STACK_CACHE"/* "$STACK_ROOT"
  fi
}

function save_brew_cache () {
  echo "Storing brew cache from $BREW_DIR to $BREW_CACHE..."
  rm -Rf "$BREW_CACHE"
  mkdir -p "$BREW_CACHE"
  if [ -d "$BREW_DIR" ]; then
	  cp -Rf "$BREW_DIR" "$BREW_CACHE"
  fi
}


function extract_brew_cache () {
  if [ -d "$BREW_CACHE" ]; then
      echo "Extracting stack cache from $BREW_CACHE to $BREW_DIR..."
      mkdir -p "$BREW_DIR"
      cp -Rf "$BREW_CACHE"/* "$BREW_DIR"
  fi
}

case $1 in
  extract_cabal_cache) extract_cabal_cache ;;
  save_cabal_cache) save_cabal_cache ;;
  extract_stack_cache) extract_stack_cache ;;
  save_stack_cache) save_stack_cache ;;
  extract_brew_cache) extract_brew_cache ;;
  save_brew_cache) save_brew_cache ;;
  *) echo "unknown mode $1" ; exit 11 ;;
esac
