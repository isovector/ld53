#! /usr/bin/env bash

set -ex

name="Deliverance"
t=$(date --rfc-3339=seconds | sed 's/ /-/g' | sed 's/:/-/g')
g=$(git rev-parse --short HEAD)
outdir="linux-$t-$g"

stack build
cp ./.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/ld53-x86_64.AppImage "releases/linux-x86_64-$name.AppImage"

