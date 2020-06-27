#!/usr/bin/env sh
time=$(date)
echo "::set-output name=time::$time"
nix-build -A ghc.late-firefly
