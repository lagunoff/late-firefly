#!/bin/sh -l

time=$(date)
echo "::set-output name=time::$time"
# nix-build -A ghc.late-firefly
