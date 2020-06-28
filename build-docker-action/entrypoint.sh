#!/bin/bash

time=$(ghc --version)
echo "::set-output name=time::$time"

nix-build -A ghc.late-firefly -o result
