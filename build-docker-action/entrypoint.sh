#!/bin/bash

time=$(ghc --version)
echo "::set-output name=time::$time"
