#!/bin/bash

# disable the keychain credential helper
git config --global credential.helper ""
# enable the local store credential helper
git config --global --add credential.helper store
# add credential
echo "https://x-access-token:${1}@github.com" >> ~/.git-credentials
# tell git to use https instead of ssh whenever it encounters it
git config --global url."https://github.com/".insteadof git@github.com:

nix-build -A ghc.late-firefly -o result
