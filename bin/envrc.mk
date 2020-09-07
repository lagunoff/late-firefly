%.envrc: ./*.nix ./*.cabal
	nix-shell -A shell --pure --run 'export -p > .envrc'
