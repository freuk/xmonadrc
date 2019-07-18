# this file was tested using GNUMAKE >= 4.2.1.

# this is necessary for using multi-line strings as command arguments.
SHELL := $(shell which bash)

# this allows omitting newlines.
.ONESHELL:

# "nix-shell -p" constructs an expression that relies on <nixpkgs> for
# selecting attributes, so we override it.
# https://github.com/NixOS/nix/issues/726#issuecomment-161215255
NIX_PATH := nixpkgs=./.

.PHONY: all
all: gen/EITHER-ROOTCOMMAND-WMCOMMAND.dic gen/EITHER-ROOTCOMMAND-WMCOMMAND.lm gen/READCOMMAND.lm gen/READCOMMAND.dic cabal-build

.PHONY: cabal-build
cabal-build: src
	cabal v2-build

.PHONY: run
run: all
	cabal v2-run voice-server serve ./gen 3

.PHONY: vendor
vendor: xmonadrc.nix

.PHONY: resources
resources: gen/EITHER-ROOTCOMMAND-WMCOMMAND.dic gen/EITHER-ROOTCOMMAND-WMCOMMAND.lm gen/EITHER-ROOTCOMMAND-WMCOMMAND.vocab gen/EITHER-ROOTCOMMAND-WMCOMMAND.sent src

.PRECIOUS:gen/%.sent gen/%.vocab
gen/%.lm: gen/%.sent gen/%.vocab src
	vendor/quick_lm.pl -s $< -w $(word 2,$^)
	mv gen/$*.sent.arpabo gen/$*.lm

gen/%.dic: gen/%.vocab src
	pronounce -P40 -r vendor/ -d cmudict_SPHINX_40 -i $< -o $@

gen/%.sent gen/%.vocab: src
	cabal v2-run -v0 voice-server dict


.PHONY: ghcid
ghcid: ghcid-xmonadrc

ghcid-%: xmonadrc.cabal .hlint.yaml xmonadrc.nix
	@nix-shell -E '
		with import <nixpkgs> {};
		with haskellPackages;
		shellFor {
			packages = p: [p.xmonadrc];
			buildInputs = [ghcid cabal-install hlint];
		}
	' --pure --run bash <<< '
		ghcid --command "cabal v2-repl $*" \
			--restart=xmonadrc.cabal \
			--restart=default.nix \
			--test=Main.ghcid
			-l
	'

.PHONY: pre-commit
pre-commit: ormolu dhall-format shellcheck hlint

.PHONY: shellcheck
shellcheck:
	@nix-shell --pure -p fd shellcheck --run bash <<< '
		for F in $$(fd -e sh); do
			shellcheck -s bash $$F
		done
	'

.PHONY: hlint
hlint:
	@nix-shell --pure -p hlint --run bash <<< '
		hlint src/ --hint=./.hlint.yaml
	'

.PHONY: dhall-format
dhall-format:
	@nix-shell --pure -p fd haskellPackages.dhall --run bash <<< '
		RETURN=0
		for F in $$(fd -e dhall); do
			dhall format < $$F | cmp -s $$F -
			if [ $$? -ne 0 ]; then
				echo "[!] $$F does not pass dhall-format format check. Formatting.." >&2
				dhall format --inplace $$F
				RETURN=1
			fi
		done
		if [ $$RETURN -ne 0 ]; then exit 1; fi
	'

.PHONY: ormolu
ormolu:
	@nix-shell --pure -E '
		let pkgs = import <nixpkgs> {};
		in pkgs.mkShell {
			buildInputs = [pkgs.fd pkgs.ormolu];
			shellHook =
				"export LOCALE_ARCHIVE=$${pkgs.glibcLocales}/lib/locale/locale-archive \n" +
				"export LANG=en_US.UTF-8";
		}
	' --run bash <<< '
		RETURN=0
		for F in $$(fd -E src/Bandit/Tutorial.hs -e hs); do
			ormolu -o -XTypeApplications -o -XPatternSynonyms -m check $$F
			if [ $$? -ne 0 ]; then
				echo "[!] $$F does not pass ormolu format check. Formatting.." >&2
				ormolu -o -XTypeApplications -o -XPatternSynonyms -m inplace $$F
				RETURN=1
			fi
		done
		if [ $$RETURN -ne 0 ]; then exit 1; fi
	'

.PHONY: doc
doc: src/Bandit/Tutorial.hs xmonadrc.cabal xmonadrc.nix
	@nix-shell -E '
		with import <nixpkgs> {};
		with haskellPackages;
		shellFor {
			packages = p: [p.xmonadrc];
			buildInputs = [cabal-install];
		}
	' --run <<< bash '
		cabal v2-haddock xmonadrc --haddock-internal
	'

.PHONY:clean
clean:
	rm -rf dist*
	rm -f xmonadrc.nix
	rm -f xmonadrc.cabal
	rm -rf dhall-to-cabal
	rm -rf gen/*
