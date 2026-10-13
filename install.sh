#!/usr/bin/env bash
# Install terminal Emacs and link this repo as ~/.emacs.d.
# Meant to run as a devcontainer dotfiles install command, but works anywhere
# apt is available.
set -euo pipefail

repo_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

sudo=""
if [ "$(id -u)" -ne 0 ]; then
  sudo="sudo"
fi

# Emacs, plus what vterm and tree-sitter grammars need to compile
export DEBIAN_FRONTEND=noninteractive
$sudo apt-get update -qq
$sudo apt-get install -y -qq --no-install-recommends \
  emacs-nox git ca-certificates build-essential cmake libtool-bin libvterm-dev

# Link the config, keeping any existing ~/.emacs.d out of the way
if [ -e "$HOME/.emacs.d" ] && [ ! -L "$HOME/.emacs.d" ]; then
  mv "$HOME/.emacs.d" "$HOME/.emacs.d.bak.$(date +%Y%m%d%H%M%S)"
fi
ln -sfn "$repo_dir" "$HOME/.emacs.d"

# Org files live in their own private repo so they survive container rebuilds.
# A failed clone (e.g. no GitHub credentials yet) shouldn't stop the install.
org_repo="${ORG_REPO:-https://github.com/i-c-grant/org.git}"
if [ ! -d "$HOME/org/.git" ]; then
  if ! git clone -q "$org_repo" "$HOME/org"; then
    echo "WARNING: couldn't clone $org_repo into ~/org; clone it by hand later." >&2
  fi
fi
mkdir -p "$HOME/org/notes"

# Install packages, compile vterm, and fetch the Python grammar now so the
# first interactive launch is quick
emacs --batch \
  --eval "(setq vterm-always-compile-module t)" \
  -l "$HOME/.emacs.d/init.el" \
  --eval "(progn
            (require 'vterm)
            (add-to-list 'treesit-language-source-alist
                         '(python \"https://github.com/tree-sitter/tree-sitter-python\" \"v0.21.0\"))
            (unless (treesit-language-available-p 'python)
              (treesit-install-language-grammar 'python)))"

echo "Emacs config installed. Run: emacs"
