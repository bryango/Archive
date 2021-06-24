#!/bin/bash
# package LaTeX source for distribution

# FILENAME="${1%.tex}"; shift
FILENAME=susy
TARGET=release

[[ -r "$FILENAME".tex ]] \
    || { echo "# Couldn't access $FILENAME.tex"; exit 1; }

mkdir "$TARGET" || true
# echo "*" > "$TARGET/.gitignore"

PREAMBLE=preamble.tex
sed-latex "$PREAMBLE" > "$TARGET/$PREAMBLE"

rsync \
    --exclude='.git*' --recursive --copy-links \
    --update --partial \
    --progress --verbose \
    "$@" \
    "$FILENAME.tex" \
    "$FILENAME.bib" \
    "$FILENAME.bbl" \
    "$FILENAME.pdf" \
    "img/" \
    "$TARGET"
