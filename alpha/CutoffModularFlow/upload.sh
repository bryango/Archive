#!/bin/bash
# package LaTeX source for distribution

# FILENAME="${1%.tex}"; shift
FILENAME=notes
TARGET=upstream

[[ -r "$FILENAME".tex ]] \
    || { echo "# Couldn't access $FILENAME.tex"; exit 1; }

function sync {
    rsync \
        --exclude='.git*' --recursive --copy-links \
        --update --partial \
        --progress --verbose \
        "$@" \
        "$FILENAME.tex" \
        "$FILENAME.bbl" \
        "$FILENAME.pdf" \
        "cutoff.bib" \
        "img" \
        "nb" \
        "$TARGET"
}

sync --dry-run
echo
read -r -p "### Continue?"
echo
sync "$@"
