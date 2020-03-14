#!/bin/bash
# shellcheck disable=2011

[[ -n $1 ]] \
    && DIR=$1 \
    || DIR=$(dirname "$(readlink -f "$0")")

echo "### $DIR"
ls | xargs

FILES=$(find . \
    -regex "./[^/]+/[^/]+/[^/]+.tex" \
    -not -path "./Templates/*"
)
echo "### Files:"
echo "$FILES"

for file in $FILES; do
    echo "### Compiling \`$file\` ..."
    cd "$(dirname "$file")" || exit 1

    latexmk \
        -xelatex \
        -synctex=1 \
        -interaction=nonstopmode \
        -shell-escape \
        "$(basename "$file")"

    cd "$DIR" || exit 1
done
