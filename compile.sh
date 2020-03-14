#!/bin/bash

DIR=$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")
cd "$DIR" || exit 1

FILES=$(find . \
    -regex "./[^/]+/[^/]+/[^/]+.tex" \
    -not -path "./Templates/*"
)

for file in $FILES; do
    echo "# Compiling \`$file\` ..."
    cd "$(dirname "$file")" || exit 1

    latexmk \
        -xelatex \
        -synctex=1 \
        -interaction=nonstopmode \
        -shell-escape \
        "$(basename "$file")"

    cd "$DIR" || exit 1
done
