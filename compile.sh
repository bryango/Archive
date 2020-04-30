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
    filename=$(basename "$file")

    # Process magic comment
    tex_program=$(
        grep -E "^[ \t]*%[ ]*\!TeX[ ]*TS-program[ ]*=[ ]*" "$filename" \
        | cut -d '=' -f2 | xargs
    )

    if [[ $tex_program == pdflatex ]] \
    || [[ $tex_program == xelatex ]] \
    || [[ $tex_program == lualatex ]]; then
        latexmk \
            "-$tex_program" \
            -synctex=1 \
            -interaction=nonstopmode \
            -shell-escape \
            "$filename"
    else
        eval "$tex_program" \
            "$filename"
    fi


    cd "$DIR" || exit 1
done
