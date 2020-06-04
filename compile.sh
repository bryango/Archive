#!/bin/bash
# shellcheck disable=2011

function log { echo "### " "$@"; }

[[ -n $1 ]] \
    && DIR=$1 \
    || DIR=$(dirname "$(readlink -f "$0")")

log "$DIR"
ls | xargs
mkdir -p release

FILES=$(find . \
    -regex "./[^/]+/[^/]+/[^/]+.tex" \
    -not -path "./Templates/*"
)
log "Files:"
echo "$FILES"

for file in $FILES; do
    log "Compiling \`$file\` ..."

    cd "$(dirname "$file")" || exit 1
    filename=$(basename "$file")

    # Process magic comment
    tex_program=$(
        grep -E "^[ \t]*%[ ]*\!TeX[ ]*TS-program[ ]*=[ ]*" "$filename" \
        | cut -d '=' -f2 | xargs
    )
    [[ -z $tex_program ]] && tex_program=xelatex

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

    log "Check & release PDF:"
    pdf="$(basename "$filename" .tex).pdf"
    ls "$pdf" \
        && mv -v -f "$pdf" "$DIR/release/."

    cd "$DIR" || exit 1
done
