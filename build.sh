#!/bin/bash
# shellcheck disable=2011

function log { echo "###" "$@"; }

[[ -n $1 ]] \
    && DIR=$1 \
    || DIR=$(dirname "$(readlink -f "$0")")

log "$DIR"
ls | xargs
mkdir -p release

# naming: $project/$sub_project/$main.tex
FILES=$(find . \
    -regex "./[^/]+/[^/]+/[^/]+.tex" \
    -not -path "./Templates/*"
)
log "Files:"
echo "$FILES"

for file in $FILES; do
    log "Compiling \`$file\` ..."

    pathname=$(dirname "$file")
    filename=$(basename "$file")

    cd "$pathname" || exit 1

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
            -synctex=15 \
            -interaction=nonstopmode \
            -shell-escape \
            "$filename"
    else
        eval "$tex_program" \
            "$filename"
    fi

    log "Check & release PDF:"
    pdf="$(basename "$filename" .tex).pdf"
    if ls "$pdf"; then

        # shortened name: $project/$sub_project.tex
        target="$DIR/release/$(dirname "$pathname")"
        mkdir -p "$target"

        # shellcheck disable=2015
        [[ $USER == bryan ]] \
            && cp -a -v -f "$pdf" "$target/." \
            || mv -v -f "$pdf" "$target/."
    fi

    cd "$DIR" || exit 1
done

ls -alF release
cp README.md LICENSE release

TZ='Asia/Shanghai' date +'%F %R UTC+8' > release/TIMESTAMP.txt
