#!/bin/bash
# sync mathematica dependencies

DEPS_PATH=(
    ~/Templates/diffgeoM
    ~/Templates/Physica
)

for origin in "${DEPS_PATH[@]}"; do
    rsync \
        --exclude='.git*' --recursive --copy-links \
        --update --partial \
        --progress --verbose \
        "$@" \
        "$origin" .
done
