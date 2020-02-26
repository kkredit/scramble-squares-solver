#!/bin/bash

LANGS="c go haskell"

function get_word_n() {
    echo $1 | cut -d" " -f$2
}

function get_times() {
    local TIME_FMT="%e %M"
    STATS=$(command time -f "$TIME_FMT" $@ 2>&1 > /dev/null)
    TIME_S=$(get_word_n $STATS 1)
    MEM_KB=$(get_word_n $STATS 2)
}

function print_row() {
    printf "| %-8s | %-14s | %-13s | %-11s | %-13s |\n" "$1" "$2" "$3" "$4" "$5"
}

print_row "Language" "Build time (s)" "Exe Size (KB)" "Runtime (s)" "Mem: RSS (KB)"
echo    '|:--------:|:--------------:|:-------------:|:-----------:|:-------------:|'

for LANG in $LANGS; do
    cd $LANG >/dev/null

    make clean >/dev/null
    get_times make
    BUILD_S=$TIME_S
    SIZE_KB=$(( $( stat --printf="%s" bin/puzzle) / 1024))
    get_times ./bin/puzzle

    print_row "$LANG" "$BUILD_S" "$SIZE_KB" "$TIME_S" "$MEM_KB"

    cd - >/dev/null
done
