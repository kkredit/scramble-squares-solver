#!/bin/bash

LANGS="c go haskell rust clojure"

function get_word_n() {
    echo $1 | cut -d" " -f$2
}

function get_scc() {
    local STATS=$(scc --ci $1 | grep -i "^$2")
    LOC=$(get_word_n "$STATS" 6)
    COMPLEXITY=$(get_word_n "$STATS" 7)
}

function get_times() {
    local TIME_FMT="%U %S %M"
    local STATS=$(command time -f "$TIME_FMT" bash -c "$@" 2>&1 > /dev/null | tail -1)
    local USER_TIME=$(get_word_n "$STATS" 1)
    local KERNEL_TIME=$(get_word_n "$STATS" 2)
    TIME_S=$(echo $USER_TIME + $KERNEL_TIME | bc -l | sed 's/^\./0\./g' | sed 's/^0$/0\.00/g')
    MEM_KB=$(get_word_n "$STATS" 3)
}

function print_row() {
    printf "| %-8s | %-5s | %-10s | %-14s | %-13s | %-15s | %-13s |\n" "$1" "$2" "$3" "$4" "$5" "$6" "$7"
}

print_row "Language" " LOC " "Complexity" "Build time (s)" "Exe Size (KB)" "10x Runtime (s)" "Mem: RSS (KB)"
echo    '|:--------:|:-----:|:----------:|:--------------:|:-------------:|:---------------:|:-------------:|'

for LANG in $LANGS; do
    cd $LANG >/dev/null

    # Complexity
    [[ "$LANG" == "clojure" ]] && SRC=src/puzzle/*.clj || SRC=puzzle.*
    get_scc "$SRC" "$LANG"

    # Build
    make clean >/dev/null
    get_times make
    BUILD_S=$TIME_S
    [[ "$LANG" == "clojure" ]] && EXE=target/puzzle-0.1.0-SNAPSHOT-standalone.jar || EXE=bin/puzzle
    SIZE_KB=$(( $( stat --printf="%s" $EXE) / 1024))

    # Execution
    get_times "(for i in {1..10}; do ./bin/puzzle; done)"

    print_row "$LANG" "$LOC" "$COMPLEXITY" "$BUILD_S" "$SIZE_KB" "$TIME_S" "$MEM_KB"

    cd - >/dev/null
done
