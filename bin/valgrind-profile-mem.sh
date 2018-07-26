#!/bin/bash
valgrind -q --tool=massif --massif-out-file=massif.out "$1"

mem=$(grep mem_heap_B massif.out | sed -e 's/mem_heap_B=\(.*\)/\1/' | sort -g | tail -n 1)

echo "Total memory used: $mem bytes"
