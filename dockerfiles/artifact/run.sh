#!/bin/bash

run_bench() {
  local SIZE=${1:-32}
  qsort-bench --csv bench-results/qsort.csv --svg bench-results/qsort.svg -j1 --time-mode=wall -t 10s --size "${SIZE}" +RTS -N -s

}

case "$1" in
  "full-bench")
    run_bench "32"
    ;;
  "bench")
    run_bench "$2"
    ;;
  "fast")
    run_bench 4
    ;;
  "test")
    pure-borrow-test
    ;;
  *)
    run_bench
    ;;
esac