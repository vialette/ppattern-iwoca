#!/bin/sh

# number of runs
RUN=1

# running ppattern-benchmarks.sh for ever
while true; do
  echo run $RUN
  sh ppattern-benchmark.sh $RUN
  RUN=$((RUN+1))
  echo
  echo
done
