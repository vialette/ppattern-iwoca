#!/bin/bash

# number of samples
TRIALS=10

# p permutation
PMINSIZE=10
PMAXSIZE=20
PSTEPSIZE=2

# q permutation
QSIZE=200

# generate
for ((PSIZE=$PMINSIZE; PSIZE<=$PMAXSIZE ; PSIZE+=$PSTEPSIZE)); do
  # output csv file
  CSV=../data/ppattern-random-benchmark-psize-${P}-qsize-${Q}.csv

  for ((I=1; I<=TRIALS ; I+=1)); do
        DATE=`date +"%T"`
        echo "RUN: #$1 - ITERATION: #${I} - ${DATE}";

        # random generator seed
        SEED=$RANDOM

        # benchmark
        echo ppattern-random-benchmark --psize=${PSIZE} --qsize=${QSIZE} --seed=${SEED}
        ../dist/build/ppattern-random-benchmark/ppattern-random-benchmark --psize=${PSIZE} --qsize=${QSIZE} --seed=${SEED} >> ${CSV}
        echo
      done
    done
  done
done
