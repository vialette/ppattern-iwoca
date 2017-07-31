#!/bin/bash

# permutation size
MINSIZE=${1:-10}
MAXSIZE=${2:-100}
STEPSIZE=${3:-10}

# number of trials for every split parameter
TRIALS=${4:-2}

# generate
for ((SIZE=$MINSIZE; SIZE<=$MAXSIZE; SIZE+=$STEPSIZE)); do
  # output csv file
  CSV=../data/split-parameter-size-${SIZE}-trials-${TRIALS}.csv

  for ((I=1; I<=$TRIALS ; I+=1)); do
    # echo
    DATE=`date +"%T"`
    echo "#${I} - ${DATE}";

    # random generator seed
    SEED=$RANDOM

    # distribution
    echo split-parameter --size=$SIZE --trials=$TRIALS --seed=$SEED
    ../dist/build/split-parameter/split-parameter --size=$SIZE --seed=$SEED >> ${CSV}
    echo
  done
done
