#!/bin/bash

# permutation size
SIZE=${1:-10}

# number of trials for every split parameter
TRIALS=${2:-1000000}

# split parameter
MINSPLITPARAMETER=${3:-1}
MAXSPLITPARAMETER=${4:-$SIZE}
SPLITPARAMETERSTEP=${5:-1}

# output csv file
CSV=../data/split-distribution-size-${SIZE}-splitparameter-${SPLITPARAMETERSTEP}-trials-${TRIALS}.csv

# generate
for ((K=$MINSPLITPARAMETER; K<=$MAXSPLITPARAMETER ; K+=$SPLITPARAMETERSTEP)); do
  # echo
  DATE=`date +"%T"`
  echo "splitparameter=#${K} - ${DATE}";

  # random generator seed
  SEED=$RANDOM

  # distribution
  echo split-distribution --size=$SIZE --splitparameter=$K --trials=$TRIALS --seed=$SEED
  ../dist/build/split-distribution/split-distribution --size=$SIZE --splitparameter=$K --trials=$TRIALS --seed=$SEED >> ${CSV}
  echo
done
