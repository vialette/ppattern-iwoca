#!/bin/bash

# permutation size
SIZE=${1:-10}

# output csv file
CSV=../data/exact-distribution-size-${SIZE}.csv

# generate
DATE=`date +"%T"`
echo "${DATE}";

# distribution
echo exact-distribution --size=$SIZE
../dist/build/exact-distribution/exact-distribution --size=$SIZE >> ${CSV}
echo
