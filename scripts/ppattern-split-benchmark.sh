#!/bin/bash

# number of samples
N=10

# p permutation
PMINSIZE=10
PMAXSIZE=20
PSTEPSIZE=2
PMINSPLIT=2
PMAXSPLIT=6
PSPLITSTEP=1

# q permutation
QIZE=200
QSPLITSTEP=1

# generate
for ((PSIZE=$PMINSIZE; PSIZE<=$PMAXSIZE; PSIZE+=$PSTEPSIZE)); do
  for ((PSPLIT=$PMINSPLIT; PSPLIT <= $PMAXSPLIT ; PSPLIT+=$PSPLITSTEP)); do
    QSPLITMAX=$((PSPLIT+2))
    for ((QSPLIT = $PSPLIT; QSPLIT <= $QSPLITMAX ; QSPLIT+=$QSPLITSTEP)); do

      # output csv file
      CSV=../data/ppattern-benchmark-psize-${PSIZE}-qsize-${QSIZE}-psplit-${PSPLIT}-qsplit-${QSPLIT}.csv

      for ((I = 1; I <= $N; I++)); do
        DATE=`date +"%T"`
        echo "RUN: #$1 - ITERATION: #${I} - ${DATE}";

        # random generator seed
        SEED=$RANDOM

        # benchmark
        echo ppattern-split-benchmark --psize=${PSIZE} --qsize=${QSIZE} --psplit=${PSPLIT} --qsplit=${QSPLIT} --seed=${SEED}
        ../dist/build/ppattern-split-benchmark/ppattern-split-benchmark --psize=${PSIZE} --qsize=${QSIZE} --psplit=$PSPLIT --qsplit=${QSPLIT} --seed=${SEED} >> ${CSV}
        echo
      done
    done
  done
done

# surprising difficult case
# 20,250,6,8,"[1,11,2,6,3,4,7,10,5,8,12,9,18,20,14,19,15,13,16,17]","[2,223,1,7,137,73,72,92,3,4,6,69,24,32,11,13,64,70,98,115,88,5,14,90,116,97,15,126,17,135,8,162,167,9,203,10,18,153,21,209,211,225,25,12,26,27,28,29,16,19,31,33,20,22,23,30,34,35,36,37,41,38,39,43,40,42,46,44,45,47,48,50,59,71,76,77,49,51,52,82,53,54,55,94,95,56,57,104,58,105,106,60,123,128,129,61,62,63,130,134,138,65,140,145,148,149,152,155,156,66,157,158,67,164,68,74,75,78,165,166,170,79,80,172,81,83,84,173,85,175,86,177,87,89,179,188,191,91,93,96,99,100,101,102,193,195,103,201,204,206,107,212,220,108,109,221,110,111,224,227,112,228,113,114,117,118,119,231,120,236,243,121,122,247,248,124,125,127,131,132,133,136,139,141,142,143,144,146,147,150,151,154,159,160,161,163,168,169,171,174,176,178,180,181,182,183,184,185,186,187,189,190,192,194,196,197,198,199,200,202,205,207,208,210,213,214,215,216,217,218,219,222,226,229,230,232,233,234,235,237,238,239,240,241,242,244,245,246,249,250]","Nothing","leftmost conflict first",10.10 m
