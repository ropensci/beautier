#!/bin/bash
# Script for the Groningen Peregrine computer cluster

#SBATCH --time=0:10:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --ntasks=1
#SBATCH --mem=1M
#SBATCH --job-name=sbatch_test_random_1
#SBATCH --output=sbatch_test_random.log
for i in `seq 1 100`
do
  echo $i
  sbatch test_random_1.sh
  sleep 1
done
