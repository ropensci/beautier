#!/bin/bash
#SBATCH --time=1:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --ntasks=1
#SBATCH --mem=1G
#SBATCH --job-name=test_random_1
#SBATCH --output=test_random_1.log
module load R/3.3.1-foss-2016a
time Rscript -e 'source("test_random_1.R")'
