#!/bin/bash
#SBATCH --time=1:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --ntasks=1
#SBATCH --mem=1G
#SBATCH --job-name=install_this_r_package
#SBATCH --output=install_this_r_package.log
module load R/3.3.1-foss-2016a
time Rscript -e 'source("install_this_r_package.R")'
