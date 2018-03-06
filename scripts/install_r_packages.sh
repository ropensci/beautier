#!/bin/bash
# Script for the Groningen Peregrine computer cluster

#SBATCH --time=1:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --ntasks=1
#SBATCH --mem=1G
#SBATCH --job-name=install_r_packages
#SBATCH --output=install_r_packages.log
module load R/3.3.1-foss-2016a
time Rscript -e 'source("install_r_packages.R")'
