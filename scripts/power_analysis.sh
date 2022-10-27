#!/bin/bash

#SBATCH --job-name=salts_exp_power_analysis
#SBATCH --output=joblogs/%x_%j.txt
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=16GB
#SBATCH --partition=week
#SBATCH --time=7-00:00:00
#SBATCH --mail-type=FAIL,END,INVALID_DEPEND

module load R/4.1.0-foss-2020b

echo Running script: scripts/salts_exp_power_analysis.sh

cd analysis/

Rscript power_analysis-freq.r
