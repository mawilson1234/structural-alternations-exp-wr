#!/bin/bash

#SBATCH --job-name=salts_exp_simulations-30
#SBATCH --output=joblogs/%x_%j.txt
#SBATCH --cpus-per-task=16
#SBATCH --constraint=cascadelake
#SBATCH --time=01-00:00:00
#SBATCH --mail-type=FAIL,END,INVALID_DEPEND

module load R/4.1.0-foss-2020b

echo Running script: scripts/simulations_30.sh

cd analysis/

Rscript Bayesian\ scripts/simulations_30.r
