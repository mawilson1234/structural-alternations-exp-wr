#!/bin/bash

#SBATCH --job-name=salts_exp_bayes-crossed-non-linear
#SBATCH --output=joblogs/%x_%j.txt
#SBATCH --nodes=1
#SBATCH --ntasks=4
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=24:00:00
#SBATCH --mail-type=FAIL,END,INVALID_DEPEND

module load R/4.1.0-foss-2020b

echo Running script: scripts/bayesian_analysis_crossed_non-linear.sh

cd analysis/

Rscript Bayesian\ scripts/crossed_model_non-linear.r
