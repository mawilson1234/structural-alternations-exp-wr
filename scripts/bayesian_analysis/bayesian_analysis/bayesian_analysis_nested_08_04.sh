#!/bin/bash

#SBATCH --job-name=salts_exp_bayes_nested_08_04
#SBATCH --output=joblogs/%x_%j.txt
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --mem-per-cpu=16G
#SBATCH --partition=day
#SBATCH --time=01-00:00:00
#SBATCH --mail-type=FAIL,END,INVALID_DEPEND

module load R/4.1.0-foss-2020b

echo Running script: scripts/bayesian_analysis_nested_08_04.sh

cd analysis/

Rscript Bayesian\ scripts/models/models_nested_08_04.r