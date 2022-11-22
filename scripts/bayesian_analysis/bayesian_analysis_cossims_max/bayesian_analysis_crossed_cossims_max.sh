#!/bin/bash

#SBATCH --job-name=salts_exp_bayes_crossed-cossims_max
#SBATCH --output=joblogs/%x_%j.txt
#SBATCH --nodes=1
#SBATCH --ntasks=4
#SBATCH --cpus-per-task=4
#SBATCH --mem=16G
#SBATCH --partition=week
#SBATCH --time=02-00:00:00
#SBATCH --mail-type=FAIL,END,INVALID_DEPEND

module load R/4.1.0-foss-2020b

echo Running script: scripts/bayesian_analysis/bayesian_analysis_cossims_max/bayesian_analysis_crossed_cossims_max.sh

cd analysis/

Rscript Bayesian\ scripts/models-cossims_max/models_crossed_cossims_max.r
