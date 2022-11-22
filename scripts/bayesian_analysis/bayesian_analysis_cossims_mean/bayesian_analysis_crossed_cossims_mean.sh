#!/bin/bash

#SBATCH --job-name=salts_exp_bayes_crossed-cossims_mean
#SBATCH --output=joblogs/%x_%j.txt
#SBATCH --nodes=1
#SBATCH --ntasks=4
#SBATCH --cpus-per-task=4
#SBATCH --mem=16G
#SBATCH --partition=week
#SBATCH --time=02-00:00:00
#SBATCH --mail-type=FAIL,END,INVALID_DEPEND

module load R/4.1.0-foss-2020b

echo Running script: scripts/bayesian_analysis/bayesian_analysis_cossims_mean/bayesian_analysis_crossed_cossims_mean.sh

cd analysis/

Rscript Bayesian\ scripts/models-cossims_mean/models_crossed_cossims_mean.r
