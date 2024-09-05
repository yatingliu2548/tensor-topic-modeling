#!/bin/bash

#SBATCH --job-name=array_K
#SBATCH --output=experiments/logs/exp_K_array_%A_%a.out
#SBATCH --error=experiments/logs/exp_K_array_%A_%a.err
#SBATCH --array=1-50
#SBATCH --time=10:00:00
#SBATCH --partition=cdonnat
#SBATCH --ntasks=1
#SBATCH --mem=10G
#SBATCH --account=pi-cdonnat

# Print the task id.
echo "My SLURM_ARRAY_TASK_ID: " $SLURM_ARRAY_TASK_ID
echo "My SLURM_ARRAY_JOB_ID: " $SLURM_ARRAY_JOB_ID
# Add lines here to run your computations
job_id=$SLURM_ARRAY_JOB_ID
module load R/4.2.0
module load gcc
module load gsl

id_experiment="${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}"
name_experiment="exp_K_$1-$2-$3-${id_experiment}"
echo "name experiment is ${name_experiment}"
cd $SCRATCH/tensor-topic-modeling/tensor-topic-modeling/

# Run one experiment  to create the dataset
Rscript synthetic_array_K.R ${SLURM_ARRAY_TASK_ID} ${name_experiment} $1 $2 $3

#Q1 = $1
#R = $2
#sparse= $3
