#!/bin/bash

#SBATCH --job-name=array
#SBATCH --output=experiments/logs/array_%A_%a.out
#SBATCH --error=experiments/logs/array_%A_%a.err
#SBATCH --array=1-20
#SBATCH --time=35:00:00
#SBATCH --partition=cdonnat
#SBATCH --ntasks=1
#SBATCH --mem=5G
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
name_experiment="$1-$2-$3-$4-${id_experiment}"
echo "name experiment is ${name_experiment}"
cd $SCRATCH/tensor-topic-modeling/tensor-topic-modeling/

# Run one experiment  to create the dataset
Rscript synthetic_array.R ${SLURM_ARRAY_TASK_ID} ${name_experiment} $1 $2 $3 $4

#Q2 = $1
#K1 = $2
#K2 = $3
#K3 = $4
