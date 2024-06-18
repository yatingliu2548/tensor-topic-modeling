#!/bin/bash

#SBATCH --job-name=array
#SBATCH --output=experiments/synthetic/logs/array_%A_%a.out
#SBATCH --error=experiments/synthetic/logs/array_%A_%a.err
#SBATCH --array=1-1
#SBATCH --time=40:00:00
#SBATCH --partition=cdonnat
#SBATCH --ntasks=1
#SBATCH --mem=6G
#SBATCH --account=pi-cdonnat
#SBATCH --qos=cdonnat
#SBATCH --mail-type=ALL
#SBATCH --mail-user=yatingliu@rcc.uchicago.edu

# Print the task id.
echo "My SLURM_ARRAY_TASK_ID: " $SLURM_ARRAY_TASK_ID
echo "My SLURM_ARRAY_JOB_ID: " $SLURM_ARRAY_JOB_ID
# Add lines here to run your computations
job_id=$SLURM_ARRAY_JOB_ID
#module load gsl
#module load R/4.2.0
#module load matlab
#module load python
source activate ${SCRATCH}/${USER}/.local/share/r-miniconda/envs/r-reticulate
#sinteractive
module load gsl
module load R/4.2.0
module load matlab
module load python
source activate ${SCRATCH}/${USER}/.local/share/r-miniconda/envs/r-reticulate
#source activate ~/.local/share/r-miniconda/envs/r-reticulate

MATLAB_PATH="/software/matlab-2023a-el8-x86_64/bin/matlab"
result_file="${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}_$1"
#result_file="${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}_$1_$2"
#result_file="${SLURM_ARRAY_JOB_ID}_${1234}_$1"
echo "result file is ${result_file}"
cd $SCRATCH/$USER/tensor-topic-modeling/
#cd topic-modeling/
working_dir="${SCRATCH}/${USER}/tensor-topic-modeling/"
#working_dir="topic-modeling/"
#Rscript synthetic_array.R $1234 $result_file $15 $MATLAB_PATH # 5 topic
Rscript synthetic_array.R $SLURM_ARRAY_TASK_ID $result_file $1 $MATLAB_PATH 
# 5 topic