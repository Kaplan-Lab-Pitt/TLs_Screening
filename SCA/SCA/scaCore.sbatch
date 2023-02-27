#!/bin/bash
#
#SBATCH -N 1 # Ensure that all cores are on one machine
#SBATCH -t 4-00:00 # Runtime in D-HH:MM
#SBATCH -J scaCore
#SBATCH --cpus-per-task=16 # Request that ncpus be allocated per process.
#SBATCH --mem=230g # Memory pool for all cores (see also --mem-per-cpu)
#SBATCH --output=scaCore-%A_%a.out
#SBATCH --array=0-8 # job array index

module purge

source activate sca


names=($(cat ./output/jobs))

echo ${names[${SLURM_ARRAY_TASK_ID}]}

./scaCore.py -i ./output/${names[${SLURM_ARRAY_TASK_ID}]}.db


