#!/bin/bash
#
#SBATCH -N 1 # Ensure that all cores are on one machine
#SBATCH -t 1-00:00 # Runtime in D-HH:MM
#SBATCH -J scaProcess
#SBATCH --cpus-per-task=8 # Request that ncpus be allocated per process.
#SBATCH --mem=130g # Memory pool for all cores (see also --mem-per-cpu)

#SBATCH --output=scaProcess.out

module purge

source activate sca



for file in ./Inputs/05112022_5thAttempt_everyMSA/*.fa; do echo $file && ./scaProcessMSA.py -a $file; done

