#!/bin/bash
#
#SBATCH -N 1 # Ensure that all cores are on one machine
#SBATCH -t 1-00:00 # Runtime in D-HH:MM
#SBATCH -J Tcoffee_trim_sce
#SBATCH --cpus-per-task=4 # Request that ncpus be allocated per process.
#SBATCH --mem=60g # Memory pool for all cores (see also --mem-per-cpu)

#SBATCH --output=T-coffee_trim_id90_sce.out

module purge
module load python/ondemand-jupyter-python3.8
source activate sca



t_coffee -other_pg seq_reformat -in Eukaryota_minus_plastid_spp_MSA.fa -in2 sce_name.fa -out Eukaryota_minus_plastid_spp_MSA_Tcoffee_id90_sce.fa -action +trim _aln_%%90

cat Eukaryota_minus_plastid_spp_MSA_Tcoffee_id90_sce.fa | grep ">" | wc -l

