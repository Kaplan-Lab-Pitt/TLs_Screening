#!/bin/bash
#
#SBATCH -N 1 # Ensure that all cores are on one machine
#SBATCH -t 5-00:00 # Runtime in D-HH:MM
#SBATCH -J Tcoffee_aln_OMAFFT
#SBATCH --cpus-per-task=16 # Request that ncpus be allocated per process.
#SBATCH --mem=230g # Memory pool for all cores (see also --mem-per-cpu)
#SBATCH --mail-type=END
#SBATCH --output=T-coffee_aln_OMAFFT.out

module purge

source activate sca

for file in *fa; do echo $file && t_coffee -other_pg seq_reformat -in $file -output fasta_seq > ${file/.fa/_rmGap.fa}; done

for file in *rmGap.fa; do echo $file && t_coffee -reg -seq $file -nseq 100 -tree mbed -method mafftginsi_msa -outfile ${file/.fa/_OM.fa} -outtree ${file/.fa/.mbed}; done



