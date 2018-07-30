#!/bin/bash

#PBS -l ncpus=10
#PBS -l walltime=110:00:00
#PBS -l mem=32GB
#PBS -N Simhyd
#PBS -P RDS-FSC-CCH-RW
#PBS -m ae
#PBS -M willem.vervoort@sydney.edu.au
# #PBS -q defaultQ

module load R/3.4.0

R --vanilla </project/RDS-FSC-CCH-RW/MDProjectdata/SimHydGridmodelling7.R> Shell_output.out
