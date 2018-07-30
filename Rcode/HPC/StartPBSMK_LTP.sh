#!/bin/bash

#PBS -l ncpus=13
#PBS -l walltime=250:00:00
#PBS -l mem=32GB
#PBS -N MannKendall
#PBS -P RDS-FSC-CCH-RW
#PBS -m ae
#PBS -M willem.vervoort@sydney.edu.au
#PBS -q defaultQ

module load R/3.4.0

R --vanilla </project/RDS-FSC-CCH-RW/MDProjectdata/MannkendallLTP.R> Shell_output.out
