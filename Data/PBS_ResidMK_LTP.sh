#!/bin/bash

#PBS -l ncpus=24
#PBS -l walltime=100:00:00
#PBS -l mem=8GB
#PBS -N MannKendall_resid
#PBS -P RDS-FSC-CCH-RW
#PBS -m ae
#PBS -M willem.vervoort@sydney.edu.au
#PBS -q defaultQ

module load R/3.5.0

R --vanilla </project/RDS-FSC-CCH-RW/MDProjectdata/scripts/ResidualsMannkendallLTP.R> Shell_output.out
