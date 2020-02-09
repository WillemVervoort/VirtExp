#!/bin/bash

#PBS -P RDS-FSC-CCH-RW
#PBS -N RR_residuals6
#PBS -l select=1:ncpus=10:mem=8GB
#PBS -l walltime=30:00:00
#PBS -m ae
#PBS -M willem.vervoort@sydney.edu.au
#PBS -q defaultQ

module load R/3.4.0

cd $PBS_O_WORKDIR

Rscript RRmodelRes_MK_LTP6.R
