#!/bin/bash

#PBS -P RDS-FSC-CCH-RW
#PBS -N RR_residuals
#PBS -l select=1:ncpus=20:mem=8GB
#PBS -l walltime=60:00:00
#PBS -m ae
#PBS -M willem.vervoort@sydney.edu.au
#PBS -q defaultQ

module load R/3.4.0

cd $PBS_O_WORKDIR

Rscript RRmodelRes_MK_LTP.R
