#!/bin/bash

#PBS -l ncpus=10
#PBS -l walltime=0:10:00
#PBS -l mem=4GB
#PBS -N Test
#PBS -P RDS-FSC-CCH-RW
#PBS -m ae
#PBS -q defaultQ
#PBS -M willem.vervoort@sydney.edu.au

#module load hdf5/1.8.10
#module load netcdf/4.2.1.1
module load R/3.4.0
#module load gdal
#module load proj

R --vanilla </project/RDS-FSC-CCH-RW/MDProjectdata/testscript.R> Shell_output.out