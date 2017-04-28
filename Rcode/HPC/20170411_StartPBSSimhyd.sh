#!/bin/bash
#PBS -l ncpus=10
#PBS -l walltime=45:00:00
#PBS -l mem=32GB
#PBS -P xa5
#PBS -q normal
#PBS -M willem.vervoort@sydney.edu.au

#module load hdf5/1.8.10
#module load netcdf/4.2.1.1
module load R/3.3.0
#module load gdal
#module load proj

R --vanilla < /home/562/wxv562/MD_ProjectRCode/HPCSimHydmodelling.R > Shell_output.out
