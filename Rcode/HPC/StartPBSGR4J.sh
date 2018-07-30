#!/bin/bash

#PBS -l ncpus=10
#PBS -l walltime=45:00:00
#PBS -l mem=32GB
#PBS -N GR4J
#PBS -P RDS-FSC-CCH-RW
#PBS -m ae
#PBS -M willem.vervoort@sydney.edu.au
#PBS -q defaultQ

module load R/3.4.0

R --vanilla < /project/RDS-FSC-CCH-RW/MD_Projectdata/GR4Jmodelling.R > Shell_output.out