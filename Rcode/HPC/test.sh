#!/bin/bash

#PBS -l ncpus=5
#PBS -l walltime=0:10:00
#PBS -l mem=16GB
#PBS -N HPCExample
#PBS -P RDS-FSC-CCH-RW
#PBS -m ae
#PBS -M willem.vervoort@sydney.edu.au
#PBS -q defaultQ

module load R/3.4.0

R --vanilla </project/RDS-FSC-CCH-RW/MDProjectdata/testscript.R> Shell_output.out