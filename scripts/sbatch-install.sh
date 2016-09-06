#!/bin/bash
#
# Partition:
#SBATCH --partition=savio2
#
# Account:
###SBATCH --account=co_biostat
#
# QoS:
###SBATCH --qos=biostat_normal
#SBATCH --qos=savio_lowprio
#
###SBATCH --job-name=ck37_vim
#SBATCH --mail-user=ck37@berkeley.edu
###SBATCH --workdir=/global/home/users/alhubbard/Trauma
#
# Job output
#SBATCH --output=slurm.out
#SBATCH --error=slurm.out
#
# Nodes (computers)
#SBATCH --nodes=1
#
# Processors: (1 node, 20 cores)
###SBATCH --ntasks=24
####SBATCH --exclusive
#
# Wall clock limit:
#SBATCH --time=1:00:00
#
## Run command
###export R_LIBS=~/.R-packages
# module unload intel
#module load r gcc
module load gcc/4.8.5 java lapack #mkl

R CMD BATCH --no-save --no-restore install.R install.out
