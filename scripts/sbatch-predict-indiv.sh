#!/bin/bash

######### Sbatch configuration.
#
# Partition:
#SBATCH --partition=savio2
#
# Account:
#SBATCH --account=co_biostat
#
# QoS:
##SBATCH --qos=biostat_normal
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
####SBATCH --nodes=2
#SBATCH --nodes=4
#
# Processors: (1 node, 20 cores)
###SBATCH --ntasks=24
####SBATCH --exclusive
#
# Wall clock limit:
#SBATCH --time=24:00:00
#
#### Done configuring sbatch.

## Run command
###export R_LIBS=~/.R-packages
# module unload intel
# module load r gcc
# module load gcc/4.8.5 java
module load gcc/4.8.5 java lapack

#R --no-save -q < exsnowslurm.R > exsnowslurm.Rout
#R CMD knit  --no-save --no-restore OrpheaRepTP.Rmd test1.out

# Could prefix with "nice":
file="predict-individual"
dir_output="output"
Rscript -e "knitr::knit('$file.Rmd', '$dir_output/$file.md')" 2>&1
# Convert markdown to html once the Rmd file is complete.
Rscript -e "markdown::markdownToHTML('$dir_output/$file.md', '$dir_output/$file.html')"
