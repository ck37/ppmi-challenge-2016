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
#SBATCH --qos=biostat_normal
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
#SBATCH --time=8:00:00
#
#### Done configuring sbatch.

## Run command
###export R_LIBS=~/.R-packages
#module unload intel
#module load r gcc
module load gcc/4.8.5 java lapack mkl

#R --no-save -q < exsnowslurm.R > exsnowslurm.Rout
#R CMD knit  --no-save --no-restore OrpheaRepTP.Rmd test1.out

# Could prefix with "nice":
file="variable-importance"
Rscript -e "knitr::knit('$file.Rmd', 'output/$file.md)" 2>&1
# Convert markdown to html once the Rmd file is complete.
Rscript -e "markdown::markdownToHTML('output/$file.md', 'output/$file.html')"
