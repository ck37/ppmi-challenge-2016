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

for i in "$@"
do
case $i in
    -f=*|--file=*)
    file="${i#*=}"
    ;;
    -d=*|--dir=*)
    dir_output="${i#*=}"
    ;;
esac
done

## Run command
# module load r gcc
# module load gcc/4.8.5 java
module load gcc/4.8.5 java lapack

# knitr does not support subdirectories - need to use cd.
cd $dir_output
# This assumes we are in a subdirectory; remove "../" if not.
Rscript -e "knitr::knit('../$file.Rmd', '$file.md')" 2>&1
# Convert markdown to html once the Rmd file is complete.
Rscript -e "markdown::markdownToHTML('$file.md', '$file.html')"
