#!/bin/bash
######### Sbatch configuration.
#
# Partition:
###SBATCH --partition=savio2
#
# Account:
###SBATCH --account=co_biostat
#
# QoS:
##SBATCH --qos=biostat_normal
###SBATCH --qos=savio_lowprio
#
###SBATCH --job-name=ck37_vim
#SBATCH --mail-user=ck37@berkeley.edu
###SBATCH --workdir=/global/home/users/alhubbard/Trauma
#
# Job output
#SBATCH --output=slurm.out
#SBATCH --error=slurm.out
#
# Wall clock limit:
#SBATCH --time=24:00:00
#
#### Done configuring sbatch.

# Output to current directory by default. Overriden by --dir option.
dir_output=.

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

# Load R if we are using the built-in R module:
# Here we are using a custom compiled version of R, so we don't load the r module.
# module load r

# Load a newer version of gcc than the default.
module load gcc/4.8.5 java lapack

R CMD BATCH --no-save --no-restore ${file}.R ${dir_output}/${file}.out
