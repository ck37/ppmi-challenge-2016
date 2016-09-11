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
    shopt -s extglob    # Turn on extended pattern support
    # Remove .Rmd if it's included in the filename.
    file=${file/\.Rmd$//)
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
module load gcc/4.8.5

# Load Java if any R packages need RJava (bartMachine, h2o, etc.)
module load java

# Load a better linear algebra system.
module load lapack

# knitr does not support subdirectories - need to use cd.
cd $dir_output
# This assumes we are in a subdirectory; remove "../" if not.
Rscript -e "knitr::knit('../$file.Rmd', '$file.md')" 2>&1
# Convert markdown to html once the Rmd file is complete.
Rscript -e "markdown::markdownToHTML('$file.md', '$file.html')"
