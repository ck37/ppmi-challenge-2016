
#######################
# Setup
ACCOUNT=co_biostat
PARTITION=savio2
SCRIPT_DIR=scripts
OUTPUT_DIR=output

#######################
# Targets

all: install data analysis

install: install.R ${SCRIPT_DIR}/sbatch-install.sh
	sbatch ${SCRIPT_DIR}/sbatch-install.sh

#setup: setup-cluster.sh
#	bash $^

data: merge-data create-dataset

create-dataset: create-dataset.Rmd
	Rscript -e "knitr::knit('create-dataset.Rmd')" 2>&1

merge-data:
	sbatch ${SCRIPT_DIR}/sbatch-rmd.sh --file=merge-data --dir=${OUTPUT_DIR}
	# Could prefix with "nice":
	#Rscript -e "knitr::knit('merge-data.Rmd', 'output/merge-data.md')" 2>&1
	#sbatch -A $ACCOUNT -p $PARTITION --qos=biostat_normal -N 1 -t 5:00:00 --wrap "Rscript -e \"knitr::knit('merge-data.Rmd')\" 2>&1"
	#Rscript -e "markdown::markdownToHTML('merge-data.md', 'merge-data.html')"

vim: variable-importance.Rmd
	sbatch scripts/sbatch-vim.sh

predict-cumu: predict-cumulative.Rmd
	sbatch scripts/sbatch-predict-cumu.sh

predict-indiv: create-dataset.Rmd predict-individual.Rmd
	sbatch scripts/sbatch-predict-indiv.sh

bash:
	# Start a bash session with 2 nodes, for up to 5 hours.
	srun -A $ACCOUNT -p $PARTITION  -N 2 -t 5:00:00 --pty bash

.PHONY : clean
clean:
	rm -f *.Rout
	rm -f slurm*.out
	rm -f install*.out
	rm -f cache/*
	rm -f output/*.md
