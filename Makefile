
#######################
# Setup

######
# Savio configuration.
ACCOUNT=co_biostat
PARTITION=savio2

# This allows us to override the default QOS by setting an environmental variable.
# e.g. we run in BASH: "export QOS=biostat_normal"
ifndef QOS
	# Choose one QOS and comment out the other.
	#QOS=biostat_normal
	QOS=savio_lowprio
endif

SBATCH=sbatch -A ${ACCOUNT} -p ${PARTITION} --qos ${QOS}

######
# Makefile configuration.
SCRIPT_DIR=scripts
OUTPUT_DIR=output


#######################
# Targets

all: setup data analysis

data: merge-data create-dataset

analysis: predict-indiv predict-cumu vim

setup: setup.R
	${SBATCH} --nodes 1 ${SCRIPT_DIR}/sbatch-r.sh --file=$<

# Dependencies: merge-data
create-dataset: create-dataset.Rmd
	${SBATCH} --nodes 1 ${SCRIPT_DIR}/sbatch-rmd.sh --file=$< --dir=${OUTPUT_DIR}

# Dependencies: install
merge-data: merge-data.Rmd
	${SBATCH} --nodes 1 ${SCRIPT_DIR}/sbatch-rmd.sh --file=merge-data --dir=${OUTPUT_DIR}

# Dependencies: create-dataset
vim: variable-importance.Rmd
	${SBATCH} --nodes 2 ${SCRIPT_DIR}/sbatch-rmd.sh --file=variable-importance --dir=${OUTPUT_DIR}

# Dependencies: create-dataset
predict-cumu: predict-cumulative.Rmd
	${SBATCH} --nodes 6 ${SCRIPT_DIR}/sbatch-rmd.sh --file=predict-cumulative --dir=${OUTPUT_DIR}

# Dependencies: create-dataset
predict-indiv: predict-individual.Rmd
	${SBATCH} --nodes 6 ${SCRIPT_DIR}/sbatch-rmd.sh --file=predict-individual --dir=${OUTPUT_DIR}

bash:
	# Start a bash session with 2 nodes, for up to 5 hours.
	srun -A ${ACCOUNT} -p ${PARTITION} --qos $QOS  -N 2 -t 5:00:00 --pty bash

# Next line ensures that this rule works even if there's a file named "clean".
.PHONY : clean
clean:
	rm -f *.Rout
	rm -f slurm*.out
	rm -f install*.out
	rm -f cache/*
	rm -f output/*.md
