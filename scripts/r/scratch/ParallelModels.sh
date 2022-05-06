#!/bin/bash

#SBATCH -p BioCompute,hpc3,hpc5,hpc6,htc4
#SBATCH -n 1
#SBATCH -c 50
#SBATCH -A animalsci
#SBATCH --mem 50G
#SBATCH -t 00-10:00
#SBATCH -o ParallelModels-r-%j.out
#SBATCH --mail-type END
#SBATCH --mail-user cgrohmann@mail.missouri.edu

source activate r-sensors
Rscript ParallelModels.R
