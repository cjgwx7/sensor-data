#!/bin/bash

#SBATCH -p BioCompute,hpc3,hpc5,hpc6,htc4
#SBATCH -n 1
#SBATCH -c 1
#SBATCH -A animalsci
#SBATCH --mem 5G
#SBATCH -t 00-00:10
#SBATCH -o slurm-out/WaterIntakeHdRMS-Intrinsic-r-%j.out
#SBATCH --mail-type END
#SBATCH --mail-user cgrohmann@mail.missouri.edu

source activate r-sensors
Rscript ../r/WaterIntakeHdRMS-Intrinsic.R
