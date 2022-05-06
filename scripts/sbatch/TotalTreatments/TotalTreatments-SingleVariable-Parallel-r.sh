#!/bin/bash

#SBATCH -p BioCompute,hpc3,hpc5,hpc6,htc4
#SBATCH -n 1
#SBATCH -c 56
#SBATCH -A animalsci
#SBATCH --mem 50G
#SBATCH -t 00-08:00
#SBATCH -o /group/deckerlab/cjgwx7/sensor-data/scripts/sbatch/slurm-out/TotalTreatments/TotalTreatments-SingleVariable-Parallel-r-%j.out
#SBATCH --mail-type END
#SBATCH --mail-user cgrohmann@mail.missouri.edu

source activate r-sensors
Rscript "/group/deckerlab/cjgwx7/sensor-data/scripts/r/single-variable/TotalTreatments-SingleVariable-Parallel.R"
