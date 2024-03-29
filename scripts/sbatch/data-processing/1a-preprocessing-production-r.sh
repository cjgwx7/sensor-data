#!/bin/bash

#SBATCH -p hpc3,hpc5,hpc6,htc4
#SBATCH -n 1
#SBATCH -c 1
#SBATCH -A animalsci
#SBATCH --mem 1G
#SBATCH -t 00-00:10
#SBATCH -o /group/deckerlab/cjgwx7/sensor-data/scripts/r/data-processing/log/1a-preprocessing-production-r-%j.out
#SBATCH --mail-type END
#SBATCH --mail-user cgrohmann@mail.missouri.edu

source activate r-sensors
Rscript "/group/deckerlab/cjgwx7/sensor-data/scripts/r/data-processing/1a-preprocessing-production.R" "/group/deckerlab/cjgwx7/sensor-data/data/production/raw/%ORIGINAL_V1%_2023-12-10.csv"
