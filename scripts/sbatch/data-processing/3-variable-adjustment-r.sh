#!/bin/bash

#SBATCH -p BioCompute,hpc3,hpc5,hpc6,htc4
#SBATCH -n 1
#SBATCH -c 1
#SBATCH -A animalsci
#SBATCH --mem 1G
#SBATCH -t 00-10:00
#SBATCH -o /group/deckerlab/cjgwx7/sensor-data/scripts/r/data-processing/log/3-variable-adjustment-r-%j.out
#SBATCH --mail-type END
#SBATCH --mail-user cgrohmann@mail.missouri.edu

source activate r-sensors
Rscript /group/deckerlab/cjgwx7/sensor-data/scripts/r/data-processing/3-variable-adjustment.R \
--data /group/deckerlab/cjgwx7/sensor-data/data/master/master-RUN=2023-12-21-01-49-36_MAXDATE=2023-10-14.RData \
--data-export /group/deckerlab/cjgwx7/sensor-data/data/master/master-adjusted-RUN=2023-12-21-01-49-36_MAXDATE=2023-10-14