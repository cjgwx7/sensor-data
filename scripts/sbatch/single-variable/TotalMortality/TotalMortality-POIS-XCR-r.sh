#!/bin/bash

#SBATCH -p BioCompute,hpc3,hpc5,hpc6,htc4
#SBATCH -n 1
#SBATCH -c 1
#SBATCH -A animalsci
#SBATCH --mem 10G
#SBATCH -t 00-24:00
#SBATCH -o /group/deckerlab/cjgwx7/sensor-data/scripts/r/single-variable/TotalMortality/log/TotalMortality-POIS-XCR-r-%j.out
#SBATCH --mail-type END
#SBATCH --mail-user cgrohmann@mail.missouri.edu

source activate r-sensors
Rscript /group/deckerlab/cjgwx7/sensor-data/scripts/r/single-variable/TotalMortality/single-variable-POIS-XCR.R \
--data /group/deckerlab/cjgwx7/sensor-data/data/master/master-lagged-RUN=2023-01-12-21-07-38_MAXDATE=2022-07-20.csv \
--data-export /group/deckerlab/cjgwx7/sensor-data/results/single-variable/TotalMortality/TotalMortality-POIS-XCR-RUN=2023-01-12-21-07-38_MAXDATE=2022-07-20