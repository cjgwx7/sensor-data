#!/bin/bash

#SBATCH -p BioCompute,hpc3,hpc5,hpc6,htc4
#SBATCH -n 1
#SBATCH -c 1
#SBATCH -A animalsci
#SBATCH --mem 1G
#SBATCH -t 00-00:10
#SBATCH -o /group/deckerlab/cjgwx7/sensor-data/scripts/r/single-variable/ExtremeHighMortalityDay/log/ExtremeHighMortalityDay-STD-XC-L-r-%j.out
#SBATCH --mail-type END
#SBATCH --mail-user cgrohmann@mail.missouri.edu

source activate r-sensors
Rscript /group/deckerlab/cjgwx7/sensor-data/scripts/r/single-variable/ExtremeHighMortalityDay/single-variable-STD-XC-L.R \
--data /group/deckerlab/cjgwx7/sensor-data/data/master/master-2022-07-20-lagged-standardized.csv \
--data-export /group/deckerlab/cjgwx7/sensor-data/results/single-variable/ExtremeHighMortalityDay/ExtremeHighMortalityDay-SingleVariable-STD-XC-L