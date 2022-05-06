#!/bin/bash

#SBATCH -p BioCompute,hpc3,hpc5,hpc6,htc4
#SBATCH -n 1
#SBATCH -c 1
#SBATCH -A animalsci
#SBATCH --mem 5G
#SBATCH -t 00-05:00
#SBATCH -o /group/deckerlab/cjgwx7/sensor-data/scripts/sbatch/slurm-out/SecondaryRespiratory/SecondaryRespiratory-Intrinsic-NegativeBinomial-r-%j.out
#SBATCH --mail-type END
#SBATCH --mail-user cgrohmann@mail.missouri.edu

source activate r-sensors
Rscript /group/deckerlab/cjgwx7/sensor-data/scripts/r/intrinsic-models/SecondaryRespiratory/NegativeBinomialSecondaryRespiratory.R SecondaryRespiratory
