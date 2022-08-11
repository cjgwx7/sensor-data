#!/bin/bash

#SBATCH -p BioCompute,hpc3,hpc5,hpc6,htc4
#SBATCH -n 1
#SBATCH -c 1
#SBATCH -A animalsci
#SBATCH --mem 1G
#SBATCH -t 00-00:10
#SBATCH -o /group/deckerlab/cjgwx7/sensor-data/scripts/r/statistical-modeling/Residual_ReHS/log/Residual_ReHS-r-%j.out
#SBATCH --mail-type END
#SBATCH --mail-user cgrohmann@mail.missouri.edu

source activate r-sensors
Rscript /group/deckerlab/cjgwx7/sensor-data/scripts/r/statistical-modeling/Residual_ReHS/Residual_ReHS.R \
--data /group/deckerlab/cjgwx7/sensor-data/data/master/master-2022-07-19.RData \
--fit-list /group/deckerlab/cjgwx7/sensor-data/scripts/r/statistical-modeling/Residual_ReHS/fit_list.csv \
--results /group/deckerlab/cjgwx7/sensor-data/results/statistical-modeling/Residual_ReHS/Residual_ReHS-CrossValidation.xlsx
