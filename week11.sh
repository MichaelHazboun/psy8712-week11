#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=24
#SBATCH --mem=16gb
#SBATCH -t 01:00:00
#SBATCH --mail-type=ALL
#SBATCH --mail-user=hazbo002@umn.edu
#SBATCH -p msismall
cd ~/week11-cluster
module load R/4.3.0-openblas
Rscript week11-cluster.R
