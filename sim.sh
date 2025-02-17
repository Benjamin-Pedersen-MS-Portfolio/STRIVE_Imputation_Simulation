#!/bin/bash
#SBATCH --time=12:00:00
#SBATCH --mem=8G
#SBATCH --tmp=8G
#SBATCH --array=1-2000
#SBATCH --mail-type=ALL
#SBATCH --mail-user=peder422@umn.edu
#SBATCH --job-name=analysis_0
#SBATCH -A PI_userID
#SBATCH --output=/project_file_path/drs_60/slurm_output/analysis_0/%A_%a.out

date
path=/project_file_path/drs_60
# cd $path/results
module load R/4.4.0-openblas-rocky8

Rscript $path/code/simulation.R "alt_sim.RDS" > $path/R_output/analysis_0/job_$SLURM_JOB_ID\_$SLURM_ARRAY_TASK_ID.txt

date