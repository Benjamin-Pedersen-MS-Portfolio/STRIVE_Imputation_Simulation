library(MASS) # To avoid conflict with dplyr::select
library(dplyr)

# Root folder of project
setwd("my_project/drs_60")

# Load external code
source("code/induce_missingness.R")
source("code/direct_imputation.R")
source("code/passive_imputation.R")

# Load in original data set
dat <- read.csv("data/tico_drs.csv")
# Subset to complete data
dat <- dat[rowSums(!(dat %>% dplyr::select(starts_with("dailyStatus")) %>% is.na())) == 62, ]

# Parameters - read from command line/slurm file
command_args <- commandArgs(trailingOnly = T)
sim_params <- readRDS("sim_config/alt_sim.RDS")
job_number <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID')) # Task number in job
set.seed(job_number)

for (i in 1:nrow(sim_params)) {
  print(i)
  prop_missing <- sim_params[i, "prop_missing"]
  missing_type <- sim_params[i, "missing_type"]
  
  # Induce missingness with parameters
  dat_m <- induce_missingness(dat, prop_missing, missing_type)
  
  # Direct approach
  direct_res <- analyse_dta(dat_m, dat_m$rangp, dat$ordpulmbl, dat$drs)
  result_direct <- c(job_number, prop_missing, missing_type, "direct",
                     direct_res[2, 1], direct_res[2, 2],
                     direct_res[2, 3], direct_res[2, 4],
                     direct_res[2, 5], NA)
 
  # Passive approach
  passive_imp <- passive_impute(dat_m)
  #analyze_individual_imputations(passive_imp) # troubleshoot same OR  
  passive_res <- analyze_imputed_data(passive_imp[[1]])
  passive_res_alt <- analyze_imputed_data(passive_imp[[2]])
  
  # Previous analysis function also returns complete case analysis results
  result_cca <- c(job_number, prop_missing, missing_type, "cca",
                  direct_res[1, 1], direct_res[1, 2],
                  direct_res[1, 3], direct_res[1, 4],
                  direct_res[1, 5], passive_imp[[3]])
  result_passive <- c(job_number, prop_missing, missing_type, "passive",
                      passive_res$odds_ratio, passive_res$ci_lower,
                      passive_res$ci_upper, passive_res$p_value,
                      passive_res$standard_error,# SE is log SE!
                      passive_imp[[4]]
                      ) 
  result_passive_alt <- c(job_number, prop_missing, missing_type, "passive_alt",
                      passive_res_alt$odds_ratio, passive_res_alt$ci_lower,
                      passive_res_alt$ci_upper, passive_res_alt$p_value,
                      passive_res_alt$standard_error,  # SE is log SE!
                      passive_imp[[5]]
                      ) 

  # Save results for one seed
  result <- rbind(result_direct, result_passive, result_passive_alt, result_cca)
  saveRDS(result, paste0("results/analysis_1/", missing_type, "_", prop_missing * 1000, "_", job_number, ".RDS"))
}










