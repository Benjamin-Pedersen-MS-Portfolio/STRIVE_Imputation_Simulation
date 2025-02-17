# Create df of simulation params
# Not meant to be run as slurm script

# Preliminary results
prelim_sim <- data.frame(missing_type = rep(c("mar", "mcar"), each = 6),
                         prop_missing = rep(c(.01, .1, .2, .3, .4, .5), 2))
saveRDS(prelim_sim, "sim_config/prelim_sim.RDS")

# Just 1% missing metrics as a gut check
sim_config <- data.frame(missing_type = c("mar", "mcar"),
                         prop_missing = rep(c(.01), 2))
saveRDS(sim_config, "sim_config/prelim_01.RDS")


# New 10/30/24
prelim_sim <- data.frame(missing_type = rep(c("mar", "mcar"), each = 10),
                         prop_missing = rep(c(.005, .01, .1, .2, .3, .4, .5, .6, .7, .8), 2))
saveRDS(prelim_sim, "sim_config/alt_sim.RDS")