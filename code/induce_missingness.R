# Nidhi Pai and Tanvi Mehta
# 08/17/2024

# Calculate proportion missing based on parameters -----------------------------

# If mcar, based on a, b, and x_beta = ht_alpha = 0
# If mar, x_beta and ht_alpha are calculated based on subjects
f <- function(a, b, x_beta, ht_alpha) {
  gamma_vec <- c(1:60) * a + b
  p_vec <- sapply(1:length(x_beta), function(i) {
    eta_i <- x_beta[i] + ht_alpha[i, ] + gamma_vec
    prob_vec <- 1 / (1 + exp(eta_i))  # Prob of not dropping at each day
    prob_missing_q <- 1 - prod(prob_vec)
    prob_missing_q
  })
  mean(p_vec)
}

# Main function ----------------------------------------------------------------


# Helper that calculates covariate matrices, etc, once
# induce_missingness_helper <- function(dat) {
#   ## Calculate ht_alpha and x_beta for MAR
#   # Standardize covariates
#   scale_age <- (dat$age - mean(dat$age)) / sd(dat$age)
#   vaccat_dummy <- cbind(dat$vaccat == 1, dat$vaccat == 3)
#   
#   all_cs <- matrix(nrow = nrow(dat), ncol = 61) # Cumul. days home
#   all_at_home <- matrix(nrow = nrow(dat), ncol = 61) # Curr location
#   for (i in 1:nrow(dat)) {
#     # Cumulative days at home
#     locs <- dat[i, ] %>%
#       select(starts_with("dailyStatus")) %>%
#       select(!dailyStatus61) %>%
#       t() %>% c()
#     cs <- cumsum(locs == "out of hospital")
#     # Indicator if at home
#     at_home <- I(locs == "out of hospital")
#     all_cs[i, ] <- cs
#     all_at_home[i, ] <- at_home
#   }
#   
#   # Remove day 0 bc no dropout on day 0
#   all_cs <- all_cs[, 2:61]
#   all_at_home <- all_at_home[, 2:61]
#   
#   # Calculate eta_t for each patient
#   x_beta_mar <- scale_age + 1 * vaccat_dummy[, 1]  + .5 * vaccat_dummy[, 2]
#   ht_alpha_mar <- sapply(1:nrow(dat), function(i) {
#     all_cs[i, ] * .04 + all_at_home[i, ] * 1
#   }) %>% t()
#    
#   ## ht_alpha and x_beta for MCAR
#   x_beta_mcar <- rep(0, nrow(dat))
#   ht_alpha_mcar <- matrix(0, nrow = nrow(dat), ncol = 60)
#   
#   # Find the day each participant died
#   day_died_vec <- rep(NA, nrow(dat))
#   for (i in 1:nrow(dat)) {
#     id <- dat[i, "npid"]
#     # Find the day participant died
#     vitals <- dat %>%
#       filter(npid == id) %>%
#       select(starts_with("dailyStatus")) %>%
#       t() %>% c()
#     match_res <- which(vitals == "dead") # Index
#     if (length(match_res) == 0) { # Alive through study
#       day_died <- 61
#     } else {
#       day_died <- min(match_res) - 1
#     }
#     day_died_vec[i] <- day_died
#   }
#   
#   saveRDS(x_beta_mcar, "data/tm_x_beta_mcar.RDS")
#   saveRDS(ht_alpha_mcar, "data/tm_ht_alpha_mcar.RDS")
#   saveRDS(x_beta_mar, "data/tm_x_beta_mar.RDS")
#   saveRDS(ht_alpha_mar, "data/tm_ht_alpha_mar.RDS")
#   saveRDS(day_died_vec, "data/tm_day_died_vec.RDS")
# }

induce_missingness_helper <- function(dat) {
  ## Calculate ht_alpha and x_beta for MAR
  # Standardize covariates
  # scale_age <- (dat$age - mean(dat$age)) / sd(dat$age)
  
  # 1: Invasive ventilation or ECMO (reference)
  # 2: Non-invasive ventilation
  # 3: High-flow nasal cannula (HFNC) oxygen device
  # 4: Conventional supplemental oxygen
  # 5: Not receiving supplemental oxygen
  # assuming less intense oxygen makes you more likely to drop out
  ordpulmbl_dummy <- cbind(dat$ordpulmbl == 2, 
                           dat$ordpulmbl == 3,
                           dat$ordpulmbl == 4,
                           dat$ordpulmbl == 5)
  
  all_cs <- matrix(nrow = nrow(dat), ncol = 61) # Cumul. days home
  all_at_home <- matrix(nrow = nrow(dat), ncol = 61) # Curr location
  for (i in 1:nrow(dat)) {
    # Consecutive days at home
    locs <- dat[i, ] %>% dplyr::select(starts_with("dailyStatus")) %>% dplyr::select(!dailyStatus61) %>% t() %>% c()
    #cs <- cumsum(locs == "out of hospital")
    all_cs[i, 1] = (locs[1] == "out of hospital")
    for(d in 2:61) all_cs[i,d] = ifelse(locs[d] == "out of hospital",all_cs[i,d-1]+1,0)
    # Indicator if at home
    all_at_home[i, ] <- I(locs == "out of hospital")
  }
  
  # Remove day 0 bc no dropout on day 0
  #all_cs <- all_cs[, 2:61]
  #all_at_home <- all_at_home[, 2:61]
  
  # Calculate eta_t for each patient, more likely to drop out with higher baseline respiratory support requirements
  #x_beta_mar <-  4 * ordpulmbl_dummy[, 1]  + 
  #  6 * ordpulmbl_dummy[, 2] + 
  #  7 * ordpulmbl_dummy[, 3] +
  #  10 * ordpulmbl_dummy[, 4] 
  x_beta_mar <-  4 * ordpulmbl_dummy[, 1]  + 
    4.25 * ordpulmbl_dummy[, 2] + 
    4.5 * ordpulmbl_dummy[, 3] +
    5 * ordpulmbl_dummy[, 4] 
  
  # Calculate alpha_t for each patient, more likely to drop out when out of hospital and increasingly so when out for 7+ consecutive days.
  ht_alpha_mar <- sapply(1:nrow(dat), function(i) {
    #all_cs[i, ] * .04 + all_at_home[i, ] * 1 #Depends on number of consecutive days at home
    (all_cs[i, ]>=7)*.5 + all_at_home[i, ] * 1 #Depends on 7+ consecutive days at home
  }) %>% t()
  ht_alpha_mar = ht_alpha_mar[,-61] #Last day doesn't matter as can't drop out subsequently
  
  ## ht_alpha and x_beta for MCAR
  x_beta_mcar <- rep(0, nrow(dat))
  ht_alpha_mcar <- matrix(0, nrow = nrow(dat), ncol = 60)
  
  # Find the day each participant died
  day_died_vec <- rep(NA, nrow(dat))
  for (i in 1:nrow(dat)) {
    id <- dat[i, "npid"]
    # Find the day participant died
    vitals <- dat %>%
      filter(npid == id) %>%
      dplyr::select(starts_with("dailyStatus")) %>%
      t() %>% c()
    match_res <- which(vitals == "dead") # Index
    if (length(match_res) == 0) { # Alive through study
      day_died <- 61
    } else {
      day_died <- min(match_res) - 1
    }
    day_died_vec[i] <- day_died
  }
  
  #Tom can't write to these due to permisson, so writing things to data directory instead
  #  saveRDS(x_beta_mcar, "data/ordpulmbl_update/x_beta_mcar.RDS")
  #  saveRDS(ht_alpha_mcar, "data/ordpulmbl_update/ht_alpha_mcar.RDS")
  #  saveRDS(x_beta_mar, "data/ordpulmbl_update/x_beta_mar.RDS")
  #  saveRDS(ht_alpha_mar, "data/ordpulmbl_update/ht_alpha_mar.RDS")
  #  saveRDS(day_died_vec, "data/ordpulmbl_update/day_died_vec.RDS")
  saveRDS(x_beta_mcar, "data/x_beta_mcar.RDS")
  saveRDS(ht_alpha_mcar, "data/ht_alpha_mcar.RDS")
  saveRDS(x_beta_mar, "data/x_beta_mar.RDS")
  saveRDS(ht_alpha_mar, "data/ht_alpha_mar.RDS")
  saveRDS(day_died_vec, "data/day_died_vec.RDS")
  
}

induce_missingness <- function(dat, prop_missing, missing_type) {
  # Read in precalculated matrices
  #print(paste0("data/ordpulmbl_update/x_beta_", missing_type, ".RDS"))
  #x_beta <- readRDS(paste0("data/ordpulmbl_update/x_beta_", missing_type, ".RDS"))
  #ht_alpha <- readRDS(paste0("data/ordpulmbl_update/ht_alpha_", missing_type, ".RDS"))
  #day_died_vec <- readRDS("data/ordpulmbl_update/day_died_vec.RDS")
  print(paste0("data/x_beta_", missing_type, ".RDS"))
  x_beta <- readRDS(paste0("data/x_beta_", missing_type, ".RDS"))
  ht_alpha <- readRDS(paste0("data/ht_alpha_", missing_type, ".RDS"))
  day_died_vec <- readRDS("data/day_died_vec.RDS")
  
  
  # Find b based on prop_missing
  a <- -.05
  b <- uniroot(function(x) f(a, x, x_beta, ht_alpha) - prop_missing, c(-150, 20))$root
  gamma_vec <- c(1:60) * a + b
  
  # Add missingness
  for (i in 1:nrow(dat)) {
    eta_i <- x_beta[i] + ht_alpha[i, ] + gamma_vec
    # Apply inv logit to built p_t from eta_t
    prob_vec <- 1 / (1 + exp(eta_i)) # Prob of not missing
    day_died <- day_died_vec[i]
    # Flip coins for all days
    dropout_indicator <- sapply(prob_vec, function(p) {
      rbinom(1, 1, 1 - p)
    })
    # Find first day of heads for dropout
    if (sum(dropout_indicator) != 0) { # Dropping out
      dropout_day <- which(dropout_indicator == 1)[1]
      if (day_died >= dropout_day) {
        # Make columns missing
        days_missing <- dropout_day:61
        cols_missing <- c(paste0("dailyStatus", days_missing), 
                          paste0("locStatus", days_missing),
                          paste0("vitalStatus", days_missing))
        dat[i, cols_missing] <- NA
      }
    }
  }
  
  # Update drs 60 variable
  dat <- drs60(dat)
  dat
  #Working as expected; higher missingness among higher OrdPulm; lower true DRS among those missing DRS within OrdPulm strata
  #prop.table(table(dat$ordpulmbl,is.na(dat$drs),useNA = "ifany"),1)
  #aggregate(drs60~ordpulmbl+is.na(drs),data=dat,mean)
}

# drs60 function with one change -----------------------------------------------
# The change is the second if statement - check missingness at day 60 & 61

drs60 <- function(df) {
  statusvars <- as.matrix(df %>% dplyr::select(c(paste0("dailyStatus", 0:61)))) # pull out status variables as a matrix
  drs <- rep(-1, nrow(statusvars))
  
  for (i in 1:nrow(statusvars)) { # for each participant
    if ("dead" %in% statusvars[i,1:61] ) { # if they died on or before day 60
      drs[i] <- 62
    } else if (is.na(statusvars[i,61]) | is.na(statusvars[i,62])) { # if their status on day 60 or 61 is unknown
      drs[i] <- NA
    } else if (statusvars[i,61]=="in hospital" & statusvars[i,62]=="out of hospital") { # in the hospital & then discharged on day 60
      drs[i] <- 60
    } else if (statusvars[i,61]=="in hospital" & statusvars[i,62]=="in hospital") { # in the hospital on d60 & stayed thru d61
      drs[i] <- 61
    } else {
      for (j in 60:0) { # each day
        if (drs[i]== -1) { # if DRS-60 has not yet been assigned for this person
          if (is.na(statusvars[i,j])) {
            drs[i] <- NA
          } else if (statusvars[i,j] == "in hospital") {
            drs[i] <- j - 1
          }
        }
      }
    }
  }
  df$drs<- drs
  return(df)
}