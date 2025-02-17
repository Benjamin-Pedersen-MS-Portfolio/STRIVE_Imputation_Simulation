# Define Functions --------------------
# Passive Imputation:
# Passive imputation function
passive_impute <- function(data, m = 50) {
  
  # Inputs:
  #   data: A dataframe containing the variables ordpulmbl, active, and
  #         dailyStatus0 to dailyStatus61
  #   m: Number of imputations (default: 50)
  
  # Output:
  #   A mids object containing the imputed datasets
  
  # Prepare data
  use.dat <- data %>%
    dplyr::select(ordpulmbl, active, dailyStatus0:dailyStatus61) %>%
    as_tibble() %>%
    mutate(across(dailyStatus0:dailyStatus61,
                  ~factor(., levels = c("dead", "in hospital",
                                        "out of hospital"))),
           ordpulmbl = as.factor(ordpulmbl),
           active = as.factor(active))
  
  # Add indicator(s) for being home (4+ and) 7+ consecutive days
  #home4.dat = sapply(5:61,function(d) rowSums(use.dat[,paste0("dailyStatus",d-1:4)]=="out of hospital")==4); colnames(home4.dat) = paste0("home4D",5:61)
  home7.dat = sapply(8:61,function(d) rowSums(use.dat[,paste0("dailyStatus",d-1:7)]=="out of hospital")==7); colnames(home7.dat) = paste0("home7D",8:61)
  #col.order = c("ordpulmbl","active",paste0("dailyStatus",0:4),sapply(5:7,function(d) paste0(c("home4D","dailyStatus"),d)),sapply(8:61,function(d) paste0(c("home4D","home7D","dailyStatus"),d)))
  col.order = c("ordpulmbl","active",paste0("dailyStatus",0:7),sapply(8:61,function(d) paste0(c("home7D","dailyStatus"),d)))
  #use.dat = cbind(use.dat,home4.dat,home7.dat)[,col.order] %>% as_tibble() %>% mutate_all(as.factor)
  use.dat = cbind(use.dat,home7.dat)[,col.order] %>% as_tibble() %>% mutate_all(as.factor) 
  
  # Predictive matrix based off of previous day status
  imp.dat = mice(use.dat, maxit=0, print=FALSE, remove.collinear = FALSE)
  method = imp.dat$method
  method[paste0("dailyStatus",0:61)] = "polyreg"
  method[paste0("home7D",8:61)] = sapply(8:61,function(d) paste("~ I(",paste(paste0(paste0("dailyStatus",d-1:7),"=='out of hospital'"),collapse=" & "),")",sep=""))
  #method
  
  pm = 0 * imp.dat$predictorMatrix
  pm[ ,c("ordpulmbl", "active")] = 1
  pm[paste0("dailyStatus", 1:61), paste0("dailyStatus", 0:60)] = diag(61)
  pm[paste0("dailyStatus", 8:61),paste0("home7D", 8:61)] = diag(54) 
  diag(pm) = 0
  
  #imp.dat = mice(use.dat, pred=pm, maxit=0, print=FALSE, remove.collinear=FALSE, method=method)
  #imp.dat$formulas
  #imp.dat$method
  
  ## Passive Imputation
  # Perform imputation
  imp.dat = mice(use.dat, pred=pm, m=m, maxit=1, remove.collinear = FALSE, print = FALSE, method=method)
  
  # Calculate DRS-60: Tom's updated definition to match dataset, 10/31
  long.imp.dat = complete(imp.dat, "long", include = TRUE)
  long.imp.dat$drs = factor(
    apply(long.imp.dat[, grep("dailyStatus", names(long.imp.dat))], 1, function(status) {
      if ("dead" %in% status[1:61]) { # If patient died within 60 days, DRS is 62
        return(62)
      } else if (is.na(status[61])) {
        return(NA)  # If patient's status was unknown on day 60, DRS is NA [only for original dataset where (.imp==0)]
      } else if (status[61] == "in hospital" & status[62] == "out of hospital") {
        return(60)  # If patient was in hospital on day 60 but discharged that day, DRS is 60
      } else if (status[61] == "in hospital") {
        return(61)  # If patient was in hospital on day 60, DRS is 61
      } else {  # Otherwise, DRS is the last day the patient was in hospital (need to subtract one to account for day 0)
        last_hospital_day <- max(which(status[-62] == "in hospital"), 0)-1
        return(last_hospital_day)
      }
    }),
    levels = 0:62
  )
  
  ## Passive Imputation Alt
  #Set dailyStatus to NA on days after death
  for(i in which(rowSums(use.dat[,paste0("dailyStatus",0:61)]=="dead",na.rm=TRUE)>0)) use.dat[i,paste0("dailyStatus",0:61)[-c(1:which(use.dat[i,paste0("dailyStatus",0:61)]=="dead")[1])]] = NA
  #View(use.dat[which(rowSums(use.dat[,paste0("dailyStatus",0:61)]=="dead",na.rm=TRUE)>0),])
  
  #Do not impute dailyStatus after death
  where.to.impute = is.na(use.dat); where.to.impute[which(rowSums(use.dat[,paste0("dailyStatus",0:61)]=="dead",na.rm=TRUE)>0),]=FALSE
  
  # Set up imputation model, predictions based off of previous day status
  #  imp.dat.alt = mice(use.dat, maxit=0, print=FALSE, remove.collinear = FALSE)
  #  pm = 0 * imp.dat.alt$predictorMatrix
  #  pm[ ,c("ordpulmbl", "active")] = 1
  #  pm[paste0("dailyStatus", 1:61), paste0("dailyStatus", 0:60)] = diag(61)
  
  # Perform imputation
  # imp.dat.alt = mice(use.dat, pred=pm, m=m, maxit=1, where=where.to.impute, remove.collinear = FALSE, print = FALSE)
  #imp.dat.alt = mice(use.dat %>% droplevels(), pred = pm, m = m, maxit = 1, where = where.to.impute, remove.collinear = FALSE)
  imp.dat.alt = mice(use.dat, pred=pm, m=m, maxit=1, where=where.to.impute, remove.collinear = FALSE, print = FALSE, method=method)
  
  #Check that polyreg is just dropping a level from the dailyStatus covariate as there will be no "dead" values since these people will have a missing outcome value
  #nnet::multinom(relevel(dailyStatus1,ref="out of hospital")~relevel(dailyStatus0,ref="out of hospital")+active+ordpulmbl,data=use.dat)
  # Calculate DRS-60: Tom's updated definition to match dataset, 10/31
  long.imp.dat.alt = complete(imp.dat.alt, "long", include = TRUE)
  long.imp.dat.alt$drs = factor(
    apply(long.imp.dat.alt[, grep("dailyStatus", names(long.imp.dat.alt))], 1, function(status) {
      if ("dead" %in% status[1:61]) { # If patient died within 60 days, DRS is 62
        return(62)
      } else if (is.na(status[61])) {
        return(NA)  # If patient's status was unknown on day 60, DRS is NA [only for original dataset where (.imp==0)]
      } else if (status[61] == "in hospital" & status[62] == "out of hospital") {
        return(60)  # If patient was in hospital on day 60 but discharged that day, DRS is 60
      } else if (status[61] == "in hospital") {
        return(61)  # If patient was in hospital on day 60, DRS is 61
      } else {  # Otherwise, DRS is the last day the patient was in hospital (need to subtract one to account for day 0)
        last_hospital_day <- max(which(status[-62] == "in hospital"), 0)-1
        return(last_hospital_day)
      }
    }),
    levels = 0:62
  )
  
  # Return the mids object with DRS-60
  return(list(
    as.mids(long.imp.dat),  # Passive Imputation
    as.mids(long.imp.dat.alt)  # Passive Imputation Alt
  )) 
}


# Passive Imputation Analysis Function
analyze_imputed_data <- function(imputed_data) {
  # Input:
  #   imputed_data: A mids object containing the imputed datasets with DRS-60
  
  # Output:
  #   A list containing the odds ratio estimate, confidence interval, standard error and p-value
  
  # Empirical Distribution with Imputed DRS60 for Active/Placebo and Each Baseline Pulmonary Scale Level
  dist.res = cbind(c("placebo","active","pulmord2","pulmord3","pulmord4","pulmord5"),round(Reduce("+",lapply(1:50,function(j) rbind(prop.table(table(complete(imputed_data,j)$active,factor(complete(imputed_data,j)$drs,levels=0:62)),1),prop.table(table(factor(complete(imputed_data,j)$ordpulmbl,levels=2:5),factor(complete(imputed_data,j)$drs,levels=0:62)),1))))/50,5))
  
  # Model for data with imputed DRS60
  pom.imp <-with(imputed_data, polr(factor(drs)~active+factor(ordpulmbl),Hess=TRUE))   
  polr.res = c(-1,1,-1,-1)*summary(pool(pom.imp),conf.int=TRUE)[1,c("estimate","std.error","conf.high","conf.low")]
  
  return(list(dist.res,polr.res))
}

# Individual imputation OR estimates
analyze_individual_imputations <- function(imputed_data) {
  # Number of imputations
  m <- imputed_data$m
  
  # List to store results
  results <- vector("list", m)
  
  for (i in 1:m) {
    # Get the i-th imputed dataset
    data_i <- complete(imputed_data, i)
    
    # Fit the model
    model <- MASS::polr(drs ~ active + factor(ordpulmbl), data = data_i, Hess = TRUE)
    
    # Extract the log odds ratio and its standard error
    log_odds_ratio <- coef(summary(model))["active1", "Value"]
    log_std_err <- coef(summary(model))["active1", "Std. Error"]
    
    # Calculate CI on the log scale
    alpha <- 0.05  # for 95% CI
    z_score <- qnorm(1 - alpha/2)
    log_ci_lower <- -log_odds_ratio - z_score * log_std_err
    log_ci_upper <- -log_odds_ratio + z_score * log_std_err
    
    # Exponentiate to get odds ratio and CI
    odds_ratio <- exp(-log_odds_ratio)
    ci_lower <- exp(log_ci_lower)
    ci_upper <- exp(log_ci_upper)
    
    # Calculate p-value
    p_value <- 2 * (1 - pnorm(abs(log_odds_ratio / log_std_err)))
    
    # Store results
    results[[i]] <- data.frame(
      imputation = i,
      odds_ratio = odds_ratio,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      p_value = p_value,
      std_error = log_std_err
    )
  }
  
  # Combine results into a single data frame
  results_df <- do.call(rbind, results)
  print(results_df$odds_ratio)
  #return(results_df)
}
