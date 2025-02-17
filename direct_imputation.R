# libraries
library(mice)
library(MASS)

# Function inputs
# "data" is a tico dataset where drs60 has some missingness and the dataset should have the following variables
# i) "rangp" is a variable with the treatment variable where 1 represents the treated and 2 the untreated
# ii) ordpulmbl is a variable for the oxygen levels
# iii) drs is the drs60 variable

# Sample Output
#                                est     lower    upper     pvalue
#Proportional Odds            1.1778953 0.9802235 1.415706 0.08102322
#Proportional Odds (Imputed)  1.1768666 0.9780189 1.416143 0.08484261

#data <-read.csv("/Users/nmusinguzi/Library/CloudStorage/Box-Box/M-CIDeR-Summer-Project/Data Creation/tico_drs.csv")

analyse_dta <- function(data,active,ordpulmbl,drs) {
  dat_ful<- data
  
  #Define DRS-60, treatment variable, severity variable, and death indicator
  #dat_ful = within(dat_ful, {
   # trt = 1*(rangp==1) #Treatment covariate
  #})
  
  dat <- subset(dat_ful, select=c("active","drs","ordpulmbl"))
  dat$ordpulmbl <- as.factor(dat$ordpulmbl)  # ordpulmbl should be a factor b4 imp
  
  #set.seed(232)
  # imputation 
  dat_imp <- mice(dat,
                  method = c("pmm", "pmm","pmm"),
                  maxit = 1, ## number of MICE iterations, only 1 iteration is needed as the drs is the only variable with missingness, so there are no interdependencies between variables that need to converge
                  m = 50, ## ## number of multiple imputations
                  printFlag = FALSE##seed=232
                )
  
  ### Van Elteren
  #==============
  
  # vet = sanon(drs60 ~ grp(trt, ref="1")+strt(factor(ordpulmbl)), data=dat)
  # vetres = c(confint(vet)$ci,vet$p)
  # raw_ve <- cbind (est=vetres[1],
  #                  lower=vetres[2],
  #                  upper=vetres[3],
  #                  pvalue=vetres[4])
  
  #0.5232676 0.4928941 0.5536410 0.1332445
  
  #M <- dat_imp$m
  #M
  # 
  # # Computing VE Statistic from one imputation
  # data_1 <- complete(dat_imp,1)
  # vet_1 = sanon(drs60 ~ grp(trt, ref="1"), data=data_1)
  # vetres_1 = c(confint(vet_1)$ci,vet_1$p)
  # 
  # ## set up R objects to store results across M imputed datasets
  # k <- length(coef(vet))
  # 
  # coef.total <- matrix(NA, ncol=k,nrow=M) ## coefs
  # se.total <- matrix(NA,ncol=k,nrow=M) ## SE's
  # vcov.total <- array(NA, dim = c(k, k, M)) ## var covar matrices
  # 
  # # Loop over each imputed dataset and fit model
  # 
  # for (m in 1:M) {
  #   
  #   data_m <- complete(dat_imp,m)
  #   fit_m <- sanon(drs60 ~ grp(trt, ref="1")+strt(factor(ordpulmbl)), data=data_m)
  #   
  #   coef.total[m, ] <- confint(fit_m)$ci[1]
  #   se.total[m, ] <- sqrt(diag(vcov(fit_m)))
  #   vcov.total[, , m] <- vcov(fit_m)
  #   
  # }
  # 
  # colnames(coef.total) <- colnames(se.total) <- names(coef(fit_m))
  # print(coef.total[1:6,], digits=3)
  # 
  # print(se.total[1:6,], digits=10)
  # 
  # # Vbar, the average variance-covariance matrix across the M imputations   
  # Vbar <- apply(vcov.total, c(1,2), mean) #?????????
  # colnames(Vbar) <-names(coef(fit_m))
  # print(Vbar,digits=3)
  # 
  # # B , the extra variance due to the missing values/data in the sample, ie. the sample variance of the parameter estimates across the M imputations
  # 
  # B <- var(coef.total)
  # colnames(B) <- names(coef(fit_m))
  # rownames(B) <- NULL
  # print(B, digits=3)
  # 
  # # Average coefficient estimates across the imputations
  # (vestat <-colMeans(coef.total))
  # 
  # # The overall variance estimate
  # sigma_hat_star <- Vbar + (1+1/M)*B
  # (se_hat_star <- sqrt(diag(sigma_hat_star)))
  # 
  # # Proportion of variation attributable to missing data, lambda
  # (lambda <- (1+1/M) * (sum(diag(B %*% solve(sigma_hat_star))))/k)
  # 
  # # dof
  # (nu <- (M-1)/lambda^2)
  # 
  # # new dof
  # # https://stefvanbuuren.name/fimd/sec-whyandwhen.html
  # vcom<-nrow(data_m)-length(coefficients(fit_m))
  # vobs<-((vcom+1)/(vcom+3))*vcom*(1-lambda)
  # nu_dof<-(nu*vobs)/(nu+vobs)
  # 
  # # pvalue
  # tstat<-(vestat-0.5)/se_hat_star 
  # pvalue<- 2*pt(abs(tstat),nu_dof,lower.tail = FALSE)
  # 
  # # 95% CI
  # imp_ve <- round(cbind(est=vestat,
  #                       lower=vestat - se_hat_star*qt(0.975,df=nu_dof),
  #                       upper=vestat + se_hat_star*qt(0.975,df=nu_dof),
  #                       p_value=pvalue),5)
  # 
  # # combine results
  # ve_comb<-rbind(raw_ve,imp_ve)
  # ve_comb
  
  
  ### PO Model
  #============
  
  # Model for data with missing DRS60 
  # pom = polr(factor(drs)~active+factor(ordpulmbl),Hess=TRUE, data=dat)
  # raw_po_p<- 2*(1-pt(abs(summary(pom)$coefficients[1,3]),df= nrow(dat)-length(coefficients(pom))))
  # raw_po = as.numeric(c(exp(-coef(pom)["active"]),
  #                       rev(exp(-confint(pom, type="Wald")["active",])),
  #                       raw_po_p,
  #                       -1))
  
  # Model for data with missing DRS60
  pom = polr(factor(drs)~active+ordpulmbl,Hess=TRUE, data=dat)

  # Extract coefficients and standard errors
  coef_summary <- summary(pom)$coefficients
  active_coef <- coef_summary["active", "Value"]
  active_se <- coef_summary["active", "Std. Error"]

  # Calc p-value
  raw_po_p<- 2*(1-pt(abs(summary(pom)$coefficients[1,3]),df= nrow(dat)-length(coefficients(pom))))

  # Calculate confidence intervals
  # ci <- exp(-confint(pom, parm = "active", type="Wald"))
  ci <- exp(- (active_coef + c(-1, 1) * qnorm(0.975) * active_se))

  # Create results vector
  raw_po <- c(exp(-active_coef),  # Odds Ratio
                  ci[2],              # Lower 95% CI
                  ci[1],              # Upper 95% CI
                  raw_po_p,           # P-value
                  exp(active_se))     # SE (exponentiated)
  
  # Model for data with imputed DRS60
  pom.imp <-with(dat_imp, polr(factor(drs)~active+factor(ordpulmbl),Hess=TRUE))   
  #print(summary(pool(pom.imp)), digits = 2)
  log_po_estimate <- -summary(pool(pom.imp))[1,2]
  log_po_se <- summary(pool(pom.imp))[1,3]
  imputed_po<-c(exp(log_po_estimate), # Point estimate
                exp(log_po_estimate - qnorm(0.975) * log_po_se), # Lower
                exp(log_po_estimate + qnorm(0.975) * log_po_se), # Upper
                summary(pool(pom.imp))[1,6], # p-value
                exp(log_po_se)) # SE
  
  #result<-rbind(raw_po,imputed_po,raw_ve,imp_ve)
  #rownames(result) <-c("Proportional Odds", "Proportional Odds (Imputed)", "Van Elteran", "Van Elteran (Imputed)")
  
  result<-rbind(raw_po,imputed_po)
  rownames(result) <-c("Proportional Odds", "Proportional Odds (Imputed)")
  colnames(result) <-c("Odds Ratio","Lower 95%CI","Upper 95%CI","P-value", "SE")
  
  return(result)
  
}