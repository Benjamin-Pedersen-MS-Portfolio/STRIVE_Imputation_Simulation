library(tidyverse)

setwd("/home/murra484/shared/M-CIDeR/drs_60")

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


# MCAR -------------------------------------------------------------------------
# Gives line plot of probability of dropout on each day per person by prop_missing

prop_missing_vec <- c(.1, .3, .5)

# Calculate b for prop_missing
x_beta <- rep(0, 2)
ht_alpha <- matrix(0, nrow = 2, ncol = 60)
a <- -.05
b_vec <- sapply(prop_missing_vec, function(prop_missing) {
  uniroot(function(x) f(a, x, x_beta, ht_alpha) - prop_missing, c(-150, 10))$root
})

# t x |prop_missing_vec|
eta_vecs <- sapply(b_vec, function(b) {
  c(1:60) * a + b
})

p_vecs <- exp(eta_vecs) / (1 + exp(eta_vecs)) # Prob of dropping out

# Pad with 0 because no one drops on day 0
data.frame(rbind(rep(0, length(prop_missing_vec)), p_vecs)) %>%
  set_names(prop_missing_vec) %>%
  mutate(day = 0:60) %>%
  pivot_longer(cols = -day, names_to = "prop_missing", values_to = "p_t") %>%
  ggplot(aes(x = day, y = p_t, color = prop_missing)) +
  geom_point() +
  theme_bw() +
  labs(x = "Day", y = "Probability of dropping out", color = "Prop missing",
       title = "Probability of dropping out each day in MCAR")


# MAR -------------------------------------------------------------------------

prop_missing <- .9

## Get mean and sd of ht and x in real data so that we can standardize
# Load in original data set
dat <- read.csv("data/tico_drs_copy.csv")
dat <- dat[rowSums(!(dat %>% select(starts_with("dailyStatus")) %>% is.na())) == 62, ]

# Standardize covariates
vaccat_dummy <- cbind(dat$vaccat == 1, dat$vaccat == 3)
mean_vaccat <- colMeans(vaccat_dummy)
sd_vaccat <- apply(vaccat_dummy, 2, sd)
scale_vaccat <- vaccat_dummy #scale(vaccat_dummy) # TODO consider not standardizing

mean_age <- mean(dat$age)
sd_age <- sd(dat$age)
scale_age <- scale(dat$age)

all_cs <- matrix(nrow = nrow(dat), ncol = 61) # Cumul. days home
all_at_home <- matrix(nrow = nrow(dat), ncol = 61) # Curr location
for (i in 1:nrow(dat)) {
  # Cumulative days at home
  locs <- dat[i, ] %>%
    select(starts_with("dailyStatus")) %>%
    select(!dailyStatus61) %>%
    t() %>% c()
  cs <- cumsum(locs == "out of hospital")
  # Indicator if at home
  at_home <- I(locs == "out of hospital")
  all_cs[i, ] <- cs
  all_at_home[i, ] <- at_home
}

# Remove day 0 bc no dropout on day 0 and get mean/sd
all_cs <- all_cs[, 2:61]
scale_cs <- scale(all_cs)
all_at_home <- all_at_home[, 2:61]
scale_at_home <- scale(all_at_home)

mean_cs <- colMeans(all_cs)
sd_cs <- apply(all_cs, 2, sd)
mean_at_home <- colMeans(all_at_home)
sd_at_home <- apply(all_at_home, 2, sd)

## Calculate b for desired prop missingness according to (saved) real data
# x_beta_mar <- readRDS("data/x_beta_mar.RDS")
# ht_alpha_mar <- readRDS("data/ht_alpha_mar.RDS")

# Calculate eta_t for each patient
x_beta_mar <- scale_age + 1 * scale_vaccat[, 1]  + .5 * scale_vaccat[, 2]
ht_alpha_mar <- sapply(1:nrow(dat), function(i) {
  scale_cs[i, ] * .04 + scale_at_home[i, ] * 1
}) %>% t()

a <- -.05
b <- uniroot(function(x) f(a, x, x_beta_mar, ht_alpha_mar) - prop_missing, c(-30, 10))$root

gamma <- c(1:60) * a + b

dat$dropout_day <- NA
day_died_vec <- readRDS("data/day_died_vec.RDS")
# Add missingness
for (i in 1:nrow(dat)) {
  eta_i <- x_beta_mar[i] + ht_alpha_mar[i, ] + gamma
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
      # # Make columns missing
      # days_missing <- dropout_day:61
      # cols_missing <- c(paste0("dailyStatus", days_missing), 
      #                   paste0("locStatus", days_missing),
      #                   paste0("vitalStatus", days_missing))
      # dat[i, cols_missing] <- NA
      dat[i, "dropout_day"] <- dropout_day
    }
  }
}

hist(dat$dropout_day)

ggplot(dat, aes(x = dropout_day)) +
  geom_histogram(color = "black", fill = "skyblue", bins = 60) +
  theme_bw() +
  labs(x = "Dropout day", y = "Count", title = "MAR, prop_missing = .9")



# Fake individuals -------------------------------------------------------------

## Make up data for hypothetical individuals
fake_indivs <- data.frame(rbind(
  c(60, 3, rep("in hospital", 61)), # in hospital full time
  c(60, 3, "in hospital",  rep("out of hospital", 60)), # go home day 1
  c(60, 3, rep("in hospital", 31), rep("out of hospital", 30)), # go home day 31
  c(60, 3, rep("in hospital", 15), rep("out of hospital", 20), rep("in hospital", 26)), # Re-enter hospital
  c(50, 3, rep("in hospital", 31), rep("out of hospital", 30)), # go home day 31
  c(70, 3, rep("in hospital", 31), rep("out of hospital", 30)), # go home day 31
  c(60, 1, rep("in hospital", 31), rep("out of hospital", 30)), # go home day 31
  c(60, 2, rep("in hospital", 31), rep("out of hospital", 30)) # go home day 31
))
colnames(fake_indivs) <- c("age", "vaccat", paste0("locStatus", 0:60))
fake_indivs$age <- as.numeric(fake_indivs$age)
fake_indivs$vaccat <- as.numeric(fake_indivs$vaccat)

cs <- t(apply(fake_indivs[, 4:63], 1, function(x) cumsum(x == "out of hospital")))
at_home <- fake_indivs[, 4:63] == "out of hospital"

## Standardize fake data according to real data mean and sd
age_std <- (fake_indivs$age - mean_age) / sd_age
vaccat_dummy <- cbind(fake_indivs$vaccat == 1, fake_indivs$vaccat == 3)
vaccat_std <- vaccat_dummy # sapply(1:ncol(vaccat_dummy), function(i) (vaccat_dummy[, i] - mean_vaccat[i]) / sd_vaccat[i])
cs_std <- cs # sapply(1:ncol(cs), function(i) (cs[, i] - mean_cs[i]) / sd_cs[i])
at_home_std <- at_home #sapply(1:ncol(at_home), 
                      #function(i) (at_home[, i] - mean_at_home[i]) / sd_at_home[i])

## Calculate eta_t for each patient
x_beta <- age_std + 1 * vaccat_std[, 1]  + .5 * vaccat_std[, 2]
ht_alpha <- sapply(1:nrow(fake_indivs), function(i) {
  cs_std[i, ] * .04 + at_home_std[i, ] * 1
}) %>% t()

gamma <- c(1:60) * a + b
p_vecs <- sapply(1:nrow(fake_indivs), function(i) {
  eta_i <- x_beta[i] + ht_alpha[i, ] + gamma
  print("x_beta")
  print(x_beta[i])
  print("ht_alpha")
  print(ht_alpha[i,])
  print("eta_i")
  print(eta_i)
  exp(eta_i) / (1 + exp(eta_i))
}) %>% t()

# Pad with 0 because no one drops on day 0
p_vecs_pad <- data.frame(cbind(rep(0, nrow(fake_indivs)), p_vecs))

plot_data <- p_vecs_pad %>%
  set_names(0:60) %>%
  mutate(indiv = 1:nrow(.)) %>%
  mutate(indiv = as.factor(indiv)) %>%
  pivot_longer(cols = -indiv, names_to = "day", values_to = "p_it") %>%
  mutate(day = as.numeric(day))

# Vary trajectory
plot_data %>%
  filter(indiv %in% c(1, 2, 3, 4)) %>%
  ggplot(aes(x = day, y = p_it, color = indiv)) +
  geom_point() +
  theme_bw() +
  labs(x = "Day", y = "Probability of dropping out", color = "Individual",
       title = "Probability of dropping out each day in MAR")



# Cumulative probability of dropping by individual
prob_not_missing_vecs <- 1 - p_vecs
cum_prob_missing <- sapply(1:nrow(fake_indivs), 
                           function(i) 1 - prod(prob_not_missing_vecs[i, ]))















