############################################l
#
#
# eBird modeling Bayesian Framework
# by Nick Masto
# last updated 3/9/2022
#
#
#
###########################################'


# Set it up------------------------------------
library(tidybayes)
library(tidyverse)
library(emmeans)
library(rstan)
library(rstanarm)
library(bayesplot)

# Source datamgt script
source("scripts/datamgt_ebird_20220223.R")

# Continental Waterfowl Response to February 2021 ECE-------------
names(dat2)

m1 <- stan_lmer(North.South ~ Foraging * year_bin + (1 | Year), data = dat2,
                iter = 10000,
                warmup = 5000,
                chains = 3,
               seed = 300)

print(m1)
summary(m1, 
        pars = c("(Intercept)", "sigma", "Sigma[Year:(Intercept),(Intercept)]",
                 "Foragingwetland.obligate:year_bin1", "Foraginggrubbing:year_bin1",
                  "Foraginggrubbing", "Foragingwetland.obligate","year_bin1"),
        point_est = "median",
        probs = c(0.025, 0.975, 0.17, 0.83),
        digits = 2)

# Set color and plot posteriors estimates with 66% credible intervals
color_scheme_set("blue")
contECE <- mcmc_areas(m1, 
                      area_method = "scaled height",
                      prob = 0.66,
                      pars = c("Foragingwetland.obligate:year_bin1", "Foraginggrubbing:year_bin1",
                               "Foraginggrubbing", "Foragingwetland.obligate","year_bin1", "(Intercept)"),
                      point_est = "median") +
  theme_classic(base_size = 15) +
  scale_y_discrete(labels = c("Wetland Obligate:ECE",
                              "Grubbing/Browsing:ECE",
                              "Grubbing/Browsing Forager",
                              "Wetland Obligate Forager",
                              "Severe February Weather",
                              "Intercept \n (Generalist in 'Normal' Years)")) +  
  ggtitle("February 2021 ECE") +
  theme(plot.title = element_text(hjust = 0.5, size = 19)) +
  theme(axis.text = element_text(size = 18))


# Take a look at contrasts among groups
emmeans(m1, pairwise ~year_bin|Foraging, type = "response")
emmeans(m1, pairwise ~year_bin, type = "response")
emmeans(m1, pairwise ~Foraging, type = "response") 

###########################################################################l
### For fun: year-level intercept draws for overall mean----------------------------

mu_a_sims <- as.matrix(m1, 
                       pars = "(Intercept)")
# draws for 14 years' year-level error
u_sims <- as.matrix(m1, 
                    regex_pars = "b\\[\\(Intercept\\) Year\\:")
# draws for 14 years' varying intercepts               
a_sims <- as.numeric(mu_a_sims) + u_sims          

# Obtain sigma_y and sigma_alpha^2
# draws for sigma_y
s_y_sims <- as.matrix(m1, 
                      pars = "sigma")
# draws for sigma_alpha^2
s__alpha_sims <- as.matrix(m1, 
                           pars = "Sigma[Year:(Intercept),(Intercept)]")

#Compute mean, SD, median, and 95% credible interval of varying intercepts

# Posterior mean and SD of each alpha
a_mean <- apply(X = a_sims,     # posterior mean
                MARGIN = 2,
                FUN = mean)
a_sd <- apply(X = a_sims,       # posterior SD
              MARGIN = 2,
              FUN = sd)

# Posterior median and 95% credible interval
a_quant <- apply(X = a_sims, 
                 MARGIN = 2, 
                 FUN = quantile, 
                 probs = c(0.025, 0.50, 0.975))
a_quant <- data.frame(t(a_quant))
names(a_quant) <- c("Q2.5", "Q50", "Q97.5")

# Combine summary statistics of posterior simulation draws
a_df <- data.frame(a_mean, a_sd, a_quant)
round(head(a_df), 2)

# Sort dataframe containing estimated alpha's mean and sd for every Year
a_df <- a_df[order(a_df$a_mean), ]
a_df$a_rank <- c(1 : dim(a_df)[1])  # a vector of for each Year 

# Plot Year-level alphas's posterior mean and 95% credible interval
plot <- ggplot(data = a_df, 
               aes(x = a_rank, 
                   y = a_mean)) +
  geom_pointrange(aes(ymin = Q2.5, 
                      ymax = Q97.5),
                  position = position_jitter(width = 0.1, 
                                             height = 0)) + 
  geom_hline(yintercept = mean(a_df$a_mean), 
             size = 0.5, 
             col = "red") + 
  scale_x_continuous("Year", 
                     breaks = seq(from = 1, 
                                  to = 12, 
                                  by = 2)) + 
  scale_y_continuous(expression(paste("varying intercept, ", alpha[j]))) + 
  theme_classic(base_family = "serif")

ggsave("plots/figure_errors.jpeg", width = 5, height = 5, units = "in", dpi = 500)

###########################################################################l


# Continental Waterfowl Response to severe weather in February (2021, 2019, 2015)---------------

m2 <- stan_lmer(North.South ~ Foraging * year_bin + (1 | Year), data = dat,
                iter = 10000,
                warmup = 5000,
                chains = 3,
                seed = 300)

print(m2)
summary(m2, 
        pars = c("(Intercept)", "sigma", "Sigma[Year:(Intercept),(Intercept)]",
                 "Foragingwetland.obligate:year_bin1", "Foraginggrubbing:year_bin1",
                 "Foraginggrubbing", "Foragingwetland.obligate","year_bin1"),
        point_est = "median",
        probs = c(0.025, 0.975, 0.17, 0.83),
        digits = 2)

# Set color and plot posteriors estimates with 66% credible intervals
color_scheme_set("purple")
contseverefebs <- mcmc_areas(m2, 
                             area_method = "scaled height",
                             prob = 0.66,
                             pars = c("Foragingwetland.obligate:year_bin1", "Foraginggrubbing:year_bin1",
                                      "Foraginggrubbing", "Foragingwetland.obligate","year_bin1", "(Intercept)"),
                             point_est = "median") +
  theme_classic(base_size = 15) +
  scale_y_discrete(labels = c("Wetland Obligate:ECE",
                              "Grubbing/Browsing:ECE",
                              "Grubbing/Browsing Forager",
                              "Wetland Obligate Forager",
                              "Severe February Weather",
                              "Intercept \n (Generalist in 'Normal' Years)")) + 
  ggtitle("Severe February Temperature") +
  theme(plot.title = element_text(hjust = 0.5, size = 19)) +
  theme(axis.text = element_text(size = 18))

# Take a look at contrasts among groups
emmeans(m2, pairwise ~year_bin|Foraging, type = "response")
emmeans(m2, pairwise ~year_bin, type = "response")
emmeans(m2, pairwise ~Foraging, type = "response") 

# Plot these in another script                
# Flyway Waterfowl Response to February 2021--------------------

### Atlantic Flyway-----------------------------------------
m3 <- stan_lmer(North.South ~ Foraging * year_bin + (1 | Year), data = Atlantic2,
                iter = 10000,
                warmup = 5000,
                chains = 3,
                seed = 300)

summary(m3, 
        pars = c("(Intercept)", "sigma", "Sigma[Year:(Intercept),(Intercept)]",
                 "Foragingwetland.obligate:year_bin1", "Foraginggrubbing:year_bin1",
                 "Foraginggrubbing", "Foragingwetland.obligate","year_bin1"),
        point_est = "median",
        probs = c(0.025, 0.975, 0.17, 0.83),
        digits = 2)

# Set color and plot posteriors estimates with 66% credible intervals
color_scheme_set("orange")
atlpost <- mcmc_areas(m3, 
                      area_method = "scaled height",
                      prob = 0.66,
                      pars = c("Foragingwetland.obligate:year_bin1", "Foraginggrubbing:year_bin1",
                               "Foraginggrubbing", "Foragingwetland.obligate","year_bin1", "(Intercept)"),
                      point_est = "median") +
  theme_classic(base_size = 15) +
  scale_y_discrete(labels = c("Wetland Obligate:ECE",
                              "Grubbing/Browsing:ECE",
                              "Grubbing/Browsing Forager",
                              "Wetland Obligate Forager",
                              "February 2021 ECE",
                              "Intercept \n(Generalist in 'Normal' Years)")) + 
  ggtitle("Atlantic Flyway") +
  theme(plot.title = element_text(hjust = 0.5, size = 19)) +
  theme(axis.text = element_text(size = 18))

# Take a look at contrasts among groups
?emmeans
emmeans(m3, pairwise ~year_bin|Foraging, type = "response")
emmeans(m3, pairwise ~year_bin, type = "response")
emmeans(m3, pairwise ~Foraging, type = "response")

### Mississippi Flyway------------------------------
m4 <- stan_lmer(North.South ~ Foraging * year_bin + (1 | Year), data = Mississippi2,
                iter = 10000,
                warmup = 5000,
                chains = 3,
                seed = 300)

summary(m4, 
        pars = c("(Intercept)", "sigma", "Sigma[Year:(Intercept),(Intercept)]",
                 "Foragingwetland.obligate:year_bin1", "Foraginggrubbing:year_bin1",
                 "Foraginggrubbing", "Foragingwetland.obligate","year_bin1"),
        point_est = "median",
        probs = c(0.025, 0.975, 0.17, 0.83),
        digits = 2)

# Set color and plot posteriors estimates with 66% credible intervals
color_scheme_set("purple")
mspost <- mcmc_areas(m4, 
                     area_method = "scaled height",
                     prob = 0.66,
                     pars = c("Foragingwetland.obligate:year_bin1", "Foraginggrubbing:year_bin1",
                              "Foraginggrubbing", "Foragingwetland.obligate","year_bin1", "(Intercept)"),
                     point_est = "median") +
  theme_classic(base_size = 15) +
  scale_y_discrete(labels = c("Wetland Obligate:ECE",
                              "Grubbing/Browsing:ECE",
                              "Grubbing/Browsing Forager",
                              "Wetland Obligate Forager",
                              "February 2021 ECE",
                              "Intercept \n(Generalist in 'Normal' Years)")) + 
  ggtitle("Mississippi Flyway") +
  theme(plot.title = element_text(hjust = 0.5, size = 19)) +
  theme(axis.text = element_text(size = 18))  

# Take a look at contrasts among groups
emmeans(m4, pairwise ~year_bin|Foraging, type = "response")
emmeans(m4, pairwise ~year_bin, type = "response")
emmeans(m4, pairwise ~Foraging, type = "response")

### Central Flyway-----------------------
m5 <- stan_lmer(North.South ~ Foraging * year_bin + (1 | Year), data = Central2,
                iter = 10000,
                warmup = 5000,
                chains = 3,
                seed = 300)

summary(m5, 
        pars = c("(Intercept)", "sigma", "Sigma[Year:(Intercept),(Intercept)]",
                 "Foragingwetland.obligate:year_bin1", "Foraginggrubbing:year_bin1",
                 "Foraginggrubbing", "Foragingwetland.obligate","year_bin1"),
        point_est = "median",
        probs = c(0.025, 0.975, 0.17, 0.83),
        digits = 2)

# Set color and plot posteriors estimates with 66% credible intervals
color_scheme_set("blue")
cenpost <- mcmc_areas(m5, 
                      area_method = "scaled height",
                      prob = 0.66,
                      pars = c("Foragingwetland.obligate:year_bin1", "Foraginggrubbing:year_bin1",
                               "Foraginggrubbing", "Foragingwetland.obligate","year_bin1", "(Intercept)"),
                      point_est = "median") +
  theme_classic(base_size = 15) +
  scale_y_discrete(labels = c("Wetland Obligate:ECE",
                              "Grubbing/Browsing:ECE",
                              "Grubbing/Browsing Forager",
                              "Wetland Obligate Forager",
                              "February 2021 ECE",
                              "Intercept \n(Generalist in 'Normal' Years)")) + 
  ggtitle("Central Flyway") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 19)) +
  theme(axis.text = element_text(size = 18))

# Take a look at contrasts among groups
emmeans(m5, pairwise ~year_bin|Foraging, type = "response")
emmeans(m5, pairwise ~year_bin, type = "response")
emmeans(m5, pairwise ~Foraging, type = "response")

### Pacific Flyway-------------------------
m6 <- stan_lmer(North.South ~ Foraging * year_bin + (1 | Year), data = Pacific2,
                iter = 10000,
                warmup = 5000,
                chains = 3,
                seed = 300)
plot(m6, "rhat")
summary(m6, 
        pars = c("(Intercept)", "sigma", "Sigma[Year:(Intercept),(Intercept)]",
                 "Foragingwetland.obligate:year_bin1", "Foraginggrubbing:year_bin1",
                 "Foraginggrubbing", "Foragingwetland.obligate","year_bin1"),
        point_est = "median",
        probs = c(0.025, 0.975, 0.17, 0.83),
        digits = 2)

# Set color and plot posteriors estimates with 66% credible intervals
color_scheme_set("red")
pacpost <- mcmc_areas(m6, 
                      area_method = "scaled height",
                      prob = 0.66,
                      pars = c("Foragingwetland.obligate:year_bin1", "Foraginggrubbing:year_bin1",
                               "Foraginggrubbing", "Foragingwetland.obligate","year_bin1", "(Intercept)"),
                      point_est = "median") +
  theme_classic(base_size = 15) +
  scale_y_discrete(labels = c("Wetland Obligate:ECE",
                              "Grubbing/Browsing:ECE",
                              "Grubbing/Browsing Forager",
                              "Wetland Obligate Forager",
                              "February 2021 ECE",
                              "Intercept \n(Generalist in 'Normal' Years)")) + 
  ggtitle("Pacific Flyway") +
  theme(plot.title = element_text(hjust = 0.5, size = 19)) +
  theme(axis.text = element_text(size = 18))
  

# Take a look at contrasts among groups
emmeans(m6, pairwise ~year_bin|Foraging, type = "response")
emmeans(m6, pairwise ~year_bin, type = "response")
emmeans(m6, pairwise ~Foraging, type = "response")
