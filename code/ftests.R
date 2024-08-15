library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(fixest)
library(readxl)
library(car)

setwd('/Users/annabelroth/Desktop/cs_classes/thesis/data/')

raw <- read_excel('final_data.xlsx')

full_controls <- c('pct_black_2001', 'pct_m15_29_2001', 'per_cap_police_2001', 'pov_rate_2001', 'unemp_rate_2001', 
                   'alc_gal_per_cap_2001', 'encarceration_rate_2001', 'log_pop_calc_2001', 'pop_density_calc_2001', 
                   'crime_per_cap_2001', 'guns_per_household_2001', 'homicides_lag')

setFixest_fml(..full_ctrls = ~ Year[pct_black_2001, pct_m15_29_2001, per_cap_police_2001,
                                    pov_rate_2001, unemp_rate_2001, alc_gal_per_cap_2001, encarceration_rate_2001,
                                    log_pop_calc_2001, pop_density_calc_2001, guns_per_household_2001,
                                    crime_per_cap_2001, homicides_lag] + State)

sunab_beta_vcv <- 
  function(sunab_fixest){
    
    ## The following code block extracts the weights on individual coefs used in
    # the fixest aggregation ##
    sunab_agg   <- sunab_fixest$model_matrix_info$sunab$agg_period
    sunab_names <- names(sunab_fixest$coefficients)
    sunab_sel   <- grepl(sunab_agg, sunab_names, perl=TRUE)
    sunab_names <- sunab_names[sunab_sel]
    if(!is.null(sunab_fixest$weights)){
      sunab_wgt <- colSums(sunab_fixest$weights * sign(model.matrix(sunab_fixest)[, sunab_names, drop=FALSE]))
    } else {
      sunab_wgt <- colSums(sign(model.matrix(sunab_fixest)[, sunab_names, drop=FALSE]))
    }
    
    #Construct matrix sunab_trans such that sunab_trans %*% non-aggregated coefs = aggregated coefs,
    sunab_cohorts <- as.numeric(gsub(paste0(".*", sunab_agg, ".*"), "\\2", sunab_names, perl=TRUE))
    sunab_mat     <- model.matrix(~ 0 + factor(sunab_cohorts))
    sunab_trans   <- solve(t(sunab_mat) %*% (sunab_wgt * sunab_mat)) %*% t(sunab_wgt * sunab_mat)
    
    #Get the coefs and vcv
    sunab_coefs   <- sunab_trans %*% cbind(sunab_fixest$coefficients[sunab_sel])
    sunab_vcov    <- sunab_trans %*% sunab_fixest$cov.scaled[sunab_sel, sunab_sel] %*% t(sunab_trans)
    
    return(list(beta = sunab_coefs,
                sigma = sunab_vcov))
  }



# add variables for log of age-adj rate and time to treatment
#NOTE: if never treated, then set to 3000
# also add log_state_tot and set to 0 if age_adj_rate_tot was null 
reg_data <- raw %>% 
  # filter out states with no homicide data
  filter(!(fip %in% c(38, 50, 56))) %>% 
  mutate(t2t_syg = if_else(syg_yr != 3000, year - syg_yr, -1),
         t2t_may_vs_other = if_else(may_vs_other_yr != 3000, year - may_vs_other_yr, -1),
         t2t_erpo = if_else(red_flag_yr != 3000, year - red_flag_yr, -1),
         log_state_tot = if_else(!is.na(age_adj_rate_tot), log(age_adj_rate_tot), 0)) %>% 
  select(State = fip,
         Year = year,
         log_state_tot,
         syg_yr,
         t2t_syg,
         may_vs_other_yr,
         t2t_may_vs_other,
         red_flag_yr,
         t2t_erpo,
         all_of(full_controls))

############################ SYG
twfe_syg = feols(log_state_tot ~ i(t2t_syg, ref = -1) | ..full_ctrls,
                cluster = "State",
                data = reg_data)

sunab_syg <- feols(log_state_tot ~ sunab(syg_yr, Year) | ..full_ctrls, 
                   cluster = "State",
                   data = reg_data)

var_names <- c("t2t_syg::-10",
               "t2t_syg::-9",
               "t2t_syg::-8",
               "t2t_syg::-7",
               "t2t_syg::-6",
               "t2t_syg::-5",
               "t2t_syg::-4",
               "t2t_syg::-3",
               "t2t_syg::-2")

full_beta_syg_sunab <- as.data.frame(sunab_beta_vcv(sunab_syg)$beta) 
beta_syg_sunab <- slice(full_beta_syg_sunab, c(9, 10, 11, 12, 13, 14, 15, 16, 17))
# rownames(beta_syg) <- var_names
beta_syg_vec_sunab = beta_syg_sunab[['V1']]


full_sigma_syg <- as.data.frame(sunab_beta_vcv(sunab_syg)$sigma)
sigma_syg <- slice(full_sigma_syg, c(9, 10, 11, 12, 13, 14, 15, 16, 17)) %>% 
  select(`factor(sunab_cohorts)-10`,
         `factor(sunab_cohorts)-9`,
         `factor(sunab_cohorts)-8`,
         `factor(sunab_cohorts)-7`,
         `factor(sunab_cohorts)-6`,
         `factor(sunab_cohorts)-5`,
         `factor(sunab_cohorts)-4`,
         `factor(sunab_cohorts)-3`,
         `factor(sunab_cohorts)-2`)
# rownames(sigma_syg) <- var_names
# colnames(sigma_syg) <- var_names
sigma_syg_mat = as.matrix(sigma_syg)


table <- etable(twfe_syg, sunab_syg)


syg_ftest <- linearHypothesis(twfe_syg, c("t2t_syg::-10 = 0",
                                          "t2t_syg::-9 = 0",
                                          "t2t_syg::-8 = 0",
                                          "t2t_syg::-7 = 0",
                                          "t2t_syg::-6 = 0",
                                          "t2t_syg::-5 = 0",
                                          "t2t_syg::-4 = 0",
                                          "t2t_syg::-3 = 0",
                                          "t2t_syg::-2 = 0"),
                              error.df = 46, # was told to do num_clusters - 1
                              test = "F")

# chi square test for sunab
sunab_t_stat_syg <- t(beta_syg_vec) %*% solve(sigma_syg_mat) %*% beta_syg_vec
sunab_p_val_syg <- 1 - pchisq(sunab_t_stat_syg, df = length(beta_syg_vec))

# chi square test for twfe
full_sigma_twfe <- as.data.frame(twfe_syg$cov.scaled)
sigma_twfe_syg <- slice(full_sigma_twfe, c(9, 10, 11, 12, 13, 14, 15, 16, 17)) %>% 
  select(all_of(var_names))
sigma_twfe_syg_mat = as.matrix(sigma_twfe_syg)

full_beta_twfe <- as.data.frame(twfe_syg$coefficients) 
beta_twfe_syg <- slice(full_beta_twfe, c(9, 10, 11, 12, 13, 14, 15, 16, 17))
colnames(beta_twfe_syg) <- 'V1'
beta_twfe_syg_vec = beta_twfe_syg[['V1']]

twfe_fstat_syg <- t(beta_twfe_syg_vec) %*% solve(sigma_twfe_syg_mat) %*% beta_twfe_syg_vec
twfe_pval_syg <- 1 - pchisq(twfe_fstat_syg, df = length(beta_twfe_syg_vec))




############################ ERPO
twfe_erpo = feols(log_state_tot ~ i(t2t_erpo, ref = -1) | ..full_ctrls,
                  cluster = "State",
                  data = reg_data)

erpo_ftest <- linearHypothesis(twfe_erpo, c("t2t_erpo::-10 = 0",
                                            "t2t_erpo::-9 = 0",
                                            "t2t_erpo::-8 = 0",
                                            "t2t_erpo::-7 = 0",
                                            "t2t_erpo::-6 = 0",
                                            "t2t_erpo::-5 = 0",
                                            "t2t_erpo::-4 = 0",
                                            "t2t_erpo::-3 = 0",
                                            "t2t_erpo::-2 = 0"),
                              # error.df = 46, # was told to do num_clusters - 1
                              test = "Chisq")

sunab_erpo <- feols(log_state_tot ~ sunab(red_flag_yr, Year) | ..full_ctrls, 
                   cluster = "State",
                   data = reg_data)

sunab_var_names <- c('factor(sunab_cohorts)-10',
                     'factor(sunab_cohorts)-9',
                     'factor(sunab_cohorts)-8',
                     'factor(sunab_cohorts)-7',
                     'factor(sunab_cohorts)-6',
                     'factor(sunab_cohorts)-5',
                     'factor(sunab_cohorts)-4',
                     'factor(sunab_cohorts)-3',
                     'factor(sunab_cohorts)-2')

sigma_erpo_sunab <- sunab_beta_vcv(sunab_erpo)$sigma[sunab_var_names, sunab_var_names]
beta_erpo_sunab <- sunab_beta_vcv(sunab_erpo)$beta[10:18]

sunab_t_stat_erpo <- t(beta_erpo_sunab) %*% solve(sigma_erpo_sunab) %*% beta_erpo_sunab
sunab_p_val_erpo <- 1 - pchisq(sunab_t_stat_erpo, df = length(beta_erpo_sunab))

# check with erpo_twfe
twfe_var_names <- c("t2t_erpo::-10",
               "t2t_erpo::-9",
               "t2t_erpo::-8",
               "t2t_erpo::-7",
               "t2t_erpo::-6",
               "t2t_erpo::-5",
               "t2t_erpo::-4",
               "t2t_erpo::-3",
               "t2t_erpo::-2")

sigma_twfe_erpo <- twfe_erpo$cov.scaled[twfe_var_names, twfe_var_names]
beta_twfe_erpo <- twfe_erpo$coefficients[twfe_var_names]

twfe_fstat_erpo <- t(beta_twfe_erpo) %*% solve(sigma_twfe_erpo) %*% beta_twfe_erpo
twfe_pval_erpo <- 1 - pchisq(twfe_fstat_erpo, df = length(beta_twfe_erpo))
