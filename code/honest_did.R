library(tidyverse)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(ggplot2)
library(fixest)
library(ggfixest)
library(HonestDiD)

setwd('/Users/annabelroth/Desktop/cs_classes/thesis/data/')

pretty_plot <- function(reg_df, title, fig_name, firstT, lastT){
  reg_df$relativeTime <- c(seq(firstT,-1),seq(1,lastT))-1
  reg_df$ub <- reg_df$Estimate + 1.96 * reg_df$`Std. Error`
  reg_df$lb <- reg_df$Estimate - 1.96 * reg_df$`Std. Error`
  reg_df <- reg_df %>% filter(relativeTime >= -9, relativeTime <= 15)
  plt <-ggplot(reg_df,
               aes(x = relativeTime,
                   y = Estimate,
                   xmin = -10,
                   xmax = 15,
                   ymax = ub,
                   ymin = lb)) +
    geom_point() +
    geom_vline(xintercept=-1, color = 'red') +
    geom_point(x=-1,y=0) +
    geom_point(x=-10,y=0) +
    geom_errorbar(width = 0.5, color='blue') +
    xlab("Year Relative to Treatment") +
    ylab("Estimate") +
    ggtitle(title) +
    theme(plot.title= element_text(hjust=0.5))
  print(plt)
}

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

raw <- read_excel('final_data.xlsx')

full_controls <- c('pct_black_2001', 'pct_m15_29_2001', 'per_cap_police_2001', 'pov_rate_2001', 'unemp_rate_2001', 
                   'alc_gal_per_cap_2001', 'encarceration_rate_2001', 'log_pop_calc_2001', 'pop_density_calc_2001', 
                   'crime_per_cap_2001', 'guns_per_household_2001', 'homicides_lag')

setFixest_fml(..full_ctrls = ~ Year[pct_black_2001, pct_m15_29_2001, per_cap_police_2001,
                                    pov_rate_2001, unemp_rate_2001, alc_gal_per_cap_2001, encarceration_rate_2001,
                                    log_pop_calc_2001, pop_density_calc_2001, guns_per_household_2001,
                                    crime_per_cap_2001, homicides_lag] + State,
              ..full_ctrls_lin_trend = ~ Year[pct_black_2001, pct_m15_29_2001, per_cap_police_2001,
                                              pov_rate_2001, unemp_rate_2001, alc_gal_per_cap_2001, encarceration_rate_2001,
                                              log_pop_calc_2001, pop_density_calc_2001, guns_per_household_2001,
                                              crime_per_cap_2001, homicides_lag, State])

setFixest_dict(t2t_syg="Year", 
               t2t_may_vs_other="Year",
               t2t_erpo="Year",
               log_state_tot="Log of Age-Adjusted Rate",
               homicides_lag="Lagged Firearm Homicides",
               guns_per_household_2001='Adults in Household with Gun',
               crime_per_cap_2001="Violent Crime per Capita")

# add variables for log of age-adj rate and time to treatment for expand and restrict 
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

# reset wd so can save coefplots
setwd('/Users/annabelroth/Desktop/cs_classes/thesis/outputs/coefplots/')

twfe_cc = feols(log_state_tot ~ i(t2t_may_vs_other, ref = -1) | ..full_ctrls,
                        cluster = "State",
                        data = reg_data)

# twfe with lin trend
twfe_cc_lin_trend = feols(log_state_tot ~ i(t2t_may_vs_other, ref = c(-1, -10)) +  t2t_may_vs_other| ..full_ctrls,
                cluster = "State",
                data = reg_data)


firstT = -10
lastT = 19
lin_trend_df <- as.data.frame(coeftable(twfe_cc_lin_trend)[1:(lastT-firstT),])
pretty_plot(lin_trend_df, '', 'syg_full_covs.pdf', -10, 19)

table <- etable(twfe_cc, twfe_cc_lin_trend)

betahat_twfe <- summary(twfe_cc)$coefficients #save the coefficients 
sigma_twfe <- summary(twfe_cc)$cov.scaled #save the covariance matrix

original_results <- HonestDiD::constructOriginalCS(betahat = betahat_twfe,
                               sigma = sigma_twfe,
                               numPrePeriods = 11,
                               numPostPeriods = 20,
                               l_vec = basisVector(2, 20))

delta_rm_results <- HonestDiD::createSensitivityResults_relativeMagnitudes(betahat = betahat_twfe,
                                                                           sigma = sigma_twfe,
                                                                           numPrePeriods = 11,
                                                                           numPostPeriods = 20,
                                                                           Mbarvec = seq(0.05,.2,by=0.05),
                                                                           # Mbarvec = seq(0.5,2,by=0.5),
                                                                           l_vec = basisVector(2, 20))

delta_rm_results

delta_rm_plt <- HonestDiD::createSensitivityPlot_relativeMagnitudes(delta_rm_results, original_results)
delta_rm_plt

delta_sd_results <- HonestDiD::createSensitivityResults(betahat = betahat_twfe,
                                      sigma = sigma_twfe,
                                      numPrePeriods = 11,
                                      numPostPeriods = 20,
                                      Mvec = seq(from = 0, to = 0.05, by = 0.01),
                                      l_vec = basisVector(2, 20))

delta_sd_results

delta_sd_plt <- createSensitivityPlot(delta_sd_results, original_results)
delta_sd_plt

sunab_cc <- feols(log_state_tot ~ sunab(may_vs_other_yr, Year) | ..full_ctrls, 
                   cluster = "State",
                   data = reg_data)

betahat_sunab <- sunab_beta_vcv(sunab_cc)$beta
sigma_sunab <- sunab_beta_vcv(sunab_cc)$sigma

original_results_sunab <- HonestDiD::constructOriginalCS(betahat = betahat_sunab,
                                                   sigma = sigma_sunab,
                                                   numPrePeriods = 11,
                                                   numPostPeriods = 18,
                                                   l_vec = basisVector(2, 18))

delta_rm_sunab <- HonestDiD::createSensitivityResults_relativeMagnitudes(betahat = betahat_sunab,
                                                                           sigma = sigma_sunab,
                                                                           numPrePeriods = 11,
                                                                           numPostPeriods = 18,
                                                                           Mbarvec = seq(0.05,.2,by=0.05),
                                                                           l_vec = basisVector(2, 18))

delta_rm_sunab

delta_rm_sunab_plt <- HonestDiD::createSensitivityPlot_relativeMagnitudes(delta_rm_sunab, original_results_sunab)
delta_rm_sunab_plt

delta_sd_sunab <- HonestDiD::createSensitivityResults(betahat = betahat_sunab,
                                                        sigma = sigma_sunab,
                                                        numPrePeriods = 11,
                                                        numPostPeriods = 18,
                                                        Mvec = seq(from = 0, to = 0.05, by = 0.01))

delta_sd_sunab

delta_sd_sunab_plt <- createSensitivityPlot(delta_sd_sunab, original_results_sunab)
delta_sd_sunab_plt






