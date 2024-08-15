library(tidyverse)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(tidycensus)
library(ggplot2)
library(fixest)
library(ggpubr)
library(vtable)
library(ggfixest)

setwd('/Users/annabelroth/Desktop/cs_classes/thesis/data/')

raw <- read_excel('final_data.xlsx')

full_controls <- c('pct_black_2001', 'pct_m15_29_2001', 'per_cap_police_2001', 'pov_rate_2001', 'unemp_rate_2001', 
                   'alc_gal_per_cap_2001', 'encarceration_rate_2001', 'log_pop_calc_2001', 'pop_density_calc_2001', 
                   'crime_per_cap_2001', 'guns_per_household_2001', 'homicides_lag')

standard_ctrls <- setdiff(full_controls, c('guns_per_household_2001', 'crime_per_cap_2001', 'homicides_lag'))

setFixest_fml(..full_ctrls = ~ Year[pct_black_2001, pct_m15_29_2001, per_cap_police_2001,
                                    pov_rate_2001, unemp_rate_2001, alc_gal_per_cap_2001, encarceration_rate_2001,
                                    log_pop_calc_2001, pop_density_calc_2001, guns_per_household_2001,
                                    crime_per_cap_2001, homicides_lag] + State,
              
              ..full_ctrls_no_lag = ~ Year[pct_black_2001, pct_m15_29_2001, per_cap_police_2001,
                                           pov_rate_2001, unemp_rate_2001, alc_gal_per_cap_2001, encarceration_rate_2001,
                                           log_pop_calc_2001, pop_density_calc_2001, guns_per_household_2001,
                                           crime_per_cap_2001] + State,
              
              ..part_ctrls_lag = ~ Year[pct_black_2001, pct_m15_29_2001, per_cap_police_2001,
                                        pov_rate_2001, unemp_rate_2001, alc_gal_per_cap_2001, encarceration_rate_2001,
                                        log_pop_calc_2001, pop_density_calc_2001, homicides_lag] + State,
              
              ..part_ctrls_no_lag = ~ Year[pct_black_2001, pct_m15_29_2001, per_cap_police_2001,
                                           pov_rate_2001, unemp_rate_2001, alc_gal_per_cap_2001, encarceration_rate_2001,
                                           log_pop_calc_2001, pop_density_calc_2001] + State,
              
              ..fes_only = ~ Year + State)

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

# func to create pretty plot by extracting ceofs and CIs from reg output
# takes in reg_df and name to save fig as
pretty_plot <- function(reg_df, title, fig_name, firstT, lastT){
  reg_df$relativeTime <- c(seq(firstT,-1),seq(1,lastT))-1
  # reg_df$relativeTime <- c(seq(firstT,-2),seq(0,lastT))-1 # use this line for syg ref = -2
  reg_df$ub <- reg_df$Estimate + 1.96 * reg_df$`Std. Error`
  reg_df$lb <- reg_df$Estimate - 1.96 * reg_df$`Std. Error`
  reg_df <- reg_df %>% filter(relativeTime >= -10, relativeTime <= 15)
  plt <-ggplot(reg_df,
               aes(x = relativeTime,
                   y = Estimate,
                   xmin = -10,
                   xmax = 15,
                   ymax = ub,
                   ymin = lb)) +
    geom_point() +
    geom_vline(xintercept=-1, color = 'red') +
    # geom_hline(yintercept=0.1, lty = 'dashed') + # use this line for syg ref = -1
    geom_point(x=-1,y=0) +
    # geom_vline(xintercept=-2, color = 'red') + # use this line for syg ref = -2
    # geom_point(x=-2,y=0) + # use this line for syg ref = -2
    geom_errorbar(width = 0.5, color='blue') +
    xlab("Year Relative to Treatment") +
    ylab("Estimate") +
    ggtitle(title) +
    theme(plot.title= element_text(hjust=0.5))
  print(plt)
}


################# SYG
event_study_syg = feols(log_state_tot ~ i(t2t_syg, ref = -1) | 
                          sw(..full_ctrls, ..full_ctrls_no_lag, ..part_ctrls_lag, ..part_ctrls_no_lag, ..fes_only),
                        cluster = "State",
                        data = reg_data)

sunab_syg <- feols(log_state_tot ~ sunab(syg_yr, Year) | 
                     sw(..full_ctrls, ..full_ctrls_no_lag, ..part_ctrls_lag, ..part_ctrls_no_lag, ..fes_only),
                   cluster = "State",
                   data = reg_data)

event_study_syg_2 = feols(log_state_tot ~ i(t2t_syg, ref = -2) | 
                          sw(..full_ctrls, ..full_ctrls_no_lag, ..part_ctrls_lag, ..part_ctrls_no_lag),
                        cluster = "State",
                        data = reg_data)

sunab_syg_2 <- feols(log_state_tot ~ sunab(syg_yr, Year, ref.p = c(-2, .F)) | ..full_ctrls,
                     cluster = "State",
                     data = reg_data)

table_syg <- etable(event_study_syg, sunab_syg)

syg_tex <- etable(event_study_syg, sunab_syg,
                  fontsize = 'tiny',
                  se.row = FALSE,
                  headers=list("Dynamic TWFE"=5, "Sun-Abraham"=5),
                  i.equal = ": ",
                  se.below = FALSE,
                  signif.code = NA,
                  file ="syg_tex.tex",
                  replace = TRUE)

# syg plots:
firstT = -17
lastT = 19

# plot for ref yr = -2
full_ctrls_df_2 <- as.data.frame(coeftable(event_study_syg_2[fixef = 1])[1:(lastT-firstT),])
pretty_plot(full_ctrls_df_2, '', 'syg_full_covs.pdf', -17, 19)

full_ctrls_df <- as.data.frame(coeftable(event_study_syg[fixef = 1])[1:(lastT-firstT),])
pretty_plot(full_ctrls_df, 'TWFE Coefficients (full covariate panel)', 'syg_full_covs.pdf', firstT, lastT)

full_ctrls_no_lag_df <- as.data.frame(coeftable(event_study_syg[fixef = 2])[1:(lastT-firstT),])
pretty_plot(full_ctrls_no_lag_df, 'Model 2 (TWFE)', 'syg_full_no_lag.pdf', firstT, lastT)

part_ctrls_df <- as.data.frame(coeftable(event_study_syg[fixef = 3])[1:(lastT-firstT),])
pretty_plot(part_ctrls_df, 'Model 3 (TWFE)', 'syg_part_covs.pdf', firstT, lastT)

full_ctrls_no_lag_df <- as.data.frame(coeftable(event_study_syg[fixef = 4])[1:(lastT-firstT),])
pretty_plot(full_ctrls_no_lag_df, 'Model 4 (TWFE)', 'syg_part_no_lag.pdf', firstT, lastT)

#### sunab coefplots
lastT = 15

# plot for ref yr = -2
full_ctrls_sunab_df_2 <- as.data.frame(coeftable(sunab_syg_2)[1:(lastT-firstT),])
pretty_plot(full_ctrls_sunab_df_2, '', 'syg_full_covs.pdf', -17, 15)

full_ctrls_sunab_df <- as.data.frame(coeftable(sunab_syg[fixef = 1])[1:(lastT-firstT),])
pretty_plot(full_ctrls_sunab_df, 'Sun-Abraham Coefficient (full covariate panel)', 'syg_full_covs_sunab.pdf', firstT, lastT)

full_ctrls_no_lag_sunab_df <- as.data.frame(coeftable(sunab_syg[fixef = 2])[1:(lastT-firstT),])
pretty_plot(full_ctrls_no_lag_sunab_df, 'Model 2 (Sun-Abraham)', 'syg_full_no_lag_sunab.pdf', firstT, lastT)

part_ctrls_sunab_df <- as.data.frame(coeftable(sunab_syg[fixef = 3])[1:(lastT-firstT),])
pretty_plot(part_ctrls_sunab_df, 'Model 3 (Sun-Abraham)', 'syg_part_covs_sunab.pdf', firstT, lastT)

full_ctrls_no_lag_sunab_df <- as.data.frame(coeftable(sunab_syg[fixef = 4])[1:(lastT-firstT),])
pretty_plot(full_ctrls_no_lag_sunab_df, 'Model 4 (Sun-Abraham)', 'syg_part_no_lag_sunab.pdf', firstT, lastT)


################# CONCEALED CARRY
event_study_cc = feols(log_state_tot ~ i(t2t_may_vs_other, ref = -1) | 
                         sw(..full_ctrls, ..full_ctrls_no_lag, ..part_ctrls_lag, ..part_ctrls_no_lag),
                       cluster = "State",
                       data = reg_data)

sunab_cc <- feols(log_state_tot ~ sunab(may_vs_other_yr, Year) | 
                    sw(..full_ctrls, ..full_ctrls_no_lag, ..part_ctrls_lag, ..part_ctrls_no_lag),
                  cluster = "State",
                  data = reg_data)

table_cc <- etable(event_study_cc, sunab_cc)

cc_tex <- etable(event_study_cc, sunab_cc,
                 fontsize = 'tiny',
                 se.row = FALSE,
                 headers=list("Dynamic TWFE"=4, "Sun-Abraham"=4),
                 # fixef.group = list("Standard Controls" = "^_2001$",
                 #                    "Lagged Homicides" = "homicides_lag",
                 #                    "Violent Crime and Guns/Household" = 'guns_per_household_2001|crime_per_cap_2001'),
                 drop = standard_ctrls,
                 i.equal = ": ",
                 se.below = FALSE,
                 signif.code = NA,
                 file ="cc_tex.tex",
                 replace = TRUE)

# cc plots:
firstT = -11
lastT = 19

full_ctrls_df <- as.data.frame(coeftable(event_study_cc[fixef = 1])[1:(lastT-firstT),])
pretty_plot(full_ctrls_df, 'TWFE Coefficients (full covariate panel)', 'cc_full_covs.pdf', firstT = -11, lastT = 19)

full_ctrls_no_lag_df <- as.data.frame(coeftable(event_study_cc[fixef = 2])[1:(lastT-firstT),])
pretty_plot(full_ctrls_no_lag_df, 'Model 2 (TWFE)', 'cc_full_no_lag.pdf', firstT = -11, lastT = 19)

part_ctrls_df <- as.data.frame(coeftable(event_study_cc[fixef = 3])[1:(lastT-firstT),])
pretty_plot(part_ctrls_df, 'Model 3 (TWFE)', 'cc_part_covs.pdf', firstT = -11, lastT = 19)

part_ctrls_df <- as.data.frame(coeftable(event_study_cc[fixef = 4])[1:(lastT-firstT),])
pretty_plot(part_ctrls_df, 'Model 4 (TWFE)', 'cc_part_no_lag.pdf', firstT = -11, lastT = 19)

### sunab coefplots
lastT = 17

full_ctrls_sunab_df <- as.data.frame(coeftable(sunab_cc[fixef = 1])[1:(lastT-firstT),])
pretty_plot(full_ctrls_sunab_df, 'Sun-Abraham Coefficients (full covariate panel)', 'cc_full_covs_sunab.pdf', firstT = -11, lastT = 17)

full_ctrls_no_lag_df <- as.data.frame(coeftable(sunab_cc[fixef = 2])[1:(lastT-firstT),])
pretty_plot(full_ctrls_no_lag_df, 'Model 2 (Sun-Abraham)', 'cc_full_no_lag_sunab.pdf', firstT = -11, lastT = 17)

part_ctrls_sunab_df <- as.data.frame(coeftable(sunab_cc[fixef = 3])[1:(lastT-firstT),])
pretty_plot(part_ctrls_sunab_df, 'Model 3 (Sun-Abraham)', 'cc_part_covs_sunab.pdf', firstT = -11, lastT = 17)

part_ctrls_sunab_df <- as.data.frame(coeftable(sunab_cc[fixef = 4])[1:(lastT-firstT),])
pretty_plot(full_ctrls_no_lag_df, 'Model 4 (Sun-Abraham)', 'cc_part_no_lag_sunab.pdf', firstT = -11, lastT = 17)



################# ERPO
event_study_erpo <- feols(log_state_tot ~ i(t2t_erpo, ref = -1) | 
                            sw(..full_ctrls, ..full_ctrls_no_lag, ..part_ctrls_lag, ..part_ctrls_no_lag),
                          cluster = "State",
                          data = reg_data)

sunab_erpo <- feols(log_state_tot ~ sunab(red_flag_yr, Year) | 
                      sw(..full_ctrls, ..full_ctrls_no_lag, ..part_ctrls_lag, ..part_ctrls_no_lag),
                    cluster = "State",
                    data = reg_data)

table_erpo <- etable(event_study_erpo, sunab_erpo)

erpo_tex <- etable(event_study_erpo, sunab_erpo,
                   fontsize = 'tiny',
                   se.row = FALSE,
                   headers=list("Dynamic TWFE"=4, "Sun-Abraham"=4),
                   i.equal = ": ",
                   se.below = FALSE,
                   signif.code = NA,
                   file ="erpo_tex.tex",
                   replace = TRUE)

# erpo plots:
firstT = -18
lastT = 19

full_ctrls_df <- as.data.frame(coeftable(event_study_erpo[fixef = 1])[1:(lastT-firstT),])
pretty_plot(full_ctrls_df, 'TWFE Coefficients (full covariate panel)', 'erpo_full_covs.pdf', firstT = -18, lastT = 19)

full_ctrls_no_lag_df <- as.data.frame(coeftable(event_study_erpo[fixef = 2])[1:(lastT-firstT),])
pretty_plot(full_ctrls_no_lag_df, 'Model 2 (TWFE)', 'erpo_full_no_lag.pdf', firstT = -18, lastT = 19)

part_ctrls_df <- as.data.frame(coeftable(event_study_erpo[fixef = 3])[1:(lastT-firstT),])
pretty_plot(part_ctrls_df, 'Model 3 (TWFE)', 'erpo_part_covs.pdf', firstT = -18, lastT = 19)

full_ctrls_no_lag_df <- as.data.frame(coeftable(event_study_erpo[fixef = 4])[1:(lastT-firstT),])
pretty_plot(full_ctrls_no_lag_df, 'Model 4 (TWFE)', 'erpo_part_no_lag.pdf', firstT = -18, lastT = 19)

### sunab coefplots
lastT = 15

full_ctrls_sunab_df <- as.data.frame(coeftable(sunab_erpo[fixef = 1])[1:(lastT-firstT),])
pretty_plot(full_ctrls_sunab_df, 'Sun-Abraham Coefficients (full covariate panel)', 'erpo_full_covs_sunab.pdf', firstT = -18, lastT = 15)

full_ctrls_no_lag_sunab_df <- as.data.frame(coeftable(sunab_erpo[fixef = 2])[1:(lastT-firstT),])
pretty_plot(full_ctrls_no_lag_sunab_df, 'Model 2 (Sun-Abraham)', 'erpo_full_no_lag_sunab.pdf', firstT = -18, lastT = 15)

part_ctrls_sunab_df <- as.data.frame(coeftable(sunab_erpo[fixef = 3])[1:(lastT-firstT),])
pretty_plot(part_ctrls_sunab_df, 'Model 3 (Sun-Abraham)', 'erpo_part_covs_sunab.pdf', firstT = -18, lastT = 15)

full_ctrls_no_lag_sunab_df <- as.data.frame(coeftable(sunab_erpo[fixef = 4])[1:(lastT-firstT),])
pretty_plot(full_ctrls_no_lag_sunab_df, 'Model 4 (Sun-Abraham)', 'erpo_part_no_lag_sunab.pdf', firstT = -18, lastT = 15)


######### static TWFE model for cc laws
cc_data <- reg_data %>% 
  mutate(post = if_else(syg_yr >= Year, 1, 0),
         ever_treat = reg_data$Year == 2020,
         post_x_treat = post * ever_treat) 

static_twfe_cc <- feols(log_state_tot ~ post_x_treat |
                          sw(..full_ctrls, ..full_ctrls_no_lag, ..part_ctrls_lag, ..part_ctrls_no_lag),
                        cluster = "State",
                        data = cc_data)
table <- etable(static_twfe_cc)

