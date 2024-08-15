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

setFixest_fml(..full_ctrls = ~ Year[pct_black_2001, pct_m15_29_2001, per_cap_police_2001,
                                    pov_rate_2001, unemp_rate_2001, alc_gal_per_cap_2001, encarceration_rate_2001,
                                    log_pop_calc_2001, pop_density_calc_2001, guns_per_household_2001,
                                    crime_per_cap_2001, homicides_lag] + State,
              ..no_ctrls = ~Year + State)

pretty_plot <- function(reg_df1, reg_df2, title, fig_name, firstT, lastT){
  reg_df1$relativeTime <- c(seq(firstT,-1),seq(1,lastT))-1
  reg_df1$ub <- reg_df1$Estimate + 1.96 * reg_df1$`Std. Error`
  reg_df1$lb <- reg_df1$Estimate - 1.96 * reg_df1$`Std. Error`
  reg_df1 <- reg_df1 %>% filter(relativeTime >= -10, relativeTime <= 15) %>% 
    mutate(est = 'Full Covariate Panel')
  
  reg_df2$relativeTime <- c(seq(firstT,-1),seq(1,lastT))-1
  reg_df2$ub <- reg_df2$Estimate + 1.96 * reg_df2$`Std. Error`
  reg_df2$lb <- reg_df2$Estimate - 1.96 * reg_df2$`Std. Error`
  reg_df2 <- reg_df2 %>% filter(relativeTime >= -10, relativeTime <= 15) %>% 
    mutate(est = 'FEs Only')
  
  combined_df <- bind_rows(reg_df1, reg_df2)
  
  plt <-ggplot() +
    geom_point(data = combined_df, aes(x = relativeTime, y = Estimate, color = est)) + 
    geom_errorbar(data = combined_df, aes(x = relativeTime, ymin = lb, ymax = ub, color = est)) +
    # geom_point(data = reg_df1, aes(x = relativeTime, y = Estimate), color = "black") +
    # geom_errorbar(data = reg_df1, aes(x = relativeTime, ymin = lb, ymax = ub), width = 0.5, color = "black") +
    # geom_point(data = reg_df2, aes(x = relativeTime, y = Estimate), color = "blue") +
    # geom_errorbar(data = reg_df2, aes(x = relativeTime, ymin = lb, ymax = ub), width = 0.5, color = "blue") +
    geom_vline(xintercept=-1, color = 'red') +
    geom_point(x=-1,y=0) +
    xlab("Year Relative to Treatment") +
    ylab("Estimate") +
    ggtitle(title) +
    theme(plot.title= element_text(hjust=0.5)) + 
    scale_color_manual(name = "Model", values = c("black", "blue"))
  print(plt)
}

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

syg_twfe <- feols(log_state_tot ~ i(t2t_syg, ref = -1) | ..full_ctrls,
                        cluster = "State",
                        data = reg_data)

syg_twfe_no_ctrls <- feols(log_state_tot ~ i(t2t_syg, ref = -1) | ..no_ctrls,
                          cluster = "State",
                          data = reg_data)

firstT = -17
lastT = 19

full_ctrls_df <- as.data.frame(coeftable(syg_twfe)[1:(lastT-firstT),])
no_ctrls_df <- as.data.frame(coeftable(syg_twfe_no_ctrls)[1:(lastT-firstT),])
pretty_plot(full_ctrls_df, no_ctrls_df, 'TWFE Coefficients (full covariate panel vs. FEs only)', 'cc_full_covs.pdf', firstT = -17, lastT = 19)


syg_sunab <- feols(log_state_tot ~ sunab(syg_yr, Year) | ..full_ctrls,
                           cluster = "State",
                           data = reg_data)

syg_sunab_no_ctrls <- feols(log_state_tot ~ sunab(syg_yr, Year) | ..no_ctrls,
                   cluster = "State",
                   data = reg_data)
lastT = 15
full_ctrls_df <- as.data.frame(coeftable(syg_sunab)[1:(lastT-firstT),])
no_ctrls_df <- as.data.frame(coeftable(syg_sunab_no_ctrls)[1:(lastT-firstT),])
pretty_plot(full_ctrls_df, no_ctrls_df, 'Sun-Abrahram Coefficients (full covariate panel vs. FEs only)', 'cc_full_covs.pdf', firstT = -17, lastT = 15)


syg_twfe_agg <- aggregate(syg_twfe, c("att" = "t2t_syg::[^-]"))
syg_sunab_agg <- aggregate(syg_sunab, c("att" = "Year::[^-]"))





cc_twfe <- feols(log_state_tot ~ i(t2t_may_vs_other, ref = -1) | ..full_ctrls,
                  cluster = "State",
                  data = reg_data)

cc_twfe_no_ctrls <- feols(log_state_tot ~ i(t2t_may_vs_other, ref = -1) | ..no_ctrls,
                          cluster = "State",
                          data = reg_data)

firstT = -11
lastT = 19

full_ctrls_df <- as.data.frame(coeftable(cc_twfe)[1:(lastT-firstT),])
no_ctrls_df <- as.data.frame(coeftable(cc_twfe_no_ctrls)[1:(lastT-firstT),])
pretty_plot(full_ctrls_df, no_ctrls_df, 'TWFE Coefficients (full covariate panel vs. FEs only)', 'cc_full_covs.pdf', firstT = -11, lastT = 19)

cc_sunab <- feols(log_state_tot ~ sunab(may_vs_other_yr, Year) | ..full_ctrls,
                   cluster = "State",
                   data = reg_data)

cc_sunab_no_ctrls <- feols(log_state_tot ~ sunab(may_vs_other_yr, Year) | ..no_ctrls,
                            cluster = "State",
                            data = reg_data)
lastT = 17
full_ctrls_df <- as.data.frame(coeftable(cc_sunab)[1:(lastT-firstT),])
no_ctrls_df <- as.data.frame(coeftable(cc_sunab_no_ctrls)[1:(lastT-firstT),])
pretty_plot(full_ctrls_df, no_ctrls_df, 'Sun-Abrahram Coefficients (full covariate panel vs. FEs only)', 'cc_full_covs.pdf', firstT = -11, lastT = 17)

cc_twfe_agg <- aggregate(cc_twfe, c("att" = "t2t_may_vs_other::[^-]"))
cc_sunab_agg <- aggregate(cc_sunab, c("att" = "Year::[^-]"))






erpo_twfe <- feols(log_state_tot ~ i(t2t_erpo, ref = -1) | ..full_ctrls,
                 cluster = "State",
                 data = reg_data)

erpo_twfe_no_ctrls <- feols(log_state_tot ~ i(t2t_erpo, ref = -1) | ..no_ctrls,
                           cluster = "State",
                           data = reg_data)

firstT = -18
lastT = 19

full_ctrls_df <- as.data.frame(coeftable(erpo_twfe)[1:(lastT-firstT),])
no_ctrls_df <- as.data.frame(coeftable(erpo_twfe_no_ctrls)[1:(lastT-firstT),])
pretty_plot(full_ctrls_df, no_ctrls_df, 'TWFE Coefficients (full covariate panel vs. FEs only)', 'cc_full_covs.pdf', firstT = -18, lastT = 19)

erpo_sunab <- feols(log_state_tot ~ sunab(red_flag_yr, Year) | ..full_ctrls,
                  cluster = "State",
                  data = reg_data)

erpo_sunab_no_ctrls <- feols(log_state_tot ~ sunab(red_flag_yr, Year) | ..no_ctrls,
                            cluster = "State",
                            data = reg_data)
lastT = 15
full_ctrls_df <- as.data.frame(coeftable(erpo_sunab)[1:(lastT-firstT),])
no_ctrls_df <- as.data.frame(coeftable(erpo_sunab_no_ctrls)[1:(lastT-firstT),])
pretty_plot(full_ctrls_df, no_ctrls_df, 'Sun-Abrahram Coefficients (full covariate panel vs. FEs only)', 'cc_full_covs.pdf', firstT = -18, lastT = 15)

erpo_twfe_agg <- aggregate(erpo_twfe, c("att" = "t2t_erpo::[^-]"))
erpo_sunab_agg <- aggregate(erpo_sunab, c("att" = "Year::[^-]"))


