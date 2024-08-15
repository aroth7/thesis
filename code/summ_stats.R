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
# library(extrafont)
library(ggfixest)

# # load in latex fonts
# font_import(pattern = "lmroman*")
# loadfonts()

setwd('/Users/annabelroth/Desktop/cs_classes/thesis/data/')

raw <- read_excel('final_data.xlsx')

full_controls <- c('pct_black_2001', 'pct_m15_29_2001', 'per_cap_police_2001', 'pov_rate_2001', 'unemp_rate_2001', 
                   'alc_gal_per_cap_2001', 'encarceration_rate_2001', 'log_pop_calc_2001', 'pop_density_calc_2001', 
                   'crime_per_cap_2001', 'guns_per_household_2001', 'homicides_lag')

names_dict <- c('pct_black_2001'="% Black", 'pct_m15_29_2001'="% Male + Age 15-29", 'per_cap_police_2001'="Law Enforcement per Capita", 
                'pov_rate_2001'="Poverty Rate", 'unemp_rate_2001'="Unemployment Rate", 'alc_gal_per_cap_2001'="Alcohol Consumption per Capita", 
                'encarceration_rate_2001'="Encarceration Rate", 'log_pop_calc_2001'="Log of Population", 
                'pop_density_calc_2001'="Population Density", 'crime_per_cap_2001'="Violent Crime per Capita", 
                'guns_per_household_2001'='Gun Ownership', 'homicides_lag'='Age-Adjusted Firearm Homicide Rate (2001)')

rel_data <- raw %>% 
  mutate(log_state_tot = if_else(!is.na(age_adj_rate_tot), log(age_adj_rate_tot), 0)) %>% 
  select(fip,
         year,
         age_adj_rate_tot,
         log_state_tot,
         syg,
         may_vs_other,
         red_flag,
         all_of(full_controls))

syg_summ <- did_means(fml = pct_black_2001 + pct_m15_29_2001 + per_cap_police_2001 + pov_rate_2001 + unemp_rate_2001 +
                  alc_gal_per_cap_2001 + encarceration_rate_2001 + log_pop_calc_2001 + pop_density_calc_2001 +
                  crime_per_cap_2001 + guns_per_household_2001 + homicides_lag ~ syg,
                  treat_dict = c('1'='SYG Law', '0'='No SYG Law'),
                  dict = names_dict,
                  base = rel_data,
                  file ="syg_summ_stats.tex",
                  replace = TRUE)

cc_summ <- did_means(fml = pct_black_2001 + pct_m15_29_2001 + per_cap_police_2001 + pov_rate_2001 + unemp_rate_2001 +
                        alc_gal_per_cap_2001 + encarceration_rate_2001 + log_pop_calc_2001 + pop_density_calc_2001 +
                        crime_per_cap_2001 + guns_per_household_2001 + homicides_lag ~ may_vs_other,
                      treat_dict = c('1'='"May Issue" Law', '0'='"Shall Issue" or Permitless Carry Law'),
                      dict = names_dict,
                      base = rel_data,
                      file ="cc_summ_stats.tex",
                      replace = TRUE)

erpo_summ <- did_means(fml = pct_black_2001 + pct_m15_29_2001 + per_cap_police_2001 + pov_rate_2001 + unemp_rate_2001 +
                       alc_gal_per_cap_2001 + encarceration_rate_2001 + log_pop_calc_2001 + pop_density_calc_2001 +
                       crime_per_cap_2001 + guns_per_household_2001 + homicides_lag ~ red_flag,
                     treat_dict = c('1'='ERPO Law', '0'='No ERPO Law'),
                     dict = names_dict,
                     base = rel_data,
                     file ="erpo_summ_stats.tex",
                     replace = TRUE)



