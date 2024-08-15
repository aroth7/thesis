library(tidyverse)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(tidycensus)
library(XLConnect)
library(writexl)

setwd('/Users/annabelroth/Desktop/cs_classes/thesis/data/')

data(fips_codes)
fips_codes$state_code <- as.integer(fips_codes$state_code)

# law data
laws <- read_excel('laws_clean.xlsx')

# controls data
full_controls <- read_excel('controls_data.xlsx')
pretreat_covs <- read_excel('pretreat_covs.xlsx')

# homicides data
homicides_breakdown <- read_excel('homicides_data.xlsx')
homicides <- read_excel('homicides_guns/no_breakdown.xlsx') %>%
  mutate(age_adj_rate_tot = as.numeric(`Age-Adjusted Rate`))  %>% 
  select(State,
         year = Year,
         age_adj_rate_tot)

lagged_homicides <- read_excel('mike_death_data.xlsx') %>% 
  filter(year == 2000) %>% 
  rename(homicides_lag = age_adj_rate_tot)

# add col for lagged homicides to full homicide data
full_homicides <- merge(homicides, fips_codes[c("state_name", "state_code")], by.x = "State", by.y = "state_name") %>%
  rename(fip = state_code) %>% 
  distinct(year, fip, .keep_all = TRUE) %>% 
  left_join(lagged_homicides, by = "fip") %>% 
  select(!year.y) %>% 
  rename(year = year.x) %>% 
  arrange(fip, year)

# data for num restrictive vs expansive
law_counts <- read_excel('law_count_info.xlsx')

law_counts_to_merge <- merge(law_counts, fips_codes[c("state_name", "state_code")], by.x = "state", by.y = "state_name") %>%
  distinct(state_code, year, .keep_all = TRUE) %>%
  mutate(fip = as.integer(state_code))

intermed <- pretreat_covs %>%
  # filter(year != 2000) %>% 
  left_join(full_homicides, by = c('year', 'fip')) %>% 
  left_join(laws, by = c('year', 'fip')) %>% 
  left_join(law_counts_to_merge, by = c('year','fip'))


# add group based on which law types have implemented by final yr
add_groups <- intermed %>% 
  filter(year == 2020) %>% 
  mutate(group = case_when(ever_treated_increase == 1 & ever_treated_decrease == 0 ~ 1,
                           ever_treated_increase == 0 & ever_treated_decrease == 1 ~ 2,
                           ever_treated_increase == 1 & ever_treated_decrease == 1 ~ 3,
                           ever_treated_increase == 0 & ever_treated_decrease == 0 ~ 4)) %>% 
  select(fip, group)

final_data <- intermed  %>% 
  filter(year != 2000) %>% 
  left_join(add_groups, by = "fip")

write_xlsx(
  final_data,
  path = "final_data.xlsx",
  col_names = TRUE,
)
