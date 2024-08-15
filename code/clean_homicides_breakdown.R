library(tidyverse)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(tidycensus)
library(XLConnect)
library(writexl)

setwd('/Users/annabelroth/Desktop/cs_classes/thesis/data/homicides_guns/')

# load in fips codes to state name crosswalk
data(fips_codes)
fips_codes$state_code <- as.integer(fips_codes$state_code)

# get sheet names (yrs) from race_full
homicides_sheets <- excel_sheets("race_full.xlsx")

# empty list to store data frames for each yr
data_list_race <- list()
data_list_sex <- list()
data_list_metro <- list()

# for each sheet, create col corresponding to sheet name (yr) and add df to list
for (sheet_name in homicides_sheets) {
  race_df <- read_excel("race_full.xlsx", sheet = sheet_name)
  race_df$year <- as.integer(sheet_name)  # add col for yr
  data_list_race[[sheet_name]] <- race_df
  
  sex_df <- read_excel("sex_full.xlsx", sheet = sheet_name)
  sex_df$year <- as.integer(sheet_name)  # add col for yr
  data_list_sex[[sheet_name]] <- sex_df
  
  metro_df <- read_excel("metro_full.xlsx", sheet = sheet_name)
  metro_df$year <- as.integer(sheet_name)  # add col for yr
  data_list_metro[[sheet_name]] <- metro_df
}

# combine dfs in list to 1 df with new yr col
homicides_race_raw <- bind_rows(data_list_race)  %>% 
  mutate(age_adj_rate = as.numeric(`Age-Adjusted Rate`))

homicides_sex_raw <- bind_rows(data_list_sex)  %>% 
  mutate(age_adj_rate = as.numeric(`Age-Adjusted Rate`))

homicides_metro_raw <- bind_rows(data_list_metro)  %>% 
  mutate(age_adj_rate = as.numeric(`Age-Adjusted Rate`),
         metro_indicator = `Metro / Non-Metro`)


race_breakdown <- homicides_race_raw %>% 
  filter(State != "Total") %>% 
  group_by(State, year, Race) %>%
  mutate(Race = case_when(
    Race == "American Indian / Alaska Native" ~ "AI",
    Race == "Asian / HI Native / Pac. Islander" ~ "AAPI",
    Race == "Black" ~ "black",
    Race == "White" ~ "white"
  )) %>% 
  select(state_name = State,
         year,
         race = Race,
         deaths = Deaths,
         age_adj_rate)  %>%
  pivot_wider(names_from = race, values_from = c(deaths, age_adj_rate))


sex_breakdown <- homicides_sex_raw %>% 
  filter(State != "Total") %>% 
  group_by(State, year, Sex) %>%
  mutate(Sex = case_when(
    Sex == "Males" ~ "m",
    Sex == "Females" ~ "f"
  )) %>% 
  select(state_name = State,
         year,
         sex = Sex,
         deaths = Deaths,
         age_adj_rate)  %>%
  pivot_wider(names_from = sex, values_from = c(deaths, age_adj_rate))

metro_breakdown <- homicides_metro_raw %>% 
  filter(State != "Total") %>% 
  group_by(State, year, metro_indicator) %>%
  mutate(metro_indicator = case_when(
    metro_indicator == "Metro" ~ "metro",
    metro_indicator == "Non-Metro" ~ "non_metro"
  )) %>% 
  select(state_name = State,
         year,
         metro_indicator,
         deaths = Deaths,
         age_adj_rate)  %>%
  pivot_wider(names_from = metro_indicator, values_from = c(deaths, age_adj_rate))

setwd('/Users/annabelroth/Desktop/cs_classes/thesis/data/')
mike_homicides <- read_excel('mike_death_data.xlsx') %>% 
  rename(state_code = fip)

state_full <- mike_homicides %>%
  left_join(fips_codes[c("state_code", "state_name")], by = "state_code", relationship = "many-to-many") %>% 
  filter(year >= 2000, year <= 2020) %>%
  select(fip = state_code,
         state_name,
         year,
         state_tot,
         age_adj_rate_tot
  ) %>% 
  distinct(year, fip, .keep_all = TRUE)


full_data <- state_full %>%
  left_join(race_breakdown, by = c("state_name", "year")) %>%
  left_join(sex_breakdown, by = c("state_name", "year")) %>%
  left_join(metro_breakdown, by = c("state_name", "year")) %>%
  left_join(fips_codes[c("state_name", "state_code")], by = "state_name", relationship = "many-to-many") %>%
  select(fip = state_code,
         year,
         state_tot,
         age_adj_rate_tot,
         deaths_m,
         deaths_f,
         deaths_AI,
         deaths_AAPI,
         deaths_white,
         deaths_black,
         deaths_metro,
         deaths_non_metro,
         age_adj_rate_m,
         age_adj_rate_f,
         age_adj_rate_AI,
         age_adj_rate_AAPI,
         age_adj_rate_white,
         age_adj_rate_black,
         age_adj_rate_metro,
         age_adj_rate_non_metro
  ) %>% 
  distinct(year, fip, .keep_all = TRUE)

write_xlsx(
  full_data,
  path = "homicides_data.xlsx",
  col_names = TRUE,
)

write_xlsx(
  state_full,
  path = "mike_death_data.xlsx",
  col_names = TRUE,
)



