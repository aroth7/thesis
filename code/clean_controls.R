library(tidyverse)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(tidycensus)
library(XLConnect)
library(writexl)

setwd('/Users/annabelroth/Desktop/cs_classes/thesis/data/')

# general pop data
data <- read_csv('census_data.csv')

data_nec <- data %>% 
  group_by(YEAR, STATEFIP) %>% 
  summarize(num_divorced = sum(ifelse(MARST == 4, PERWT, 0)),
            num_black = sum(ifelse(RACE == 2, PERWT, 0)),
            num_poverty = sum(ifelse(POVERTY <= 100, PERWT, 0)),
            num_unemp = sum(ifelse(EMPSTAT == 2, PERWT, 0)),
            num_lf = sum(ifelse(EMPSTAT == 2 | EMPSTAT == 1 , PERWT, 0)),
            pop = sum(PERWT),
            num_m15_29 = sum(ifelse(AGE >= 15 & AGE <= 29 &  SEX == 1, PERWT, 0))
  )

data_rates <- data_nec %>% 
  group_by(YEAR, STATEFIP) %>% 
  mutate(pct_black = num_black/pop,
         pct_m15_29 = num_m15_29/pop) %>% 
  select(STATEFIP,
         year = YEAR,
         pop,
         num_black, pct_black,
         num_m15_29, pct_m15_29)

# unemployment data
unemp_data <- read_excel('unemp.xlsx')
unemp_data$YEAR <- as.integer(unemp_data$YEAR)

unemp_data_nec <- unemp_data %>%
  filter(YEAR >= 2000 & YEAR <= 2019 & FIPS != "51000" & FIPS != "037") %>% 
  select(STATEFIP = FIPS,
         year = YEAR,
         non_inst_pop = CIV_NON_INST_POP,
         unemp_rate = CIV_LF_UNEMP_RATE)

unemp_data_nec$STATEFIP <- as.integer(unemp_data_nec$STATEFIP)

# poverty rate data

# store sheets from excel
pov_sheets <- excel_sheets("pov_stats.xlsx")

# empty list to store data frames for each yr
data_list <- list()

for (sheet_name in pov_sheets) {
  df <- read_excel("pov_stats.xlsx", sheet = sheet_name)
  df$year <- as.integer(sheet_name)  # add col for yr
  data_list[[sheet_name]] <- df
}

pov_data_raw <- bind_rows(data_list)

# load fips code to state crosswalk dataset
data(fips_codes)
fips_codes$state_code <- as.integer(fips_codes$state_code)

pov_data <- merge(pov_data_raw, fips_codes, by.x = "State", by.y = "state_name") %>% 
  mutate(tot_pop = `Total population`*1000) %>% 
  select(STATEFIP = state_code,
         year,
         tot_pop,
         pov_rate = `Percent in poverty`) %>% 
  distinct(year, STATEFIP, .keep_all = TRUE) %>% 
  arrange(STATEFIP, year)

# alc data
alc_data_raw <- read_csv('alc_per_cap_1997_2018.csv') %>% 
  rename(state_name = state) %>% 
  mutate(state_name = str_to_title(state_name))

alc_data <- merge(alc_data_raw, fips_codes[c("state_name", "state_code")], by = "state_name", all.x = TRUE) %>%
  filter(year >= 2000) %>% 
  select(STATEFIP = state_code,
         year,
         alc_gal_per_cap = ethanol_all_drinks_gallons_per_capita) %>% 
  distinct(year, STATEFIP, .keep_all = TRUE) %>% 
  arrange(STATEFIP, year)

# prisoners data
prisoners_data_raw <- read_excel('yr_end_prisoners.xlsx', skip = 1)

# convert cols for each yr to yr col and value col
prisoners_data_convert <- prisoners_data_raw %>%
  gather(key = "Year", value = "yr_end_prisoners", -State) %>% 
  select(state_name = State,
         year = Year,
         yr_end_prisoners) %>% 
  mutate(state_name = str_to_title(state_name),
         year = as.integer(year),
         yr_end_prisoners = as.integer(yr_end_prisoners))
  
prisoners_data <- merge(prisoners_data_convert, fips_codes[c("state_name", "state_code")], by = "state_name")  %>% 
  select(STATEFIP = state_code,
         year,
         yr_end_prisoners) %>% 
  distinct(year, STATEFIP, .keep_all = TRUE) %>% 
  arrange(STATEFIP, year)

# gun ownership data
gun_data <- read_excel("gun_data_mini.xlsx") %>%
  filter(Year >= 2000) %>% 
  select(STATEFIP = FIP,
         year = Year,
         guns_per_household = HFR)

# pop density
areas_raw <- read_excel("state_areas.xlsx") %>%
  select(state_name = State,
        land_area_mi
        )

areas <- merge(areas_raw, fips_codes[c("state_name", "state_code")], by = "state_name") %>%
  distinct(state_code, .keep_all = TRUE) %>% 
  select(STATEFIP = state_code,
         land_area_mi
  )

# per cap law enforcement data
police_data_raw <- read_excel('per_cap_police.xlsx')

police_data_convert <- police_data_raw %>%
  gather(key = "Year", value = "per_cap_police", -state) %>% 
  select(state_name = state,
         year = Year,
         per_cap_police) %>% 
  mutate(state_name = str_to_title(state_name),
         year = as.integer(year))

police_data <- merge(police_data_convert, fips_codes[c("state_name", "state_code")], by = "state_name")  %>% 
  select(STATEFIP = state_code,
         year,
         per_cap_police) %>% 
  distinct(year, STATEFIP, .keep_all = TRUE) %>% 
  arrange(STATEFIP, year)

# violent crime and homicides

# all violent crime
crime_raw <- read_excel('crime_and_homicides.xlsx', sheet = 'violent_crime')

crime_convert <- crime_raw %>%
  gather(key = "Year", value = "per_cap_crime", -state) %>% 
  select(state_name = state,
         year = Year,
         per_cap_crime) %>% 
  mutate(state_name = str_to_title(state_name),
         year = as.integer(year))

homicides_raw = read_excel('crime_and_homicides.xlsx', sheet = 'homicide')

homicides_convert <- homicides_raw %>%
  gather(key = "Year", value = "per_cap_homicides", -state) %>% 
  select(state_name = state,
         year = Year,
         per_cap_homicides) %>% 
  mutate(state_name = str_to_title(state_name),
         year = as.integer(year))

violent_crime_data <- crime_convert %>% 
  left_join(homicides_convert, by = c("state_name", "year")) %>% 
  left_join(fips_codes[c("state_name", "state_code")], by = "state_name", relationship = "many-to-many") %>% 
  mutate(crime_per_cap = per_cap_crime - per_cap_homicides) %>% 
  select(STATEFIP = state_code,
         year,
         per_cap_crime,
         per_cap_homicides,
         crime_per_cap) %>% 
  distinct(year, STATEFIP, .keep_all = TRUE) %>% 
  arrange(STATEFIP, year)


# merge data together
full_data <- data_rates %>%
  filter(STATEFIP != 11) %>% # get rid of DC
  left_join(pov_data, by = c("STATEFIP", "year")) %>%
  left_join(unemp_data_nec, by = c("STATEFIP", "year")) %>% 
  left_join(alc_data, by = c("STATEFIP", "year")) %>%
  left_join(prisoners_data, by = c("STATEFIP", "year")) %>%
  left_join(gun_data, by = c("STATEFIP", "year")) %>%
  left_join(police_data, by = c("STATEFIP", "year")) %>%
  left_join(violent_crime_data, by = c("STATEFIP", "year")) %>%
  left_join(areas, by = "STATEFIP") %>%
  mutate(encarceration_rate = yr_end_prisoners/pop,
         pop_density_calc = pop/land_area_mi,
         pop_density_bls = tot_pop/land_area_mi,
         log_pop_calc = log(pop),
         log_pop_bls = log(tot_pop)) %>% 
  select(fip = STATEFIP,
         year,
         pop_calc = pop,
         pop_bls = tot_pop,
         num_black, pct_black,
         num_m15_29, pct_m15_29,
         per_cap_police,
         crime_per_cap,
         pov_rate,
         unemp_rate,
         alc_gal_per_cap,
         yr_end_prisoners,
         encarceration_rate,
         guns_per_household,
         log_pop_calc,
         log_pop_bls,
         pop_density_calc,
         pop_density_bls
  ) 

# create pre-treat only (vals from 2001)
covs_2001 <- full_data %>% 
  filter(year == 2001)

cov_names <- setdiff(colnames(covs_2001), c("fip", "year"))

# loop through cols and add "_2001" to name
for (col in cov_names) {
  new_col_name <- paste0(col, "_2001")
  colnames(covs_2001)[colnames(covs_2001) == col] <- new_col_name
}

pretreat_covs <- full_data %>% 
  left_join(covs_2001, by="fip", relationship = "many-to-many") %>% 
  select(fip,
         year = year.x,
         contains("_2001"))

# write to excel

write_xlsx(
  full_data,
  path = "controls_data.xlsx",
  col_names = TRUE,
)

write_xlsx(
  pretreat_covs,
  path = "pretreat_covs.xlsx",
  col_names = TRUE,
)




