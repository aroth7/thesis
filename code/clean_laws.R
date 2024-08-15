library(tidyverse)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(tidycensus)
library(XLConnect)
library(writexl)

setwd('/Users/annabelroth/Desktop/cs_classes/thesis/data/')

laws_raw <- read_excel('laws_data.xlsx', sheet = "Data")

laws_to_keep <- laws_raw %>% 
  filter(year >= 2001) %>% 
  mutate(shall_issue = ifelse(mayissue == 0,
                              ifelse(permitconcealed == 1,
                                     1, 0), 0),
         no_permit_cc = ifelse(mayissue == 0,
                               ifelse(permitconcealed == 0,
                                      1, 0), 0),
         syg = ifelse(nosyg == 0, 1, 0) 
  ) %>%
  mutate(
    may_vs_other = ifelse(mayissue == 1, 0, 1),
    may_vs_shall = ifelse(mayissue == 0,
                          ifelse(shall_issue == 1,
                                 1, NA), 0),
    shall_vs_no_perm = ifelse(shall_issue == 0,
                          ifelse(no_permit_cc == 1,
                                 1, NA), 1),
    may_vs_no_perm = ifelse(mayissue == 0,
                              ifelse(no_permit_cc == 1,
                                     1, NA), 1)
  ) %>% 
  select(state,
         year,
         red_flag,
         shall_issue,
         may_issue = mayissue,
         permit_cc = permitconcealed,
         no_permit_cc,
         may_vs_other,
         may_vs_shall,
         shall_vs_no_perm,
         may_vs_no_perm,
         syg,
         no_syg = nosyg)

# load fips code data
data(fips_codes)
fips_codes$state_code <- as.integer(fips_codes$state_code)

# make list of all relevant indicator cols
all_cols <- c('red_flag',
              'shall_issue',
              'may_vs_other',
              'no_permit_cc',
              'may_vs_shall',
              'shall_vs_no_perm',
              'may_vs_no_perm',
              'syg')

# add col for colName_yr to denote yr when had law change for each law/col 
for (col in all_cols) {
  laws_to_keep[paste(col, "_yr", sep = "")] <- NA
}

# store lists of all yrs and all states so don't have to recompute each time
yrs <- unique(laws_to_keep$year)
all_states <- unique(laws_to_keep$state)

for (curr_col in all_cols) {
  for (curr_state in all_states) {
    # count the number of rows for given state where given law/col is a 0
    rows_with_zero <- rows_with_zero <- laws_to_keep[laws_to_keep[[curr_col]] == 0 & laws_to_keep$state == curr_state, ]
    yr_idx <- nrow(rows_with_zero)
    # get the year corresponding to yr_idx + 1 (bc R is 1 indexed, this will be the first yr where state has a 1 for given law)
    yr <- yrs[yr_idx + 1]
    
    # get the rows to update (i.e. all rows for given state)
    rows_to_update <- laws_to_keep$state == curr_state
    
    # add the calculated year to the correct col for all rows for the given state (if no yr, then add 3000 as placeholder)
    laws_to_keep[rows_to_update, paste(curr_col, "_yr", sep = "")] <- ifelse(! is.na(yr), yr, 3000)
  }
}

laws_clean <- merge(laws_to_keep, fips_codes[c("state_name", "state_code")], by.x = "state", by.y = "state_name") %>%
  filter (year >= 2001) %>%
  distinct(year, state_code, .keep_all = TRUE) %>% 
  arrange(state_code, year) %>% 
  select(
    fip = state_code,
    year,
    red_flag,
    red_flag_yr,
    shall_issue,
    shall_issue_yr,
    may_issue,
    permit_cc,
    no_permit_cc,
    no_permit_cc_yr,
    may_vs_other, 
    may_vs_other_yr,
    may_vs_shall,
    may_vs_shall_yr,
    shall_vs_no_perm,
    shall_vs_no_perm_yr,
    may_vs_no_perm,
    may_vs_no_perm_yr,
    syg,
    syg_yr,
    no_syg
  ) 


write_xlsx(
  laws_clean,
  path = "laws_clean.xlsx",
  col_names = TRUE,
)
