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

setFixest_fml(..full_ctrls = ~ pct_black_2001 + pct_m15_29_2001 + per_cap_police_2001 +
                                    pov_rate_2001 + unemp_rate_2001 + alc_gal_per_cap_2001 + encarceration_rate_2001 +
                                    log_pop_calc_2001 + pop_density_calc_2001 + guns_per_household_2001 +
                                    crime_per_cap_2001 + homicides_lag)

prop_score_data <- raw %>%
  filter(year == 2020) %>% 
  mutate(cc_cat = case_when(may_issue == 1 ~ "may",
                            shall_issue == 1 ~ "shall",
                            no_permit_cc == 1 ~ "shall"),
         erpo_cat = ifelse(red_flag == 1, "erpo", "no_erpo"),
         syg_cat = ifelse(syg == 1, "syg", "no_syg"))
  select(fips,
         year,
         syg,
         may_vs_other,
         red_flag,
         cc_cat,
         erpo_cat,
         syg_cat,
         all_of(full_controls))

# train logit models
syg_model <- femlm(syg ~ ..full_ctrls,
                        data = prop_score_data,
                        family = "logit")

cc_model <- femlm(may_vs_other ~ ..full_ctrls,
                       data = prop_score_data,
                       family = "logit")

erpo_model <- femlm(red_flag ~ ..full_ctrls,
                         data = prop_score_data,
                         family = "logit")

table <- etable(syg_model, cc_model, erpo_model)


# predict propensity scores
syg_prop_scores <- data.frame(syg_pr = predict(syg_model),
                              syg = prop_score_data$syg)
syg_prop_scores$syg_cat = prop_score_data$syg_cat

cc_prop_scores <- data.frame(cc_pr = predict(cc_model),
                             cc = prop_score_data$may_vs_other)
cc_prop_scores$cc_cat = prop_score_data$cc_cat

erpo_prop_scores <- data.frame(erpo_pr = predict(erpo_model),
                               erpo = prop_score_data$red_flag)
erpo_prop_scores$erpo_cat = prop_score_data$erpo_cat


# boxplots
syg_boxplot <- ggplot(syg_prop_scores, aes(x = syg_cat, y = syg_pr, fill = syg_cat)) +
  geom_boxplot() +
  scale_fill_manual(
    values = alpha(c("syg" = "orangered", "no_syg" = "green4"), .8),
    guide = NULL
  ) +
  scale_x_discrete(labels = c('Untreated', 'Treated')) + 
  labs(x = "", y = "Propensity Score")

syg_boxplot

cc_boxplot <- ggplot(cc_prop_scores, aes(x = cc_cat, y = cc_pr, fill = cc_cat)) +
  geom_boxplot() +
  scale_fill_manual(
    values = alpha(c("shall" = "orangered", "may" = "green4"), .8),
    guide = NULL
  ) +
  scale_x_discrete(labels = c('Untreated', 'Treated')) + 
  labs(x = "", y = "Propensity Score")

cc_boxplot

erpo_boxplot <- ggplot(erpo_prop_scores, aes(x = erpo_cat, y = erpo_pr, fill = erpo_cat)) +
  geom_boxplot() +
  scale_fill_manual(
    values = alpha(c("no_erpo" = "green4", "erpo" = "orangered"), .8),
    guide = NULL
  ) +
  scale_x_discrete(labels = c('Treated', 'Untreated')) + 
  labs(x = "", y = "Propensity Score")

erpo_boxplot

# plot distributions

syg_density <- ggplot(syg_prop_scores, aes(x = syg_pr, fill = syg_cat)) +
  geom_density() +
  scale_fill_manual(
    values = alpha(c("syg" = "orangered", "no_syg" = "green4"), .8),
    labels = c('Untreated', 'Treated'),
    name = ""
  ) + 
  labs(x = "Propensity Score", y = "Density")

syg_density

syg_density2 <- ggplot(syg_prop_scores, aes(x = syg_pr)) +
  geom_density(color="blue") +
  labs(x = "Propensity Score", y = "Density")

syg_density2

cc_density <- ggplot(cc_prop_scores, aes(x = cc_pr, fill = cc_cat)) +
  geom_density() +
  scale_fill_manual(
    values = alpha(c("shall" = "orangered", "may" = "green4"), .8),
    labels = c('Untreated', 'Treated'),
    name = ""
  ) + 
  labs(x = "Propensity Score", y = "Density")

cc_density

cc_density2 <- ggplot(cc_prop_scores, aes(x = cc_pr)) +
  geom_density(color="blue") +
  labs(x = "Propensity Score", y = "Density")

cc_density2

erpo_density <- ggplot(erpo_prop_scores, aes(x = erpo_pr, fill = erpo_cat)) +
  geom_density() +
  scale_fill_manual(
    values = alpha(c("no_erpo" = "green4", "erpo" = "orangered"), .8),
    labels = c('Treated', 'Untreated'),
    name = ""
  ) + 
  labs(x = "Propensity Score", y = "Density")

erpo_density

erpo_density2 <- ggplot(erpo_prop_scores, aes(x = erpo_pr)) +
  geom_density(color="blue") +
  labs(x = "Propensity Score", y = "Density")

erpo_density2

pr_score_combo <- cbind(cc_prop_scores, syg_prop_scores, erpo_prop_scores)

leq_10_df <- pr_score_combo %>% 
  mutate(leq_10_syg = mean(syg_pr <= .1),
         leq_10_cc = mean(cc_pr <= .1),
         leq_10_erpo = mean(erpo_pr <= .1)) %>% 
  distinct(leq_10_syg, leq_10_cc, leq_10_erpo)

leq_10_df_t <- t(leq_10_df)
colnames(leq_10_df_t) <- "pct_leq_10"
rownames(leq_10_df_t) <- c("syg", "cc", "erpo")

geq_90_df <- pr_score_combo %>% 
  mutate(geq_90_syg = mean(syg_pr >= .9),
         geq_90_cc = mean(cc_pr >= .9),
         geq_90_erpo = mean(erpo_pr >= .9)) %>% 
  distinct(geq_90_syg, geq_90_cc, geq_90_erpo)

geq_90_df_t <- t(geq_90_df)
colnames(geq_90_df_t) <- "pct_geq_90"
rownames(geq_90_df_t) <- c("syg", "cc", "erpo")

prop_score_summ <- data.frame(cbind(leq_10_df_t, geq_90_df_t)) %>% 
  mutate(pct_on_edge = leq_10_df_t + geq_90_df_t)





