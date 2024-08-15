library(tidyverse)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(tidycensus)
library(XLConnect)
library(writexl)
library(ggplot2)
library(usmap)

# library(extrafont)
# 
# # load in latex fonts
# font_import(pattern = "lmroman*")
# loadfonts()

setwd('/Users/annabelroth/Desktop/cs_classes/thesis/data/')

final_data <- read_excel('final_data.xlsx') %>% 
  rename(fips = fip)

# make graph showing number of laws implemented by each state
data(fips_codes)
fips_codes$state_code <- as.integer(fips_codes$state_code)

deg_of_treatment <- merge(final_data, fips_codes[c("state_name", "state_code")], by.x = "fips", by.y = "state_code") %>%
  filter(year == 2020) %>%
  select(state_name,
         num_decrease,
         num_increase) %>%
  distinct(state_name, .keep_all=TRUE) %>%
  gather(key = "law_category", value = "count", -state_name) %>%
  arrange(., state_name)

deg_of_treatment_plot <- ggplot(deg_of_treatment, aes(x = state_name, y = count, fill = law_category)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("num_increase" = "red", "num_decrease" = "blue"), labels = c("Restrictive", "Expansive")) +
  guides(fill = guide_legend(title = "Law Category")) +
  labs(x = "State", y = "Number of Laws Implemented") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
        # text = element_text(family="LM Roman 10"))
  
print(deg_of_treatment_plot)
  
  

# make graph of states color coded by group
map_data <- merge(final_data, fips_codes[c("state", "state_code")], by.x = "fips", by.y = "state_code") %>%
  filter(year == 2020) %>%
  distinct(fips, .keep_all=TRUE) %>%
  mutate(category = case_when(group == 1 ~ "increase",
                              group == 2 ~ "decrease",
                              group == 3 ~ "both",
                              group == 4 ~ "neither")) %>% 
  select(fips, category)

map <- plot_usmap(data = map_data, values = "category", color = "white") + 
  guides(fill = guide_legend(title = "Types of Laws Implemented")) + 
  scale_fill_manual(
    values = alpha(c("increase" = "red", "decrease" = "blue", "both" = "purple", "neither" = "black"), .8),
    labels = c("Both", "Only Restrictive Laws", "Only Expansive Laws", 'Neither')
  ) +
  theme(legend.position = "right") 
        # text = element_text(family="LM Roman 10"))

print(map)


# make map with continuous color scale based on number of increase/decrease laws
map_data_2 <- merge(final_data, fips_codes[c("state", "state_code")], by.x = "fips", by.y = "state_code") %>%
  filter(year == 2020) %>%
  distinct(fips, .keep_all=TRUE) %>%
  mutate(deg_of_treatment = num_decrease - num_increase) %>% 
  select(fips, deg_of_treatment)

map_2 <- plot_usmap(data = map_data_2, values = "deg_of_treatment", color = "white") + 
  guides(fill = guide_legend(title = "Degree of Treatment")) + 
  scale_fill_continuous(low = "white", high = "blue")+
                        # breaks=c(-10, 0, 10, 20),
                        # labels=c("-  10", "0", "10", "20")) + 
  theme(legend.position = "right") 
        # text = element_text(family="LM Roman 10"))

print(map_2)

# make map with continuous color scale based on number of increase/decrease laws divided by num total laws
# denotes pct expansive vs restrictive
map_data_3 <- merge(final_data, fips_codes[c("state", "state_code")], by.x = "fips", by.y = "state_code") %>%
  filter(year == 2020) %>%
  distinct(fips, .keep_all=TRUE) %>%
  mutate(deg_of_treatment = (num_decrease - num_increase)/(num_decrease + num_increase)*100) %>% 
  select(fips, deg_of_treatment)

map_3 <- plot_usmap(data = map_data_3, values = "deg_of_treatment", color = "white") + 
  guides(fill = guide_legend(title = "Degree of Treatment")) + 
  scale_fill_continuous(low = "red", high = "blue")+
                        # breaks=c(-100, -50, 0, 50, 100),
                        # labels=c("-  100", "-  50", "0", "50", "100")) + 
  theme(legend.position = "right")
        # text = element_text(family="LM Roman 10"))

print(map_3)

# make maps for syg vs no syg and may vs shall/permitless and mental vs not
cc_map_data <- final_data %>%
  filter(year == 2020) %>%
  distinct(fips, .keep_all=TRUE) %>%
  mutate(cc_cat = case_when(may_issue == 1 ~ "may",
                              shall_issue == 1 ~ "shall",
                              no_permit_cc == 1 ~ "shall"),
         erpo_cat = ifelse(red_flag == 1, "erpo", "no_erpo"),
         syg_cat = ifelse(syg == 1, "syg", "no_syg")) %>%
  select(fips, cc_cat, syg_cat, erpo_cat)

map_4 <- plot_usmap(data = cc_map_data, values = "cc_cat", color = "white") + 
  guides(fill = guide_legend(title = "Type of Concealed Carry Law")) + 
  scale_fill_manual(
    values = alpha(c("shall" = "red", "may" = "blue"), .8),
    labels = c("May Issue", "Shall Issue or Permitless")
  ) +
  theme(legend.position = "right") 
        # text = element_text(family="LM Roman 10"))

print(map_4)

map_5 <- plot_usmap(data = cc_map_data, values = "syg_cat", color = "white") + 
  guides(fill = guide_legend(title = "")) + 
  scale_fill_manual(
    values = alpha(c("syg" = "red", "no_syg" = "blue"), .8),
    labels = c("No Stand Your Ground Law", "Stand Your Ground Law")
  ) +
  theme(legend.position = "right")
        # text = element_text(family="LM Roman 10"))

print(map_5)

map_6 <- plot_usmap(data = cc_map_data, values = "erpo_cat", color = "white") + 
  guides(fill = guide_legend(title = "")) + 
  scale_fill_manual(
    values = alpha(c("no_erpo" = "red", "erpo" = "blue"), .8),
    labels = c("Red Flag Law", "No Red Flag Law")
  ) +
  theme(legend.position = "right")
# text = element_text(family="LM Roman 10"))

print(map_6)


# maps to show variation in always vs bc vs never treated

state_variation_data <- final_data %>%
  filter(year == 2001 | year == 2020) %>%
  select(fips, state, year, may_vs_other, syg, red_flag) %>% 
  pivot_wider(names_from = c(year), values_from = c(syg, may_vs_other, red_flag)) %>% 
  rename(cc_2001 = may_vs_other_2001, 
         cc_2020 = may_vs_other_2020) %>% 
  mutate(cc_cat = case_when(cc_2020 == 0 ~ "never_treat",
                            cc_2020 - cc_2001 == 1 ~ "bc_treat",
                            cc_2020 - cc_2001 == 0 & cc_2020 == 1 ~ "always_treat"),
         erpo_cat = case_when(red_flag_2020 == 0 ~ "never_treat",
                              red_flag_2020 - red_flag_2001 == 1 ~ "bc_treat",
                              red_flag_2020 - red_flag_2001 == 0 & red_flag_2020 == 1 ~ "always_treat"),
         syg_cat = case_when(syg_2020 == 0 ~ "never_treat",
                             syg_2020 - syg_2001 == 1 ~ "bc_treat",
                             syg_2020 - syg_2001 == 0 & syg_2020 == 1 ~ "always_treat"))


map_7 <- plot_usmap(data = state_variation_data, values = "cc_cat", color = "white") + 
  guides(fill = guide_legend(title = "Treatment Status")) + 
  scale_fill_manual(values = alpha(c("always_treat" = "blue4", "bc_treat" = "royalblue", "never_treat" = "grey60"), .8),
                    labels = c("Always Treated", "Becomes Treated", "Never Treated")) +
  theme(legend.position = "right") + 
  ggtitle("Concealed Carry Laws") +
  theme(plot.title = element_text(hjust=0.5))
# text = element_text(family="LM Roman 10"))

print(map_7)

map_8 <- plot_usmap(data = state_variation_data, values = "syg_cat", color = "white") + 
  guides(fill = guide_legend(title = "Treatment Status")) + 
  scale_fill_manual(values = alpha(c("always_treat" = "blue4", "bc_treat" = "royalblue", "never_treat" = "grey60"), .8),
                    labels = c("Always Treated", "Becomes Treated", "Never Treated")) +
  theme(legend.position = "right") + 
  ggtitle("SYG Laws") +
  theme(plot.title = element_text(hjust=0.5))
# text = element_text(family="LM Roman 10"))

print(map_8)

map_9 <- plot_usmap(data = state_variation_data, values = "erpo_cat", color = "white") + 
  guides(fill = guide_legend(title = "Treatment Status")) + 
  scale_fill_manual(values = alpha(c("always_treat" = "blue4", "bc_treat" = "royalblue", "never_treat" = "grey60"), .8),
                    labels = c("Always Treated", "Becomes Treated", "Never Treated")) +
  theme(legend.position = "right") + 
  ggtitle("ERPO Laws") +
  theme(plot.title = element_text(hjust=0.5))
# text = element_text(family="LM Roman 10"))

print(map_9)


# graph avg age-adj rate over time by law type for cc and syg
homicides_plt_data <- final_data %>% 
  filter(!(fips %in% c(38, 50, 56))) %>% 
  mutate(cc_cat = case_when(may_issue == 1 ~ "may",
                              shall_issue == 1 ~ "shall",
                              no_permit_cc == 1 ~ "shall"),
         erpo_cat = ifelse(red_flag == 1, "erpo", "no_erpo"),
         syg_cat = ifelse(syg == 1, "syg", "no_syg")) %>% 
  select(fips,
         year,
         age_adj_rate_tot, 
         cc_cat,
         erpo_cat,
         syg_cat) %>% 
  # complete.cases(.) filters out rows with NA
  filter(complete.cases(.))
  

homicides_over_t_cc <- ggplot(homicides_plt_data, aes(x = year, y = age_adj_rate_tot, color=cc_cat, group = cc_cat)) +
  # Calculate the mean based on y, set geom = line
  stat_summary(fun = "mean", linewidth = .5, geom = "line") +
  scale_color_manual(name = "Type of Concealed Carry Law",
                     values = c("shall" = "red", "may" = "blue"),
                     labels = c("May Issue", "Shall Issue or Permitless")) +
  labs(x = "Year", y = "Average Age-Adjusted Homicide Rate")
  # theme(text = element_text(family="LM Roman 10"))
  
print(homicides_over_t_cc)

homicides_over_t_syg <- ggplot(homicides_plt_data, aes(x = year, y = age_adj_rate_tot, color=syg_cat, group = syg_cat)) +
  # Calculate the mean based on y, set geom = line
  stat_summary(fun = "mean", linewidth = .5, geom = "line") +
  scale_color_manual(name = "",
                     values = c("syg" = "red", "no_syg" = "blue", "both" = "purple", "neither" = "black"),
                     labels = c("No Stand Your Ground Law", "Stand Your Ground Law")
  ) +
  geom_vline(xintercept = 2005, linetype = "longdash") + 
  labs(x = "Year", y = "Average Age-Adjusted Homicide Rate")
  # theme(text = element_text(family="LM Roman 10"))

print(homicides_over_t_syg)


homicides_over_t_erpo <- ggplot(homicides_plt_data, aes(x = year, y = age_adj_rate_tot, color=erpo_cat, group = erpo_cat)) +
  # Calculate the mean based on y, set geom = line
  stat_summary(fun = "mean", linewidth = .5, geom = "line") +
  scale_color_manual(name = "",
                     values = c("no_erpo" = "red", "erpo" = "blue"),
                     labels = c("Red Flag Law", "No Red Flag Law")) +
  labs(x = "Year", y = "Average Age-Adjusted Homicide Rate")
# theme(text = element_text(family="LM Roman 10"))

print(homicides_over_t_erpo)
