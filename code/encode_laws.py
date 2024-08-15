# %%
import numpy as np
import pandas as pd

# %%
laws_raw = pd.read_excel('/Users/annabelroth/Desktop/cs_classes/thesis/data/laws_data.xlsx', sheet_name="Data")

# filter to 2001 to 2016
laws_subset = laws_raw[laws_raw['year'] >= 2001]
law_lst = laws_subset.columns.tolist()
law_lst.remove('state')
law_lst.remove('year')
# %%
# function to count num restrictive and num expansive laws passed for each state
def count_changes(state_data, law_lst):
    state_data_2001 = state_data[state_data["year"] == 2001]
    state_data_2020 = state_data[state_data["year"] == 2020]

    num_increase = 0
    num_decrease = 0

    for law in law_lst:
        val_2020 = state_data_2020[law].item()
        val_2001 = state_data_2001[law].item()
        if val_2020 - val_2001 == -1:
            num_increase += 1
        if val_2020 - val_2001 == 1:
            num_decrease += 1

    return (num_increase, num_decrease)

# %%
# function to get yr of first restrive/expansive law passed
def get_first_yr(state_data, law_lst, change):
    state_data_2001 = state_data[state_data["year"] == 2001]
    state_data_2020 = state_data[state_data["year"] == 2020]

    change_yr = 3000
    first_change_yr = 3000

    for law in law_lst:
        # check if ever treated BEFORE looping through all yrs
        val_2020 = state_data_2020[law].item()
        val_2001 = state_data_2001[law].item()
        if val_2020 - val_2001 == change:
            for yr in range(2002, 2017):
                curr_yr_val = state_data[state_data["year"] == yr][law].item()
                prev_yr_val = state_data[state_data["year"] == (yr-1)][law].item()
                if curr_yr_val - prev_yr_val == change:
                    # print('found change', law)
                    change_yr = yr
                    # print(change_yr)
                if change_yr < first_change_yr:
                    # print('less than')
                    first_change_yr = change_yr

    return first_change_yr

            
# %%
state_info = {}
state_info['state'] = []
state_info['year'] = []

state_info['num_increase'] = []
state_info['ever_treated_increase'] = []
state_info['first_increase_yr'] = []

state_info['num_decrease'] = []
state_info['ever_treated_decrease'] = []
state_info['first_decrease_yr'] = []

for state in laws_subset['state'].unique():
    state_data = laws_subset[laws_subset['state'] == state]
    num_increase, num_decrease =  count_changes(state_data, law_lst)

    first_increase_yr = get_first_yr(state_data, law_lst, -1)
    first_decrease_yr = get_first_yr(state_data, law_lst, 1)

    for yr in range(2001, 2021):
        state_info['state'].append(state)
        state_info['year'].append(yr)
        state_info['num_increase'].append(num_increase)

        if yr >= first_increase_yr:
            state_info['ever_treated_increase'].append(1)
        else: 
            state_info['ever_treated_increase'].append(0)

        state_info['first_increase_yr'].append(first_increase_yr)


        state_info['num_decrease'].append(num_decrease)
        if yr >= first_decrease_yr:
            state_info['ever_treated_decrease'].append(1)
        else: 
            state_info['ever_treated_decrease'].append(0)

        state_info['first_decrease_yr'].append(first_decrease_yr)

# %%
state_info_df = pd.DataFrame.from_dict(state_info)
state_info_df.to_excel('law_count_info.xlsx')

# %%
