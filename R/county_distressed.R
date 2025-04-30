library(tidyverse)
library(sf)

# calculate county-level data
st_counties <-
  st_bgs_all %>%
  st_drop_geometry %>%
  filter(year %in% c(2009, 2023)) %>%
  .[,-grep('pct|avg|rate|density', colnames(.))] %>%
  group_by(county, year) %>%
  summarize(
    across(c(tot_pop:agg_income, ends_with('sqmi')), ~ sum(.)),
    num_block_groups = n(),
    num_distressed_block_groups = sum(is_distressed == 1),
    num_non_distressed_block_groups = sum(is_distressed == 0)
  ) %>%
  mutate(
    pct_below_poverty = tot_pop_pov_count / pop_in_poverty,
    avg_family_income = agg_fam_income / tot_families,
    per_capita_income = agg_income / tot_pop,
    avg_rent = agg_gross_rent / renter_occ_housing_units,
    pct_pop_over_16_didnt_work = pop_over_16_didnt_work / pop_over_16,
    pct_family_hhs = tot_families / tot_hhs,
    hu_vacancy_rate = vacant_housing_units / tot_housing_units,
    pct_renter_occupied_housing_units = renter_occ_housing_units / tot_housing_units,
    pct_agriculture = Agriculture_area_sqmi / area_sqmi,
    pct_barren = `Barren Land_area_sqmi` / area_sqmi,
    pct_developed = Developed_area_sqmi / area_sqmi,
    pct_forest = Forest_area_sqmi / area_sqmi,
    pct_grassland = Grassland_area_sqmi / area_sqmi,
    pct_ice_snow = `Ice & Snow_area_sqmi` / area_sqmi,
    pct_shrubland = Shrubland_area_sqmi / area_sqmi,
    pct_water = Water_area_sqmi / area_sqmi,
    pct_wetlands = Wetlands_area_sqmi / area_sqmi
  ) %>%
  ungroup()
