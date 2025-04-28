# create function to download US-level ACS data for multiple years
get_acs_data <- function(yr, geom) {
  # set variables to pull
  variables <- c(
    tot_pop = 'B01003_001',
    median_income = 'B19113A_001',
    tot_pop_pov_count = 'B17001_001',
    tot_below_poverty = 'B17001_002',
    per_capita_income = 'B19301A_001',
    tot_pop_over_15 = 'B12001_001',
    males_divorced = 'B12001_010',
    females_divorced = 'B12001_019',
    tot_pop_over_16 = 'B23022_001',
    males_didnt_work = 'B23022_025',
    females_didnt_work = 'B23022_049'
  )
  
  # pull data
  acs <-
    get_acs(
      geography = geom,
      variables = variables,
      year = yr,
      output = 'wide',
      cache_table = TRUE
    ) %>% mutate(year = yr)
  
  # # check for missing variables
  # expected_cols <- paste(names(variables), 'E', sep = '')
  # missing_cols <- setdiff(expected_cols, names(acs))
  # 
  # # check which variables are missing
  # if (length(missing_cols) > 0) {
  #   message(
  #     paste('Year ', yr,
  #           ': Missing variables ->', paste(missing_cols, collapse = ', '))
  #   )
  # 
  #   acs[missing_cols] <- NA
  # } else {
  #   message(paste('Year ', yr, ': All variables retrieved successfully.'))
  # }
  
  # return data
  return(acs)
}

# define interpolate_data() function: apply population-weighted areal interpretion
# with centroid assignment to 2009-2019 block group boundaries using 2020 boundaries
# a reference, 2020 block-level population as weights, and a calculated scaling factor
# applied to each individual counts/aggregated variable
interpolate_data <- function(yr) {
  #print(paste('Interpolating data for', as.character(yr)))
  
  # get bgs for year
  yr_df <- 
    st_bgs %>%
    filter(year == yr) %>%
    mutate(agg_income = per_capita_income * tot_pop,
           tot_pop = ifelse(tot_pop < 1, 1, tot_pop)) %>%
    select(-c(year, per_capita_income)) %>%
    st_transform(5070) %>%
    st_make_valid()
  
  # create weights for use in interpolation
  b_weights <-
    st_blocks.2020 %>%
    st_transform(5070) %>%
    st_make_valid() %>%
    st_buffer(50) %>%
    filter(POP20 > 0) %>%
    st_make_valid()
  
  # process data to interpolate following 2020 tract boundaries
  interpolated <-
    yr_df %>%
    interpolate_pw(
      from = .,
      to = st_bgs.2020 %>% st_transform(st_crs(yr_df)) %>% st_make_valid(),
      to_id = 'GEOID',
      extensive = TRUE,
      weights = b_weights,
      weight_column = 'POP20',
      weight_placement = 'centroid',
      crs = st_crs(yr_df)
    ) %>%
    mutate(
      year = yr,
      across(tot_pop:pop_over_16_didnt_work, ~ as.numeric(.))
    ) %>%
    relocate(year, .after = GEOID) %>%
    st_transform(crs = st_crs(st_tracts)) %>%
    st_make_valid()
  
  # fill null values
  interpolated[is.na(interpolated)] <- 0
  
  # create scaling factors for each variable, then apply it to interpolated data
  for (var in vars_to_scale) {
    scaling_factor <- sum(yr_df[[var]]) / sum(interpolated[[var]])
    interpolated[[var]] <- round(interpolated[[var]] * scaling_factor)
  }
  
  # # display change in tot_pop after interpolation
  # print(
  #   tibble(
  #     year = yr,
  #     original_pop = sum(yr_df$tot_pop),
  #     interpolated_pop = sum(interpolated$tot_pop),
  #     loss_pct = ((interpolated_pop - original_pop) / original_pop) * 100
  #   )
  # )
  
  # return interpolated data with new variables
  return(interpolated)
}

# define decode_transition() function: 
decode_transition <- function(class) {
  # extract old and new class codes
  old <- floor(class / 10)
  new <- class %% 10
  
  # get old and new class labels
  old_label <- lc_classes$label[match(old, lc_classes$class)]
  new_label <- lc_classes$label[match(new, lc_classes$class)]
  
  # create new label describing the transition
  paste(old_label, '→', new_label)
}
