library(tidyverse)
library(terra)
library(ggspatial)

# load reclassified 2009 and 2023 NLCD rasters and 
# resample nlcd.2023 to ensure alignment
#nlcd.2009 <- nlcd_stack.reclass$`2009`
#nlcd.2023 <- nlcd_stack.reclass$`2023` %>% resample(., nlcd.2009, method = 'near')
nlcd.2009 <- list.files(mrlc_data_dir, '2009_reclass', full.names = TRUE)[[1]]
nlcd.2023 <- list.files(mrlc_data_dir, '2023_reclass', full.names = TRUE)[[1]]

# stack the two reclassified rasters
lc_chg_stack <- c(nlcd.2009, nlcd.2023)

# compute transitions: old class * 10 + new class
# pixel went from 3 to 7 between the 2 years => value is 37
# if no change => NA
change_map <- lapp(
  lc_chg_stack, fun = function(old, new){
    return(ifelse(old != new, old * 10 + new, 0))
  })

# convert to dataframe
change_df <- as.data.frame(change_map, xy = TRUE)
colnames(change_df)[3] <- 'transition_code'

# assign labels
change_df$transition_label <- sapply(change_df$transition_code, decode_transition)

# class codes and labels dataframe
lc_classes <- data.frame(
  class = 0:9,
  label = c('Other or Unknown', 'Developed', 'Forest', 'Shrubland', 'Grassland', 'Agriculture',
            'Wetlands', 'Ice & Snow', 'Water', 'Barren')
)

# decode transitions
transitions <- unique(change_df$transition_code)

# get unique transition labels, then add them along with 
# their transitions to a dataframe
transition_labels <- unique(change_df$transition_label)
transitions_df <- data.frame(Code = transitions, Transition = transition_labels)

# move No Change to the front of the list to make it easier to change color
transition_labels <- c('No Change', setdiff(transition_labels, 'Other or Unknown → Other or Unknown'))

# get distinct colors for each transition type
set.seed(123)
colors <- c('grey93', sample(colors(distinct = TRUE), length(transition_labels) - 1))

# create land cover change map
lc_chg_map <-
  ggplot() +
  geom_raster(data = change_df, aes(x = x, y = y, fill = transition_label)) +
  geom_sf(data = study_counties, fill = NA, color = 'black', size = 0.3) +
  scale_fill_manual(values = colors, name = NA) +
  coord_sf(expand = FALSE) +
  labs(title = 'Land Cover Change in NY Southern Tier (2009 → 2023)',
       caption = 'Only showing pixels where land cover class changed',
       x = 'Longitude', y = 'Latitude') +
  theme_map() +
  theme(legend.position = 'bottom',
        legend.text = element_text(size = 5),
        legend.title = element_blank()) +
  guides(fill = guide_legend(ncol = 4)) +
  annotation_scale(location = 'br', width_hint = 0.2, pad_y = unit(-0.5, 'cm'))

lc_chg_map