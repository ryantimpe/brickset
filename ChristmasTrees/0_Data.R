library(tidyverse)
library(googlesheets4)
library(ggbrickr)

#DATA PROCESSING ----
gs4_deauth()

gg_trees <- "https://docs.google.com/spreadsheets/d/1FSh3w8yCoFtaPLJnSaHn3_Fobg7kykivn6RA09Icj24/edit?usp=sharing"

dat_raw <- read_sheet(gg_trees)
names(dat_raw)

dat_p1 <- dat_raw %>% 
  mutate(across(c(foliage_color, light_color, light_piece),
                ~purrr::map(., str_split, pattern = ", ")
  )) %>% 
  # Full height
  # 8 SOT units = 3 SNOT units
  # https://www.holgermatthes.de/bricks/en/snot.php
  replace_na(list(height_sot = 0, height_snot = 0)) %>% 
  mutate(height_full = height_sot + 8/3 * height_snot)
