# make species list
library(tidyverse)

vall_veg <- readr::read_csv("data/valles_caldera/microclimate - VALL_veg.csv") |>
  dplyr::select(plot, species) |>
  dplyr::mutate(occurrence_val = 1,
                plot = str_to_lower(plot) %>% str_remove_all("_"),
                species = str_replace_all(species, " ", "_")|>
                  str_replace_all("-", "_")) |>
  dplyr::group_by(species) |>
  summarise(prevalence = n()) |>
  ungroup()

mef_veg <- readr::read_csv("data/mef/microclimate - MEF_veg.csv")|>
  dplyr::select(plot, actual_species) |>
  dplyr::mutate(occurrence_mef = 1,
                plot = str_to_lower(plot) %>% str_remove_all("_"),
                species = str_replace_all(actual_species, " ", "_")|>
                  str_replace_all("-", "_")) |>
  dplyr::group_by(species) |>
  summarise(prevalence= n()) |>
  ungroup()

# full_join(vall_veg, mef_veg)|>
#   arrange(desc(prevalence_val)) |>
#   write_csv('out/species_list.csv')


vall_veg |>
  filter(prevalence >= 5) |>
  arrange(desc(prevalence)) |>
  write_csv('out/common_species_val.csv')

mef_veg |>
  filter(prevalence >= 5) |>
  arrange(desc(prevalence)) |>
  write_csv('out/common_species_mef.csv')
