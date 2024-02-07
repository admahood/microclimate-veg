vall_veg_long <- readr::read_csv("data/valles_caldera/microclimate - VALL_veg.csv") |>
  dplyr::select(plot, species) |>
  dplyr::mutate(occurrence = 1,
                plot = str_to_lower(plot) %>% str_remove_all("_"),
                species = str_replace_all(species, " ", "_")|>
                  str_replace_all("-", "_"))

mef_veg_long <- readr::read_csv("data/mef/manitou_species_data.csv")|>
  dplyr::select(plot, actual_species) |>
  dplyr::mutate(occurrence = 1,
                plot = str_to_lower(plot) %>% str_remove_all("_"),
                actual_species = str_replace_all(actual_species, " ", "_")|>
                  str_replace_all("-", "_"))
