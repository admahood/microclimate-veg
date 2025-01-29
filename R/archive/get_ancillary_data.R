# get dem related data for plots

library(tidyverse)
library(elevatr)
library(sf)
library(terra)
library(spatialEco)

# bennett
ben_locations <- read_csv("data/sensor_locations.csv") |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  mutate(id = str_to_lower(id)) |>
  filter(str_sub(id,1,3) == "ben", id != "bennett_parking") |>
  dplyr::select(id)

dem_ben <- elevatr::get_aws_terrain(ben_locations[5,],z = 12, prj = st_crs(ben_locations))

plot(dem_ben); plot(ben_locations, add=T, col="black")

dem_ben$slope <- terra::terrain(dem_ben[[1]], v = "slope")
dem_ben$aspect <- terra::terrain(dem_ben[[1]], v = "aspect")
dem_ben$twi <- topomicro::get_twi(dem_ben[[1]], resolution = 15)$twi
dem_ben$fa <- topomicro::get_folded_aspect(dem_ben$aspect)
dem_ben$fa_x_slope <- sqrt(dem_ben$slope) * dem_ben$fa
dem_ben$hli <- topomicro::get_hli(dem_ben[[1]])

ben_locations %>%
  mutate(terra::extract(dem_ben, .)) |>
  st_set_geometry(NULL) |>
  dplyr::rename(elevation = 3) |>
  dplyr::select(-ID) |>
  write_csv("data/bennett_topo.csv")

plot(dem_ben$hli)

# vall

val_locations <- read_csv("data/sensor_locations.csv") |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  mutate(id = str_to_lower(id)) |>
  filter(str_sub(id,1,2) %in% c("vg", 'rs') ) |>
  dplyr::select(id)

dem_val <- elevatr::get_aws_terrain(val_locations |> st_buffer(dist = 0.5),z = 12, prj = st_crs(val_locations))

plot(dem_val); plot(val_locations, add=T, col="black")

dem_val$slope <- terra::terrain(dem_val[[1]], v = "slope")
dem_val$aspect <- terra::terrain(dem_val[[1]], v = "aspect")
dem_val$twi <- topomicro::get_twi(dem_val[[1]], resolution = 15)$twi
dem_val$fa <- topomicro::get_folded_aspect(dem_val$aspect)
dem_val$fa_x_slope <- sqrt(dem_val$slope) * dem_val$fa
dem_val$hli <- topomicro::get_hli(dem_val[[1]])

twi_val <- terra::rast("data/vc_twi.tif")

val_locations %>%
  mutate(terra::extract(dem_val, .)) %>%
  # mutate(terra::extract(twi_val, .))%>%
  st_set_geometry(NULL) |>
  dplyr::rename(elevation = 3) |>
  dplyr::select(-ID) |>
  dplyr::mutate(id = str_replace_all(id, "g0", "g")) |>
  write_csv("data/val_topo.csv")

# plot(dem_val$twi)
# mef

mef_locations <- read_csv("data/sensor_locations.csv") |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  mutate(id = str_to_lower(id)) |>
  filter(str_sub(id,1,3) == "mef") |>
  dplyr::select(id)

dem_mef <- elevatr::get_aws_terrain(mef_locations,z = 12, prj = st_crs(mef_locations))

plot(dem_mef); plot(mef_locations, add=T, col="black")

dem_mef$slope <- terra::terrain(dem_mef[[1]], v = "slope")
dem_mef$aspect <- terra::terrain(dem_mef[[1]], v = "aspect")
dem_mef$twi <- topomicro::get_twi(dem_mef[[1]], resolution = 15)$twi
dem_mef$fa <- topomicro::get_folded_aspect(dem_mef$aspect)
dem_mef$fa_x_slope <- sqrt(dem_mef$slope) * dem_mef$fa
dem_mef$hli <- topomicro::get_hli(dem_mef[[1]])

mef_locations %>%
  mutate(terra::extract(dem_mef, .)) |>
  st_set_geometry(NULL) |>
  dplyr::rename(elevation = 3) |>
  dplyr::select(-ID) |>
  write_csv("data/mef_topo.csv")

plot(dem_mef$twi)

# model topo vs microclimate data ==============================================

f <- list.files("data/cleaned_bm/", full.names = T)
db <- read_csv(f[1]) |>
  plotwise_summary() |>
  as_tibble(rownames = "id") |>
  left_join(read_csv("data/bennett_topo.csv")) |>
  mutate(rel_elv = elevation - min(elevation), site = "ben")
dv <- read_csv(f[4]) |> bind_rows(read_csv(f[5]))|>
  plotwise_summary() |>
  as_tibble(rownames = "id") |>
  left_join(read_csv("data/val_topo.csv"))|>
  mutate(rel_elv = elevation - min(elevation), site = "val")
dm <- read_csv(f[2]) |> bind_rows(read_csv(f[3]))|>
  plotwise_summary() |>
  as_tibble(rownames = "id") |>
  left_join(read_csv("data/mef_topo.csv"))|>
  mutate(rel_elv = elevation - min(elevation), site = "mef")

d <- db |> bind_rows(dv) |> bind_rows(dm)
mod_td <- lm(tdelta ~ twi + site +elevation , d)
summary(mod_td); car::Anova(mod_td); performance::check_model(mod_td)

ggeffects::ggpredict(mod_td, c("elevation", "twi")) |> plot(show_residuals = T)
ggeffects::ggpredict(mod_td, c("twi")) |> plot(show_residuals = T)
ggeffects::ggpredict(mod_td, c("elevation")) |> plot(show_residuals = T)


mod_vm <- lm(vmin ~ poly(twi,3) + rel_elv + fa +  site, d)
summary(mod_vm); car::Anova(mod_vm); performance::check_model(mod_vm)

ggeffects::ggpredict(mod_vm, c("rel_elv", "twi")) |> plot(show_residuals = T)
ggeffects::ggpredict(mod_vm, c("twi")) |> plot(show_residuals = T)
ggeffects::ggpredict(mod_vm, c("fa")) |> plot(show_residuals = T)
ggeffects::ggpredict(mod_vm, c("rel_elv")) |> plot(show_residuals = T)



