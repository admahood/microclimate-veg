library(tidyverse)
library(topomicro)
library(sf)
library(terra)

locations <- read_csv("data/sensor_locations.csv") |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  dplyr::select(-starts_with("alpha")) |>
  filter(str_sub(id, 1,2) %in% c('vg', 'me', 'rs')) |>
  print(n=99)

mef_clim <- read_csv("data/cleaned_bm/mef_jul23.csv") |>
  bind_rows(read_csv("data/cleaned_bm/mef_febmay24.csv"))

# mef16 only has a few weeks of data, maybe exclude
# ggplot(mef_clim |> filter(id == "mef16")) +
#   geom_line(aes(x=dt, y=temperature_c))

vall_clim <- read_csv("data/cleaned_bm/vall_cleaned_all.csv"); glimpse(vall_clim)

# standardisation data frames

load("data/mef_station_data.rda")

val_std <- read_csv("data/vall_stations_cleaned.csv") |>
  dplyr::select(humidity_pct = rh_pct, temperature_c = temp_c, dt, ymd = date_mmddyyyy, site) |>
  dplyr::mutate(vpd_kPa = topomicro::get_vpd(temp_c = temperature_c, rh = humidity_pct)) |>
  dplyr::filter(site == "ValleGrande")

# hours between 24-32C - calculate that?
mef_plotwise_summaries <-
  mef_clim |>
  plotwise_summary_std(stdf = mef_station_data)

vall_plotwise_summaries <-
  vall_clim |>
  plotwise_summary_std(stdf = val_std)

# prism ATR
# tmax <- terra::rast("data/prism/PRISM_tmax_30yr_normal_800mM5_annual_bil/PRISM_tmax_30yr_normal_800mM5_annual_bil.bil")
# tmin <- terra::rast("data/prism/PRISM_tmin_30yr_normal_800mM5_annual_bil/PRISM_tmin_30yr_normal_800mM5_annual_bil.bil")
#
# ATR <- tmax - tmin
# plot(ATR)
