library(tidyverse)
library(topomicro)
library(sf)
library(terra)

unbroken <- c('mef1', 'mef10', 'mef11', 'mef12', 'mef14', 'mef16', 'mef17',
              'mef18', 'mef19', 'mef2', 'mef20', 'mef21', 'mef22', 'mef23',
              'mef24', 'mef25', 'mef27', 'mef3', 'mef4', 'mef5', 'mef6', 'mef7',
              'mef8', 'mef9', 'rs1', 'rs2', 'rs3', 'rs4', 'rs5', 'vg1', 'vg10',
              'vg11', 'vg12', 'vg13', 'vg14', 'vg15', 'vg16', 'vg2', 'vg3', 'vg4',
              'vg5', 'vg6', 'vg7', 'vg8', 'vg9')

locations <- read_csv("data/sensor_locations.csv") |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  dplyr::select(-starts_with("alpha")) |>
  dplyr::mutate(id  = str_replace_all(id, 'vg0', 'vg')) |>
  filter(str_sub(id, 1,2) %in% c('vg', 'me', 'rs'),
         id %in% unbroken) |>
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
  dplyr::mutate(vpd_kPa = topomicro::vpd(temp_c = temperature_c, rh = humidity_pct)) |>
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
