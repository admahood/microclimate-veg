# get era5 data
remotes::install_github("dklinges9/mcera5", ref = "no_microclima")
library(mcera5)
library(sf)
library(tidyverse)
# vignette("mcera5_vignette")

# download from cds ============
uid <- '176012'
cds_api_key <- '46a2883c-9c82-4500-a98b-7c629608d48e'

ecmwfr::wf_set_key(user = uid,
                   key = cds_api_key,
                   service = "cds")

mef_locations <- mef_locations <- read_csv("data/sensor_locations.csv") |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  dplyr::select(-starts_with("alpha")) |>
  filter(str_sub(id, 1,3) == "mef")

mef_bbox <- mef_locations |>
  st_bbox()

val <- mef_locations <- read_csv("data/sensor_locations.csv") |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  dplyr::select(-starts_with("alpha")) |>
  filter(str_sub(id, 1,2) %in% c("rs", "vg")) |>
  st_bbox()

mef_tr <- read_csv("data/cleaned_bm/mef_jul23.csv") |>
  dplyr::pull(dt) |>
  range()

file_prefix <- "mef_era5"
file_path <- str_c(getwd(), "/data")

req_mef <- build_era5_request(xmin = mef_bbox$xmin,
                              xmax = mef_bbox$xmax,
                              ymin = mef_bbox$ymin,
                              ymax = mef_bbox$ymax,
                              start_time = mef_tr[1],
                              end_time = mef_tr[2],
                              outfile_name = file_prefix)

request_era5(request = req_mef, uid = uid, out_path = file_path)

# format the data ===============
x <- st_coordinates(mef_locations)[1,1] |> unname()
y <- st_coordinates(mef_locations)[1,2] |> unname()
ncs <- list.files("data", pattern = "nc$", full.names = T)

library(terra)
library(ncdf4)
library(tidync)
terra::rast(ncs) -> ncr
ncr |> as.data.frame() |> as_tibbledplyr::select(temp_k = t2m)

ncdf4::nc_open(ncs) |> mcera5::nc_to_df()
clim_point <- mcera5::extract_clim(nc = ncs,
                                   d_weight = F,
                           long = x,
                           lat = y,
                           start_time = mef_tr[1],
                           end_time = lubridate::as_datetime("2022-12-21 00:00:00"))

tidync::tidync(ncs)
head(clim_point)


