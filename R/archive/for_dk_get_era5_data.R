# get era5 data
# remotes::install_github("dklinges9/mcera5", ref = "no_microclima")
library(mcera5)
library(sf)
library(tidyverse)
# vignette("mcera5_vignette")

# download from cds ============
uid <- 'XXXX'
cds_api_key <- 'XXXX'

ecmwfr::wf_set_key(user = uid,
                   key = cds_api_key,
                   service = "cds")

file_prefix <- "mef_era5"
file_path <- str_c(getwd(), "/data")

req_mef <- build_era5_request(xmin = -105.10217,
                              xmax = 39.08483,
                              ymin = -105.02117,
                              ymax = 39.10346,
                              start_time = "2022-12-12 19:27:13 UTC",
                              end_time = "2022-12-31 11:59:00 UTC",
                              outfile_name = file_prefix)

request_era5(request = req_mef, uid = uid, out_path = file_path)

# format the data ===============

ncs <- list.files("data", pattern = "nc$", full.names = T)

x <- -105.25
y <- 39.25

clim_point <- mcera5::extract_clim(nc = ncs,
                                   long = x,
                                   lat = y,
                                   start_time = mef_tr[1],
                                   end_time = lubridate::as_datetime("2022-12-21 00:00:00"))




