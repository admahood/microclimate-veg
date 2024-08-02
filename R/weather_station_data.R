library(tidyverse)
library(janitor)

# functions ============================================================
clean_wrcc <- function(filename){
  print(filename)
  d <- readxl::read_xlsx(filename, skip = 3) |>
    janitor::clean_names()
  hv <- readxl::read_xlsx(filename) |>
    janitor::clean_names()
  h <- hv[1:3,]
  old_names <- names(hv)
  new_names <- c()
  for(i in 1:ncol(h)){
    new_names[i] <- h |>
      pull(old_names[i]) |>
      paste(collapse = "_") |>
      str_remove_all("\\:")  |>
      str_remove_all(" ") |>
      str_remove_all("\\/") |>
      str_remove_all("NA")|>
      str_remove_all("^_") |>
      str_remove_all("_$")
    }
  names(d) <- new_names
  d <- janitor::clean_names(d)
  d <- d |>
    mutate(date_mmddyyyy = as.Date(date_mmddyyyy),
      lst_time_hhmm = str_sub(lst_time_hhmm, 12,19),
           dt = lubridate::as_datetime(paste(date_mmddyyyy, lst_time_hhmm)))
  d$filename <- filename
  return(d)
}

clean_mef <- function(filename){
  print(filename)
  d <- readxl::read_xlsx(filename, skip = 3) |>
    janitor::clean_names()
  hv <- readxl::read_xlsx(filename) |>
    janitor::clean_names()
  h <- hv[1:3,]
  old_names <- names(hv)
  new_names <- c()
  for(i in 1:ncol(h)){
    new_names[i] <- h |>
      pull(old_names[i]) |>
      paste(collapse = "_") |>
      str_remove_all("\\:")  |>
      str_remove_all(" ") |>
      str_remove_all("\\/") |>
      str_remove_all("NA")|>
      str_remove_all("^_") |>
      str_remove_all("_$") |>
      str_to_lower()
  }
  names(d) <- new_names
  d <- janitor::clean_names(d)
  d$filename <- filename
  return(d)
}

# cleaning =========
val_files <- list.files("data/big/weather_stations", full.names = T)

cleand_wrx <- lapply(val_files, clean_wrcc)

cleand_wrx[[2]] |> glimpse()

all <- bind_rows(cleand_wrx) |>
  dplyr::select(ppt_in = in_precip,
                site = filename,
                rh_pct = percent_rel_humidty,
                deg_wind_direc,
                mph_wind_speed,
                temp_f = deg_f_av_air_temp,
                date_mmddyyyy, dt, year) |>
  dplyr::mutate(site = str_remove_all(site, "data/big/weather_stations/WRCC_")|>
                  str_remove_all("_RAWS_Data.xlsx"),
                temp_c = (temp_f - 32) * (5/9))
write_csv(all,"data/vall_stations_cleaned.csv")

# mef ================

mef_files <- "data/Julian Date 177 1000 to 03_12_24 post pub backup.xlsx"

cleaned_mef <- clean_mef(mef_files)
write_csv(cleaned_mef, "data/mef_station_cleaned.csv")


# full record vall ====================

filez <- list.files("data/big/full_record_vall", full.names=T)

lapply(filez, clean_wrcc) -> wlist


wlist[[2]] |> glimpse()
clist <- list()
for(i in 1:length(wlist)){
  clist[[i]] <- wlist[[i]] |>
    dplyr::select(ppt_mm = mm_precip,
                  site = filename,
                  rh_pct = percent_rel_humidty,
                  temp_c = deg_c_av_air_temp,
                  date = date_mmddyyyy, dt) |>
    dplyr::mutate(site = str_remove_all(site, "data/big/full_record_vall/WRCC_")|>
                    str_remove_all("_RAWS_Data.xlsx"))

}

all <- bind_rows(clist)

# write_csv(all,"data/vall_stations_cleaned_full_record.csv")

all |> group_by(date, site) |>
  summarise(dtr = max(temp_c) - min(temp_c)) |>
  ungroup() |>
  ggplot(aes(x=date, y=dtr, color = site)) +
  geom_line()
