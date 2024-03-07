library(tidyverse)
library(janitor)

files <- list.files("data/big/weather_stations", full.names = T)

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

cleand_wrx <- lapply(files, clean_wrcc)

all <- bind_rows(cleand_wrx) |>
  dplyr::select(ppt_in = in_precip,
                rh_pct = percent_rel_humidty,
                deg_wind_direc,
                mph_wind_speed,
                temp_f = deg_f_2m_av_temp,
                date_mmddyyyy, dt, year)
write_csv(all,"data/vall_stations_cleaned.csv")

