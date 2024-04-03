# standardise BM data against weather station data
library(tidyverse)
library(topomicro)


bm_mef <- read_csv("data/cleaned_bm/mef_jul23.csv") |>
  dplyr::mutate(dt = round_date(dt, unit = "hour")) |>
  dplyr::select(dt, ymd, temperature_c, humidity_pct, vpd_kPa, id)|>
  filter(dt < as.Date("2022-12-20"), id == "mef2") |>
  dplyr::select(dt, temperature_c) |>
  mutate(source = "Blue Maestro")

wrx_mef <- read_csv("data/mef_station_cleaned.csv") |>
  dplyr::select(dt = timestamp_ts,
                temperature_c = ta_avg_c_avg,
                humidity_pct = rh_avg_percent_avg,
                wind_speed_m_per_s = ws_wvc_1_ms_1_wvc) |>
  dplyr::mutate(vpd_kPa = topomicro::get_vpd(rh = humidity_pct, temp_c = temperature_c),
                dt = lubridate::force_tz(dt, "America/Denver")) |>
  filter(dt > as.Date("2022-12-11"),
         dt < as.Date("2022-12-20")) |>
  dplyr::select(dt, temperature_c) |>
  mutate(source = "Weather Station")

era5 <- tidync::tidync("data/mef_era5_2022.nc") %>%
  tidync::hyper_tibble() %>%
  dplyr::mutate(., obs_time = lubridate::ymd_hms("1900:01:01 00:00:00") + (time * 3600),
                timezone = lubridate::tz(obs_time)) %>% # convert to readable times
  dplyr::rename(., pressure = sp) %>%
  dplyr::mutate(., temperature_c = t2m - 273.15) |>
  dplyr::mutate(dt = obs_time, "America/Denver") |>
  filter(longitude == -105.25, latitude == 39.25) |>
  filter(dt > as.Date("2022-12-11"),
         dt < as.Date("2022-12-20")) |>
  dplyr::select(dt, temperature_c) |>
  mutate(source = "ERA5")

ggplot(bind_rows(bm_mef, wrx_mef, era5),
       aes(x=dt, y=temperature_c, color = source)) +
  geom_line()
ggsave("out/compare_sources.png")
