library(tidyverse)
library(topomicro)


# vall data ===============
if(!file.exists("data/cleaned_bm/vall_dec22.csv")){
va <- list.files('data/valles_caldera/aug_23/', pattern = "csv$", full.names = TRUE)
vb <- list.files('data/valles_caldera/dec22_vall/', pattern = "csv$", full.names = TRUE)
  d_raw <- lapply(vb, bm_prep) |>
    bind_rows() |>
    mutate(id = str_to_lower(id) %>% str_remove_all(" ") |> str_to_lower() |> str_replace_all("g0", "g"))
  d_raw1 <- lapply(va, bm_prep) |>
    bind_rows() |>
    mutate(id = str_to_lower(id) %>%
             str_remove_all(" ")|>
             str_remove_all("ad")|>
             str_to_lower() |>
             str_replace_all("g0", "g"))

  d_clean <- d_raw |>
    filter(temperature_c > -39)|>
    filter(!id %in% c("vg14", "vg11", "vg16", "vg13"))
  vg14 <- d_raw|>
    mutate(id = str_to_lower(id) %>% str_remove_all(" "))  %>%
    filter(id == "vg14") %>%
    mutate(dt = dt + (24*3600*6.9))%>%
    filter(dt < as.Date("2022-12-08"))
  vg11 <- d_raw|>
    mutate(id = str_to_lower(id) %>% str_remove_all(" "))  %>%
    filter(id == "vg11") %>%
    mutate(dt = dt - (24*3600*30.25)) |>
    filter(dt < as.Date('2022-10-19'),
           index > 0)

  vg13 <- d_raw |>
    mutate(id = str_to_lower(id) %>% str_remove_all(" ")) %>%
    filter(id == "vg13") %>%
    mutate(dt = dt - (24*3600*36.1)) %>%
    filter(dt < as.Date("2022-11-11"),
           index>0)

  vg16 <- d_raw|>
    mutate(id = str_to_lower(id) %>% str_remove_all(" "))  %>%
    filter(id == "vg16") %>%
    mutate(dt = dt - (24*3600*34.9)) %>%
    filter(dt < as.Date("2022-11-11"),
           index>0)

  rs1 <- d_raw1 |>
    filter(id %in% c('rs1'))  |>
    filter(index > 0) |>
    mutate(dt = dt - (24*60*60*299.1)) |>
    filter(dt < as.Date('2022-10-22'))

  rs2 <-  d_raw1 |>
    filter(id == "rs2") |>
    mutate(dt = dt - (3600*24*144.4)) |>
    filter(dt < as.Date("2023-01-25"))

  rs3 <- d_raw1 |>
    filter(id == "rs3") |>
    mutate(dt = dt - (3600*24*283.2)) |>
    filter(dt < as.Date("2022-11-14"),
           as.numeric(index)> 2)

  rs4 <-d_raw1 |>
    filter(id == "rs4") |>
    mutate(dt = dt - (3600*24*292.75)) |>
    filter(dt < as.Date("2022-11-02"),
           as.numeric(index)> 2)

  rs5 <- d_raw1 |>
    filter(id == "rs5") |>
    mutate(dt = dt - (3600*24*229.7)) |>
    filter(dt < as.Date("2022-12-08"),
           as.numeric(index)> 2)

  vg1 <-   d_raw1 |>
    filter(id == "vg1") |>
    mutate(dt = dt - (3600*24*258.85)) |>
    filter(dt < as.Date("2022-11-30"),
           as.numeric(index)> 2)

  vg9 <-  d_raw1 |>
    filter(id == "vg9") |>
    mutate(dt = dt - (3600*24*281.8)) |>
    filter(dt < as.Date("2022-11-05"),
           as.numeric(index)> 2)

  vg6 <- d_raw1 |>
    filter(id == "vg6")  |>
    mutate(dt = dt - (3600*24*199.1)) |>
    filter(dt < as.Date("2022-12-29"))

  vg3 <- d_raw1 |>
    filter(id == "vg3")  |>
    mutate(dt = dt - (3600*24*102)) |>
    filter(dt < as.Date("2023-02-17"))

  vg5 <- d_raw1 |>
    filter(id == "vg5")  |>
    mutate(dt = dt - (3600*24*155.5)) |>
    filter(dt < as.Date("2023-01-19"))
  vg12 <- d_raw1 |>
    filter(id == "vg12")  |>
    mutate(dt = dt - (3600*24*0.3))
  vg2 <- d_raw1 |>
    filter(id == "vg2")  |>
    mutate(dt = dt - (3600*24*5.3)) |>
    filter(dt < as.Date("2023-04-06"))
  vg4 <- d_raw1 |>
    filter(id == "vg4")  |>
    mutate(dt = dt + (3600*24*93.8)) |>
    filter(dt < as.Date("2023-05-22"))

  d_polished <- d_clean |>
    bind_rows(vg14, vg11, vg13, vg16, rs1, rs2, rs3, rs4, rs5, vg1, vg9, vg6,
              vg3, vg12, vg2, vg4) |>
    filter(temperature_c < 25) |>
    mutate(vpd_kPa = topomicro::get_vpd(rh = humidity_pct, temp_c = temperature_c))
  write_csv(d_polished, "data/cleaned_bm/vall_cleaned_all.csv")


}else{
  d_polished <- read_csv("data/cleaned_bm/vall_dec22.csv")
}


# mef july 2023 collection ==========

if(!file.exists("data/cleaned_bm/mef_jul23.csv")){
  ma <- list.files('data/mef/feb_23/', pattern = "csv$", full.names = TRUE)
  mb <- list.files('data/mef/dec_23/', pattern = "csv$", full.names = TRUE)
  mc <- list.files('data/mef/jul_23/', pattern = "csv$", full.names = TRUE)

  d_raw <- lapply(mc, bm_prep) %>%
    bind_rows() %>%
    mutate(id = str_to_lower(id) %>% str_remove_all("_"),
           index = as.numeric(index))

  d_clean <- d_raw |>
    filter(id != "mef5", index > 1)

  mef5 <- d_raw |>
    filter(id == "mef5", index > 1, index <610) |>
    mutate(dt = dt - (24*60*60*117.35))

  d_polished <-
    d_clean |>
    bind_rows(mef5) |>
    mutate(vpd_kPa = topomicro::get_vpd(rh = humidity_pct, temp_c = temperature_c))

  write_csv(d_polished, "data/cleaned_bm/mef_jul23.csv")
}else{
  d_polished <- read_csv("data/cleaned_bm/mef_jul23.csv")
}

# feb and may 2024 data collection =============

if(!file.exists("data/cleaned_bm/mef_febmay24.csv")){

md <- list.files('data/mef/may_24/', pattern = "csv$", full.names = TRUE)
me <- list.files("data/mef/feb_24/", pattern = "csv$", full.names = TRUE)
d_raw <- lapply(md, bm_prep) %>%
  bind_rows() %>%
  mutate(id = str_to_lower(id) %>% str_remove_all("_"),
         index = as.numeric(index)) |>
  bind_rows(lapply(me, bm_prep) %>%
              bind_rows() %>%
              mutate(id = str_to_lower(id) %>% str_remove_all("_"),
                     index = as.numeric(index)))

d_clean <- d_raw |>
  filter(id %in% c('mef19', 'mef20', 'mef22', 'mef25', 'mef27', 'mef14',
                   'mef17', 'mef18')) |>
  bind_rows(
    d_raw |>
      filter(id %in% c('mef10')) |>
      mutate(dt = dt + (24*60*60*48.95)) |>
      filter(dt < as.Date("2024-02-15"))) |>
  bind_rows(
    d_raw |>
      filter(id %in% c('mef11')) |>
      mutate(dt = dt - (24*60*60*21.55))|>
      filter(dt < as.Date("2024-01-05"))
    ) |>
    bind_rows(
      d_raw |>
        filter(id %in% c('mef21')) |>
        mutate(dt = dt - (24*60*60*83.3)) |>
        filter(dt < as.Date("2023-12-25"))
    ) |>
    bind_rows(
      d_raw |>
        filter(id %in% c('mef23')) |>
        mutate(dt = dt - (24*60*60*23.17))  |>
        filter(dt < as.Date("2024-04-15"))
    ) |>
    bind_rows(
      d_raw |>
        filter(id %in% c('mef24')) |>
        mutate(dt = dt - (24*60*60*115.9))  |>
        filter(dt < as.Date("2024-01-15"))
      ) |>
    bind_rows(
      d_raw |>
        filter(id %in% c('mef7')) |>
        mutate(dt = dt - (24*60*60*67.6))  |>
        filter(dt < as.Date("2023-12-25"))
    ) |>
    bind_rows(
      d_raw |>
        filter(id %in% c('mef8')) |>
        mutate(dt = dt - (24*60*60*125)) |>
        filter(dt < as.Date("2023-11-25"))
    ) |>
    bind_rows(
      d_raw |>
        filter(id %in% c('mef9')) |>
        mutate(dt = dt - (24*60*60*38.7)) |>
        filter(dt < as.Date("2024-01-09"))
    ) |>
  bind_rows(
    d_raw |>
      filter(id %in% c('mef12')) |>
      mutate(dt = dt + (24*60*60*15.25)) |> filter(dt < as.Date("2023-12-25"))
  ) |>
  bind_rows(
    d_raw |>
      filter(id %in% c('mef1')) |>
      mutate(dt = dt - (24*60*60*14.9)) |> filter(dt < as.Date("2023-10-07"))
  ) |>
  bind_rows(
    d_raw |>
      filter(id %in% c('mef16')) |>
      mutate(dt = dt - (24*60*60*53.7)) |> filter(dt < as.Date("2023-09-16"))
  ) |>
  bind_rows(
    d_raw |>
      filter(id %in% c('mef2')) |>
      mutate(dt = dt + (24*60*60*22.5)) |> filter(dt < as.Date("2023-10-03"))
  ) |>
  bind_rows(
    d_raw |>
      filter(id %in% c('mef3')) |>
      mutate(dt = dt - (24*60*60*27))  |> filter(dt < as.Date("2023-10-27"))
  ) |>
  bind_rows(
    d_raw |>
      filter(id %in% c('mef5')) |>
      mutate(dt = dt - (24*60*60*162.75)) |>
      filter(dt < as.Date("2023-09-10"),
             dt > as.Date("2023-07-26"))
  ) |>
  filter(index > 0)|>
  mutate(vpd_kPa = topomicro::get_vpd(rh = humidity_pct, temp_c = temperature_c))

write_csv(d_clean, "data/cleaned_bm/mef_febmay24.csv")}


# bennett creek ======================

locations <- read_csv("data/sensor_locations.csv") |>
  mutate(id = str_to_lower(id),
         alphanumeric_code = str_to_lower(alphanumeric_code)) |>
  filter(str_sub(id, 1,3) == "ben", id != "bennett_parking")

lut_ids <- locations$id
names(lut_ids) <- locations$alphanumeric_code

b1 <- list.files("data/bennett_creek/jan_24/", full.names = T, pattern = ".csv$")
b2 <- list.files("data/bennett_creek/june_24/", full.names = T, pattern = ".csv$")
b3 <- list.files("data/bennett_creek/jul_23/", full.names = T, pattern = ".csv$")
b4 <- list.files("data/bennett_creek/mar_23/", full.names = T, pattern = ".csv$")

d_raw <- lapply(b1, bm_prep) %>%
  bind_rows() %>%
  mutate(id = str_to_lower(id) %>% str_remove_all("_"),
         index = as.numeric(index),
         id = lut_ids[id]); glimpse(d_raw)
d_raw_ju24 <- lapply(b2, bm_prep) %>%
  bind_rows() %>%
  mutate(id = str_to_lower(id) %>% str_remove_all("_"),
         index = as.numeric(index),
         id = lut_ids[id]); glimpse(d_raw_ju24)

d_raw_ju23 <- lapply(b3, bm_prep) %>%
  bind_rows() %>%
  mutate(id = str_to_lower(id) %>% str_remove_all("_") |> str_remove_all(' '),
         index = as.numeric(index)); glimpse(d_raw_ju23)

d_raw_mar23a <- lapply(b4, bm_prep) %>%
  bind_rows() %>%
  mutate(id = str_to_lower(id) %>% str_remove_all("_") |> str_remove_all(' '),
         index = as.numeric(index)
         ,
         id = lut_ids[id]
         ) |>
  filter(!is.na(id)); glimpse(d_raw_mar23a)
d_raw_mar23b <- lapply(b4, bm_prep) %>%
  bind_rows() %>%
  mutate(id = str_to_lower(id) %>% str_remove_all("_") |> str_remove_all(' '),
         index = as.numeric(index)) |>
  filter(id %in% c("ben11", "ben3")); glimpse(d_raw_mar23b)

d_clean <- d_raw |>
  filter(id %in% c('ben5', "ben8")) |>
  bind_rows(d_raw |>
              filter(id %in% c('ben1'),
                     dt > as.Date("2023-12-23"))) |>
  bind_rows(d_raw |>
              filter(id %in% c('ben1'))  |>
              filter(dt < as.Date('2023-12-02')) |>
              mutate(dt = dt - (24*60*60*88.1))) |>
  bind_rows(d_cleaner <- d_raw |>
              filter(id %in% c('ben10'))  |>
              filter(dt > as.Date('2023-12-23')))|>
  bind_rows(d_cleaner <- d_raw |>
              filter(id %in% c('ben11'))  |>
              filter(dt > as.Date('2023-12-23')))|>
  bind_rows(d_cleaner <- d_raw |>
              filter(id %in% c('ben12'))  |>
              filter(dt > as.Date('2023-12-23')))|>
  bind_rows(d_cleaner <- d_raw |>
              filter(id %in% c('ben2'))  |>
              filter(dt > as.Date('2023-12-23')))|>
  bind_rows(d_cleaner <- d_raw |>
              filter(id %in% c('ben6'))  |>
              filter(dt > as.Date('2023-12-23')))|>
  bind_rows(d_cleaner <- d_raw |>
              filter(id %in% c('ben9'))  |>
              filter(dt > as.Date('2023-12-23'))) |>
  bind_rows(d_raw |>
              filter(id %in% c('ben10'))  |>
              filter(dt < as.Date('2023-10-29')) |>
              mutate(dt = dt - (24*60*60*53))) |>
  bind_rows(d_raw |>
              filter(id %in% c('ben11'))  |>
              filter(dt < as.Date('2023-12-13')) |>
              mutate(dt = dt - (24*60*60*18.6))) |>
  bind_rows(d_raw |>
              filter(id %in% c('ben12'))  |>
              filter(dt < as.Date('2023-11-27')) |>
              mutate(dt = dt - (24*60*60*56.3))) |>
  bind_rows(d_raw |>
              filter(id %in% c('ben2'))  |>
              filter(dt < as.Date('2023-12-05')) |>
              mutate(dt = dt - (24*60*60*100.85))) |>
  bind_rows(d_raw |>
              filter(id %in% c('ben3'))  |>
              filter(dt < as.Date('2023-12-25')) |>
              mutate(dt = dt - (24*60*60*60.85)) |>
              filter(dt > as.Date("2023-07-22"))) |>
  bind_rows(d_raw |>
              filter(id %in% c('ben4'))  |>
              filter(dt > as.Date("2023-07-22"))) |>
  bind_rows(d_raw |>
              filter(id %in% c('ben7'))  |>
              filter(dt < as.Date('2023-12-31')) |>
              mutate(dt = dt - (24*60*60*34.2))) |>
  bind_rows( d_raw |>
               filter(id %in% c('ben7'))  |>
               filter(dt > as.Date('2024-01-07')) |>
               mutate(dt = dt - (24*60*60*15.1)) |>
               filter(dt < as.Date("2024-01-16"))) |>
  bind_rows(d_raw |>
              filter(id %in% c('ben9'))  |>
              mutate(dt = dt - (24*60*60*63.4)) |>
              filter(dt < as.Date("2023-10-12"))) |>
  bind_rows(d_raw_ju24 |>
              filter(!id %in% c('ben3', 'ben4'))  |>
              filter(dt > as.Date("2024-01-30"))) |>
  bind_rows(d_raw_ju24 |>
              filter(id %in% c('ben3'))  |>
              filter(dt < as.Date("2024-03-11"),
                     dt > as.Date("2024-02-25")) |>
              mutate(dt = dt - (24*60*60*57.9))) |>
  bind_rows(d_raw_ju24 |>
              filter(id %in% c('ben4'))  |>
              mutate(dt = dt + (24*60*60*92.67)) |>
              filter(dt < as.Date("2024-05-21"),
                     dt > as.Date("2024-01-30"))) |>
  bind_rows(d_raw_ju23 |>
              filter(!id %in% c('ben3', 'ben4', 'ben7', 'ben8'))  |>
              filter(dt > as.Date("2024-01-30")))|>
  bind_rows(d_raw_mar23a |>
              filter(!id %in% c('ben3', 'ben7', 'ben8', 'ben11', NA))) |>
  bind_rows(d_raw_mar23b |>
              filter(id %in% c('ben11'))  |>
              filter(index < 5900) |>
              mutate(dt = dt + (24*60*60*14))) |>
  filter(index > 0)|>
  mutate(vpd_kPa = topomicro::get_vpd(rh = humidity_pct, temp_c = temperature_c))

write_csv(d_clean, 'data/cleaned_bm/bennett_all.csv')

# manual exploration and cleaning ========================================
val_std <- read_csv("data/vall_stations_cleaned.csv") |>
  dplyr::select(humidity_pct = rh_pct, temperature_c = temp_c, dt, ymd = date_mmddyyyy, site) |>
  dplyr::mutate(vpd_kPa = topomicro::get_vpd(temp_c = temperature_c, rh = humidity_pct)) |>
  dplyr::filter(site == "ValleGrande")

d_cleaner <-d_raw|>
  mutate(id = str_to_lower(id) %>% str_remove_all(" "))  %>%
  filter(id == "vg16") %>%
  mutate(dt = dt - (24*3600*34.9)) %>%
  filter(dt < as.Date("2022-11-11"),
         index>0)


val_std |>
  bind_rows(d_cleaner) |>
  filter(dt > as.Date('2022-09-21')
         ,
         dt < as.Date('2022-11-19')
  ) |>
    ggplot(aes(x=dt, y=temperature_c, color = id)) +
    geom_line() +
    # scale_x_datetime(date_breaks = 'month') +
  theme(axis.text.x = element_text(angle=90))# +
  # facet_wrap(~id, ncol =1)

ggplot(d_raw_mar23a, aes(x=dt, y=temperature_c)) +
  geom_line() +
  facet_wrap(~id, ncol=1)
