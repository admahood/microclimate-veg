library(tidyverse)
library(topomicro)

if(!file.exists("data/cleaned_bm/vall_dec22.csv")){
va <- list.files('data/valles_caldera/aug_23/', pattern = "csv$", full.names = TRUE)
vb <- list.files('data/valles_caldera/dec22_vall/', pattern = "csv$", full.names = TRUE)
  d_raw <- lapply(vb, bm_prep) |>
    bind_rows()
  d_raw1 <- lapply(va, bm_prep) |>
    bind_rows()
  d_clean <- d_raw |>
    filter(temperature_c > -39) |>
    mutate(id = str_to_lower(id) %>% str_remove_all(" ")) |>
    filter(!id %in% c("vg14", "vg11", "vg16", "vg13"))
  vg14 <- d_raw|>
    mutate(id = str_to_lower(id) %>% str_remove_all(" "))  %>%
    filter(id == "vg14") %>%
    mutate(dt = dt + (24*3600*7)+(3600*4))%>%
    filter(dt < as.Date("2022-12-08"))
  vg11 <- d_raw |>
    mutate(id = str_to_lower(id) %>% str_remove_all(" ")) %>%
    filter(id == "vg11") %>%
    mutate(dt = dt - (24*3600*30),
           dt = dt + (3600*1)) %>%
    filter(dt < as.Date("2022-10-18"))
  vg13 <- d_raw |>
    mutate(id = str_to_lower(id) %>% str_remove_all(" ")) %>%
    filter(id == "vg13") %>%
    mutate(dt = dt - (24*3600*36),
           dt = dt+ (3600*4)) %>%
    filter(dt < as.Date("2022-11-08"))
  vg16 <- d_raw|>
    mutate(id = str_to_lower(id) %>% str_remove_all(" "))  %>%
    filter(id == "vg16") %>%
    mutate(dt = dt - (24*3600*35),
           dt = dt+ (3600*7)) %>%
    filter(dt < as.Date("2022-11-08"))
  rs1 <- d_raw1 |>
    filter(id == "Rsad 1") |>
    mutate(dt = dt - (3600*24*298.9),
           id = "rs1") |>
    filter(dt < as.Date("2022-10-21"),
           as.numeric(index) >5)
  rs2 <- d_raw1 |>
    filter(id == "Rsad 2") |>
    mutate(dt = dt - (3600*24*144.1),
           id = 'rs2') |>
    filter(dt < as.Date("2023-01-25"))
  rs3 <- d_raw1 |>
    filter(id == "Rsad3") |>
    mutate(dt = dt - (3600*24*282.9),
           id = 'rs3') |>
    filter(dt < as.Date("2022-11-15"),
           as.numeric(index)> 2)
  rs4 <- d_raw1 |>
    filter(id == "Rsad 4") |>
    mutate(dt = dt - (3600*24*292.5),
           id = 'rs4') |>
    filter(dt < as.Date("2022-11-03"),
           as.numeric(index)> 2)
  rs5 <- d_raw1 |>
    filter(id == "Rsad5") |>
    mutate(dt = dt - (3600*24*229.25),
           id = 'rs5') |>
    filter(dt < as.Date("2022-12-14"),
           as.numeric(index)> 2)
  vg1 <- d_raw1 |>
    filter(id == "VG 1") |>
    mutate(dt = dt - (3600*24*258.65),
           id = 'vg1') |>
    filter(dt < as.Date("2022-12-01"),
           as.numeric(index)> 2)
  vg9 <- d_raw1 |>
    filter(id == "Vg9") |>
    mutate(dt = dt - (3600*24*281.5),
           id = 'vg9') |>
    filter(dt < as.Date("2022-11-13"),
           as.numeric(index)> 2)

  vg6 <- d_raw1 |> # min(vg6$dt) - dt0
    filter(id == "Vg 6")  |>
    mutate(dt = dt - (3600*24*198.86),
           id = 'vg6') |>
    filter(dt < as.Date("2022-12-29"),
           dt > as_datetime("2022-12-15 11:54:05 MST"))
  vg3 <- d_raw1 |>
    filter(id == "Vg3")  |>
    mutate(dt = dt - (3600*24*101.70),
           id = 'vg3') |>
    filter(dt < as.Date("2023-02-17"))

  vg5 <- d_raw1 |> # min(vg6$dt) - dt0
    filter(id == "Vg 5")  |>
    mutate(dt = dt - (3600*24*155.2),
           id = 'vg5') |>
    filter(dt < as.Date("2023-01-20"))

  d_polished <- d_clean |>
    bind_rows(vg14, vg11, vg13, vg16, rs1, rs2, rs3, rs4, rs5, vg1, vg9, vg6,
              vg3) |>
    filter(temperature_c < 25) |>
    mutate(vpd_kPa = topomicro::get_vpd(rh = humidity_pct, temp_c = temperature_c))
  write_csv(d_polished, "data/cleaned_bm/vall_dec22.csv")

  vg12 <- d_raw1 |>
    filter(id == "Vg12") |>
    mutate(id = 'vg 12')
  vg2 <- d_raw1 |> # min(vg6$dt) - dt0
    filter(id == "Vg 2")  |>
    mutate(dt = dt - (3600*24*5.1),
           id = 'vg2') |>
    filter(dt < as.Date("2023-04-07"))
  vg4 <- d_raw1 |> # min(vg6$dt) - dt0
    filter(id == "Vg 4")  |>
    mutate(dt = dt + (3600*24*94.1),
           id = 'vg4') |>
    filter(dt < as.Date("2023-05-22"))

  d_polished1 <- bind_rows(vg12, vg2, vg4) |>
    filter(temperature_c < 25) |>
    mutate(vpd_kPa = topomicro::get_vpd(rh = humidity_pct, temp_c = temperature_c))
  write_csv(d_polished, "data/cleaned_bm/vall_aug23.csv")

}

dt0 <- d_polished |>
  filter(id == "vg11") |>
  pull(dt) |>
  min()

d_polished %>%
  filter(id == "vg6") %>%
  pull(dt) %>%
  max()

vg5 <- d_raw1 |> # min(vg6$dt) - dt0
  filter(id == "Vg 5")  |>
  mutate(dt = dt - (3600*24*155.2),
         id = 'vg 5') |>
  filter(dt < as.Date("2023-01-20"))

ggplot(bind_rows(vg5, filter(d_polished, id == 'vg 3')), aes(x=dt, y=temperature_c, color = id)) +
  geom_line() #+
 #facet_wrap(~id, ncol=1)

ggplot(d_raw1 |> filter(str_sub(id, 1,2) == "Vg",
                        !id %in% c("Vg9", "Vg12", 'Vg 6', "Vg3", "Vg 2", "Vg 4",
                                   "Vg 5")),
       aes(x=as.numeric(index),
  # x = dt,
  y=temperature_c)) +
  geom_line() +
  geom_hline(yintercept = -20, lty=2)+
  facet_wrap(~id, ncol=1)
ggplot(d_polished,# |> filter(str_sub(id, 1,2) == "vg"),
       aes(x=dt, y=temperature_c)) +
  geom_line() +
  facet_wrap(~id, ncol =1)
ggsave(filename = "out/vall_temp_ts.png")
ggplot(d_polished1,# |> filter(str_sub(id, 1,2) == "vg"),
       aes(x=dt, y=temperature_c)) +
  geom_line() +
  facet_wrap(~id, ncol =1)

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
}
