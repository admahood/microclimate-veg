library(tidyverse)

# manitou
mef_wx <- read_csv("data/mef_station_cleaned.csv")
glimpse(mef_wx)
dd <- mef_wx |>
  mutate(rubisco = ifelse(ta_avg_c_avg > 10 & ta_avg_c_avg <30, 1, 0),
         month = lubridate::month(timestamp_ts),
         day = lubridate::day(timestamp_ts),
         year = lubridate::year(timestamp_ts)) |>
  group_by(year, month, day) |>
  summarise(dtr = max(ta_avg_c_avg) - min(ta_avg_c_avg),
            wind =  mean(ws_wvc_1_ms_1_wvc),
            opt_hrs = sum(rubisco)) |>
  ungroup()

dd |>
  # filter(wind < 1.7) |>
  ggplot(aes(x=month |> as.factor(), y=dtr, fill=year |> as.factor())) +
  geom_boxplot()

dd |>
  # filter(wind < 1.7) |>
  ggplot(aes(x=month |> as.factor(), y=opt_hrs, fill=year |> as.factor())) +
  geom_boxplot()

mef_wx |>
  mutate(rubisco = ifelse(ta_avg_c_avg > 15 & ta_avg_c_avg <30, 1, 0),
         month = lubridate::month(timestamp_ts),
         day = lubridate::day(timestamp_ts),
         year = lubridate::year(timestamp_ts)) |>
  group_by(year, month, day) |>
  summarise(dtr = max(ta_avg_c_avg) - min(ta_avg_c_avg),
            opt_hrs = sum(rubisco)) |>
  ungroup() |>
  group_by(year, month) |>
  summarise(dtr_mean = mean(dtr),
            dtr_sd = sd(dtr),
            opt_mean = mean(opt_hrs)) |>
  ungroup() |>
  print(n=30)

# valles caldera

vall_wx <- read_csv("data/vall_stations_cleaned.csv")
dd <- vall_wx |>
  mutate(rubisco = ifelse(temp_c >= 12 & temp_c <=30, 1, 0),
         month = lubridate::month(dt) |> as.factor(),
         day = lubridate::day(dt) |> as.factor(),
         year = lubridate::year(dt) |> as.factor(),
         vpd = get_vpd(rh_pct, temp_c)) |>
  group_by(year, month, day, site) |>
  summarise(dtr = max(temp_c, na.rm=T) - min(temp_c, na.rm=T),
            vpd = min(vpd),
            wind =  mean(mph_wind_speed ),
            rub_hrs = sum(rubisco)/6) |>
  ungroup()

dd |>
  filter(!is.na(year)) |>
  ggplot(aes(x=month |> as.factor(), y=dtr, fill=site)) +
  geom_boxplot() +
  facet_wrap(~year, ncol=1) +
  ggtitle("Diurnal Temperature Range (C) at Valles Caldera") +
  theme_clean() +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0))
ggsave("out/dtr_vall.png", width=7, height=7, bg="white")

dd |>
  filter(!is.na(year), year != "2024", vpd>0) |>
  ggplot(aes(x=month |> as.factor(), y=vpd, fill=site)) +
  geom_boxplot() +
  facet_wrap(~year, ncol=1) +
  ggtitle("minimum daily vpd (kPa) at Valles Caldera") +
  theme_clean() +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0))
ggsave("out/vpdmin_vall.png", width=7, height=7, bg="white")

dd |>
  filter(!is.na(year), year != "2024") |>
  ggplot(aes(x=month |> as.factor(), y=rub_hrs, fill=site)) +
  geom_boxplot() +
  facet_wrap(~year, ncol=1) +
  ggtitle("hours between 12C and 30C at Valles Caldera") +
  theme_clean() +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))
ggsave("out/topt_vall.png", width=7, height=7, bg="white")
