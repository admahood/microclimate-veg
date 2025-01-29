#plot weather stationdata

library(tidyverse)
d <- read_csv("data/mef_station_cleaned.csv") |>
  dplyr::select(timestamp_ts, ta_avg_c_avg, rh_avg_percent_avg, ppt_tot_c_tot) |>
  dplyr::filter(timestamp_ts > as.Date("2024-02-25")) |>
  pivot_longer(-timestamp_ts)

ggplot(d, aes(x=timestamp_ts, y=value, color = name)) +
  geom_line() +
  facet_wrap(~name, scales = "free_y", ncol=1) +
  geom_vline(xintercept = as.Date("2024-02-28") |> lubridate::as_datetime(), lty=2) +
  scale_x_datetime(date_breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 90))

ggsave("out/mef_station_viz_march24.png")



