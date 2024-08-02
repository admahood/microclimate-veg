library(tidyverse)
library(topomicro)
library(ggthemes)
library(sf)
library(geomtextpath)
library(plantecophys)
library(ggthemes)
library(tseries)
library(terra)
library(ggpubr)

vall_twi <- terra::rast("data/vc_twi.tif")

vall_stations <- data.frame(name = c("Redondo Saddle", "Valle Grande", "Hidden Valley"),
                            x = c(-106.55361111111111, -106.521, -106.50055555555555),
                            y = c(35.88388888888889, 35.858333333333334,35.840833333333336)) |>
  st_as_sf(coords = c(x='x',y='y'), crs = 4326) |>
  st_transform(crs = st_crs(vall_twi)) %>%
  mutate(terra::extract(vall_twi, .),
         twi = round(twi, 1))



daily_dtr <- read_csv(file = "data/vall_stations_cleaned.csv") |>
  mutate(vpd_kpa = plantecophys::RHtoVPD(rh_pct, temp_c)) |>
  group_by(date = date_mmddyyyy, Station= site) |>
  reframe(dtr = max(temp_c) - min(temp_c),
          vmin = min(vpd_kpa),
          vmax = max(vpd_kpa)) |>
  filter(date < as.Date('2024-01-01')) |>
  mutate(month = lubridate::month(date, abbr = T,label =T),
         year = lubridate::year(date),
         Station = case_when(Station == "HiddenValley" ~ "1. Hidden Valley (TWI: 17.9)",
                             Station == "ValleGrande" ~ "2. Valle Grande (TWI: 13.3)",
                             Station == "Redondo" ~ "3. Redondo Saddle (TWI: 6.0)"))


ggplot(daily_dtr |> filter(vmin > 0),
       aes(x=vmin, y=dtr, color = Station)) +
  geom_point() +
  xlab("VPD (kpa)") +
  geom_hline(yintercept = c(12, 14), lty = 2) +
  facet_wrap(~Station, scales = 'free')

p1<- ggplot(daily_dtr |> filter(vmin > 0) |> mutate(month = lubridate::month(date)),
       aes(Station, vmin, fill = as.factor(month))) +
  geom_boxplot(, position = "dodge")

p2 <- ggplot(daily_dtr |> filter(vmin > 0) |> mutate(month = lubridate::month(date)),
       aes(Station, dtr, fill = as.factor(month))) +
  geom_boxplot(, position = "dodge")
ggarrange(p1, p2, nrow =2) |>
  ggsave(filename = 'out/vpdmin_dtr_vall_sensors.png', bg='white', height =9, width=9)
# panel 1: dtr raw data

p1 <- ggplot(daily_dtr, aes(x=month, y=dtr, fill = Station)) +
  geom_boxplot() +
  facet_wrap(~year, ncol=1) +
  theme_clean() +
  theme(panel.background = element_rect(color = "black"),
        panel.grid.major.x = element_line(color = 'grey', linetype = 2),
        legend.position = c(1,1),
        legend.justification = c(1,1));p1

ggsave(plot = p1, filename = "out/dtr_vall_raw.png", height = 7.5, width=8.5, bg='white')

# panel 2: mean stability
resdf <- data.frame(n=NA, mean = NA, sd = NA, Station = NA, dftest = NA)
c = 1
for(i in 10:600){
  for(s in unique(daily_dtr$Station)){
    ddd <- daily_dtr |>
      filter(Station == s) |>
      slice(1:i)

    x <- ddd |>
      summarise(mean = mean(dtr), sd = sd(dtr))

    adf <- tseries::adf.test(ddd$dtr)

    resdf[c,1] <- i
    resdf[c,2] <- x$mean
    resdf[c,3] <- x$sd
    resdf[c,4] <- s
    resdf[c,5] <- adf$p.value
    c=c+1
  }}

p2 <- ggplot(resdf |> na.omit(), aes(x=n, y=mean, color = Station)) +
    geom_line() +
    xlab("Number of Days") +
    scale_x_continuous(breaks = c(100,200,300,400,500)) +
    ggtitle("Mean stability for sequentially sampled days") +
    theme_clean() +
    theme(legend.position = "none");p2

p3 <- ggplot(resdf |> na.omit(), aes(x=n, y=dftest, color = Station)) +
  geom_line() +
  xlab("Number of Days") +
  ylab("P") +
  scale_x_continuous(breaks = c(100,200,300,400,500)) +
  ggtitle("P-Values from Augmented Dickey-Fuller Test") +
  geom_hline(yintercept = c(.05), lty = 2) +
  geom_text(x=300, y=0.2, label = "p = 0.05", color = "grey30") +
  theme_clean();p3


# t-tests



resdf_t <- data.frame(n=NA, `HV_RS` = NA, `HV_VG` = NA, `VG_RS` = NA)
i=1
for(n in 5:100){
  d <- daily_dtr |>
    pivot_wider(names_from = Station, values_from = dtr) |>
    slice(1:n)

  resdf_t[i,1] <- n
  resdf_t[i,2] <- t.test(dplyr::select(d,4), dplyr::select(d,5))$p.value
  resdf_t[i,3] <- t.test(dplyr::select(d,4), dplyr::select(d,6))$p.value
  resdf_t[i,4] <- t.test(dplyr::select(d,6), dplyr::select(d,5))$p.value
  i=i+1
}

pt <- resdf_t |>
  pivot_longer(cols = c(`HV_RS`, `HV_VG`, `VG_RS`), names_to = 'Comparison', values_to = "p") |>
  ggplot(aes(x=n, y=p, color = Comparison)) +
  geom_line() +
  scale_y_log10(breaks = c(0.05, 0.001, 1e-10, 1e-22),
                labels = c('0.05',  '0.001', '1e-10','1e-22')) +
  xlab("Number of Days Compared") +
  ylab("P") +
  ggtitle("P-Values from T-Test Comparisons") +
  theme_clean() #+
  # theme(legend.position = c(0,0),
  #       legend.justification = c(0,0))

ggarrange(p2, p3, pt, ncol =1, heights = c(1,1,1))

ggsave(filename = 'out/mean_stability.png', width = 7, height=7, bg='white')



# what day had a 40C DTR

read_csv(file = "data/vall_stations_cleaned.csv") |>
  filter(date_mmddyyyy >= as.Date('2022-01-01'),
         date_mmddyyyy < as.Date('2022-03-01')) |>
  ggplot(aes(x=dt, y = temp_c, color = site, group = site)) +
  geom_line()

ggsave('out/yes_dtrisreallythishigh.png', bg='white')
