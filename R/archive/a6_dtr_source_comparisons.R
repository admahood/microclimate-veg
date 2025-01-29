# comparing dtr at different scales
# figure 3: NMDS
library(tidyverse)
library(vegan)
library(terra)
library(ggpubr)
library(ggthemes)
library(sf)
library(ggrepel)
library(terra)
library(topomicro)
# need to do a rank index, all that jazz along tdelta

locations <- read_csv("data/sensor_locations.csv") |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  dplyr::select(-starts_with("alpha")) |>
  mutate(id = str_replace_all(id, 'vg0', "vg"))

val_topo <- read_csv('data/val_topo.csv')
mef_topo <- read_csv('data/mef_topo.csv')

mef_clim <- read_csv("data/cleaned_bm/mef_jul23.csv") |>
  bind_rows(read_csv("data/cleaned_bm/mef_febmay24.csv")); glimpse(mef_clim)

vall_clim <- read_csv("data/cleaned_bm/vall_cleaned_all.csv"); glimpse(vall_clim)

# standardisation data frames

load("data/mef_station_data.rda")

val_std <- read_csv("data/vall_stations_cleaned.csv") |>
  dplyr::select(humidity_pct = rh_pct, temperature_c = temp_c, dt, ymd = date_mmddyyyy, site) |>
  dplyr::mutate(vpd_kPa = topomicro::get_vpd(temp_c = temperature_c, rh = humidity_pct)) |>
  dplyr::filter(site == "ValleGrande")

# hours between 24-32C - calculate that
mef_plotwise_summaries <-
  mef_clim |>
  plotwise_summary_std(stdf = mef_station_data)|>
  tibble::rownames_to_column("id")

vall_plotwise_summaries <-
  vall_clim |>
  plotwise_summary_std(stdf = val_std) |>
  tibble::rownames_to_column("id")

# ggplot(vall_plotwise_summaries, aes(x=tdelta, y=stdelta, label = rownames(vall_plotwise_summaries))) + geom_text()

## topoterra ==== from THoecker
prism_files <- list.files("data/prism", pattern = "bil$", full.names=TRUE)

p_tmax <- terra::rast(prism_files[str_detect(prism_files, 'tmax')])
p_tmin <- terra::rast(prism_files[str_detect(prism_files, 'tmin')])
p_DTR <- p_tmax - p_tmin
p_ppt <- terra::rast(prism_files[str_detect(prism_files, 'ppt')])
p_tmean <- terra::rast(prism_files[str_detect(prism_files, 'tmean')])
p_vmax <- terra::rast(prism_files[str_detect(prism_files, 'vpdmax')])
p_vmin <- terra::rast(prism_files[str_detect(prism_files, 'vpdmin')])

c(p_tmax, p_tmin, p_DTR, p_ppt, p_tmean, p_vmax, p_vmin) -> prisms
names(prisms) <- c("p_tmax", "p_tmin", "p_DTR", 'p_ppt', 'p_tmean', 'p_vmax', 'p_vmin')

terra::rast("data/big/topoterra_hist_1961-1990.tif") -> xx

locations_xx <- locations |>
  st_transform(crs = st_crs(xx)) %>%
  mutate(terra::extract(xx, .))%>%
  mutate(terra::extract(prisms, .))

tc_mef <- locations_xx |>
  st_set_geometry(NULL) |>
  dplyr::select(-ID, tc_aet = aet,
                tc_def = def,
                tc_tmx = tmax,
                tc_tmin = tmin,
                p_tmax, p_tmin, p_DTR, p_ppt, p_tmean, p_vmax, p_vmin) |>
  mutate(tc_dtr = tc_tmx -tc_tmin) |>
  filter(str_sub(id, 1,3) == "mef") |>
  filter(id %in% (mef_plotwise_summaries$id)) |>
  left_join(mef_topo) |>
  left_join(mef_plotwise_summaries) |>
  mutate(site = 'mef')

tc_val <- locations_xx |>
  st_set_geometry(NULL) |>
  dplyr::select(-ID, tc_aet = aet,
                tc_def = def,
                tc_tmx = tmax,
                tc_tmin = tmin,
                p_tmax, p_tmin, p_DTR, p_ppt, p_tmean, p_vmax, p_vmin) |>
  mutate(tc_dtr = tc_tmx -tc_tmin) |>
  filter(id %in% (vall_plotwise_summaries$id)) |>
  left_join(val_topo) |>
  left_join(vall_plotwise_summaries) |>
  mutate(site = "val")

d <- bind_rows(tc_val, tc_mef)
glimpse(d)

lm(tdelta ~ p_DTR, d) |> summary()
lm(tdelta ~ tc_dtr, d) |> summary()
lm(tdelta ~ vmin, d) |> summary()
lm(p_DTR ~ p_vmin, d) |> summary()
lm(tc_dtr ~ tc_def, d) |> summary()


# plotting

linedf <- data.frame(int = c(14, 15), site = c("val", "mef"))



d |>
  mutate(TopoTerra = tc_dtr/100,
         PRISM = p_DTR,
         threshold = case_when(site == "val" & tdelta > 14 ~ "over",
                               site == "val" & tdelta < 14 ~ "under",
                               site == "mef" & tdelta > 15 ~ "over",
                               site == "mef" & tdelta < 15 ~ "under")) |>
  pivot_longer(cols = c('PRISM', 'TopoTerra'), names_to = "source") |>
  ggplot() +
  geom_point(aes(y=tdelta, x = value, color = threshold)) +
  # geom_vline(data = linedf, aes(xintercept = int), lty = 2, color = 'grey') +
  # geom_hline(data = linedf, aes(yintercept = int), lty = 2, color = 'grey') +
  scale_color_manual(values =wesanderson::wes_palette("Royal1",2)) +
  facet_grid(source~site) +
  geomtextpath::geom_textabline(color = 'grey30', lty=2, hjust = .2,
                                label = '1:1 line') +
  # geom_abline() +
  scale_x_continuous(limits =c(7,21)) +
  scale_y_continuous(limits =c(7,21)) +
  theme_classic() +
  ylab("DTR (\u00B0C) from Sensors") +
  xlab("DTR (\u00B0C) from Modelled Climate Data") +
  theme(panel.background = element_rect(color = "black", fill=NA),
        legend.position = 'none')
ggsave(filename = "out/dtr_comparison.png", bg = "white", width=4.5, height=3.5)


lm(tdelta ~ vmin + site, d |>
     filter(id != 'mef16') ) |> summary()

d |>
  filter(id != 'mef16') |>
  ggplot(aes(x=vmin, y=tdelta, color = site)) +
  geom_point() +
  theme_classic() +
  ylab("DTR (\u00B0C)") +
  xlab('Daily Minimum VPD (hPa)') +
  geom_hline(data = linedf, aes(yintercept = int, color = site), lty=2)
ggsave(filename = 'out/vmin_dtr.png', width = 4.5, height=3, bg = "white")


# analyze elevation variation within a prism pixel

mef_dem <- terra::rast("data/mef_dem.tif")
mef_prism <- terra::crop(p_DTR, mef_dem)
mef_p_polygons <- terra::as.polygons(mef_prism, aggregate = F, value =T, cell=T)

as.data.frame(mef_p_polygons) |>
  tibble::rownames_to_column('ID')

mef_locations <- locations_xx |>
  filter(str_sub(id, 1,3) == "mef") %>%
  dplyr::select(id) %>%
  mutate(terra::extract(mef_prism, y=., cells=T, ID=F)) |>
  dplyr::rename(p_dtr = PRISM_tmax_30yr_normal_800mM5_annual_bil)

plot(mef_dem);plot(mef_p_polygons, add=T)

mef_var_df <- terra::extract(mef_dem, mef_p_polygons) |>
  dplyr::rename(elevation = 2)|>
  group_by(ID) |>
  summarise(elevation_sd = sd(elevation),
            elevation_min = min(elevation),
            elevation_max = max(elevation),
            elevation_mean = mean(elevation),
            n = n()) |>
  mutate(elevation_range = elevation_max - elevation_min,
         site = "mef") |>
  filter(n>8000) |>
  rename(cell = ID)

mef_locations <- locations_xx |>
  filter(str_sub(id, 1,3) == "mef") %>%
  dplyr::select(id) %>%
  mutate(terra::extract(mef_prism, y=., cells=T, ID=F)) |>
  dplyr::rename(p_dtr = PRISM_tmax_30yr_normal_800mM5_annual_bil)  |>
  left_join(mef_var_df) |>
  left_join(mef_plotwise_summaries) |>
  mutate(dtr_anomaly = abs(p_dtr - tdelta))

ggplot(mef_locations) +
  geom_point(aes(x=dtr_anomaly, y=elevation_sd))

ggplot(mef_var_df) +
  geom_point(aes(x=elevation_mean, y=elevation_range)) +
  scale_y_continuous(limits = c(0,NA))

val_dem <- terra::rast("data/vc_dem.tif")
val_prism <- terra::crop(p_DTR, val_dem)
val_p_polygons <- terra::as.polygons(val_prism, aggregate = F, value =T, cell=T)

as.data.frame(val_p_polygons) |>
  tibble::rownames_to_column('ID')

val_locations <- locations_xx |>
  filter(str_sub(id, 1,2) %in% c('rs', "vg")) %>%
  dplyr::select(id) %>%
  mutate(terra::extract(val_prism, y=., cells=T, ID=F)) |>
  dplyr::rename(p_dtr = PRISM_tmax_30yr_normal_800mM5_annual_bil)

plot(val_dem);plot(val_p_polygons, add=T)

val_var_df <- terra::extract(val_dem, val_p_polygons) |>
  dplyr::rename(elevation = 2)|>
  group_by(ID) |>
  summarise(elevation_sd = sd(elevation),
            elevation_min = min(elevation),
            elevation_max = max(elevation),
            elevation_mean = mean(elevation),
            n = n()) |>
  mutate(elevation_range = elevation_max - elevation_min,
         site = "val") |>
  filter(n>8000) |>
  rename(cell = ID)

val_locations <- locations_xx |>
  filter(str_sub(id, 1,2) %in% c('rs', "vg")) %>%
  dplyr::select(id) %>%
  mutate(terra::extract(val_prism, y=., cells=T, ID=F)) |>
  dplyr::rename(p_dtr = PRISM_tmax_30yr_normal_800mM5_annual_bil)  |>
  left_join(val_var_df) |>
  left_join(vall_plotwise_summaries) |>
  mutate(dtr_anomaly = abs(p_dtr - tdelta))

ggplot(val_locations) +
  geom_point(aes(x=dtr_anomaly, y=elevation_sd))

ggplot(bind_rows(val_locations, mef_locations) |>
         mutate(
           threshold = case_when(site == "val" & tdelta >= 14 ~ "over",
                                 site == "val" & tdelta < 14 ~ "under",
                                 site == "mef" & tdelta >= 15 ~ "over",
                                 site == "mef" & tdelta < 15 ~ "under")) |> na.omit(),
       aes(x=elevation_sd, y=dtr_anomaly, color = threshold)) +
  geom_point() +
  theme_classic() +
  scale_color_manual(values = wesanderson::wes_palette("Royal1")) +
  facet_wrap(~site) +
  theme(panel.background = element_rect(color = 'black'))

ggsave('out/dtr_anomaly_elv.png', width=7, height =3.5, bg='white')
