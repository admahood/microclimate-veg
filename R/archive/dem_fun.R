# TWI maps
library(sf);library(terra); library(tidyverse)
library(topomicro)
# manitou ef ===================================================
mef_dem <- terra::rast("data/dem/USGS_13_n40w106_20230602.tif")
mef_locations <- read_csv("data/sensor_locations.csv") |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  dplyr::select(-starts_with("alpha")) |>
  st_transform(crs=st_crs(mef_dem)) |>
  filter(str_sub(id, 1,3) == "mef")

mef_ext <- ext(locations |> st_buffer(dist = 2000))
mdc <- mef_dem |> crop(mef_ext)

mdc |>
  as.data.frame(xy=T) |>
  dplyr::rename(elv_m = 3) |>
  ggplot() +
  geom_raster(aes(x=x,y=y,fill=elv_m)) +
  geom_sf_text(data = locations, aes(label = id))


mef_twi <- mdc |>
  topomicro::get_twi(resolution = 10)

plot(mef_twi)
mef_twi |>
  as.data.frame(xy=T) |>
  ggplot() +
  geom_raster(aes(x=x,y=y,fill=twi)) +
  geom_sf(data = mef_locations, color = "red") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_viridis_c(direction = -1) +
  theme(axis.title = element_blank())
ggsave("out/mef_twi.png", bg="white")

terra::writeRaster(mef_twi$twi, filename = "data/mef_twi.tif")
terra::writeRaster(mdc, filename = "data/mef_dem.tif")

# valles caldera =====================================================

vc_dem <- terra::rast("data/dem/USGS_13_n36w107_20231208.tif")
locations <- read_csv("data/sensor_locations.csv") |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  dplyr::select(-starts_with("alpha")) |>
  st_transform(crs=st_crs(vc_dem)) |>
  filter(str_sub(id, 1,2) %in% c("rs", "vg"))

vc_ext <- ext(locations |> st_buffer(dist = 2500))
vcc <- vc_dem |> crop(vc_ext)

vcc |>
  as.data.frame(xy=T) |>
  dplyr::rename(elv_m = 3) |>
  ggplot() +
  geom_raster(aes(x=x,y=y,fill=elv_m)) +
  geom_sf_text(data = locations, aes(label = id))


vc_twi <- vcc |>
  topomicro::get_twi(resolution = 10) # add warning if no resolution is specified

plot(vc_twi)
vc_twi |>
  as.data.frame(xy=T) |>
  ggplot() +
  geom_raster(aes(x=x,y=y,fill=twi)) +
  geom_sf(data = locations, color = "red") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_viridis_c(direction = -1) +
  theme(axis.title = element_blank())
ggsave("out/vall_twi.png", bg="white")

terra::writeRaster(vc_twi$twi, filename = "data/vc_twi.tif", overwrite=T)
terra::writeRaster(vcc, filename = "data/vc_dem.tif", overwrite=T)

# analyze twi vs microclimate data =============================================

# summarising climate data then relating to community composition
library(tidyverse)
library(topomicro)
library(vegan)
library(terra)
library(ggpubr)
library(sf)
library(ggrepel)

locations <- read_csv("data/sensor_locations.csv") |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  dplyr::select(-starts_with("alpha"))


mef_l <- locations |>
  filter(str_sub(id,1,2) == "me") %>%
  mutate(terra::extract(rast("data/mef_twi.tif"),.))

vc_l <- locations |>
  filter(str_sub(id,1,2) %in% c("vg", "rs")) %>%
  mutate(terra::extract(rast("data/vc_twi.tif"),.),
         id = str_replace_all(id,"g0", "g"))

mef_clim <- read_csv("data/cleaned_bm/mef_jul23.csv"); glimpse(mef_clim)
vall_clim <- read_csv("data/cleaned_bm/vall_dec22.csv"); glimpse(vall_clim)

mef_plotwise_summaries <-
  mef_clim |>
  plotwise_summary() |>
  tibble::rownames_to_column("id") |>
  left_join(mef_l) |>
  mutate(site = "mef")

vall_plotwise_summaries <-
  vall_clim |>
  plotwise_summary()|>
  tibble::rownames_to_column("id") |>
  left_join(vc_l) |>
  mutate(site = "vc")

pw_all <- bind_rows(vall_plotwise_summaries, mef_plotwise_summaries)

ggplot(pw_all, aes(x=twi, y=vmin, color = site)) +
  geom_point()+
  geom_smooth(method = "lm")

ggplot(pw_all, aes(x=twi, y=tdelta, color = site)) +
  geom_point() +
  geom_smooth(method = "lm")


lm(vmin ~ twi+site, pw_all) |> summary()
lm(tdelta ~ twi + site, pw_all) |> summary()
lm(tdelta~ twi * vmin + site, pw_all) |> summary()
