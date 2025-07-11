# map figure for microclimate-veg paper
library(tidyverse)
library(sf)
library(terra)
library(topomicro)
library(ggnewscale)
library(ggpubr)
library(ggthemes)
library(geomtext)
library(ggspatial)

# make a 4 panel map: 1. TWI, 2. DEM/hillshade, 3. Worldclim DTR, 4. Modelled DTR
# make sure we're using USGS 3dep 10meter DEM, correct TWI

unbroken <- c('mef1', 'mef10', 'mef11', 'mef12', 'mef14', 'mef16', 'mef17',
              'mef18', 'mef19', 'mef2', 'mef20', 'mef21', 'mef22', 'mef23',
              'mef24', 'mef25', 'mef27', 'mef3', 'mef4', 'mef5', 'mef6', 'mef7',
              'mef8', 'mef9', 'rs1', 'rs2', 'rs3', 'rs4', 'rs5', 'vg1', 'vg10',
              'vg11', 'vg12', 'vg13', 'vg14', 'vg15', 'vg16', 'vg2', 'vg3', 'vg4',
              'vg5', 'vg6', 'vg7', 'vg8', 'vg9')

prism <- terra::rast("data/prism/PRISM_tmin_30yr_normal_800mM5_annual_bil.bil")

dems <- terra::rast('data/mef_dem.tif') |>
  list(terra::rast('data/vc_dem.tif'))

vall_stations <- data.frame(name = c("Redondo\nSaddle\nWx", "Valle Grande Wx", "Hidden Valley Wx"),
                            x = c(-106.55361111111111, -106.521, -106.50055555555555),
                            y = c(35.88388888888889, 35.858333333333334,35.840833333333336)) |>
  st_as_sf(coords = c(x='x',y='y'), crs = 4326) |>
  st_transform(crs = st_crs(dems[[2]]))

mef_station <- data.frame(name = c("Wx", "Mt. Deception", "Hotel Gulch"),
                          y= c(39.100509242733764,39.100509242733764, 39.09),
                          x=-c(105.09422836459245, 105.05999, 105.05999))|>
  st_as_sf(coords = c(x='x',y='y'), crs = 4326)

locations <- read_csv("data/sensor_locations.csv") |>
  mutate(id = str_replace_all(id, 'vg0', 'vg'))

locations_sf <- locations |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  dplyr::select(-starts_with("alpha")) |>
  filter(str_sub(id, 1,2) %in% c('vg', 'me', 'rs'),
         id %in% unbroken)

locations_val <- locations_sf |>
  filter(str_sub(id, 1,2) %in% c('vg', 'rs')) |>
  st_transform(crs = st_crs(dems[[2]]))

wc <- prism |> #terra::rast('data/big/wc2.1_30s_bio/wc2.1_30s_bio_2.tif') |>
  terra::crop(terra::ext(locations_sf |> st_buffer(dist = 100000)))
# plot(wc); plot(locations_sf, add=T, col='black')
# plot(dems[[1]])

mef_df <- dems[[1]] |> as.data.frame(xy=TRUE)

mef_hill <- terra::shade(slope = terra::terrain(dems[[1]], v="slope", unit = "radians"),
             aspect = terra::terrain(dems[[1]], v="aspect", unit = "radians")) |>
  as.data.frame(xy=TRUE)

val_hill <- terra::shade(slope = terra::terrain(dems[[2]], v="slope", unit = "radians"),
                         aspect = terra::terrain(dems[[2]], v="aspect", unit = "radians")) |>
  as.data.frame(xy=TRUE)

prism_ll <- prism |> as.data.frame(xy=TRUE) |>
  dplyr::rename(tmin = 3) |>
  filter(x < -105, x > -106, y >39, y<40)
wc_ll <- wc |> as.data.frame(xy=TRUE) |>
  dplyr::rename(tmin = 3) |>
  filter(x < -105, x > -106, y >39, y<40)

prism_vall <- prism |> as.data.frame(xy=TRUE) |>
  dplyr::rename(tmin = 3) |>
  filter(x < -106.42, x > -106.58, y >35.82, y<35.90)

mef_dem <- dems[[1]] |> as.data.frame(xy=T) |> dplyr::rename(`Elevation (m)` =3)


pm <- ggplot() +
  geom_raster(data = mef_dem, aes(x=x, y=y, fill = `Elevation (m)`)) +
  scale_fill_binned(type = 'viridis') +
  ggnewscale::new_scale_fill() +
  geom_raster(data = mef_hill, aes(x=x, y=y, fill = hillshade), alpha = 0.5, show.legend =F) +
  scale_fill_gradient(low = 'black', high = "grey") +
  geom_tile(data = prism_ll, aes(x=x,y=y), color = "black", fill = "transparent") +
  geom_sf(data = filter(locations_sf, str_sub(id, 1,3)=="mef"),
          color = 'black', stroke =1,
          shape = 1) +
  geom_sf_text(data = mef_station, aes(label = name ), nudge_y = .001) +
  geom_sf(data = mef_station[1,], shape=15) +
  geom_sf_text(data = locations_sf |>
                 filter(id %in% c("mef25", 'mef9')), aes(label = id),nudge_x = .003) +
  ggspatial::annotation_scale(pad_x = unit(14.5, 'cm'),
                              pad_y = unit(10, 'cm')) +
  coord_sf(xlim = c(-105.105, -105.015),
           ylim = c(39.07, 39.11),
           expand = F) +
  theme_clean() +
  ggtitle("a. Manitou Experimental Forest") +
  theme(axis.title = element_blank(),
        legend.position = c(0,0),
        legend.justification = c(0,0));pm



#
#
# prism_proj <- prism |> as.data.frame(xy=TRUE) |>
#   dplyr::rename(tmin = 3) |>
#   filter(x < -106.42, x > -106.57, y >35.83, y<35.90)|>
#   st_as_sf(coords =c("x", "y"), crs = 4326)|>
#   st_transform(crs = st_crs(dems[[2]]))
#
# prism_vall <- st_coordinates(prism_proj) |>
#   as.data.frame() |>
#   janitor::clean_names()

vc_dem <- dems[[2]] |> as.data.frame(xy=T) |> dplyr::rename(`Elevation (m)` =3)

pv <- ggplot() +
  geom_raster(data = vc_dem, aes(x=x, y=y, fill = `Elevation (m)`)) +
  scale_fill_binned(type = 'viridis') +
  ggnewscale::new_scale_fill() +
  geom_raster(data = val_hill, aes(x=x, y=y, fill = hillshade), alpha = 0.5, show.legend =F) +
  scale_fill_gradient(low = 'black', high = "grey") +
  geom_tile(data = prism_vall, aes(x=x,y=y), color = "black", fill = "transparent") +
  geom_sf(data = locations_val, shape =1, stroke = 1) +
  geom_sf_text(data = vall_stations, aes(label = name), nudge_y = 0.0017) +
  geom_sf(data= vall_stations, shape = 15) +
  geom_sf_text(data = locations_val |>
                 filter(id %in% c("vg14", 'vg15')), aes(label = id), nudge_x = 0.003) +
  coord_sf(xlim = c(-106.565, -106.45),
           ylim = c(35.83, 35.89),
           expand = F)+
  ggtitle("b. Valles Caldera National Preserve") +
  ggspatial::annotation_scale(pad_x = unit(3.5, 'cm'),
                              pad_y = unit(1, 'cm')) +
  theme_clean() +
  theme(axis.title = element_blank(),
        legend.position = c(0,0), legend.justification = c(0,0))#;pv

# ggarrange(pm, pv, ncol=1) |>
#   ggsave(filename = 'out/figure_1_map_v.png', width = 6.5, height = 8.5, bg="white")
#
# ggarrange(pm, pv, ncol=2, widths = c(1.07, 1)) |>
#   ggsave(filename = 'out/figure_1_map_h.png', width = 13.5, height = 4.5, bg="white")

inset_val <- read_csv("data/cleaned_bm/vall_dec22.csv") |>
  filter(id %in% c('vg14', 'vg15'),
         dt > as.Date('2022-11-15'),
         dt < as.Date('2022-11-30')
         ) |>
  ggplot(aes(x=dt, y=temperature_c, color = id)) +
  geom_line() +
  ylab("T (\u00B0C)") +
  scale_color_manual(values = c('black', 'red')) +
  theme_clean() +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank());inset_val

inset_mef <- read_csv("data/cleaned_bm/mef_febmay24.csv") |>
  filter(id %in% c('mef25', 'mef9'),
         dt > as.Date('2023-09-15'),
         dt < as.Date('2023-09-30')) |>
  ggplot(aes(x=dt, y=temperature_c, color = id)) +
  geom_line() +
  ylab("T (\u00B0C)") +
  scale_color_manual(values = c('red', 'black')) +
  theme_clean() +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank());inset_mef


pi <- cowplot::ggdraw(ggarrange(pm, pv, ncol=1, heights = c(1,1.14))) +
  cowplot::draw_plot(inset_mef, .38, .57, .6, .13) +
  cowplot::draw_plot(inset_val, .38, .36, .6, .13)

ggsave(plot = pi, filename = 'out/map_w_inset.png', width = 8.5, height = 10.8, bg="white")
