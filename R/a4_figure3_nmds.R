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

val_topo <- read_csv('data/val_topo.csv') |>
  mutate(elv_rel = elevation - min(elevation))
mef_topo <- read_csv('data/mef_topo.csv') |>
  mutate(elv_rel = elevation - min(elevation))

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
  plotwise_summary_std(stdf = mef_station_data); dim(mef_plotwise_summaries)

vall_plotwise_summaries <-
  vall_clim |>
  plotwise_summary_std(stdf = val_std)

vmps <- mef_plotwise_summaries |>
  as_tibble(rownames = 'plot') |>
  dplyr::select(1:9) |>
  mutate(basin = "mef") |>
  bind_rows(vall_plotwise_summaries |>
              as_tibble(rownames = 'plot') |>
              dplyr::select(1:9) |>
              mutate(basin = 'vc'))

ggplot(vmps) +
  geom_density(aes(x=tdelta, fill = basin), alpha =0.5)
vmps |>
  group_by(basin) |>
  summarise(mean = mean(tdelta),
            max = max(tdelta),
            min = min(tdelta))



# ggplot(vall_plotwise_summaries, aes(x=tdelta, y=stdelta, label = rownames(vall_plotwise_summaries))) + geom_text()

# both_plotwise <- bind_rows(mef_plotwise_summaries, vall_plotwise_summaries)

vall_veg <- readr::read_csv("data/valles_caldera/microclimate - VALL_veg.csv") |>
  dplyr::select(plot, species) |>
  dplyr::mutate(occurrence = 1,
                plot = str_to_lower(plot) %>% str_remove_all("_"),
                species = str_replace_all(species, " ", "_")|>
                  str_replace_all("-", "_")) |>
  tidyr::pivot_wider(names_from = species, values_from = occurrence,
                     values_fn = first, values_fill = 0) |>
  mutate(plot = ifelse(str_sub(plot, 3,3) == "0", str_remove_all(plot, "0"), plot)) |>
  filter(plot %in% rownames(vall_plotwise_summaries)) |>
  arrange(plot) %>%
  tibble::column_to_rownames("plot");vall_veg

mef_veg <- readr::read_csv("data/mef/microclimate - MEF_veg.csv")|>
  dplyr::select(plot, actual_species) |>
  dplyr::mutate(occurrence = 1,
                plot = str_to_lower(plot) %>% str_remove_all("_"),
                actual_species = str_replace_all(actual_species, " ", "_")|>
                  str_replace_all("-", "_")) |>
  tidyr::pivot_wider(names_from = actual_species, values_from = occurrence,
                     values_fn = first, values_fill = 0) |>
  filter(plot %in% rownames(mef_plotwise_summaries)) |>
  arrange(plot) |>
  tibble::column_to_rownames("plot");mef_veg

## topoterra ==== from THoecker

wc_vars <- c('wc2.1_30s_bio_1' = 'Annual Mean Temperature',

'wc2.1_30s_bio_2' = 'Mean Diurnal Range (Mean of monthly (max temp - min temp))',

'wc2.1_30s_bio_3' = 'Isothermality (BIO2/BIO7) (×100)',

'wc2.1_30s_bio_4' = 'Temperature Seasonality (standard deviation ×100)',

'wc2.1_30s_bio_5' = 'Max Temperature of Warmest Month',

'wc2.1_30s_bio_6' = 'Min Temperature of Coldest Month',

'wc2.1_30s_bio_7' = 'Temperature Annual Range (BIO5-BIO6)',

'wc2.1_30s_bio_8' = 'Mean Temperature of Wettest Quarter',

'wc2.1_30s_bio_9' = 'Mean Temperature of Driest Quarter',

'wc2.1_30s_bio_10' = 'Mean Temperature of Warmest Quarter',

'wc2.1_30s_bio_11' = 'Mean Temperature of Coldest Quarter',

'wc2.1_30s_bio_12' = 'Annual Precipitation',

'wc2.1_30s_bio_13' = 'Precipitation of Wettest Month',

'wc2.1_30s_bio_14' = 'Precipitation of Driest Month',

'wc2.1_30s_bio_15' = 'Precipitation Seasonality (Coefficient of Variation)',

'wc2.1_30s_bio_16' = 'Precipitation of Wettest Quarter',

'wc2.1_30s_bio_17' = 'Precipitation of Driest Quarter',

'wc2.1_30s_bio_18' = 'Precipitation of Warmest Quarter',

'wc2.1_30s_bio_19' = 'Precipitation of Coldest Quarter')

wc <- terra::rast(list.files("data/big/wc2.1_30s_bio", full.names = T, pattern = "tif$"))


prism_files <- list.files("data/prism", pattern = "bil$", full.names=TRUE)

p_tmax <- terra::rast(prism_files[str_detect(prism_files, 'tmax')])
p_tmin <- terra::rast(prism_files[str_detect(prism_files, 'tmin')])
p_ATR <- p_tmax - p_tmin
p_ppt <- terra::rast(prism_files[str_detect(prism_files, 'ppt')])
p_tmean <- terra::rast(prism_files[str_detect(prism_files, 'tmean')])
p_vmax <- terra::rast(prism_files[str_detect(prism_files, 'vpdmax')])
p_vmin <- terra::rast(prism_files[str_detect(prism_files, 'vpdmin')])

c(p_tmax, p_tmin, p_ATR, p_ppt, p_tmean, p_vmax, p_vmin) -> prisms
names(prisms) <- c("p_tmax", "p_tmin", "p_ATR", 'p_ppt', 'p_tmean', 'p_vmax', 'p_vmin')

terra::rast("data/big/topoterra_hist_1961-2022.tif") -> xx

locations_xx <- locations |>
  st_transform(crs = st_crs(xx)) %>%
  mutate(terra::extract(xx, .))%>%
  mutate(terra::extract(prisms, .)) %>%
  mutate(terra::extract(wc, .)) |>
  mutate(id = str_replace_all(id, 'vg0', 'vg'))

mef_preds <- terra::rast('data/mef_dtr.tif')

tc_mef <- locations_xx %>%
  mutate(terra::extract(mef_preds, ., ID=F))|>
  st_set_geometry(NULL) |>
  dplyr::select(-ID, tc_aet = aet,
                tc_def = def,
                prediction,
                tc_tmx = tmax,
                tc_tmin = tmin,
                starts_with('wc2'),
                p_tmax, p_tmin, p_ATR, p_ppt, p_tmean, p_vmax, p_vmin) |>
  mutate(tc_ATR = tc_tmx -tc_tmin) |>
  filter(str_sub(id, 1,3) == "mef") %>%
  filter(id %in% rownames(mef_plotwise_summaries)) |>
  left_join(mef_topo) |>
  arrange(id) |>
  tibble::column_to_rownames("id")

val_preds <- terra::rast('data/val_dtr.tif')


tc_val <- locations_xx %>%
  mutate(terra::extract(val_preds, ., ID=F)) |>
  st_set_geometry(NULL) |>
  dplyr::select(-ID, tc_aet = aet,
                tc_def = def,
                prediction,
                starts_with('wc2'),
                tc_tmx = tmax,
                tc_tmin = tmin,
                p_tmax, p_tmin, p_ATR, p_ppt, p_tmean, p_vmax, p_vmin) |>
  mutate(tc_ATR = tc_tmx -tc_tmin) |>
  filter(id %in% rownames(vall_plotwise_summaries)) |>
  left_join(val_topo) |>
  arrange(id) |>
  tibble::column_to_rownames("id")

# mef_plotwise_summaries |> cbind(tc_mef) |> lm(tdelta ~ twi + elevation, data = _) |> summary()
# vall_plotwise_summaries |> cbind(tc_val) |> lm(tdelta ~ twi + elevation, data = _) |> summary()

# tc_both <- locations_xx |>
#   st_set_geometry(NULL) |>
#   dplyr::select(-ID, tc_aet = aet, tc_def = def, tc_tmx= tmax, tc_tmin = tmin) |>
#   filter(str_sub(id, 1,3) == "mef") |>
#   filter(id %in% rownames(both_plotwise)) |>
#   arrange(id) |>
#   tibble::column_to_rownames("id")

# mef nmds ====
vegan::metaMDS(mef_veg, trymax = 999) -> nmds

sites <- nmds$points |>
  as.data.frame() |>
  tibble::rownames_to_column("plot") |>
  left_join(mef_plotwise_summaries |> tibble::rownames_to_column("plot"))

# spp <- envfit(nmds, mef_veg, 9999)
envm <- envfit(nmds, mef_plotwise_summaries |> cbind(tc_mef), 9999)

# vall nmds ====

vegan::metaMDS(vall_veg, trymax = 999) -> nmdsv

sitesv <- nmdsv$points |>
  as.data.frame() |>
  tibble::rownames_to_column("plot") |>
  left_join(vall_plotwise_summaries |> tibble::rownames_to_column("plot"))

# sppv <- envfit(nmdsv, vall_veg, 9999)
env <- envfit(nmdsv, vall_plotwise_summaries |> cbind(tc_val), 9999)
envss <- envfit(nmdsv, vall_veg, permutations = 9999)
envssm <- envfit(nmds, mef_veg, permutations = 9999)

bind_rows(tidy_envfit(env) |> mutate(site = "vall"),
          tidy_envfit(envm) |> mutate(site = "mef")) |>
  dplyr::select(var, r2, p, site) |>
  dplyr::mutate(r2 = round(r2, 2),
                p = round(p, 3)) |>
  dplyr::filter(str_sub(var,1,1) != "r", str_sub(var, 1,2) != "sr") |>
  mutate(star = case_when(p <= 0.05 & p > 0.01 ~ "*",
                          p <= 0.01 & p > 0.001 ~ "**",
                          p <= 0.001 ~ "***",
                          p >= 0.05 ~ ""
  )) %>%
  arrange(site, desc(r2)) %>%
  pivot_wider(names_from = site, values_from = c(r2, p, star)) |>
  print(n=33)

env_a <-as.data.frame(env$vectors$arrows*sqrt(env$vectors$r)) %>%
  tibble::rownames_to_column("species")  %>%
  mutate(p =env$vectors$pvals, site = "Valles Caldera")# %>%
 # filter(p <= 0.001)

env_b <-as.data.frame(envm$vectors$arrows*sqrt(envm$vectors$r)) %>%
  tibble::rownames_to_column("species")  %>%
  mutate(p =envm$vectors$pvals, site = "Manitou") %>%
  filter(p <= 0.001) |>
  bind_rows(env_a) |>
  # filter(str_sub(species,1,1) != "r", str_sub(species,1,1) != "s")
  filter(species %in% c('vmin', 'tdelta', 'twi'))

ptd <- sitesv |>
  mutate(site = "Valles Caldera",
         `Diurnal\nTemperature\nRrange` = ifelse(tdelta >= 14, "CAD Recipient", "CAD Donor")) |>
  bind_rows(sites |> mutate(site = "Manitou") |>
              mutate(`Diurnal\nTemperature\nRrange` = ifelse(tdelta >= 15, "CAD Recipient", "CAD Donor"))) |>
  ggplot() +
  geom_segment(data = env_b,x=0,y=0, color = "grey",arrow = arrow(),
               aes(yend = NMDS2, xend = NMDS1), lwd=1)+
  geom_text_repel(data = env_b,size=4, aes(label = species, x=NMDS1, y=NMDS2), color = "grey40") +
  geom_text(aes(x=MDS1, y=MDS2,label = plot, color = `Diurnal\nTemperature\nRrange`), fontface='bold') +
  # scale_color_viridis_c() +
  facet_wrap(~site) +
  coord_equal() +
  scale_color_manual(values =rep(wesanderson::wes_palette("Royal1",2),2))+
  theme_clean() +
  theme(panel.background = element_rect(color='black'),
        panel.grid.major.y = element_blank(),
        # legend.position = c(1,1),
        # legend.justification = c(1,1)
        ); ptd

ptd |>
  ggsave(plot = _, filename = "out/nmds_DTR.png", bg="white", width=12, height=5)


env

# nmds on both sites together ==================================================

both_plotwise <- bind_rows(mef_plotwise_summaries, vall_plotwise_summaries)
tc_both <- bind_rows(tc_mef, tc_val)


both_veg <- readr::read_csv("data/valles_caldera/microclimate - VALL_veg.csv") |>
  dplyr::select(plot, species) |>
  bind_rows(readr::read_csv("data/mef/microclimate - MEF_veg.csv")|>
              dplyr::select(plot, species = actual_species))|>
  dplyr::mutate(occurrence = 1,
                plot = str_to_lower(plot) %>% str_remove_all("_"),
                species = str_replace_all(species, " ", "_")|>
                  str_replace_all("-", "_")) |>
  tidyr::pivot_wider(names_from = species, values_from = occurrence,
                     values_fn = first, values_fill = 0) |>
  mutate(plot = ifelse(str_sub(plot, 3,3) == "0", str_remove_all(plot, "0"), plot)) |>
  filter(plot %in% rownames(both_plotwise)) |>
  arrange(plot) %>%
  tibble::column_to_rownames("plot")
rownames(both_veg) == rownames(both_plotwise)

vegan::metaMDS(both_veg, trymax = 999) -> nmdsb

sitesb <- nmdsb$points |>
  as.data.frame() |>
  tibble::rownames_to_column("plot") |>
  left_join(both_plotwise |> tibble::rownames_to_column("plot"))

# sppb <- envfit(nmdsb, both_veg, 9999)
envb <- envfit(ord = nmdsb,
               env = both_plotwise |> cbind(tc_both),
               permutations = 9999#,
               #strata = rownames(both_plotwise) |> str_sub(1,1) |> str_replace_all("r", "v")
               )

envb_b <-as.data.frame(envb$vectors$arrows*sqrt(envb$vectors$r)) %>%
  tibble::rownames_to_column("species")  %>%
  mutate(p =envb$vectors$pvals, site = "Both") %>%
  # filter(p <= 0.001) |>
  filter(str_sub(species,1,1) != "r", str_sub(species,1,1) != "s")

ptb <- sitesb |>
  mutate(`Diurnal\nTemperature\nRrange` = case_when(tdelta >= 14 & str_sub(plot, 1,1) == "m"~"CAD Recipient",
                                                    tdelta < 14 & str_sub(plot, 1,1) == "m"~"CAD Donor",
                                                    tdelta >= 15 & str_sub(plot, 1,1) != "m"~"CAD Recipient",
                                                    tdelta < 15 & str_sub(plot, 1,1) != "m"~"CAD Donor")) |>
   ggplot() +
  geom_segment(data = envb_b,x=0,y=0, color = "grey",arrow = arrow(),
               aes(yend = NMDS2, xend = NMDS1), lwd=1)+
  geom_text_repel(data = envb_b,size=4, aes(label = species, x=NMDS1, y=NMDS2), color = "grey40") +
  geom_text(aes(x=MDS1, y=MDS2,label = plot, color = `Diurnal\nTemperature\nRrange`), fontface='bold') +
  # scale_color_viridis_c() +
  coord_equal() +
  scale_color_manual(values =rep(wesanderson::wes_palette("Royal1",2),2))+
  theme_clean() +
  theme(panel.background = element_rect(color='black'),
        panel.grid.major.y = element_blank(),
        # legend.position = c(1,1),
        # legend.justification = c(1,1)
  ); ptb
# table nmds

ef_df <- bind_rows(tidy_envfit(env) |> mutate(site = "vall"),
                   tidy_envfit(envm) |> mutate(site = "mef"),
                   tidy_envfit(envb) |> mutate(site = 'both')) |>
  dplyr::select(var, r2, p, site) |>
  dplyr::mutate(r2 = round(r2, 2),
                p = round(p, 3)) |>
  dplyr::filter(str_sub(var,1,1) != "r", str_sub(var, 1,1) != "s") |>
  mutate(star = case_when(p >= 0.05 & p < 0.1 ~ '.',
                          p < 0.05 & p >= 0.01 ~ "*",
                          p < 0.01 & p >= 0.001 ~ "**",
                          p <= 0.001 ~ "***",
                          p >= 0.1 ~ ""
  )) |>
  mutate(source = ifelse(str_sub(var,1,1)=="p", "prism", "micro"),
         source = ifelse(str_sub(var,1,3) == "wc2", "worlclim", source),
         source = ifelse(str_sub(var,1,3)=="tc_", "topoclim", source),
         source = ifelse(var %in% c('slope','aspect','twi', 'elevation', 'fa', 'fa_x_slope', 'hli', 'elv_rel'),
                         "topography", source)) %>%
  arrange(desc(site), source, desc(r2)) %>%
  pivot_wider(names_from = site, values_from = c(r2, p, star))

summary(ef_df)
ef_df  |>
  pivot_longer(cols = c('r2_mef', "r2_vall", "r2_both")) |>
  ggplot(aes(x=value)) +
  geom_histogram() +
  facet_grid(source~name)

ef_df |>
  dplyr::select(-starts_with("p")) |>
  dplyr::transmute(var=var, source=source,
                   r2_vall = str_c(r2_vall, ' ', star_vall),
                   r2_mef = str_c(r2_mef, ' ', star_mef),
                   r2_both = str_c(r2_both, ' ', star_both)) |> print(n=99)
  # write_csv("out/initial_ndms.csv")

# plot with all three ==========================================================

env_bbb <- bind_rows(env_b, envb_b) |>
    filter(species %in% c('tdelta', 'vmin', 'twi', 'tc_aet', 'tc_def')) |>
    filter(p < 0.05) |>
    dplyr::mutate(species = case_when(
      species == 'vmin' ~ "VPDmin",
      species == 'tdelta' ~ "DTR",
      species == 'twi' ~ "TWI",
      species == 'tc_aet' ~ "AET",
      species == 'tc_def' ~ "CWD",
    ))

env_labels <- env_bbb |>
  mutate(NMDS1 = ifelse(species == "VPDmin" & site == "Both", -1.1, NMDS1),
         NMDS1 = ifelse(species == "TWI" & site == "Both", .9, NMDS1),
         NMDS1 = ifelse(species == "TWI" & site == "Manitou", -1, NMDS1),
         NMDS1 = ifelse(species == "VPDmin" & site == "Manitou", 1, NMDS1),
         NMDS2 = ifelse(species == "VPDmin" & site == "Manitou", -.5, NMDS2),
         NMDS2 = ifelse(species == "AET" & site == "Both", 1, NMDS2),
         NMDS1 = ifelse(species == "AET" & site == "Both", .5, NMDS1),
         NMDS2 = ifelse(species == "CWD" & site == "Both", -.9, NMDS2),
         NMDS2 = ifelse(species == "DTR" & site == "Manitou", 1, NMDS2),
         NMDS2 = ifelse(species == "VPDmin" & site == "Valles Caldera", 1, NMDS2),
         NMDS2 = ifelse(species == "DTR" & site == "Valles Caldera", -1, NMDS2),
         NMDS2 = ifelse(species == "TWI" & site == "Valles Caldera", -.7, NMDS2),
         NMDS1 = ifelse(species == "TWI" & site == "Valles Caldera", -.6, NMDS1),
         NMDS2 = ifelse(species == "VPDmin" & site == "Both", .4, NMDS2))

spp_v <- tidy_envfit(envss) |> filter(p < 0.05 & r2 > 0.4) |> mutate(site = "Valles Caldera")
spp_m <- tidy_envfit(envssm) |> filter(p < 0.05 & r2 > 0.4) |> mutate(site = "Manitou")
sppp <- bind_rows(spp_v, spp_m) |>
  dplyr::rename(species = var) |>
  mutate(species = str_replace_all(species, "_", " ") |>
           str_remove_all("sp1"))


ptspp <- sitesv |>
  mutate(site = "Valles Caldera",
         `Diurnal\nTemperature\nRrange` = ifelse(tdelta >= 14, "CAD Recipient", "CAD Donor")) |>
  bind_rows(sites |> mutate(site = "Manitou") |>
              mutate(`Diurnal\nTemperature\nRrange` = ifelse(tdelta >= 15, "CAD Recipient", "CAD Donor"))) |>
  mutate(StudyArea = ifelse(str_sub(plot, 1,1) == 'm', "Manitou", "Valles Caldera")) |>
  ggplot() +
  geom_point(aes(x=MDS1, y=MDS2, color = `Diurnal\nTemperature\nRrange`, shape = StudyArea), size=3) +
  geom_segment(data = sppp, x=0,y=0, color = "grey",arrow = arrow(),
               aes(yend = NMDS2, xend = NMDS1), lwd=1)+
  geom_text_repel(data = sppp, size=4, aes(label = species, x=NMDS1, y=NMDS2), color = "black", fontface = "italic") +
  # scale_color_viridis_c() +
  facet_wrap(~site) +
  coord_equal() +
  scale_color_manual(values =rep(wesanderson::wes_palette("Royal1",2),2))+
  theme_clean() +
  theme(panel.background = element_rect(color='black'),
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = 'top'
        # legend.position = c(1,1),
        # legend.justification = c(1,1)
  ); ptspp
ggsave(plot = ptspp, filename = 'out/nmds_spp.png', width = 9, height =4.65, bg = "white")


ptbb <- sitesv |>
  mutate(site = "Valles Caldera",
         `Diurnal\nTemperature\nRrange` = ifelse(tdelta >= 14, "CAD Recipient", "CAD Donor")) |>
  bind_rows(sites |> mutate(site = "Manitou") |>
              mutate(`Diurnal\nTemperature\nRrange` = ifelse(tdelta >= 15, "CAD Recipient", "CAD Donor"))) |>
  bind_rows(sitesb |>
    mutate( site = 'Both',
      `Diurnal\nTemperature\nRrange` = case_when(tdelta >= 14 & str_sub(plot, 1,1) == "m"~"CAD Recipient",
                                                      tdelta < 14 & str_sub(plot, 1,1) == "m"~"CAD Donor",
                                                      tdelta >= 15 & str_sub(plot, 1,1) != "m"~"CAD Recipient",
                                                      tdelta < 15 & str_sub(plot, 1,1) != "m"~"CAD Donor")))|>
  mutate(StudyArea = ifelse(str_sub(plot, 1,1) == 'm', "Manitou", "Valles Caldera")) |>
  ggplot() +
  geom_point(aes(x=MDS1, y=MDS2,
                 color = `Diurnal\nTemperature\nRrange`,
                 shape = StudyArea),
             size=3, stroke = 1) +
  geom_segment(data = env_bbb,x=0,y=0, color = "grey",arrow = arrow(),
               aes(yend = NMDS2, xend = NMDS1), lwd=1)+
  geom_text(data = env_labels,size=4, aes(label = species, x=NMDS1, y=NMDS2), color = "black") +
  # scale_color_viridis_c() +
  facet_wrap(~site) +
  coord_equal() +
  scale_color_manual(values =rep(wesanderson::wes_palette("Royal1",2),2))+
  scale_shape_manual(values = c(1,2)) +
  theme_clean() +
  theme(panel.background = element_rect(color='black'),
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = 'top'
        # legend.position = c(1,1),
        # legend.justification = c(1,1)
  ); ptbb
ggsave(filename = 'out/nmds_3pan.png', width = 10, height=5, bg='white')


# biodiversity by tdelta ====
nsp <- vall_veg |> vegan::specnumber() |> as.data.frame() |> tibble::rownames_to_column("plot") |> dplyr::rename(nspp=2)
nspm <- mef_veg |> vegan::specnumber() |> as.data.frame() |> tibble::rownames_to_column("plot") |> dplyr::rename(nspp=2)


vall_div <- vall_plotwise_summaries |>
  tibble::rownames_to_column("plot") |>
  left_join(nsp) |>
  mutate(site = "val") |>
  mutate(dtr_threshold = ifelse(tdelta > 14, "CAD Recipient", "CAD Donor"))
mef_div <- mef_plotwise_summaries |>
  tibble::rownames_to_column("plot") |>
  left_join(nspm)|>
  mutate(site = "mef")|>
  mutate(dtr_threshold = ifelse(tdelta > 15, "CAD Recipient", "CAD Donor"))


both_div <- bind_rows(mef_div,
                      vall_div)

table(both_div$dtr_threshold); table(both_div$sdtr_threshold)
options(na.action = "na.fail")

library(brms)

m <- brm(nspp~dtr_threshold*site, data = both_div, family = "poisson")
performance::r2_bayes(m)
summary(m); performance::check_model(m); fixef(m)

ggeffects::ggpredict(m, terms = c('dtr_threshold'), back_transform = T) |>
  as.data.frame()

wesanderson::wes_palette("Royal1", 2) |> as.vector() -> cols
pd <- ggeffects::ggpredict(m, terms = c('dtr_threshold', 'site'), back_transform = T) |>
  as.data.frame() |>
  dplyr::rename(site = group) |>
  dplyr::mutate(site = ifelse(site == 'mef', "Manitou", "Valles Caldera")) |>
  ggplot() +
  geom_errorbar(aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, color = x), width=0.1) +
  geom_point(aes(x=x, y=predicted, color = x)) +
  facet_wrap(~ site) +
  ylab("Predicted Species Richness") +
  xlab("Diurnal Temperature Range Threshold") +
  theme_clean() +
  scale_color_manual(values = c(cols[2], cols[1])) +
  theme(panel.background = element_rect(color = 'black'),
        legend.position = 'none');pd

ggsave(plot = pd, filename = "out/diversity_bayesian_preds.png", height = 2.5, width = 4.5, bg="white")

broom.mixed::tidy(m) |>
  mutate_if(is.numeric, round, 3) |>
  write_csv('out/div_model_coefs.csv')
performance::r2_bayes(m)
brms::hypothesis(m, 'dtr_thresholdCADRecipient < 0', robust = TRUE)

# dtr vs tmin vs vpdmin =======================

pvpddtr <- both_plotwise |>
  tibble::rownames_to_column('id') |>
  mutate(basin = ifelse(str_sub(id,1,1) == 'm', "Manitou", "Valles\nCaldera")) |>
  filter(id != 'mef16') |>
  pivot_longer(cols = c(vmin, tmin)) |>
  mutate(name = ifelse(name == "vmin", "Minimum VPD", "Minimum Temperature")) |>
  ggplot(aes(tdelta, value, color = basin)) +
  geom_point(aes(shape = basin), size=3, stroke = 1) +
  geom_smooth(se=F, method = 'lm', show.legend = F) +
  facet_wrap(~name, scales = 'free') +
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(1,2)) +
  theme_clean() +
  xlab("Diurnal Temperature Range") +
  theme(legend.title = element_blank(),
        legend.position = c(1,1),
        legend.background = element_rect(fill=NA),
        axis.title.y = element_blank(),
        legend.justification = c(1,1))

# all together

pmultipanel <- ggarrange(ptbb,
                         ggarrange(pd, pvpddtr, nrow=1, labels = c("", 'c')),
                         nrow=2, labels='auto', heights = c(1.3,1)); pmultipanel
ggsave(pmultipanel, filename = 'out/fig3multi.png', bg='white', width = 10, height =7.45)

# supplementary analysis suggested by paula ====================================

preds <- names(both_div)[2:9]

result <- list()
cc <- 1
for(p in preds){
  ff <- formula(paste("nspp ~", p, "* site"))
  result[[cc]] <- glm(ff, data = both_div, family = "poisson")
  cc <- cc+1
}

lapply(result, performance::r2) |>
  bind_rows() |>
  mutate(pred = preds)

lapply(result, broom::tidy) |>
  bind_rows()
library(ggeffects)
lapply(result, function(x)ggpredict(x)[[1]] |> as.data.frame()) |>
  bind_rows() |>
  mutate(var = str_sub(group, 1,1),
         var1= str_sub(group, 2, nchar(group)),
         var = ifelse(var == "t", "Temperature", "VPD")) |>
  group_by(group)|>
  mutate(sx = scale(x)) |>
  ungroup() |>
  ggplot(aes(x=sx, y=predicted, color = var)) +
  geom_line(lwd=1) +
  facet_wrap(~var1, scales = 'free_x', nrow = 2) +
  geom_hline(yintercept =c(14.85, 18.75), lty=3, lwd=1) +
  scale_color_manual(values =c('cyan4', 'chocolate')) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = c(1,1),
        legend.background = element_rect(fill=NA, color = 'black'),
        legend.justification = c(1,1))

summary(m); performance::check_model(m); fixef(m)



# # species list =========
#
# vv<- vall_veg |> colnames() |> str_remove_all("cf") |> sort() |>
#   str_replace_all("_", " ") |> trimws() |> as_tibble() |> dplyr::rename(Valles = value)
# mv <- mef_veg |> colnames() |> str_remove_all("cf") |> sort() |>
#   str_replace_all("_", " ") |> str_replace_all("uva ursi", "uva-ursi")|>
#   trimws() |> as_tibble() |> dplyr::rename(Manitou = value)
#
# write_csv(vv, 'out/vc_sp_list.csv')
# write_csv(mv, 'out/mf_sp_list.csv')
#
# # threshold sensitivity analysis =================
# result <- data.frame(thresh = NA, r2 = NA, site = NA)
# for(i in 1:length(seq(13,16,.1))){
#   env <- mef_plotwise_summaries |> mutate(thresh = ifelse(tdelta > seq(13,16,.1)[i], 'over', 'under'))
#   x <- adonis2(mef_veg ~ thresh, data = env, permutations = 9999)
#   result[i, 1] <- seq(13,16,.1)[i]
#   result[i,2] <-  x$R2[1]
#   result[i,3] <- 'mef'
# }
#
# resultv <- data.frame(thresh = NA, r2 = NA, site = NA)
# for(i in 1:length(seq(10,16,.1))){
#   env <- vall_plotwise_summaries |> mutate(thresh = ifelse(tdelta > seq(10,16,.1)[i], 'over', 'under'))
#   x <- adonis2(vall_veg ~ thresh, data = env, permutations = 9999)
#   resultv[i, 1] <- seq(10,16,.1)[i]
#   resultv[i,2] <- x$R2[1]
#   resultv[i,3] <- 'val'
# }
# ggplot(bind_rows(result, resultv)) +
#   geom_line(aes(x=thresh, y=r2, color = site))
#
# # species list, how many species are common to both places?
#
# sp_list <- both_veg |>
#   tibble::rownames_to_column('site') |>
#   dplyr::mutate(site = ifelse(str_sub(site, 1,1)=='m', "Manitou", "Valles")) |>
#   pivot_longer(-site) |>
#   group_by(site, name) |>
#   summarise(value = sum(value)) |>
#   pivot_wider(names_from = site)
#
# sp_list |>
#   mutate(is_both = ifelse(Manitou * Valles > 0, 1,0)) |>
#   pull(is_both) |>
#   sum()
#
