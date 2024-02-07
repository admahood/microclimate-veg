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

mef_clim <- read_csv("data/cleaned_bm/mef_jul23.csv"); glimpse(mef_clim)
vall_clim <- read_csv("data/cleaned_bm/vall_dec22.csv"); glimpse(vall_clim)

mef_plotwise_summaries <-
  mef_clim |>
  plotwise_summary()

vall_plotwise_summaries <-
  vall_clim |>
  plotwise_summary()

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

mef_veg <- readr::read_csv("data/mef/manitou_species_data.csv")|>
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

terra::rast("data/big/topoterra_2C_2015.tif") -> xx

locations_xx <- locations |>
  st_transform(crs = st_crs(xx)) %>%
  mutate(terra::extract(xx, .))

tc_mef <- locations_xx |>
  st_set_geometry(NULL) |>
  dplyr::select(-ID, tc_aet = aet, tc_def = def, tc_tmx= tmax, tc_tmin = tmin) |>
  filter(str_sub(id, 1,3) == "mef") |>
  filter(id %in% rownames(mef_plotwise_summaries)) |>
  arrange(id) |>
  tibble::column_to_rownames("id")

tc_val <- locations_xx |>
  st_set_geometry(NULL) |>
  dplyr::select(-ID, tc_aet = aet, tc_def = def, tc_tmx= tmax, tc_tmin = tmin) |>
  filter(str_sub(id, 1,2) == "vg") |>
  filter(id %in% rownames(vall_plotwise_summaries)) |>
  arrange(id) |>
  tibble::column_to_rownames("id")

# mef nmds ====
vegan::metaMDS(mef_veg) -> nmds

sites <- nmds$points |>
  as.data.frame() |>
  tibble::rownames_to_column("plot") |>
  left_join(mef_plotwise_summaries |> tibble::rownames_to_column("plot"))

spp <- envfit(nmds, mef_veg, 9999)
envm <- envfit(nmds, mef_plotwise_summaries |> cbind(tc_mef), 9999)

# vall nmds ====

vegan::metaMDS(vall_veg) -> nmdsv

sitesv <- nmdsv$points |>
  as.data.frame() |>
  tibble::rownames_to_column("plot") |>
  left_join(vall_plotwise_summaries |> tibble::rownames_to_column("plot"))

sppv <- envfit(nmdsv, vall_veg, 9999)
env <- envfit(nmdsv, vall_plotwise_summaries |> cbind(tc_val), 9999)



bind_rows(tidy_envfit(env) |> mutate(site = "vall"),
          tidy_envfit(envm) |> mutate(site = "mef")) |>
  dplyr::select(var, r2, p, site) |>
  mutate(star = case_when(p < 0.05 & p > 0.01 ~ "*",
                          p < 0.01 & p > 0.001 ~ "**",
                          p < 0.001 ~ "***",
                          p >= 0.05 ~ ""
                           )) %>%
write_csv("out/initial_ndms.csv")

env_a <-as.data.frame(env$vectors$arrows*sqrt(env$vectors$r)) %>%
  tibble::rownames_to_column("species")  %>%
  mutate(p =env$vectors$pvals, site = "Valles Caldera") %>%
  filter(p < 0.05)

env_b <-as.data.frame(envm$vectors$arrows*sqrt(envm$vectors$r)) %>%
  tibble::rownames_to_column("species")  %>%
  mutate(p =envm$vectors$pvals, site = "Manitou EF") %>%
  filter(p < 0.05) |>
  bind_rows(env_a)

ptd <- sitesv |>
  mutate(site = "Valles Caldera") |>
  bind_rows(sites |> mutate(site = "Manitou EF")) |>
  ggplot() +
  geom_segment(data = env_b,x=0,y=0, color = "grey",arrow = arrow(),
               aes(yend = NMDS2, xend = NMDS1), lwd=1)+
  geom_text_repel(data = env_b,size=4, aes(label = species, x=NMDS1, y=NMDS2), color = "grey40") +
  geom_text(aes(x=MDS1, y=MDS2,label = plot, color = tdelta), fontface='bold') +
  scale_color_viridis_c() +
  facet_wrap(~site) +
  coord_equal() +
  theme_light(); ptd

spp_a <-as.data.frame(sppv$vectors$arrows*sqrt(sppv$vectors$r)) %>%
  tibble::rownames_to_column("species")  %>%
  mutate(p =sppv$vectors$pvals, site = "Valles Caldera") %>%
  filter(p < 0.005)

spp_b <-as.data.frame(spp$vectors$arrows*sqrt(spp$vectors$r)) %>%
  tibble::rownames_to_column("species")  %>%
  mutate(p =spp$vectors$pvals, site = "Manitou EF") %>%
  filter(p < 0.05) |>
  bind_rows(spp_a)

pvm <- sitesv |>
  mutate(site = "Valles Caldera") |>
  bind_rows(sites |> mutate(site = "Manitou EF")) |>
  ggplot() +
  geom_segment(data = spp_b,x=0,y=0, color = "grey",arrow = arrow(),
               aes(yend = NMDS2, xend = NMDS1), lwd=1)+
  geom_text_repel(data = spp_b,size=4, aes(label = species, x=NMDS1, y=NMDS2), color = "grey40") +
  geom_text(aes(label = plot, color = vmin, x=MDS1, y=MDS2), fontface='bold') +
  scale_color_viridis_c() +
  facet_wrap(~site) +
  coord_equal() +
  theme_light(); pvm

ggarrange(ptd, pvm, ncol = 1) |>
  ggsave(plot = _, filename = "out/nmds.png", bg="white", width=10, height=10)
