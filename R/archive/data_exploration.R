# summarising climate data then relating to community composition

library(vegan)
library(terra)
library(ggpubr)
library(ggthemes)
library(sf)
library(plantecophys)
library(ggrepel)

locations <- read_csv("data/sensor_locations.csv") |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  dplyr::select(-starts_with("alpha"))

mef_clim <- read_csv("data/cleaned_bm/mef_jul23.csv") |>
  bind_rows(read_csv("data/cleaned_bm/mef_febmay24.csv")); glimpse(mef_clim)

# mef16 only has a few weeks of data
# ggplot(mef_clim |> filter(id == "mef16")) +
#   geom_line(aes(x=dt, y=temperature_c))
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
  plotwise_summary_std(stdf = mef_station_data)

vall_plotwise_summaries <-
  vall_clim |>
  plotwise_summary_std(stdf = val_std)

ggplot(vall_plotwise_summaries, aes(x=tdelta, y=stdelta, label = rownames(vall_plotwise_summaries))) + geom_text()

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

# both_veg <- readr::read_csv("data/valles_caldera/microclimate - VALL_veg.csv") |>
#   dplyr::select(plot, species) |>
#   bind_rows(readr::read_csv("data/mef/manitou_species_data.csv")|>
#               dplyr::select(plot, species = actual_species))|>
#   dplyr::mutate(occurrence = 1,
#                 plot = str_to_lower(plot) %>% str_remove_all("_"),
#                 species = str_replace_all(species, " ", "_")|>
#                   str_replace_all("-", "_")) |>
#   tidyr::pivot_wider(names_from = species, values_from = occurrence,
#                      values_fn = first, values_fill = 0) |>
#   mutate(plot = ifelse(str_sub(plot, 3,3) == "0", str_remove_all(plot, "0"), plot)) |>
#   filter(plot %in% rownames(both_plotwise)) |>
#   arrange(plot) %>%
#   tibble::column_to_rownames("plot")
# rownames(both_veg) == rownames(both_plotwise)

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

# tc_both <- locations_xx |>
#   st_set_geometry(NULL) |>
#   dplyr::select(-ID, tc_aet = aet, tc_def = def, tc_tmx= tmax, tc_tmin = tmin) |>
#   filter(str_sub(id, 1,3) == "mef") |>
#   filter(id %in% rownames(both_plotwise)) |>
#   arrange(id) |>
#   tibble::column_to_rownames("id")

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
write_csv("out/initial_ndms.csv")

env_a <-as.data.frame(env$vectors$arrows*sqrt(env$vectors$r)) %>%
  tibble::rownames_to_column("species")  %>%
  mutate(p =env$vectors$pvals, site = "Valles Caldera") %>%
  filter(p < 0.01)

env_b <-as.data.frame(envm$vectors$arrows*sqrt(envm$vectors$r)) %>%
  tibble::rownames_to_column("species")  %>%
  mutate(p =envm$vectors$pvals, site = "Manitou EF") %>%
  filter(p < 0.01) |>
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
  filter(p < 0.005) |>
  bind_rows(spp_a)

pvm <- sitesv |>
  mutate(site = "Valles Caldera") |>
  bind_rows(sites |> mutate(site = "Manitou EF")) |>
  ggplot() +
  geom_segment(data = spp_b,x=0,y=0, color = "grey",arrow = arrow(),
               aes(yend = NMDS2, xend = NMDS1), lwd=1)+
  geom_text_repel(data = spp_b,size=4, aes(label = species, x=NMDS1, y=NMDS2), color = "grey40") +
  geom_text(aes(label = plot, color = vmin |> log(), x=MDS1, y=MDS2), fontface='bold') +
  scale_color_viridis_c() +
  facet_wrap(~site) +
  coord_equal() +
  theme_light(); pvm

ggarrange(ptd, pvm, ncol = 1) |>
  ggsave(plot = _, filename = "out/nmds.png", bg="white", width=10, height=10)



# biodiversity by tdelta ====
div <- vall_veg |> vegan::diversity() |> as.data.frame() |> tibble::rownames_to_column("plot") |> dplyr::rename(div=2)
nsp <- vall_veg |> vegan::specnumber() |> as.data.frame() |> tibble::rownames_to_column("plot") |> dplyr::rename(nspp=2)
divm <- mef_veg |> vegan::diversity() |> as.data.frame() |> tibble::rownames_to_column("plot") |> dplyr::rename(div=2)
nspm <- mef_veg |> vegan::specnumber() |> as.data.frame() |> tibble::rownames_to_column("plot") |> dplyr::rename(nspp=2)


vall_div <- vall_plotwise_summaries |>
  tibble::rownames_to_column("plot") |>
  left_join(div) |>
  left_join(nsp) |>
  mutate(site = "val")
mef_div <- mef_plotwise_summaries |>
  tibble::rownames_to_column("plot") |>
  left_join(divm) |>
  left_join(nspm) |>
  mutate(site = "mef")

both_div <- bind_rows(mef_div, vall_div) |>
  mutate(dtr_threshold = ifelse(tdelta > 12.5, "over 12.5C", "under 12.5C"),
         sdtr_threshold = ifelse(stdelta > -4.5, "over 12.5C", "under 12.5C"))
table(both_div$dtr_threshold); table(both_div$sdtr_threshold)
lm(div~dtr_threshold, data = both_div) |> car::Anova()
glm(nspp~dtr_threshold, data = both_div, family = "poisson") |> car::Anova()

bp1<- ggplot(both_div, aes(x=dtr_threshold, y = nspp)) +
  geom_boxplot() +
  geom_jitter(aes(color = site, shape = site), width = .1, size=2) +
  ggpubr::stat_anova_test() +
  theme_clean() +
  theme(legend.position = 'none')
bp2<- ggplot(both_div, aes(x=dtr_threshold, y = div)) +
  geom_boxplot(outliers = F) +
  geom_jitter(aes(color = site, shape = site), width = .1, size=2)+
  ggpubr::stat_anova_test() +
  theme_clean() +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0))
ggpubr::ggarrange(bp1, bp2) -> div_boxplots
ggsave(div_boxplots, filename = "out/diversity_boxplots.png", width = 7, height = 4, bg="white")

# evidence of the range squeeze hypothesis?
glm(nspp ~ (tdelta+vmin), both_div, family = "poisson") %>% summary()
lm(log(div+1) ~ (tdelta+vmin), both_div, family = Gamma()) %>% summary()

ggplot(mef_div |> bind_rows(vall_div),aes(x= stdelta, y=nspp, color = site)) +
  geom_point()+
  geom_smooth()
ggplot(mef_div |> bind_rows(vall_div),aes(x= stdelta, y=div, color = site)) +
  geom_point() +
  geom_smooth()

ggplot(both_div, aes(x=nspp))


# scatterplots ============
library(geomtextpath)
vall_plotwise_summaries |>
  mutate(site = "Val") |>
  bind_rows(mef_plotwise_summaries|>mutate(site = "mef")) |>
  as_tibble(rownames = "plot") |>
  filter(plot != "mef16") |> # mef16 needs to be standardized
  ggplot(aes(x=vmin, y=tdelta, color = site)) +
  geom_text(size=3, aes(label = plot)) +
  geom_hline(yintercept = 12.5, linetype = 3, linewidth =1) +
  geom_text(x = .1, y=13, label= '12.5C', color = 'black') +
  theme_clean()

ggsave("out/vpdmin_x_tdelta.png", width=6, height=4)
vall_plotwise_summaries |>
  mutate(site = "Val") |>
  bind_rows(mef_plotwise_summaries|>mutate(site = "mef")) |>
  lm(vmin ~ tdelta * site, data = _) |>
  summary()


