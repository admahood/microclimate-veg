# diversity analysis
# libs =============================
library(tidyverse)
library(topomicro)
library(broom)
library(car)

# data prep =========================
locations <- read_csv("data/sensor_locations.csv") |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  dplyr::select(-starts_with("alpha"))

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
  plotwise_summary_std(stdf = mef_station_data)

vall_plotwise_summaries <-
  vall_clim |>
  plotwise_summary_std(stdf = val_std)

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

terra::rast("data/big/topoterra_2C_2015.tif") -> xx

locations_xx <- locations |>
  st_transform(crs = st_crs(xx)) %>%
  mutate(terra::extract(xx, .))

tc_mef <- locations_xx |>
  st_set_geometry(NULL) |>
  dplyr::select(-ID, tc_aet = aet, tc_def = def, tc_tmx= tmax, tc_tmin = tmin) |>
  filter(str_sub(id, 1,3) == "mef") |>
  filter(id %in% rownames(mef_plotwise_summaries)) |>
  left_join(mef_topo) |>
  dplyr::rename(plot=id)

tc_val <- locations_xx |>
  st_set_geometry(NULL) |>
  dplyr::select(-ID, tc_aet = aet, tc_def = def, tc_tmx= tmax, tc_tmin = tmin) |>
  filter(str_sub(id, 1,2) %in% c("vg", 'rs')) |>
  mutate(id = str_replace_all(id, "g0", "g")) |>
  filter(id %in% rownames(vall_plotwise_summaries)) |>
  left_join(val_topo) |>
  dplyr::rename(plot=id)

# biodiversity by tdelta ====
div <- vall_veg |> vegan::diversity() |> as.data.frame() |> tibble::rownames_to_column("plot") |> dplyr::rename(div=2)
nsp <- vall_veg |> vegan::specnumber() |> as.data.frame() |> tibble::rownames_to_column("plot") |> dplyr::rename(nspp=2)
divm <- mef_veg |> vegan::diversity() |> as.data.frame() |> tibble::rownames_to_column("plot") |> dplyr::rename(div=2)
nspm <- mef_veg |> vegan::specnumber() |> as.data.frame() |> tibble::rownames_to_column("plot") |> dplyr::rename(nspp=2)


vall_div <- vall_plotwise_summaries |>
  tibble::rownames_to_column("plot") |>
  left_join(div) |>
  left_join(nsp) |>
  left_join(tc_val) |>
  mutate(site = "val") |>
  mutate(dtr_threshold = ifelse(tdelta > 14, "CAD Recipient", "CAD Donor"))
mef_div <- mef_plotwise_summaries |>
  tibble::rownames_to_column("plot") |>
  left_join(divm) |>
  left_join(nspm)|>
  left_join(tc_mef)  |>
  mutate(site = "mef")|>
  mutate(dtr_threshold = ifelse(tdelta > 15, "CAD Recipient", "CAD Donor"))


both_div <- bind_rows(mef_div ,
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
  dplyr::mutate(site = ifelse(site == 'mef', "Manitou EF", "Valles Caldera")) |>
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

# tidy(d_v, conf_int = TRUE) |>
#   mutate(response = "div_val") |>
#   bind_rows(tidy(d_m, conf_int = TRUE) |> mutate(response = "div_mef")) |>
#   bind_rows(tidy(sr_m, conf_int = TRUE) |> mutate(response = "nspp_mef"))|>
#   bind_rows(tidy(sr_v, conf_int = TRUE) |> mutate(response = "nspp_val")) |>
#   filter(term != "(Intercept)") |>
#   ggplot(aes(x=estimate, y=term)) +
#   geom_segment(aes(x = estimate - std.error, xend = estimate+std.error)) +
#   geom_point() +
#   facet_wrap(~response, scales = "free_x") +
#   geom_vline(xintercept = 0, lty = 3)
#
#
# bp1<- ggplot(both_div, aes(x=sdtr_threshold, y = nspp)) +
#   geom_boxplot() +
#   geom_jitter(aes(color = site, shape = site), width = .1, size=2) +
#   ggpubr::stat_anova_test() +
#   theme_clean() +
#   theme(legend.position = 'none')
# bp2<- ggplot(both_div, aes(x=sdtr_threshold, y = div)) +
#   geom_boxplot(outliers = F) +
#   geom_jitter(aes(color = site, shape = site), width = .1, size=2)+
#   ggpubr::stat_anova_test() +
#   theme_clean() +
#   theme(legend.position = c(1,0),
#         legend.justification = c(1,0))
# ggpubr::ggarrange(bp1, bp2) -> div_boxplots
# ggsave(div_boxplots, filename = "out/diversity_boxplots.png", width = 7, height = 4, bg="white")
