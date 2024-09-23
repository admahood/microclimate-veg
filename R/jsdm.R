library(tidyverse)
devtools::install_github('admahood/topomicro', force =TRUE)
library(topomicro)
library(Hmsc)
require(snow)
# devtools::install_github("admahood/gghmsc", force = TRUE)
library(gghmsc)
library(sf)

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
  plotwise_summary()

vall_plotwise_summaries <-
  vall_clim |>
  plotwise_summary()




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

##
locations_xx <- locations |>
  mutate(id = str_replace_all(id, 'vg0', 'vg'))

mef_preds <- terra::rast('data/mef_dtr.tif')
val_preds <- terra::rast('data/val_dtr.tif')

tc_mef <- locations_xx %>%
  mutate(terra::extract(mef_preds, ., ID=F))|>
  st_set_geometry(NULL) |>
  filter(str_sub(id, 1,3) == "mef") %>%
  filter(id %in% rownames(mef_plotwise_summaries)) |>
  left_join(mef_topo) |>
  arrange(id) |>
  left_join(mef_plotwise_summaries |> tibble::rownames_to_column('id')) |>
  mutate(site = "Manitou EF") |>
  tibble::column_to_rownames("id")|>
  mutate_if(is.character, as.factor)

tc_val <- locations_xx %>%
  mutate(terra::extract(val_preds, ., ID=F)) |>
  st_set_geometry(NULL) |>
  filter(id %in% rownames(vall_plotwise_summaries)) |>
  left_join(val_topo) |>
  arrange(id) |>
  left_join(vall_plotwise_summaries |> tibble::rownames_to_column('id')) |>
  mutate(site = "Valles Caldera NP")|>
  tibble::column_to_rownames("id")|>
  mutate_if(is.character, as.factor)

all(rownames(vall_veg) == rownames(tc_val))
all(rownames(mef_veg) == rownames(tc_mef))

prevalencev <- colSums(vall_veg) %>%
  as_tibble(rownames = "Species") %>%
  dplyr::rename(prevalence = value) %>%
  arrange(desc(prevalence))
prevalencem <- colSums(mef_veg) %>%
  as_tibble(rownames = "Species") %>%
  dplyr::rename(prevalence = value) %>%
  arrange(desc(prevalence))

ggplot(prevalencev) +
  geom_histogram(aes(x=prevalence))
ggplot(prevalencem) +
  geom_histogram(aes(x=prevalence))

xformula <- ~ tdelta + vmin

vXSel <- list()
vXSel$covGroup <- c(1:8)
vXSel$spGroup <- 1:ncol(vall_veg)
vXSel$q <- rep(0.1, ncol(vall_veg))

mXSel <- list()
mXSel$covGroup <- c(1:8)
mXSel$spGroup <- 1:ncol(mef_veg)
mXSel$q <- rep(0.1, ncol(mef_veg))

modv = Hmsc(Y = vall_veg, XFormula = xformula, XData = tc_val) # maybe do a spatial random level
modm = Hmsc(Y = mef_veg, XFormula = xformula, XData = tc_mef) # maybe do a spatial random level

nChains = 4
test.run = FALSE

if (test.run){
  thin = 10
  samples = 100
  transient = ceiling(thin*samples*.5)
  hmsc_file <- "data/hmsc/hmsc_probit_test.Rda"

}else{
  thin = 100
  samples = 700
  transient = ceiling(thin*samples*.5)
  hmsc_file <- str_c("data/hmsc/hmsc_probit_",
                     str_replace_all((thin*samples) + transient, "000$", "K"),
                     "_iters.Rda")
}

if(!dir.exists("data/hmsc"))dir.create("data/hmsc")
t0 <- Sys.time()
if(!file.exists(hmsc_file)){
  mv = sampleMcmc(modv, thin = thin,
                 samples = samples,
                 transient = transient,
                 adaptNf = rep(ceiling(0.4*samples*thin),1),
                 nChains = nChains,
                 nParallel = nChains)
  mm = sampleMcmc(modm, thin = thin,
                  samples = samples,
                  transient = transient,
                  adaptNf = rep(ceiling(0.4*samples*thin),1),
                  nChains = nChains,
                  nParallel = nChains)
  print(Sys.time()-t0)
  save(mm, mv, file=hmsc_file)
}else{load(hmsc_file)}

gghmsc::gghmsc_convergence(mm, title = "Model Convergence: Manitou") -> pcm
gghmsc::gghmsc_convergence(mv, title = "Model Convergence: Valles Caldera") -> pcv
ggpubr::ggarrange(pcm, pcv, nrow = 2) |> ggsave(filename = "out/jsdm_convergence.png", height = 6, width = 6, bg='white')


gghmsc::gghmsc_fit(mm, which = 'named', title = "Variance Explained: Manitou") -> prm
ggsave(plot = prm, filename ="out/jsdm_r2_mef.png", width = 8, height = 15, bg = 'white')

gghmsc::gghmsc_fit(mv, which = 'named', title = 'Variance Explained: Valles Caldera') -> prv
ggsave(plot = prv, filename ="out/jsdm_r2_val.png", width = 8, height = 15, bg = 'white')

gghmsc::gghmsc_beta(mm, title = "Manitou Experimental Forest") -> pbm
gghmsc::gghmsc_beta(mv, title = "Valles Caldera National Preserve") -> pbv

ggpubr::ggarrange(pbm, pbv, nrow =1) |> ggsave(filename = 'out/jsdm_betas.png', width = 10, height = 15, bg = "white")



gghmsc::gghmsc_vp(mm)
# gghmsc::gghmsc_omega(mm)

gghmsc::gghmsc_convergence(mv, title = "Model Convergence: Valles Caldera") -> pcv
gghmsc::gghmsc_vp(mv)
gghmsc::gghmsc_omega(mv)



cor(vall_plotwise_summaries) -> x
# x[abs(x)<.7] <- 0
ggcorrplot::ggcorrplot(x, type = 'lower', hc.method = 'centroid', hc.order = T, lab = T)
