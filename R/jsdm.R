library(tidyverse)
library(topomicro)
library(Hmsc)
require(snow)
# devtools::install_github("admahood/gghmsc", force = TRUE)
library(gghmsc)
mef_clim <- read_csv("data/cleaned_bm/mef_jul23.csv"); glimpse(mef_clim)
vall_clim <- read_csv("data/cleaned_bm/vall_dec22.csv"); glimpse(vall_clim)

clim_summaries <-
  mef_clim |>
  bind_rows(vall_clim) |>
  plotwise_summary()

veg <- readr::read_csv("data/valles_caldera/microclimate - VALL_veg.csv") |>
  dplyr::select(plot, species) |>
  bind_rows(readr::read_csv("data/mef/manitou_species_data.csv")|>
              dplyr::select(plot, species = actual_species))|>
  dplyr::mutate(occurrence = 1,
                plot = str_to_lower(plot) %>% str_remove_all("_"),
                species = str_replace_all(species, " ", "_")|>
                  str_remove_all("\\.") |>
                  str_replace_all("-", "_")) |>
  tidyr::pivot_wider(names_from = species, values_from = occurrence,
                     values_fn = first, values_fill = 0) |>
  mutate(plot = ifelse(str_sub(plot, 3,3) == "0", str_remove_all(plot, "0"), plot)) |>
  filter(plot %in% rownames(clim_summaries)) |>
  arrange(plot) %>%
  tibble::column_to_rownames("plot")
rownames(veg) == rownames(clim_summaries)

prevalence <- colSums(veg) %>%
  as_tibble(rownames = "Species") %>%
  dplyr::rename(prevalence = value) %>%
  arrange(desc(prevalence))

ggplot(prevalence) +
  geom_histogram(aes(x=prevalence))

xformula <- formula(paste("~ site + ", paste(names(clim_summaries), collapse = " + ")))
xdata <- clim_summaries |>
  mutate(site = as.factor(c(rep("mef", 12), rep("val", 21))))

studyDesign <- data.frame(site = as.factor(xdata$site))
rL <- HmscRandomLevel(units = studyDesign$site)


mod = Hmsc(Y = veg, XFormula = xformula, XData = xdata, studyDesign = studyDesign,
           ranLevels = list("site" = rL))

nChains = 4
test.run = TRUE

if (test.run){
  #with this option, the vignette evaluates in ca. 1 minute in adam's laptop
  thin = 10
  samples = 100
  transient = ceiling(thin*samples*.5)
}else{
  # with a spatial random effect, evaluates in 20 minutes on adam's laptop
  thin = 10
  samples = 1000
  transient = ceiling(thin*samples*.5)
}

if(!dir.exists("data/hmsc"))dir.create("data/hmsc")
t0 <- Sys.time()
hmsc_file <- "data/hmsc/hmsc_probit_test.Rda"
dir.create("data/hmsc")
if(!file.exists(hmsc_file)){
  m = sampleMcmc(mod, thin = thin,
                 samples = samples,
                 transient = transient,
                 adaptNf = rep(ceiling(0.4*samples*thin),1),
                 nChains = nChains,
                 nParallel = nChains)
  print(Sys.time()-t0)
  save(m, file=hmsc_file)
}else{load(hmsc_file)}

library(tidyverse)
gghmsc::gghmsc_convergence(m)
gghmsc::gghmsc_beta(m)
gghmsc::gghmsc_omega(m)
