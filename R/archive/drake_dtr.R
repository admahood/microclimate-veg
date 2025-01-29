# try doing microclimate preds on drake
library(tidyverse)
library(terra)
library(topomicro)

drake_dem <- terra::rast('data/dem2018_drake.tif')

drake_twi <- topomicro::get_twi(drake_dem)$twi
plot(drake_dem)
plot(drake_twi)

preds_drake <- predict_sp(c(drake_twi, drake_dem), bsp)

dd <- c(drake_dem, drake_twi) |> as.data.frame(xy=T) |>
  dplyr::rename(elevation = 3) |>
  mutate(rel_elv = elevation - min(elevation)) %>%
  mutate(dtr_pred = predict(vmm1, newdata = .))

ggplot(dd) +
  geom_raster(aes(fill = dtr_pred, x=x, y=y))

predict(vmm1,newdata = dd)
