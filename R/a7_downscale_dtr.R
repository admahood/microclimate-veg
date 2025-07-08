# downscale dtr
source('R/a1_data_prep.R')
library(fields)
library(cowplot)
library(ggspatial)
library(ggthemes)
library(ggpubr)
library(geomtextpath)
mef_dem <- terra::rast('data/mef_dem.tif')
names(mef_dem) <- 'elevation'
mef_twi <- terra::rast('data/mef_twi.tif')

val_twi <- terra::rast('data/vc_twi.tif')
val_dem <- terra::rast('data/vc_dem.tif')

val_topo <- read_csv('data/val_topo.csv')
mef_topo <- read_csv('data/mef_topo.csv')

# wc_dtr <- terra::rast('data/big/wc2.1_30s_bio/wc2.1_30s_bio_2.tif')

# locations <- locations %>%
#   mutate(terra::extract(wc_dtr, ., ID =F)) |>
#   dplyr::rename(wc_dtr = wc2.1_30s_bio_2)

dm <- mef_plotwise_summaries |>
  tibble::rownames_to_column('id') |>
  left_join(mef_topo)

dmsf <- locations |>
  filter(id %in% dm$id) |>
  left_join(dm)|>
  mutate(rel_elv = elevation - min(elevation))

dmsf1 <- locations |>
  filter(str_sub(id, 1,1) == 'm')

dv <- vall_plotwise_summaries |>
  tibble::rownames_to_column('id') |>
  left_join(val_topo)

dvsf <- locations |>
  mutate(id = str_replace_all(id, "vg0", 'vg')) |>
  filter(id %in% dv$id) |>
  left_join(dv) |>
  mutate(rel_elv = elevation - min(elevation))

mm <- lm(tdelta ~ twi + elevation, data = dmsf)
performance::check_model(mm); summary(mm)

# vm <- lm(tdelta ~ twi + elevation+wc_dtr, data = dvsf)
# performance::check_model(vm); summary(vm)


# site is not significant, model better without it, same with wc_dtr
# vmm <- lm(tdelta ~ twi + elevation + wc_dtr, data = dvsf |> bind_rows(dmsf) |>
#             mutate(site = ifelse(str_sub(id,1,1) == "m", "mef", 'val')))
# performance::check_model(vmm); summary(vmm)
vmm1 <- lm(tdelta ~ twi + elevation, data = dvsf |> bind_rows(dmsf) |>
            mutate(site = ifelse(str_sub(id,1,1) == "m", "mef", 'val')))
performance::check_model(vmm1); summary(vmm1)
# AIC(vmm, vmm1)

tidy_sp <- function(sf_df, vars, y){
  x = st_coordinates(sf_df)[,c(1,2)]
  z = dplyr::select(sf_df, vars) %>%
    st_set_geometry(NULL)
  y = pull(sf_df, y)

  spmod <- fields::spatialProcess(x=x, y=y, Z=z,
                         profileLambda = TRUE,
                         profileARange = TRUE)

  return(spmod)
}

predict_sp <- function(rast_stack, spmod){
  elv_df<-rast_stack %>%
    as.data.frame(xy=TRUE) %>%
    na.omit()
  lat <- elv_df$y
  lon <- elv_df$x
  elev <- elv_df[,3:4]
  xps <-cbind(lon, lat)
  yp = predict(spmod,
               x=xps,Z=elev)
  spmod_rast <- raster::rasterFromXYZ(data.frame(lon=lon, lat=lat,
                                                 prediction = yp),
                                      crs = st_crs(rast_stack)) %>%
    rast()
  return(spmod_rast)
  }

spmod <- tidy_sp(dmsf |> bind_rows(dvsf), vars = c("twi", 'elevation'), y = 'tdelta')
predict_sp(c(mef_twi, mef_dem), spmod) |> plot()

vsp <- tidy_sp(dvsf, vars = c("twi", 'elevation'), y = 'tdelta')
predict_sp(c(val_twi, val_dem), vsp) |> plot()

bsp <- tidy_sp(dvsf |> bind_rows(dmsf), vars = c("twi", 'elevation'), y = 'tdelta')
predict_sp(c(val_twi, val_dem), bsp) |> plot()
dtr_spmod <- bsp; save(dtr_spmod, file = 'out/dtr_spmod.rda')
preds <- predict_sp(c(val_twi, val_dem), bsp)


# leave one out cross validation -- takes a while
yes_lets_redo_loocv <- FALSE
if(yes_lets_redo_loocv){ddd <- bind_rows(dmsf, dvsf)
  loo_preds <- list()
  for(i in 1:nrow(ddd)){
    dsub <- ddd[-i,]
    mod <- tidy_sp(dsub, vars = c("twi", 'elevation'), y = 'tdelta')

    if(str_sub(ddd[i,]$id, 1,3) == "mef"){
      preds <-  predict_sp(c(mef_twi, mef_dem), mod)
      }else{preds <- predict_sp(c(val_twi, val_dem), mod)}

    loo_preds[[i]] <- ddd[i,] |> dplyr::select(tdelta, id)|>
      mutate(terra::extract(preds, ddd[i,] , ID = F))
    print(i)
  }

  bind_rows(loo_preds) |> st_write("data/loo_preds.gpkg")

}else{load('data/loo_preds.gpkg')}

bind_rows(loo_preds) |> lm(tdelta ~ prediction, data = _) |> summary()

bind_rows(loo_preds) |>
  mutate(SE =(tdelta-prediction)^2) |>
  pull(SE) |>
  mean() |>
  sqrt()

# plotting

pv <- preds |> as.data.frame(xy=TRUE) |>
  ggplot() +
  geom_raster(aes(x=x,y=y, fill = prediction)) +
  geom_sf(data = dvsf, shape = 2, stroke =1, size = 4) +
  scale_fill_gradient2(midpoint = 14, name = "Predicted\nDTR") +
  ggthemes::theme_clean() +
  annotate(geom = "text", x= -106.55, y=35.9, label = "a. Valles Caldera", fontface='bold', size=8) +
  coord_sf(expand=F) +
  ggspatial::annotation_scale(pad_x = unit(4, 'cm')) +
  # ggtitle("Valles Caldera") +
  theme(legend.position=c(0.05,0.05),
        legend.justification = c(0,0),
        axis.title = element_blank(),
        legend.background = element_rect(color = "black", fill="white"));pv

mpreds <- predict_sp(c(mef_twi, mef_dem), bsp)

pm <- mpreds |>
  as.data.frame(xy=TRUE) |>
  ggplot() +
  geom_raster(aes(x=x,y=y, fill = prediction)) +
  geom_sf(data = dmsf1, shape = 1, stroke =1, size = 3) +
  scale_fill_gradient2(midpoint = 15, name = "Predicted\nDTR") +
  ggthemes::theme_clean() +
  coord_sf(expand = F) +
  annotate(geom = 'text', x=-105.11, y=39.12, label = "b. Manitou", fontface = 'bold', size = 8) +
  ggspatial::annotation_scale(pad_x = unit(4, 'cm')) +
  theme(legend.position=c(0.05,0.05),
        legend.justification = c(0,0),
        axis.title = element_blank(),
        legend.background = element_rect(color = "black", fill="white"));pm

# ggarrange(pv, pm, nrow =2, ncol=1, heights = c(1.3,1)) |>
#   ggsave(filename = "out/twi_predictions.png", bg='white',
#          width =5, height =7)



# pr2 <- dmsf %>%
#   mutate(terra::extract(mpreds,.),
#          site = "Manitou") |>
#   bind_rows(dvsf %>%
#               mutate(terra::extract(preds,.),
#                      site = "Valles\nCaldera") )

pr2 <- loo_preds |> bind_rows() %>% mutate(y = st_coordinates(.)[,2],
                                    site = ifelse(y>38, "Manitou", "Valles\nCaldera")) |>
  ggplot(aes(x=prediction, y=tdelta)) +
  geom_point(aes(color = site, shape = site), size=3, stroke = 1) +
  # geom_smooth(method = 'lm') +
  geom_textabline(label="1:1") +
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(1,2))+
  xlab("Predicted DTR") +
  ylab("Observed DTR") +
  ggtitle("c. Leave One Out Cross-Validation") +
  theme_clean() +
  theme(#legend.position = 'bottom', #c(0,1),
        #legend.justification = c(0,1),
        legend.title = element_blank(),
        legend.background = element_rect(fill=NA));pr2
ggsave("out/tdelta_preds.png", bg='white', width=3.5, height=3.5)

fig_preds <- cowplot::ggdraw(ylim = c(0,1.2)) +
  draw_plot(pv, x=0, y=.5,width = 1, height=.7) +
  draw_plot(pm, x=0, y=0,width = 1, height=.5) +
  draw_plot(pr2, x=0.54, y=.94,width = .45, height=.25)
ggsave(plot = fig_preds, filename = 'out/fig4_multipanel.png', width = 9, height =12, bg='white')

terra::writeRaster(mpreds, filename = "data/mef_dtr.tif")
terra::writeRaster(preds, filename = "data/val_dtr.tif")

dmsf %>%
  mutate(terra::extract(mpreds,.),
         site = "Manitou EF") |>
  bind_rows(dvsf %>%
              mutate(terra::extract(preds,.),
                     site = "Valles Caldera") ) |>
  lm(tdelta ~ prediction, data = _) |>
  summary()

# vpdmin, tmin, tdelta
dmsf |> bind_rows(dvsf) |>
  mutate(site = ifelse(str_sub(id, 1,1) == 'm', 'MEF', "VC")) |>
  filter(id != 'mef16') |>
  st_set_geometry(NULL) |>
  dplyr::select(DTR=tdelta, VPDmin = vmin, Tmin= tmin, site) |>
  pivot_longer(-c(site,DTR)) |>
  ggplot(aes(y=value, x=DTR, color = site)) +
  geom_point() +
  facet_wrap(~name, scales = 'free') +
  geom_smooth(method = 'lm',
              se=F) +
  theme_classic() +
  scale_color_brewer(palette = "Set1") +
  theme(axis.title.y = element_blank(),
        panel.background = element_rect(color = 'black'),
        legend.position = c(1,1),
        legend.justification = c(1,1),
        legend.title = element_blank(),
        legend.background = element_rect(fill=NA))
ggsave(filename = 'out/vmin_tmin_compare.png', width = 5, height = 3, bg='white')


vmin_mod <- dmsf |> bind_rows(dvsf) |>
  mutate(site = ifelse(str_sub(id, 1,1) == 'm', 'MEF', "VC")) |>
  filter(id != 'mef16') |>
  st_set_geometry(NULL) |>
  dplyr::select(tdelta, vmin, tmin, site) |>
  lm(vmin ~ tdelta * site, data = _) |>
  broom::tidy() |>
  mutate(response = "VPDmin")

tmin_mod <- dmsf |> bind_rows(dvsf) |>
  mutate(site = ifelse(str_sub(id, 1,1) == 'm', 'MEF', "VC")) |>
  filter(id != 'mef16') |>
  st_set_geometry(NULL) |>
  dplyr::select(tdelta, vmin, tmin, site) |>
  lm(tmin ~ tdelta * site, data = _) |>
  broom::tidy()|>
  mutate(response = "Tmin")

bind_rows(tmin_mod, vmin_mod) |>
  write_csv('out/vmin_tmin_comparison.csv')

# try blue lake, bennett, dry creek
