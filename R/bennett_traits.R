library(tidyverse)
library(topomicro)
library(lmerTest)
library(ggeffects)
library(randomForest)

md <- read_csv("data/cleaned_bm/bennett_all.csv")
bt <- read_csv("data/bennett_traits.csv")
ps <- topomicro::plotwise_summary(md) |>
  as_tibble(rownames = "plot")

d <- left_join(bt, ps) |>
  filter(plot != "ben3") |>
  left_join(read_csv('data/bennett_topo.csv') |> dplyr::rename(plot = id))

dldmc <- d |>
  dplyr::select(-location, -plot, -individual, -leaf_area, -lma, -sla) |>
  dplyr::filter(!is.na(height_cm))
# variable exploration

res <- list()
for(i in 1:length(unique(d$species_full))){
 res[[i]] <- randomForest(ldmc ~ ., data = dldmc |>
                            filter(species_full == unique(d$species_full)[i]),
                          ntree = 2000)
 plot(res[[i]], main = unique(d$species_full)[i])
 varImpPlot(res[[i]], main = unique(d$species_full)[i])

}

mod_td <- lm(tdelta ~ hli + elevation*twi + slope , d)
summary(mod_td); car::Anova(mod_td); performance::check_model(mod_td)

mod_vm <- lm(vmin ~ hli + twi*elevation + slope, d)
summary(mod_vm); car::Anova(mod_vm); performance::check_model(mod_vm)

ggeffects::ggpredict(mod_vm, c("twi", "elevation")) |> plot(show_residuals = T)
ggeffects::ggpredict(mod_td, c("twi", "elevation")) |> plot(show_residuals = T)

# lmers

varImpPlot(res[[1]], main = unique(d$species_full)[1])
mod1 <- lmer(ldmc ~ height_cm + tmax +  tdelta + vmin + hli + twi + (1|plot),
             data=d |> filter(species_full == "Ceanothus velutina"))
summary(mod1); performance::check_model(mod1); performance::r2(mod1); car::Anova(mod1)

varImpPlot(res[[4]], main = unique(d$species_full)[4]); res[[4]]
mod2 <- lmer(ldmc ~ height_cm + tdelta + vmin + hli + twi  + (1|plot),
             data=d |> filter(species_full == "Shepheria canadensis"))
summary(mod2); performance::check_model(mod2); performance::r2(mod2); car::Anova(mod2)

varImpPlot(res[[2]], main = unique(d$species_full)[2])
mod3 <- lmer(ldmc ~ height_cm + tdelta + hli + twi+(1|plot),
             data=d |> filter(species_full == "Erigeron canadensis"))
summary(mod3); performance::check_model(mod3); performance::r2(mod4); car::Anova(mod4)

varImpPlot(res[[3]], main = unique(d$species_full)[3])
mod4 <- lmer(ldmc ~  tdelta + vmin + hli + twi +(1|plot),
             data=d |> filter(species_full == "Lactuca serriola"))
summary(mod4); performance::check_model(mod4); performance::r2(mod4); car::Anova(mod4)

mod5 <- lmer(ldmc ~ height_cm   + vmin + tdelta*species_full + hli + twi+ (1|plot),
             data=d |> filter(species_full %in% c("Ceanothus velutina", "Shepheria canadensis")))
summary(mod5); performance::check_model(mod5); performance::r2(mod5); car::Anova(mod5)
mod6 <- lmer(ldmc ~ height_cm  + vmin+  tdelta*species_full+ hli + twi + (1|plot),
             data=d |> filter(!species_full %in% c("Ceanothus velutina", "Shepheria canadensis")))
summary(mod6); performance::check_model(mod6); performance::r2(mod6); car::Anova(mod6)

library(ggeffects); library(ggpubr)

ggarrange(
  ggeffects::ggpredict(mod4, "tdelta") |> plot(show_residuals = T) + ggtitle("Erigeron canadensis"),
  ggeffects::ggpredict(mod3, "tdelta") |> plot(show_residuals = T) + ggtitle("Lactuca serriola"),
  ggeffects::ggpredict(mod5, c("tdelta", "species_full")) |> plot(show_residuals = T) + ggtitle("Shrubs"),
  ggeffects::ggpredict(mod6, c("tdelta", "species_full")) |> plot(show_residuals = T) + ggtitle("not Shrubs")
)

ggarrange(
  ggeffects::ggpredict(mod4, "twi") |> plot(show_residuals = T) + ggtitle("Erigeron canadensis"),
  ggeffects::ggpredict(mod3, "twi") |> plot(show_residuals = T) + ggtitle("Lactuca serriola"),
  ggeffects::ggpredict(mod5, c("twi", "species_full")) |> plot(show_residuals = T) + ggtitle("Shrubs"),
  ggeffects::ggpredict(mod6, c("twi", "species_full")) |> plot(show_residuals = T) + ggtitle("not Shrubs")
)

ggarrange(
  ggeffects::ggpredict(mod4, "hli") |> plot(show_residuals = T) + ggtitle("Erigeron canadensis"),
  ggeffects::ggpredict(mod3, "hli") |> plot(show_residuals = T) + ggtitle("Lactuca serriola"),
  ggeffects::ggpredict(mod5, c("hli", "species_full")) |> plot(show_residuals = T) + ggtitle("Shrubs"),
  ggeffects::ggpredict(mod6, c("hli", "species_full")) |> plot(show_residuals = T) + ggtitle("not Shrubs")
)
# lmers for sla

varImpPlot(res[[1]], main = unique(d$species_full)[1])
mod1 <- lmer(sla ~ height_cm + tmax +  tdelta + vmin + (1|plot),
             data=d |> filter(species_full == "Ceanothus velutina"))
summary(mod1); performance::check_model(mod1); performance::r2(mod1); car::Anova(mod1)

varImpPlot(res[[4]], main = unique(d$species_full)[4]); res[[4]]
mod2 <- lmer(sla ~ height_cm + tdelta + vmin  + (1|plot),
             data=d |> filter(species_full == "Shepheria canadensis"))
summary(mod2); performance::check_model(mod2); performance::r2(mod2); car::Anova(mod2)

varImpPlot(res[[2]], main = unique(d$species_full)[2])
mod3 <- lmer(sla ~ height_cm + tdelta + vmin +(1|plot),
             data=d |> filter(species_full == "Erigeron canadensis"))
summary(mod3); performance::check_model(mod3); performance::r2(mod4); car::Anova(mod4)

varImpPlot(res[[3]], main = unique(d$species_full)[3])
mod4 <- lmer(sla ~  tdelta + vmin + (1|plot),
             data=d |> filter(species_full == "Lactuca serriola"))
summary(mod4); performance::check_model(mod4); performance::r2(mod4); car::Anova(mod4)

mod5 <- lmer(sla ~ height_cm   + vmin + tdelta*species_full + (1|plot),
             data=d |> filter(species_full %in% c("Ceanothus velutina", "Shepheria canadensis")))
summary(mod5); performance::check_model(mod5); performance::r2(mod5); car::Anova(mod5)
mod6 <- lmer(sla ~ height_cm  + vmin+  tdelta*species_full + (1|plot),
             data=d |> filter(!species_full %in% c("Ceanothus velutina", "Shepheria canadensis")))
summary(mod6); performance::check_model(mod6); performance::r2(mod6); car::Anova(mod6)

library(ggeffects); library(ggpubr)

ggarrange(
  ggeffects::ggpredict(mod4, "tdelta") |> plot(show_residuals = T) + ggtitle("Erigeron canadensis"),
  ggeffects::ggpredict(mod3, "tdelta") |> plot(show_residuals = T) + ggtitle("Lactuca serriola"),
  ggeffects::ggpredict(mod5, c("tdelta", "species_full")) |> plot(show_residuals = T) + ggtitle("Shrubs"),
  ggeffects::ggpredict(mod6, c("tdelta", "species_full")) |> plot(show_residuals = T) + ggtitle("not Shrubs")
)
