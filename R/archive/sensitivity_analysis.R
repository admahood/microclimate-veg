library(tidyverse)
library(topomicro)

mef_clim <- read_csv("data/cleaned_bm/mef_jul23.csv") |>
  bind_rows(read_csv("data/cleaned_bm/mef_febmay24.csv")); glimpse(mef_clim)

# mef16 only has a few weeks of data
# ggplot(mef_clim |> filter(id == "mef16")) +
#   geom_line(aes(x=dt, y=temperature_c))
val_clim <- read_csv("data/cleaned_bm/vall_dec22.csv") |>
  bind_rows(read_csv("data/cleaned_bm/vall_aug23.csv")); glimpse(vall_clim)

ben_clim <- read_csv("data/cleaned_bm/bennett_all.csv")

counter <- 1
result <- list()
for(m in unique(ben_clim$month)) {
  for(y in unique(ben_clim$year)){
  nrw <- ben_clim |>
    dplyr::filter(month == m, year == y) |>
    nrow()

  result[[counter]] <- ben_clim |>
    dplyr::filter(month == m, year == y) |>
    topomicro::plotwise_summary() |>
    dplyr::mutate(month = m,
                  year = y,
                  samples = nrw) |>
    tibble::as_tibble(rownames = "plot")
  counter <- counter + 1
}}

result |>
  bind_rows() |>
  ggplot(aes(x=plot, y=tdelta)) +
  geom_boxplot() +
  geom_text(aes(label = month |> paste(samples), color = as.factor(month))) +
  geom_hline(yintercept = 12.5) +
  ggtitle("how stable is DTR? (Bennett)")

counter <- 1
result <- list()
for(m in unique(mef_clim$month)) {
  for(y in unique(mef_clim$year)){
    nrw <- mef_clim |>
      dplyr::filter(month == m, year == y) |>
      nrow()

    result[[counter]] <- mef_clim |>
      dplyr::filter(month == m, year == y) |>
      topomicro::plotwise_summary() |>
      dplyr::mutate(month = m,
                    year = y,
                    samples = nrw) |>
      tibble::as_tibble(rownames = "plot")
    counter <- counter + 1
  }}

result |>
  bind_rows() |>
  ggplot(aes(x=plot, y=tdelta)) +
  geom_boxplot() +
  geom_text(aes(label = month |> paste(samples), color = as.factor(month))) +
  geom_hline(yintercept = 12.5) +
  ggtitle("how stable is DTR? (Manitou)")

counter <- 1
result <- list()
for(m in unique(val_clim$month)) {
  for(y in unique(val_clim$year)){
    nrw <- val_clim |>
      dplyr::filter(month == m, year == y) |>
      nrow()

    result[[counter]] <- val_clim |>
      dplyr::filter(month == m, year == y) |>
      topomicro::plotwise_summary() |>
      dplyr::mutate(month = m,
                    year = y,
                    samples = nrw) |>
      tibble::as_tibble(rownames = "plot")
    counter <- counter + 1
  }}

result |>
  bind_rows() |>
  ggplot(aes(x=plot, y=tdelta)) +
  geom_boxplot() +
  geom_text(aes(label = month |> paste(samples), color = as.factor(month))) +
  geom_hline(yintercept = 12.5) +
  ggtitle("how stable is DTR? (Valles Caldera)")

# sensitivity analysis by day - sequential =================

daily_dtr <- read_csv(file = "data/vall_stations_cleaned.csv") |>
  group_by(date_mmddyyyy, site) |>
  reframe(dtr = max(temp_c) - min(temp_c))

resdf <- data.frame(n=NA, mean = NA, sd = NA, site = NA)
c = 1
for(i in 1:600){
  for(s in unique(daily_dtr$site)){
    x <- daily_dtr |>
      filter(site == s) |>
      slice(1:i) |>
      summarise(mean = mean(dtr), sd = sd(dtr))
    resdf[c,1] <- i
    resdf[c,2] <- x$mean
    resdf[c,3] <- x$sd
    resdf[c,4] <- s
    c=c+1
  }}

ggpubr::ggarrange(
  ggplot(resdf |> na.omit(), aes(x=n, y=mean, color = site)) +
    geom_line() +
    xlab("Number of Days") +
    scale_x_continuous(breaks = c(100,200,300,400,500)) +
    ggtitle("sequentially sampled days") +
    theme_clean() +
    theme(legend.position = "none")
  ,
  ggplot(resdf |> na.omit(), aes(x=n, y=sd, color = site)) +
    geom_line() +
    scale_x_continuous(breaks = c(100,200,300,400,500)) +
    xlab("Number of Days") +
    theme_clean() +
    theme(legend.position = c(1,0),
          legend.justification = c(1,0)), ncol =1) |>
  ggsave(filename = "out/sensitivity_mean_sd_sequential.png", bg="white")

# sensitivity analysis by day - random =================

daily_dtr <- read_csv(file = "data/vall_stations_cleaned.csv") |>
  group_by(date_mmddyyyy, site) |>
  reframe(dtr = max(temp_c) - min(temp_c))

resdf <- data.frame(n=NA, mean = NA, sd = NA, site = NA)
c = 1
for(i in 1:600){
  for(s in unique(daily_dtr$site)){
  x<- sample_n(daily_dtr |> filter(site == s), i, replace = T) |>
    summarise(mean = mean(dtr), sd = sd(dtr))
  resdf[c,1] <- i
  resdf[c,2] <- x$mean
  resdf[c,3] <- x$sd
  resdf[c,4] <- s
  c=c+1
}}

ggpubr::ggarrange(
ggplot(resdf |> na.omit(), aes(x=n, y=mean, color = site)) +
  geom_line() +
  scale_x_continuous(breaks = c(100,200,300,400,500)) +
  xlab("Number of Days") +
  ggtitle("randomly sampled days") +
  theme_clean() +
  theme(legend.position = "none")
,
ggplot(resdf |> na.omit(), aes(x=n, y=sd, color = site)) +
  geom_line() +
  scale_x_continuous(breaks = c(100,200,300,400,500)) +
  xlab("Number of Days") +
  theme_clean() +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0)), ncol =1) |>
  ggsave(filename = "out/sensitivity_mean_sd_random.png", bg="white")

# t-tests - random and sequential

library(geomtextpath)
library(ggthemes)

resdf_t <- data.frame(n=NA, hv_red = NA, hv_vg = NA, vg_red = NA)
i=1
for(n in 5:100){
  d <- daily_dtr |>
    pivot_wider(names_from = site, values_from = dtr) |>
    slice(1:n)

  resdf_t[i,1] <- n
  resdf_t[i,2] <- t.test(d$HiddenValley, d$Redondo)$p.value
  resdf_t[i,3] <- t.test(d$HiddenValley, d$ValleGrande)$p.value
  resdf_t[i,4] <- t.test(d$ValleGrande, d$Redondo)$p.value
  i=i+1
}

resdf_tr <- data.frame(n=NA, hv_red = NA, hv_vg = NA, vg_red = NA)
i=1
for(n in 5:100){
  d <- daily_dtr |>
    pivot_wider(names_from = site, values_from = dtr) |>
    sample_n(n, replace = F)

  resdf_tr[i,1] <- n
  resdf_tr[i,2] <- t.test(d$HiddenValley, d$Redondo)$p.value
  resdf_tr[i,3] <- t.test(d$HiddenValley, d$ValleGrande)$p.value
  resdf_tr[i,4] <- t.test(d$ValleGrande, d$Redondo)$p.value
  i=i+1
}

p1<- resdf_tr |>
  pivot_longer(cols = c("hv_red", "hv_vg", "vg_red"), names_to = 'comparison', values_to = "p") |>
ggplot(aes(x=n, y=p, color = comparison)) +
  geom_line() +
  geom_hline(yintercept = c(.05, .01, 0.001), lty = 3, lwd=1) +
  scale_y_log10(breaks = c(0.05, 0.01, 0.001, 1e-10, 1e-22),
                labels = c('0.05', '0.01', '0.001', '1e-10','1e-22')) +
  xlab("Number of Days") +
  ylab("P-Values From T Tests") +
  ggtitle("Random Sample (no replacement)") +
  theme_clean() +
  theme(legend.position = c(0,0),
        legend.justification = c(0,0))

p2 <- resdf_t |>
  pivot_longer(cols = c("hv_red", "hv_vg", "vg_red"), names_to = 'comparison', values_to = "p") |>
  ggplot(aes(x=n, y=p, color = comparison)) +
  geom_line() +
  geom_hline(yintercept = c(.05, .01, 0.001), lty = 3, lwd=1) +
  scale_y_log10(breaks = c(0.05, 0.01, 0.001, 1e-10, 1e-22),
                labels = c('0.05', '0.01', '0.001', '1e-10','1e-22')) +
  xlab("Number of Days") +
  ylab("P-Values From T Tests") +
  ggtitle("Days Tested in Sequential Order") +
  theme_clean() +
  theme(legend.position = c(0,0),
        legend.justification = c(0,0))


ggarrange(p1, p2, ncol = 1) |> ggsave(filename = "out/sensitivity_t_tests.png",
                                      bg="white", width=8, height=10)
