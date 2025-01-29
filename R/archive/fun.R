bm_summary <- function(l, exclude = NA, start_date = NA, end_date = NA, min_t = NA){
  stuff <- list()
  for (i in 1:length(l)){
    print(l[i])
    stuff[[i]] <- bm_prep(filename = l[i])
  }

  d <- bind_rows(stuff) |>
    mutate(number = str_extract(id, "\\d+") %>% as.numeric(),
           vpd_kPa = topomicro::get_vpd(rh=humidity_pct, temp_c = temperature_c)/10,
           id = str_to_lower(id) %>% str_remove_all(" "))
  if(!is.na(end_date)) d <- d |> filter(dt < as.Date(end_date))
  if(!is.na(start_date)) d <- d |> filter(dt > as.Date(start_date))
  if(!is.na(min_t)) d <- d |> filter(temperature_c > min_t)
  if(!is.na(exclude[1])){
    d <- d |>
      filter(!id %in% exclude)
  }

  pt <- d |>
    ggplot(aes(x=dt, y=temperature_c)) +
    geom_line() +
    facet_wrap(~id, ncol=1) +
    ggtitle("Temperature (c)")

  pr <- d |>
    ggplot(aes(x=dt, y=humidity_pct)) +
    geom_line() +
    facet_wrap(~id, ncol=1) +
    ggtitle("Relative Humidity (pct)")

  dp <- d |>
    ggplot(aes(x=dt, y=dewpoint_c)) +
    geom_line() +
    facet_wrap(~id, ncol=1) +
    ggtitle("Dewpoint (c)")

  dv <- d |>
    ggplot(aes(x=dt, y=vpd_kPa)) +
    geom_line() +
    facet_wrap(~id, ncol=1) +
    ggtitle("VPD (kPA)")

  d_all <- d |>
    pivot_longer(cols = c(temperature_c, humidity_pct, vpd_kPa, dewpoint_c)) |>
    ggplot(aes(x=dt, y=value, color = as.factor(id))) +
    geom_line(alpha = 0.5) +
    facet_wrap(~name, scales = "free", ncol=1) +
    scale_color_viridis_d() +
    ggtitle("higher number = higher elevation")

  return(list(temp_c = pt,
              rh_pct = pr,
              dewp_c = dp,
              vpd_kPa = dv,
              all = d_all,
              data = d))
}
