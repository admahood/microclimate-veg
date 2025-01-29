# test hemispher

library(hemispheR)

fish <- hemispheR::import_fisheye("data/fish_test.jpg", display = TRUE)

res <- binarize_fisheye(fish, display = TRUE) |>
  gapfrac_fisheye(display = TRUE) |>
  canopy_fisheye()

res
