library(brms)

load("onset_data_conf_0.75_det_10_grid_10.RData")

final$avg_rad <- as.numeric(scale(log1p(final$avg_rad)))

m <- brms::brm(
  value ~ 1 + avg_rad + ( 1 + avg_rad | group ),
  data = final, 
  family = gaussian(link = "identity"), 
  prior = c(
    prior(normal(0, 2), class = Intercept), 
    prior(normal(0, 0.5), class = b), 
    prior(exponential(1), class = sd), 
    prior(lkj(2), class = cor)),
  iter = 4000, 
  warmup = 2000, 
  chains = 3, 
  cores = 3, 
  thin = 1, 
  file = "onset_result_conf_0.75_det_10_grid_10"
)
