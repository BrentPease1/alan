library(tibble)
library(dplyr)
library(tidyr)
library(brms)

sites_per_block <- 5
days_per_week <- 14
nsites <- 50
nblocks <- floor(nsites / sites_per_block)
ndays <- 70
nweeks <- floor(ndays/days_per_week)
nspecies <- 20
mu_beta0 <- 0
sd_beta0 <- 0.5
mu_beta1 <- -0.15
sd_beta1 <- 0.2

beta0 <- rnorm( nblocks * nweeks * nspecies, mu_beta0, sd_beta0 )
beta1 <- rnorm( nblocks * nweeks * nspecies, mu_beta1, sd_beta1 )

day_key <- tidyr::expand_grid(
  week = 1:nweeks, 
  day = 1:days_per_week)

block_key <- tibble::tibble(
  site = 1:nsites, 
  block = rep(1:nblocks, each = sites_per_block))

alan <- runif( nsites, -2, 2)

dat <- tidyr::expand_grid(species = 1:nspecies, 
            block = 1:nblocks, 
            week = 1:nweeks) |> 
  tibble::add_column(beta0 = beta0, 
                     beta1 = beta1) |> 
  dplyr::full_join(block_key) |> 
  dplyr::full_join(day_key) |> 
  dplyr::select(species, site, block, day, week, beta0, beta1) |> 
  dplyr::rowwise() |> 
  dplyr::mutate( epsilon = rnorm(1, 0, 0.1)) |> 
  dplyr::full_join(
    tibble::tibble(
      site = 1:nsites, 
      alan = alan)) |> 
  dplyr::mutate(y = beta0 + beta1 * alan + epsilon) |> 
  dplyr::group_by(species, site, week) |> 
  dplyr::mutate(omit = rbinom(1, 1, 0.85)) |> 
  dplyr::filter(omit == 0) |> 
  dplyr::group_by(species, block, week) |> 
  dplyr::mutate(id = cur_group_id()) |> 
  dplyr::ungroup()

m <- brms::brm(
  y ~ 1 + alan + (1 + alan | id),
  data = dat, 
  family = gaussian(link = "identity"), 
  prior = c(
    prior(normal(0, 1), class = Intercept), 
    prior(normal(0, 0.25), class = b), 
    prior(exponential(1), class = sd), 
    prior(lkj(2), class = cor)
  ),
  iter = 4000, 
  warmup = 2000, 
  chains = 3, 
  cores = 3, 
  thin = 1,
  file = "alan_test"
)