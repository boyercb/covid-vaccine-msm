library(tidyverse)
library(odin)
library(patchwork)

sirvd_generator <- odin::odin("1_code/sirvd_stochastic_model.R")

df <-
  map_dfr(c(
    "nu==0.08" = 0.08,
    "nu==0.04" = 0.04,
    "nu==0.02" = 0.02,
    "nu==0.01" = 0.01,
    "nu==0.005" = 0.005,
    "nu==0" = 0
  ), function(nu) {
    model <- map_dfr(
      .x = 1:100, 
      .f = function(iter) {
        sirvd_generator$new(
          S_ini = 1e4,
          mu = 0.1, 
          mu_v = 0.005,
          nu = nu, 
          theta = 0.50, 
          omega = 0.005,
          sigma = 0.01
        )
        as_tibble(model$run(0:365))
      }, .id = "sim")
  }, .id = "nu")

df <- mutate(df, I = Is + Iv, D = Ds + Dv, N = S + I + R + V + D)

labs <- c(
  bquote(nu==800*"/day"),
  bquote(nu==400*"/day"),
  bquote(nu==200*"/day"),
  bquote(nu==100*"/day"),
  bquote(nu==50*"/day"),
  bquote(nu==0*"/day")
)

df |>
  pivot_longer(S:Dv) |>
  ggplot(aes(x = step, y = value, group = fct_cross(sim, name), color = name)) + 
  facet_wrap(~nu) +
  geom_line(linewidth = 0.5, alpha = 0.2) + 
  scale_color_brewer(palette = "Set2") +
  theme_minimal() +
  labs(
    title = "A",
    x = "\ntime",
    y = "infections\n"
  )


