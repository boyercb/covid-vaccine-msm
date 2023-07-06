

library(tidyverse)
library(odin)
library(patchwork)

sirvd_generator <- odin::odin("sirvd_model.R")

df <-
  map_dfr(c(
    "nu==0.08" = 0.08,
    "nu==0.04" = 0.04,
    "nu==0.02" = 0.02,
    "nu==0.01" = 0.01,
    "nu==0.005" = 0.005,
    "nu==0" = 0
  ), function(nu) {
    model <- sirvd_generator$new(
      S_ini = 1e4,
      mu = 0.1, 
      mu_v = 0.005,
      nu = nu, 
      theta = 0.50, 
      omega = 0.005,
      sigma = 0.01
    )
  as_tibble(model$run(0:365))
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

p1 <- ggplot(df, aes(x = step, y = I, color = nu)) + 
  geom_line(linewidth = 1.25) + 
  scale_color_viridis_d(
    name = bquote("vaccination rate ("*nu*")"),
    labels = rev(labs)
  ) +
  theme_minimal() +
  labs(
    title = "A",
    x = "\ntime",
    y = "infections\n"
  )

df <- df |> 
  group_by(nu) |> 
  mutate(cum_I = 1e4 - S+V) |> 
  ungroup()

p2 <- ggplot(df, aes(x = step, y = cum_I, color = nu)) + 
  geom_line(linewidth = 1.25) + 
  scale_color_viridis_d(
    name = bquote("vaccination rate ("*nu*")"),
    labels = rev(labs)
  ) +
  theme_minimal() +
  labs(
    x = "\ntime",
    y = "cumulative infections\n"
  )

df <- df |> group_by(nu) |> mutate(diff_D = c(0, diff(D))) |> ungroup()

p3 <- ggplot(df, aes(x = step, y = diff_D, color = nu)) + 
  geom_line(linewidth = 1.25) + 
  scale_color_viridis_d(
    name = bquote("vaccination rate ("*nu*")"),
    labels = rev(labs)
  ) +
  theme_minimal() +
  labs(
    x = "\ntime",
    y = "deaths\n"
  )

p4 <- ggplot(df, aes(x = step, y = D)) + 
  geom_line(aes(color = nu), linewidth = 1.25) + 
  scale_color_viridis_d(
    name = bquote("vaccination rate ("*nu*")"),
    labels = rev(labs)
  ) +
  theme_minimal() +
  labs(
    title = "B",
    x = "\ntime",
    y = "cumulative deaths\n"
  )


df_wide <- 
  df |>
  group_by(nu) |>
  mutate(
    dDs = c(0, diff(Ds)),
    dDv = c(0, diff(Dv)),
    dD = c(0, diff(D)),
    d_star = (dDs/(N-(V + Iv)) - dDv/(V + Iv)) * (N-(V + Iv))
  ) |> 
  select(nu, step, d_star, dD) |>
  filter(nu %in% c("nu==0.08", "nu==0")) |>
  ungroup() |>
  group_by(step) |>
  summarise( 
    d_star = first(replace(d_star, is.nan(d_star), 0)),
    d_true = last(dD) - first(dD)
  ) |>
  mutate(
    d_star_cum = cumsum(d_star),
    d_true_cum = cumsum(d_true)
  )

p5 <- ggplot(df_wide, aes(x = step, y = d_true)) + 
  geom_line(linewidth = 1.25) + 
  geom_line(
    aes(y = d_star),
    linetype = "dashed",
    color = "gray70",
    linewidth = 1.25
  ) +
  theme_minimal() +
  labs(
    title = "C",
    x = "\ntime",
    y = "excess deaths\n"
  )

p6 <- ggplot(df_wide, aes(x = step, y = d_true_cum)) + 
  geom_line(linewidth = 1.25) + 
  geom_line(
    aes(y = d_star_cum),
    linetype = "dashed",
    color = "gray70",
    linewidth = 1.25
  ) +
  theme_minimal() +
  labs(
    x = "\ntime",
    y = "cumulative excess deaths\n"
  )

# p5 + p6

(p1 | p4) / p5 +
  plot_layout(guides = 'collect') & 
  theme(legend.position = 'bottom', plot.title = element_text(face = "bold"))

