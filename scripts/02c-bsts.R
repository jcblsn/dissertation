library(tidyverse)
library(lubridate)
library(CausalImpact)

(chart_obs <- ls()[str_detect(ls(), "chart_")])
rm(list = chart_obs)

lse_red <- "#E60023"

ffg <- read_csv(paste0(
  getwd(), "/data/clean/", # "20230810-",
  "ffg_monthly.csv"
))

ffg <-
  ffg |>
  select(t, country, yit) |>
  pivot_wider(
    id_cols = t, names_from = country, names_prefix = "y_", values_from = yit
  ) |>
  select(t, y = y_germany, everything()) |>
  left_join(
    ffg |> filter(country == "germany") |> select(t, x_temp)
  ) |>
  filter(as_date(t) < as_date("2023-01-01")) |> # , as_date(t) > as_date("2020-01-01")) |>
  select_if(~ !any(is.na(.))) |>
  rownames_to_column(var = "id") |>
  mutate(
    id = as.numeric(id),
    pre.p = (id == 1) | as.character(as.Date(t, origin = "1970-01-01")) == "2021-12-01",
    post.p = (id == max(id) | as.character(as.Date(t, origin = "1970-01-01")) == "2022-01-01")
  )
(pre_period <- ffg$id[ffg$pre.p])
(post_period <- ffg$id[ffg$post.p])

times <- as.Date(ffg$t, origin = "1970-01-01")
ffgts <- zoo(as.matrix(ffg |> select(starts_with("y_"), starts_with("x_"))), times)

impact <- CausalImpact(
  ffgts,
  pre.period = times[pre_period],
  post.period = times[post_period]
)


summary(impact)
(chart_broderson <- plot(impact) + theme_bw())

cat(impact$report)

title_to_path <- "chart_broderson_monthly"
ggsave(
  str_c(
    getwd(), "/plots/", lubridate::today(), "-", title_to_path, ".png"
  ),
  chart_broderson,
  width = 4,
  height = 3,
  scale = 5 / 2,
  bg = "white"
)

# --- Visualizations ----------------------------------------------------------

# get residuals
df <- impact$series |>
  as.data.frame() |>
  rownames_to_column(var = "t") |>
  mutate(type = if_else(as_date(t) < as_date("2021-12-31"), "train", "test"), t = as_date(t)) |>
  as_tibble()

chart_bsts_predicted_monthly <-
  df |>
  pivot_longer(cols = c(response, point.pred)) |>
  ggplot() +
  geom_line(
    aes(x = as_date(t), y = value, alpha = name, color = type),
    size = 1
  ) +
  geom_ribbon(
    aes(
      x = t,
      ymin = point.pred.lower,
      ymax = point.pred.upper
    ),
    alpha = 0.1
  ) +
  geom_vline(xintercept = as_date("2021-12-31"), color = "gray", size = 1, linetype = "dotted") +
  theme_bw() +
  theme_bw() +
  # labs(subtitle = str_c(mod_desc_str, collapse = "\n")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", name = NULL) +
  scale_y_continuous(name = "Monthly electricity generation (MWh)", labels = scales::comma) +
  scale_color_manual(
    name = "Group",
    values = rev(c("black", lse_red)),
    labels = c("test", "train")
  ) +
  scale_alpha_manual(
    name = "Type",
    values = c(0.95, 1 / 3),
    labels = c("Predicted", "Observed")
  )

chart_bsts_predicted_monthly

chart_bsts_residuals_monthly <-
  df |>
  # pivot_longer(cols = c(response, point.pred)) |>
  mutate(t = as_date(t), diff = response - point.pred ) |>
  ggplot() +
  # geom_segment(
  #     aes(
  #         x = t,
  #         xend = t,
  #         y = 0,
  #         yend = diff,
  #         color = type
  #     )
  # ) +
  geom_hline(yintercept = 0, color = "darkgray") +
  geom_vline(xintercept = as_date("2021-12-31"), color = "gray", size = 1, linetype = "dotted") +
  geom_line(
    aes(
      x = t,
      y = point.effect,
      color = type
    )
  ) +
  geom_ribbon(
    aes(
      x = t,
      ymin = point.effect.lower,
      ymax = point.effect.upper
    ),
    alpha = 0.1
  ) +
  scale_color_manual(
    name = "Group",
    values = rev(c("black", lse_red)),
    labels = c("test", "train")
  ) +
  scale_color_manual(
    name = "Group",
    values = rev(c("black", lse_red)),
    labels = c("test", "train")
  ) +
  scale_alpha_manual(
    name = "Type",
    values = c(0.95, 1 / 3),
    labels = c("Predicted", "Observed")
  ) +
  theme_bw() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", name = NULL) +
  scale_y_continuous(name = "Point effect", labels = scales::comma)
chart_bsts_residuals_monthly

chart_bsts_residuals_cum_monthly <-
  df |>
  # pivot_longer(cols = c(response, point.pred)) |>
  mutate(t = as_date(t), diff = point.pred - response) |>
  mutate(cumulative = cumsum(diff)) |>
  ggplot() +
  geom_vline(xintercept = as_date("2021-12-31"), color = "gray", size = 1, linetype = "dotted") +
  geom_line(aes(x = t, y = cum.effect, color = type)) +
  geom_ribbon(
    aes(
      x = t,
      ymin = cum.effect.lower,
      ymax = cum.effect.upper
    ),
    alpha = 0.1
  ) +
  # geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(
    name = "Group",
    values = rev(c("black", lse_red)),
    labels = c("test", "train")
  ) +
  theme_bw() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", name = NULL) +
  scale_y_continuous(name = "Cumulative effect", labels = scales::comma)
chart_bsts_residuals_cum_monthly

(chart_obs <- ls()[str_detect(ls(), "chart_")])
chart_obs |>
  walk(
    function(x) {
      if (x != "chart_obs") {
        title_to_path <- deparse(x)
        ggsave(
          str_c(
            getwd(), "/plots/", lubridate::today(), "-", str_remove_all(title_to_path, '"'), ".png"
          ),
          eval(str2lang(x)),
          width = 16 / 4,
          height = 9 / 4,
          scale = 2,
          bg = "white"
        )
      }
    }
  )

# --- Performance ----------------------------------------------------------

df |>
  mutate(
    Treatment = if_else(as_date(t) < as_date("2021-12-31"), "pre", "post"),
    t = as_date(t)
  ) |>
  group_by(Treatment) |>
  summarise(
    mse = mean((response - point.pred)^2),
    mae = mean(abs(response - point.pred)),
    rmse = sqrt(mse)
  ) |>
  write_csv(
    paste0(getwd(), "/data/clean/", "bsts_performance.csv")
  )
