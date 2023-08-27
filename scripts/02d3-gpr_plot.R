library(lubridate)
library(tidyverse)
lse_red <- "#E60023"

(chart_obs <- ls()[str_detect(ls(), "chart_")])
rm(list = chart_obs)

df <- read_csv(paste0(getwd(), "/data/clean/gpr_predictions_monthly.csv"))

chart_gpr_predictions_monthly <-
  df |>
  mutate(
    train = ifelse(dt < "2021-12-31", 1, 0) |> as_factor()
  ) |>
  pivot_longer(cols = c(y, y_pred_median)) |>
  ggplot() +
  geom_vline(xintercept = as_date("2021-12-31"), color = "gray", linetype = "dotted") +
  geom_ribbon(
    aes(
      x = dt,
      ymin = y_pred_lower,
      ymax = y_pred_upper
    ),
    alpha = 0.1
  ) +
  geom_line(
    aes(x = dt, y = value, color = train, alpha = name, group = name)
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", name = NULL) +
  scale_y_continuous(
    name = "MWh",
    labels = scales::comma
  ) +
  scale_color_manual(
    name = "Group",
    values = rev(c("black", lse_red)),
    labels = c("test", "train")
  ) +
  scale_fill_manual(
    name = NULL,
    values = rev(c("black", lse_red)),
    labels = c("test", "train")
  ) +
  theme_bw() +
  scale_alpha_manual(
    name = "Type",
    values = c(0.95, 1 / 3),
    labels = c("Predicted", "Observed")
  )
chart_gpr_predictions_monthly

df$residuals <- df$y - df$y_pred_median 
chart_gpr_residuals_monthly <-
  df %>%
  mutate(
    train = ifelse(dt < "2021-12-31", 1, 0) |> as_factor()
  ) |>
  ggplot() +
  geom_hline(yintercept = 0, color = "gray") +
  geom_line(
    aes(
      x = dt,
      y = residuals,
      color = train
    )
  ) +
  geom_ribbon(
    aes(
      x = dt,
      ymin = y_pred_lower - y_pred_median,
      ymax = y_pred_upper - y_pred_median
    ),
    alpha = 0.1
  ) +
  geom_vline(xintercept = as_date("2021-12-31"), color = "gray", linetype = "dotted") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", name = NULL) +
  scale_y_continuous(name = "Point Effect", labels = scales::comma) +
  scale_color_manual(
    name = "Group",
    values = rev(c("black", lse_red)),
    labels = c("test", "train")
  ) +
  theme_bw()
chart_gpr_residuals_monthly

df$cum_residuals <- cumsum(df$residuals)
chart_gpr_residuals_cum_monthly <-
  df %>%
  mutate(
    train = ifelse(dt < "2021-12-31", 0, 1) |> as_factor(),
    cumulative = cumsum(if_else(train == 1, residuals, 0))
  ) |>
  ggplot() +
  geom_hline(yintercept = 0, color = "gray") +
  geom_line(aes(x = dt, y = cumulative, color = train)) +
  geom_ribbon(
    aes(
      x = dt,
      ymin = if_else(train==0, 0, y_pred_lower - y_pred_median + cumulative),
      ymax = if_else(train==0, 0, y_pred_upper - y_pred_median + cumulative)
    ),
    alpha = 0.1
  ) +
  geom_vline(xintercept = as_date("2021-12-31"), color = "gray", linetype = "dotted") +
  scale_color_manual(
    name = "Group",
    values = rev(c(lse_red, "black")),
    labels = c("test", "train")
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", name = NULL) +
  scale_y_continuous(name = "Cumulative Effect", labels = scales::comma, limits = c(-20e6, 20e6)) +
  theme_bw()
chart_gpr_residuals_cum_monthly

(chart_obs <- ls()[str_detect(ls(), "chart_")])
chart_obs |>
  walk(
    function(x) {
      # if (x != "chart_obs") {
      if (str_detect(x, "chart_gpr")) {
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

