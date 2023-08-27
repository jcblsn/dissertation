library(MASS)
library(tidyverse)
library(pracma)
library(RcppNumerical)

(chart_obs <- ls()[str_detect(ls(), "chart_")])
rm(list = chart_obs)

lse_red <- "#E60023"

synthetic_control <- function(X, y) {
  n <- ncol(X)
  
  loss <- function(w) {
    if (ncol(X) != length(w)) {
      stop("Dimensions of X and w are not compatible for matrix multiplication.")
    }
    sqrt(mean((y - X %*% w)^2))
  }
  
  loss_gradient <- function(w) {
    -2 * t(X) %*% (y - X %*% w) / (n * loss(w))
  }
  
  constraints <- function(w, grad) {
    c(sum(w) - 1)
  }
  
  constraints_jacobian <- function(w) {
    rep(1, n)
  }
  
  lower <- rep(0, n)
  upper <- rep(1, n)
  start <- rep(1 / n, n)
  
  res <- nloptr::slsqp(
    x0 = start,
    fn = loss,
    gr = loss_gradient,
    heq = constraints,
    heqjac = constraints_jacobian,
    lower = lower,
    upper = upper
  )
  
  coef_ <- res$par
  mse <- loss(coef_)
  
  predict <- function(X_new) {
    if (ncol(X_new) != length(coef_)) {
      stop("Dimensions of X_new and coef_ are not compatible for matrix multiplication.")
    }
    X_new %*% coef_
  }
  
  list(coef_ = coef_, mse = mse, predict = predict)
}

ffg_raw <- read_csv(paste0(getwd(), "/data/clean/ffg_monthly.csv"))
ffg <- ffg_raw |>
  dplyr::select(t, treatment_period, country, yit) |>
  pivot_wider(id_cols = c(t, treatment_period), names_from = country, names_prefix = "y_", values_from = yit) |>
  dplyr::select(t, y = y_germany, everything()) |>
  filter(as_date(t) < as_date("2023-01-01")) |>
  dplyr::select_if(~ !any(is.na(.))) 
# left_join(
# ffg_raw |> filter(country == "germany") |> dplyr::select(t, x_temp_germany = x_temp)
# )

ffg_scaled <- ffg |> mutate_all(~ (. - min(.)) / (max(.) - min(.)))

train <- ffg_scaled |> filter(treatment_period == 0)
test <- ffg_scaled |> filter(treatment_period == 1)

train_X <- train |> dplyr::select(starts_with("y_"))
test_X <- test |> dplyr::select(starts_with("y_"))
train_y <- train |> pull(y)
test_y <- test |> pull(y)

model <- synthetic_control(as.matrix(train_X), as.matrix(train_y))

tibble(par = colnames(train_X), coef = model$coef_) |>
  arrange(desc(coef)) |>
  mutate(
    par = str_remove_all(par, "y_"),
    par = str_replace_all(par, "_", " "),
    par = str_to_title(par),
    par = if_else(par == "X Temp Germany", "Temperature - Germany", par)
  ) |> 
  write_csv(
    paste0(getwd(), "/data/clean/scm_weights.csv")
  )

train$pred <- as.vector(model$predict(as.matrix(train_X)))
test$pred <- as.vector(model$predict(as.matrix(test_X)))

comb <- bind_rows(train |> mutate(type = "train"), test |> mutate(type = "test"))

comb %>%
  dplyr::select(t, type, y, pred) %>%
  mutate_at(vars(y, pred), ~ . * (max(ffg$y) - min(ffg$y)) + min(ffg$y)) %>%
  group_by(type) %>%
  summarise(
    mse = mean((y - pred)^2),
    mae = mean(abs(y - pred)),
    rmse = sqrt(mean((y - pred)^2))
  ) |>
  write_csv(
    paste0(getwd(), "/data/clean/scm_performance.csv")
  )

# --- Placebo tests -----------------------------------------------------------

post_treatment_rmspe <- function(actual, synthetic) {
  sqrt(mean((actual - synthetic)^2))
}

placebo_rmspes <- numeric()

for (country in colnames(train)[str_detect(colnames(train), "^y")]) {
  placebo_X <- train %>% dplyr::select(starts_with("y"), -all_of(country))
  placebo_y <- train %>% pull(all_of(country))
  
  placebo_model <- synthetic_control(as.matrix(placebo_X), as.matrix(placebo_y))
  
  test_placebo_X <- test %>% dplyr::select(starts_with("y"), -all_of(country))
  test$placebo_pred <- as.vector(placebo_model$predict(as.matrix(test_placebo_X)))
  
  out <- test |>
    mutate_at(vars(all_of(country), placebo_pred), ~ . * (max(ffg$y) - min(ffg$y)) + min(ffg$y))
  
  placebo_rmspes <- c(placebo_rmspes, post_treatment_rmspe(out %>% pull(all_of(country)), out$placebo_pred))
}

out <- test |>
  mutate_at(vars(y, pred), ~ . * (max(ffg$y) - min(ffg$y)) + min(ffg$y))
(germany_rmspe <- post_treatment_rmspe(out$y, out$pred))

hist(placebo_rmspes,
     breaks = 30,
     main = "Distribution of Placebo RMSPEs", xlab = "RMSPE",
     xlim = c(min(c(placebo_rmspes, germany_rmspe)), max(c(placebo_rmspes, germany_rmspe)))
); abline(v = germany_rmspe, col = "red", lwd = 2)

p_value <- mean(placebo_rmspes >= germany_rmspe); print(paste("P-value:", p_value))

# --- Estimate causal effect --------------------------------------------------

bind_rows(
  test |>
    mutate_at(vars(y, pred), ~ . * (max(ffg$y) - min(ffg$y)) + min(ffg$y)) |>
    mutate(delta = y - pred) |>
    pull(delta) |>
    sum() |>
    enframe() |>
    transmute(metric = "Estimated cumulative effect", mwh = value),
  test |>
    mutate_at(vars(y, pred), ~ . * (max(ffg$y) - min(ffg$y)) + min(ffg$y)) |>
    mutate(delta = y - pred) |>
    summarise(
      median = median(delta),
      mean = mean(delta)
    ) |>
    pivot_longer(cols = everything()) |>
    mutate(metric = str_c("Estimated ", name, " effect")) |>
    transmute(metric, mwh = value),
  p_value |>
    enframe() |>
    transmute(metric = "p-value", value = value)
) |> 
  mutate(twh = mwh / 1e6) |>
  write_csv(
    paste0(getwd(), "/data/clean/scm_effect.csv")
  )

# --- Visualizations ----------------------------------------------------------

chart_scm_predicted_monthly <-
  comb |>
  mutate_at(vars(y, pred), ~ . * (max(ffg$y) - min(ffg$y)) + min(ffg$y)) |>
  mutate(
    t = t * (max(ffg$t) - min(ffg$t)) + min(ffg$t),
    t = as_date(t)
  ) %>%
  pivot_longer(cols = c(y, pred)) %>%
  ggplot() +
  # geom_point(
  #     aes(x = as_date(t), y = y, color = type)
  # ) +
  geom_line(
    aes(x = as_date(t), y = value, alpha = name, color = type),
    size = 1
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

chart_scm_predicted_monthly

chart_scm_residuals_monthly <-
  comb %>%
  dplyr::select(t, treatment_period, y, pred) %>%
  mutate_at(vars(y, pred), ~ . * (max(ffg$y) - min(ffg$y)) + min(ffg$y)) |>
  mutate(
    diff = y - pred,
    t = t * (max(ffg$t) - min(ffg$t)) + min(ffg$t),
    t = as_date(t)
  ) %>%
  pivot_longer(-c(t, treatment_period)) %>%
  filter(name == "diff") %>%
  ggplot() +
  # geom_segment(
  #     aes(
  #         x = t,
  #         xend = t,
  #         y = 0,
  #         yend = value,
  #         color = as.factor(1 - treatment_period)
  #     )
  # ) +
  geom_line(
    aes(
      x = t,
      y = value,
      color = as.factor(1 - treatment_period)
    )
  ) +
  geom_hline(yintercept = 0, color = 'darkgray') +
  geom_vline(xintercept = as_date("2021-12-31"), color = "gray", size = 1, linetype = "dotted") +
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
  scale_y_continuous(name = "Point Effect", labels = scales::comma)
chart_scm_residuals_monthly

chart_scm_residuals_cum_monthly <-
  comb %>%
  dplyr::select(t, treatment_period, y, pred) %>%
  mutate_at(vars(y, pred), ~ . * (max(ffg$y) - min(ffg$y)) + min(ffg$y)) %>%
  mutate(
    diff = y - pred,
    t = t * (max(ffg$t) - min(ffg$t)) + min(ffg$t),
    t = as_date(t)
  ) %>%
  pivot_longer(-c(t, treatment_period)) %>%
  filter(name == "diff") |>
  mutate(cumulative = cumsum(if_else(treatment_period == 1, value, 0))) |>
  ggplot() +
  geom_line(aes(x = t, y = cumulative, color = as.factor(1 - treatment_period))) +
  # geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = as_date("2021-12-31"), color = "gray", size = 1, linetype = "dotted") +
  scale_color_manual(
    name = "Group",
    values = rev(c("black", lse_red)),
    labels = c("test", "train")
  ) +
  theme_bw() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", name = NULL) +
  scale_y_continuous(name = "Cumulative effect", labels = scales::comma, limits = c(-20e6, 20e6))
chart_scm_residuals_cum_monthly

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








