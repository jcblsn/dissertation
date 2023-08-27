library(tidyverse)
library(lubridate)
library(plm)
library(broom)
library(janitor)

lse_red <- "#E60023"
path_data <- file.path(getwd(), "data", "clean")
path_plots <- file.path(getwd(), "plots")

ffg <- read_csv(file.path(path_data, "ffg_monthly.csv")) |>
    filter(as_date(t) < as_date("2023-01-01"))

plot_model_results <- function(mod) {
    dat <- df |>
        drop_na() |>
        mutate(
            t = as_date(t),
            pred = mod$fitted.values,
            treatment_group = as_factor(treatment_group)
        ) 

    ggplot(data = dat) +
        geom_line(aes(x = t, y = value, group = str_c(i, name), color = treatment_group, alpha = name), size = 3 / 4, data = dat |> filter(treatment_period == 0) |> pivot_longer(cols = c(yit, pred))) +
        geom_line(aes(x = t, y = value, group = str_c(i, name), color = treatment_group, alpha = name), size = 3 / 4, data = dat |> filter(treatment_period == 1) |> pivot_longer(cols = c(yit, pred))) +
        geom_vline(xintercept = as_date("2021-12-31"), color = "black", size = 1, linetype = "dotted") +
        theme_bw() +
        scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
        scale_y_continuous(name = "Monthly fossil fuel electricity generation (MWh)", labels = scales::comma) +
        scale_color_manual(name = "Group", values = c("#1aac00", "#001aac"), labels = c("control", "treatment")) +
        scale_linetype_manual(name = "Type", values = c("solid", "dashed"), labels = c("Predicted", "Observed")) +
        scale_alpha_manual(name = "Type", values = c(0.95, 1 / 3), labels = c("Predicted", "Observed"))
}

normalized_ffg <- ffg |> 
    filter(treatment_period == 0) |> 
    group_by(country) |> 
    mutate(across(c(yit, x_temp), ~ (. - min(.)) / (max(.) - min(.)))) |> 
    ungroup()


mod_data <- normalized_ffg |>
    select(t, country, yit) |>
    pivot_wider(names_from = country, names_prefix = "y_", values_from = yit) |>
    rename(y = y_germany) |>
    pivot_longer(-c(t, y)) |>
    mutate(diff = y - value)

mod <- mod_data |>
    group_by(name) |>
    summarize(coefs = list(broom::tidy(lm(diff ~ t)))) |>
    unnest(coefs) |>
    janitor::clean_names() |>
    filter(term == "t")

(X_vars <- mod |>
    mutate(diff_than_zero = p_value < 5e-2) |>
    filter(!diff_than_zero) |>
    pull(name))

df <- ffg |>
    filter(country %in% c("germany", str_remove(X_vars, "y_"))) |>
    mutate(
        treatment_group = if_else(country == "germany", 1, 0),
        tq = as_factor(lubridate::quarter(as_date(t))),
        ty = as_factor(year(as_date(t)))
    ) |>
    rename(i = country)

did_mod <- lm(
    yit ~ treatment_period * treatment_group + i * x_temp,
    data = df
)

summary(did_mod)

(chart_did_monthly <- plot_model_results(did_mod))

did_performance <- df |>
    drop_na() |>
    mutate(pred = predict(did_mod), treatment_period = if_else(treatment_period == 1, "train", "test")) |>
    filter(i == "germany") |>
    group_by(treatment_period) |>
    summarize(
        mse = mean((yit - pred)^2),
        mae = mean(abs(yit - pred)),
        rmse = sqrt(mse)
    ) |>
    ungroup() |>
    mutate(across(where(is.numeric), ~ round(., 2)))

results <- tibble()
for (k in X_vars) {
    mod_temp <- lm(
        yit ~ treatment_period * treatment_group + i * x_temp,
        data = df |> filter(i != str_remove(k, "y_"))
    )
    results <- bind_rows(results, broom::tidy(mod_temp) |> mutate(excl_var = k))
}
results |> filter(str_detect(term, "_period:t")) |> pull(estimate) |> summary()

chart_parallel_trends <- normalized_ffg |>
    select(t, country, yit) |>
    pivot_wider(names_from = country, names_prefix = "y_", values_from = yit) |>
    rename(y = y_germany) |>
    pivot_longer(-c(t, y)) |>
    ggplot(aes(x = as_date(t), y = y - value)) +
    geom_line() +
    geom_smooth(method = "lm") +
    facet_wrap(~name, nrow = 3) +
    theme_bw() +
    scale_y_continuous(NULL) +
    scale_x_date(NULL, date_breaks = "3 years", date_labels = "%Y")

chart_did_residuals_monthly <- df |>
    drop_na() |>
    mutate(predicted = predict(did_mod), residuals = yit - predicted) |>
    filter(treatment_group == 1) |>
    ggplot() +
    geom_hline(yintercept = 0, color = "darkgray") +
    geom_vline(xintercept = as_date("2021-12-31"), color = "gray", size = 1, linetype = "dotted") +
    geom_line(aes(x = as_date(t), y = residuals, color = factor(treatment_group))) +
    theme_bw() +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", name = NULL) +
    scale_y_continuous(name = "Residuals (MWh)", labels = scales::comma) +
    scale_color_manual(name = "Group", values = c("#1aac00", "#001aac"), labels = c("control", "treatment"))

ggsave(file.path(path_plots, "chart_did_monthly.png"), chart_did_monthly, width = 7, height = 5)
ggsave(file.path(path_plots, "chart_parallel_trends.png"), chart_parallel_trends, width = 7, height = 7)
ggsave(file.path(path_plots, "chart_did_residuals_monthly.png"), chart_did_residuals_monthly, width = 7, height = 5)


"chart_did_monthly" |>
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

"chart_parallel_trends" |>
    walk(
        function(x) {
            if (x != "chart_obs") {
                title_to_path <- deparse(x)
                ggsave(
                    str_c(
                        getwd(), "/plots/", lubridate::today(), "-", str_remove_all(title_to_path, '"'), ".png"
                    ),
                    eval(str2lang(x)),
                    width = 4,
                    height = 3,
                    scale = 2,
                    bg = "white"
                )
            }
        }
    )