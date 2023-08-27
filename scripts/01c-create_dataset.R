library(tidyverse)

path_base <- getwd()
path_in <- file.path(path_base, "data", "clean")
path_weather <- file.path(path_base, "data", "raw", "weather")

normalize_vars <- function(data, group_var){
  data |>
    group_by({{group_var}}) |>
    mutate(across(where(is.numeric), ~ (.- min(., na.rm = TRUE)) / (max(., na.rm = TRUE) - min(., na.rm = TRUE)))) |>
    ungroup()
}

elec_gen_by_source <- read_csv(file.path(path_in, "entsoe", "elec_gen_by_source_monthly_grouped.csv"))

weather <- read_csv(file.path(path_weather, "daily_temperatures_europe.csv")) |>
  mutate(
    country = str_replace_all(tolower(country), " ", "_"),
    dt = as_datetime(dates)
  ) |>
  select(dt, temp, country)

weather_monthly <- weather |>
  mutate(mth = floor_date(dt, unit = "month")) |>
  filter(mth >= as_date("2015-01-01")) |>
  group_by(country, mth) |>
  summarize(mean_temp = mean(temp, na.rm = TRUE)) |>
  ungroup() |>
  mutate(mth = as_date(mth))

weather_monthly |>
  ggplot(aes(x = mth, y = mean_temp)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~country)

g <- elec_gen_by_source |>
  mutate(
    t0 = if_else(mth == "2021-12-01", 1, 0),
    treatment_period = if_else(mth > "2021-12-31", 1, 0),
    t = as.numeric(mth)
  ) |>
  select(t, t0, treatment_period, yit = mwh, everything(), -mth) |> 
  left_join(
    weather_monthly |>
      mutate(mth = as.numeric(mth),mean_temp = round(mean_temp, 3)) |>
      rename(x_temp = mean_temp, t = mth),
      by = c("country", "t")
  )

complete_date_seq <- seq.Date(as_date("2015-01-01"), as_date("2022-12-01"), by = "month")

(complete_cases <- expand.grid(unique(ffg$country), complete_date_seq) |>
  set_names(c("country", "t")) |>
  mutate(t = as.numeric(t)) |>
  left_join(ffg, by = c("country", "t")) |>
  drop_na() |>
  group_by(country) |>
  count() |>
  filter(n == 96) |>
  pull(country))

ffg <- g |>
  filter(
    category_group == "fossil",
    country %in% complete_cases
  )

ffg |>
  normalize_vars(country) |> 
  ggplot(aes(x = t)) +
  geom_line(aes(y = yit), color = "red", linewidth = 1 / 3) +
  geom_line(aes(y = x_temp), color = "gray", linewidth = 1 / 3) +
  facet_wrap(~country, scales = "free_y")

ffg |>
  mutate(x_temp = round(x_temp, 3)) |>
  write_csv(file.path(path_in, "ffg_monthly.csv"))

g |>
  normalize_vars(var(country, category_group)) |>
  ggplot(aes(x = t)) +
  geom_line(aes(y = yit, color = category_group), linewidth = 1 / 3) +
  facet_wrap(~country, scales = "free_y")

# write
# g |> write_csv(file.path(path_in, "g_monthly.csv"))
# ffg |> write_csv(file.path(path_in, "ffg_monthly.csv"))