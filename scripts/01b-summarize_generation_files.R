library(tidyverse)
library(entsoeapi)
library(data.table)
library(arrow)
library(lubridate)

base_path <- getwd()
path_out <- file.path(base_path, "data", "raw", "entsoe", "generation")
path_in <- file.path(base_path, "data", "raw", "entsoe", "generation-weekly")

dir.create(path_out, recursive = TRUE, showWarnings = FALSE)

eic <- en_eic()
selected_countries <- eic |>
  filter(AreaTypeCode == "CTY")

file_names <- tibble(full = list.files(path_in)) |>
  mutate(
    year = str_sub(full, 1, 4),
    country = str_extract(full, "(?<=for_)..")
  )

countries <- file_names$country |>
  enframe(name = NULL, value = "MapCode") |>
  left_join(selected_countries |> select(AreaName, MapCode), by = "MapCode") |>
  distinct() |>
  mutate(country = str_replace_all(tolower(AreaName), " ", "_"))

if (length(list.files(path_out))<2) {
  for (i in seq_len(nrow(countries))) {
    file_names |>
      filter(country == countries$MapCode[i]) |>
      pull(full) |>
      map_dfr(~ fread(file.path(path_in, .x), colClasses = "character", fill = TRUE)) |>
      transmute(
        eic,
        resource_psr_type_def,
        resolution,
        quantity_measure_unit,
        mwh = case_when(
          resolution == "PT60M" ~ as.numeric(quantity),
          resolution == "PT30M" ~ as.numeric(quantity) * 0.5,
          resolution == "PT15M" ~ as.numeric(quantity) * 0.25,
          TRUE ~ NA_real_
        ),
        dt = start
      ) |>
      filter(mwh > 0) |>
      write_parquet(file.path(path_out, paste0("2015_through_202307_", countries$country[i], ".parquet")))
  }
}

energy <- 
  list.files(path_out)[str_detect(list.files(path_out), "2015_through_202307")] |> 
  map_dfr(~ {
    arrow::read_parquet(file.path(path_out, .x)) |>
      mutate(dt = as_datetime(dt), 
             d = date(dt), 
             m = month(dt), 
             y = year(dt)) |>
      group_by(eic, m, y, resource_psr_type_def) |>
      summarise(
        mwh = sum(mwh),
        resolution_flag = n_distinct(resolution),
        resolution_vals = str_c(unique(resolution), collapse = ", "),
        unit_flag = n_distinct(quantity_measure_unit)
      ) |>
      ungroup()
  }) |>
  dplyr::filter(mwh > 0)

# checks
energy |>
  group_by(eic) |>
  summarise(
    n = n(),
    mwh_total = sum(mwh),
    resolution_flag = n_distinct(resolution_flag),
    resolution_vals = str_c(unique(resolution_vals), collapse = ", "),
    unit_flag = n_distinct(unit_flag)
  ) |>
  ungroup() |>
  arrange(desc(resolution_flag))

summary <- energy |>
  filter(eic != "") |>
  mutate(dt = ymd(paste(y, m, "01"))) |>
  group_by(eic) |>
  summarise(
    mwh_total = sum(mwh, na.rm = TRUE),
    n_periods = n_distinct(dt),
    mwh_avg = round(mwh_total / n_periods, 1),
    min_date = min(dt),
    max_date = max(dt)
  ) |>
  left_join(selected_countries |> select(AreaCode, AreaName) |>
              mutate(country = str_replace_all(tolower(AreaName), " ", "_")),
            by = c("eic" = "AreaCode")) |>
  arrange(desc(n_periods))

clean_path <- file.path(base_path, "data", "clean", "entsoe")
if(!dir.exists(clean_path)) dir.create(clean_path, recursive = TRUE, showWarnings = FALSE)

renewable_ex_nuclear_categories <- c("Geothermal", "Hydro Pumped Storage", "Hydro Run-of-river and poundage", 
                                     "Other renewable", "Solar", "Wind Onshore", "Biomass", 
                                     "Wind Offshore", "Hydro Water Reservoir") |>
  tolower() |>
  str_replace_all(" |/|-", "_")

energy_clean_df <- energy |>
  filter(eic %in% (summary |>
           filter(max_date > as_date("2023-01-01"), min_date < as_date("2021-01-01")) |>
           pull(eic))) |>
  left_join(selected_countries |> select(AreaCode, AreaName), by = c("eic" = "AreaCode")) |>
  transmute(
    mth = ymd(paste(y, m, "01")),
    country = str_replace_all(tolower(AreaName), " ", "_"),
    category = str_replace_all(tolower(resource_psr_type_def), " |/|-", "_"),
    category_group = case_when(
      category %in% renewable_ex_nuclear_categories ~ "renewable_ex_nuclear",
      category == "nuclear" ~ "nuclear",
      category == "other" ~ "other",
      TRUE ~ "fossil"
    ),
    mwh
  ) |> 
  arrange(mth, country, category_group)

energy_clean_df |> 
  write_csv(
    # paste0(getwd(), "/data/clean/entsoe/elec_gen_by_source_monthly.csv")
  )

grouped_energy_df <- energy_clean_df |>
  group_by(mth, country, category_group) |>
  summarize(mwh = sum(mwh)) |>
  ungroup()

write_csv(
  # paste0(getwd(), "/data/clean/entsoe/elec_gen_by_source_monthly_grouped.csv")
)
