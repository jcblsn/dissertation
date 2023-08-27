
library(tidyverse)
library(lubridate)
library(future)
library(entsoeapi)
library(glue)
library(purrr)

# note: api key is set in project-specific .Rprofile file
ENTSOE_PAT <- Sys.getenv("ENTSOE_PAT")

generation_data <- function(code, start_date, end_date, attempts=1) {
  tryCatch({
    generation <- en_generation_agg_gen_per_type(
      as.character(code),
      lubridate::as_datetime(paste0(start_date, "_00:00:00"), format = "%Y-%m-%d_%H:%M:%S", tz = "UTC"),
      lubridate::as_datetime(paste0(end_date, "_00:00:00"), format = "%Y-%m-%d_%H:%M:%S", tz = "UTC")
    )
    generation$req_start_date <- start_date
    generation$req_end_date <- end_date
    generation$eic <- code
    return(generation)
  },
  error = function(e) {
    if (attempts > 0) {
      message(glue::glue("Attempt failed, retrying... {attempts} attempts remaining."))
      # Sys.sleep(2^attempts)
      generation_data(code, start_date, end_date, attempts - 1)
    } else {
      message(glue::glue("Error generationing data for code {code}: {e}"))
      return(NULL)
    }
  })
}

write_data <- function(generation, filename) {
  tryCatch({
    write_csv(generation, filename)
  },
  error = function(e) {
    message(glue("Error writing data to file {filename}: {e}"))
    return(NULL)
  })
}

create_filename <- function(code, start_date, end_date, path_out) {
  glue(
    "{path_out}",
    "{format(start_date, '%Y-%m-%d %H:%M:%S %Z') %>% stringr::str_replace_all(., ':| |-', '')}",
    "_through_",
    "{format(end_date, '%Y-%m-%d %H:%M:%S %Z') %>% stringr::str_replace_all(., ':| |-', '')}",
    "_for_{names(code)}_{code}.csv"
  )
}

create_data <- possibly(function(code, start_date, end_date, path_out) {
  generation <- generation_data(code, start_date, end_date)
  filename <- create_filename(code, start_date, end_date, path_out)
  write_data(generation, filename)
}, otherwise = NULL)

eic <- en_eic()
eic$MapCode |> unique()

selected_countries <- eic |>
  mutate(
    AreaCode = if_else(
      MapCode == "GB",
      "10YGB----------A",
      AreaCode
    )
  ) |>
  filter(
    AreaTypeCode == "CTY"
  )

head(selected_countries); nrow(selected_countries)

area_codes <- selected_countries$AreaCode
names(area_codes) <- selected_countries$MapCode

start_range <- lubridate::as_date("2015-01-01")
end_range <- lubridate::as_date("2023-07-01")
interval_type <- "monthly"
date_range <- seq.Date(start_range, end_range, by = "month")
subset_date_range <- date_range

path_out <- str_c(getwd(), "/data/raw/entsoe/generation-monthly/")
if (!dir.exists(path_out)) dir.create(path_out, recursive = T)

files <- list.files(path_out, pattern = ".csv", full.names = T)

existing <- 
  files %>%
  str_remove(., paste0(getwd(), "/data/raw/entsoe/generation-weekly/")) %>%
  str_remove(., ".csv") |> 
  enframe() |> 
  separate(value, sep = "_through_", into = c("start", "value")) |> 
  separate(value, sep = "_for_", into = c("end", "value")) |> 
  transmute(
    code = str_sub(value, 4, -1),
    country = str_sub(value, 1, 2),
    start = as_date(start),
    end = as_date(end)
  )

need <- 
  expand.grid(code = area_codes, start = subset_date_range) |> 
  as_tibble() |> 
  filter(
    !(str_c(start, code) %in% str_c(existing$start, existing$code))
  )

en_generation_agg_gen_per_type(
  as.character(selected_countries$AreaCode[1]),
  lubridate::as_datetime(paste0(today()-2, "_00:00:00"), format = "%Y-%m-%d_%H:%M:%S", tz = "UTC"), 
  lubridate::as_datetime(paste0(today()-1, "_00:00:00"), format = "%Y-%m-%d_%H:%M:%S", tz = "UTC")
)

wrkrs <- 4
future::plan(future::multisession, workers = wrkrs)

ind <- 1:nrow(need)
furrr::future_walk(
  ind,
  function(x) {
    code <- need$code[x]
    start_date <- need$start[x]
    end_date <- as.Date(lubridate::ceiling_date(start_date, "month")) - 1  # Get the last day of the month
    
    generation <- generation_data(code, start_date, end_date)
    filename <- create_filename(code, start_date, end_date, path_out)
    write_data(generation, filename)
    Sys.sleep(1)
  },
  .progress = TRUE
)

# cleanup
if (!inherits(plan(), "sequential")) plan(sequential)
rm(list=ls())
.rs.restartR()
