# 2023-08

library(tidyverse)
library(httr)
library(jsonlite)

european_countries <- tibble(
    country = c(
        "Austria", "Belgium", "Bulgaria", "Bosnia and Herzegovina", "Croatia", "Cyprus", "Czech Republic",
        "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
        "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Moldova",
        "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Slovakia",
        "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom"
    ),
    city = c(
        "Vienna", "Brussels", "Sofia", "Sarajevo", "Zagreb", "Nicosia", "Prague",
        "Copenhagen", "Tallinn", "Helsinki", "Paris", "Berlin", "Athens", "Budapest",
        "Reykjavik", "Dublin", "Rome", "Riga", "Vilnius", "Luxembourg City", "Valletta", "Chisinau",
        "Amsterdam", "Oslo", "Warsaw", "Lisbon", "Bucharest", "Bratislava",
        "Ljubljana", "Madrid", "Stockholm", "Bern", "London"
    )
)

selected_countries <- entsoeapi::en_eic() %>%
    filter(AreaTypeCode == "CTY")

get_lat_lon <- function(city_country) {
    city_country <- stringr::str_replace_all(city_country, " ", "+")
    geocode_url <- paste0("https://maps.googleapis.com/maps/api/geocode/json?address=", city_country, "&key=", Sys.getenv("GOOGLE_API_KEY"))
    response <- httr::GET(geocode_url)
    data <- jsonlite::fromJSON(content(response, "text"))
    list(lat = data$results$geometry$location$lat, lon = data$results$geometry$location$lng)
}

api_call_count <- 0
get_hist_weather <- function(lat, lon, timestamp, key = Sys.getenv("OWM_API_KEY")) {
    url <- paste0("https://api.openweathermap.org/data/3.0/onecall/day_summary?lat=",
        lat, "&lon=", lon, "&date=", timestamp, "&appid=", key, "&units=imperial")
    if (api_call_count > 59) {
        message("Approaching API rate limit; sleeping for 60 seconds")
        Sys.sleep(60)
        api_call_count <<- 0
    }
    result <- tryCatch(
        {
            response <- httr::GET(url)
            api_call_count <<- api_call_count + 1
            jsonlite::fromJSON(content(response, "text"))$temperature
        },
        error = function(e) NULL
    )
    if (!is.null(result)) {
        tibble(lat = lat, lon = lon, dt = timestamp,
            min_temp = result$min, max_temp = result$max, temp = mean(c(result$min, result$max)))
    } else {
        tibble(lat = lat, lon = lon, dt = timestamp, min_temp = NA_real_, max_temp = NA_real_, temp = NA_real_)
    }
}

file_path <- paste0(getwd(), "/data/raw/weather/", "daily_temperatures_europe.csv")

if (file.exists(file_path)) {
    all_data <- readr::read_csv(file_path)
} else {
    european_countries <- european_countries %>%
        rowwise() %>%
        mutate(coords = list(get_lat_lon(paste0(city, ", ", country)))) %>%
        unpack(coords)
    dates <- seq(as.Date("2015-01-01"), as.Date("2023-01-01"), by = "day")
    all_data <- expand.grid(dates = dates, country = european_countries$country) %>%
        dplyr::left_join(european_countries) %>%
        mutate(min_temp = NA_real_, max_temp = NA_real_, temp = NA_real_)
}

existing <- all_data %>% filter(!is.na(temp))
need <- all_data %>% filter(is.na(temp))
if (nrow(need) > 0) {
    for(i in 1:nrow(need)) {
        need[i, c('min_temp', 'max_temp', 'temp')] <- get_hist_weather(need$lat[i], need$lon[i], need$dates[i])[1, c('min_temp', 'max_temp', 'temp')]
    }
}
all_data <- dplyr::bind_rows(existing, need)

# write.csv(all_data, file = paste0(getwd(), "/data/raw/weather/daily_temperatures_europe.csv"), row.names = FALSE)
