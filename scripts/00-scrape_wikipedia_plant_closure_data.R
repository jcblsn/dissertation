library(tidyverse)
library(lubridate)
library(rvest)

path <- getwd()
url <- "https://en.wikipedia.org/wiki/Nuclear_power_in_Germany"
lse_red <- "#E60023"

# --- scrape, save

html <- rvest::read_html(url)
table <- html %>% 
  rvest::html_nodes("table") %>% 
  .[[2]] %>% 
  rvest::html_table()

headers <- c("reactor_name", "unit_number", "reactor_type", "reactor_model", "status", "net_capacity_mw", "construction_start", "commercial_operation", "closure")
colnames(table) <- headers

table[1, ] <- sapply(table[1, 10:18], as.character)

history <- table %>%
  dplyr::select(headers) %>%
  dplyr::mutate_all(~ stringr::str_remove_all(., "\\[[0-9]+\\]")) %>%
  dplyr::mutate(
    closure = if_else(stringr::str_length(closure) == 4, stringr::str_c("1 Jan ", closure), closure),
    closure = lubridate::parse_date_time(closure, "d b Y", locale = "de_DE"),
    commercial_operation = lubridate::parse_date_time(commercial_operation, "d b Y", locale = "de_DE"),
    net_capacity_mw = as.numeric(net_capacity_mw)
  )

# --- read, visualize

# readr::write_csv(history, file.path(path, "DATA", paste0(lubridate::today(), "_wikipedia-plant-closure-data.csv")))

history <- readr::read_csv(file.path(path, 'data', 'raw', 'wikipedia', 'wikipedia-plant-closure-data.csv')) %>%
  dplyr::filter(!(status %in% c("Unfinished", "Finished; never entered service", "Never built")))

extra_dates <- tibble(
  commercial_operation = seq(lubridate::ymd(min(history$commercial_operation, na.rm = TRUE)), lubridate::ymd("2023-06-01"), by = "1 day"),
  net_capacity_mw = 0
) %>%
  dplyr::filter(
    !(commercial_operation %in% history$commercial_operation),
    commercial_operation < "2023-06-01"
  ) %>%
  dplyr::sample_frac(.05)

history <- dplyr::bind_rows(history, extra_dates)

timeline <- history %>%
  dplyr::transmute(
    id = stringr::str_c(reactor_name, "-", unit_number),
    start = commercial_operation,
    end = closure,
    mw = net_capacity_mw
  ) %>%
  tidyr::pivot_longer(cols = c(start, end), names_to = "event", values_to = "dt") %>%
  dplyr::mutate(
    mw = if_else(event == "end", -mw, mw),
    projected = dt >= "2023-06-01",
    closure = event == "end",
    eoi = lubridate::date(dt) == "2021-12-31"
  ) %>%
  dplyr::filter(!is.na(dt), !projected) %>%
  dplyr::group_by(dt) %>%
  dplyr::summarise(
    mw = sum(mw),
    projected = unique(projected),
    eoi = unique(eoi),
    id = list(id)
  ) %>%
  dplyr::arrange(dt)

timeline$cmw <- cumsum(timeline$mw)

chart_generation_history <-
  timeline |>
  ggplot() +
  geom_vline(
    aes(xintercept = dt, color = eoi, size = eoi),
    linetype = "dotted",
    size = 1 / 2,
    data = timeline |> filter(eoi)
  ) +
  geom_line(
    aes(x = dt, y = cmw),
    size = 1 / 2
  ) +
  scale_y_continuous(
    name = "Total capacity (GW)",
    breaks = seq(0, 25000, by = 5000),
    labels = seq(0, 25000, by = 5000) / 1000
  ) +
  scale_x_datetime(
    name = NULL,
    limits = c(as_datetime("1965-01-01"), as_datetime("2023-06-01")),
    date_breaks = "10 years",
    date_labels = "%Y"
  ) +
  scale_color_manual(
    values = lse_red
  ) +
  theme_bw() +
  theme(
    legend.position = "none"
  )

chart_generation_history

ggsave(
  str_c(getwd(), "/plots/", lubridate::today(), "-", "chart_nuclear_generation_history", ".png"),
  chart_generation_history,
  width = 16/4,
  height = 9/4,
  scale = 2,
  bg = "white"
)


# plot --------------------------------------------------------------------

history <- readr::read_csv(file.path(path, 'data', 'raw', 'wikipedia', 'wikipedia-plant-closure-data.csv')) %>%
  dplyr::filter(!(status %in% c("Unfinished", "Finished; never entered service", "Never built")))

chart_capacity_change_history <-
  history |>
  transmute(
    id = str_c(reactor_name, "-", unit_number),
    start = commercial_operation,
    end = closure, mw = net_capacity_mw
  ) |>
  pivot_longer(cols = c(start, end)) |>
  rename(event = name, dt = value) |>
  mutate(
    mw = if_else(event == "end", -mw, mw),
    closure = event == "end"
  ) |>
  filter(!is.na(dt)) |>
  group_by(yr = year(dt)) |>
  summarise(mw = sum(mw, na.rm = T)) |>
  ungroup() |>
  mutate(
    eoi = yr == 2021,
    yr = as_datetime(str_c(yr, "-12-31"))
  ) |>
  arrange(desc(yr)) |>
  ggplot() +
  geom_bar(aes(x = yr, y = mw, fill = eoi),
    # color = 'black',
    stat = "identity"
  ) +
  scale_y_continuous(
    name = "Change in total capacity (GW)",
    breaks = seq(-7500, 5000, by = 2500),
    labels = seq(-7500, 5000, by = 2500) / 1000
  ) +
  scale_x_datetime(
    name = NULL,
    limits = c(as_datetime("1965-01-01"), as_datetime("2025-01-01")),
    date_breaks = "10 years",
    date_labels = "%Y"
  ) +
  scale_fill_manual(
    values = c("black", lse_red)
  ) +
  ggtitle("") +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    legend.position = "none"
  )

chart_capacity_change_history

ggsave(
  str_c(getwd(), "/plots/", lubridate::today(), "-", "chart_de_capacity_change_history", ".png"),
  chart_capacity_change_history,
  width = 4,
  height = 3,
  scale = 2,
  bg = "transparent"
)
