
# Tue Mar 28 12:45:52 2023 ------------------------------

library(tidyverse)


# -------------------------------------------------------------------------
ff_in <- arrow::read_parquet("/Users/jacobeliason/Documents/Files/Code/Repos/energy-in-germany/DATA/eu_fossil_fuel_weekly.parquet")
nuc_in <- arrow::read_parquet("/Users/jacobeliason/Documents/Files/Code/Repos/energy-in-germany/DATA/eu_nuclear_weekly.parquet") |> mutate(wk = lubridate::as_datetime(wk))

chart_nuclear_output <- 
  nuc_in |> 
  left_join(ff_in, by = c("country", "wk")) |> 
  filter(country == "Germany") |> 
  filter(!(wk == max(wk))) |> 
  ggplot() +
  geom_line(
    aes(x = wk, y = log(sum_nuclear), color = 'blue')
  ) +
  geom_line(
    aes(x = wk, y = log(sum_fossil), color = 'red')
  ) +
  geom_vline(
    xintercept = as_date("2021-12-31"),
    color = "black",
    size = 2,
    linetype = "dotted"
  ) +
  theme_minimal() +
  labs(
    title = "Generation output - nuclear",
    x = NULL,
    y = "MWh"
  )
chart_nuclear_output

ggsave(
  str_c(getwd(), '/PLOT/', lubridate::today(), '-', "chart_nuclear_output", '.png'), 
  chart_nuclear_output,
  width = 4,
  height = 3,
  scale = 3/2,
  bg = 'white'
)


# mod ---------------------------------------------------------------------

nuc_in |> 
  filter(country == "Germany", lubridate::year(wk) %in% c(2015, 2022)) |> 
  mutate(post = wk >= "2021-12-31") |> 
  group_by(post) |> 
  summarize(
    mean_produced = mean(sum_nuclear)
  )

# 2368461/4822290 = ~50% reduction from 2020-2021 average
# 2368461/5531529 = ~57% reduction from 2015-2021 average
# 2508798/6472505 = 2022 average reduced ~61% from 2015 average

dat <- ff_in |> left_join(nuc_in, by = c("country", "wk")) |> drop_na()

mod <- lm(sum_fossil ~ sum_nuclear + country, data = dat)
summary(mod)

cor(dat$sum_nuclear[dat$country == "Germany"], dat$sum_fossil[dat$country == "Germany"])