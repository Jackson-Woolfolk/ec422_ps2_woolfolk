### Jackson Woolfolk

## Part 3
library(tidyquant)
library(tidyverse)

# unemp_raw <- tq_get(
#  c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
#  get  = "economic.data",
#  from = "1976-01-01"
# )
# 
# head(unemp_raw)
# 
# dir.create("data", showWarnings = FALSE)
# 
# write_rds(unemp_raw, "data/unemp_raw.rds")

unemp_raw <- read_rds("data/unemp_raw.rds")

## Part 4
# Clean
unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(
    region = case_when(
      symbol == "UNRATE" ~ "United States",
      symbol == "ORUR"    ~ "Oregon",
      symbol == "CAUR"    ~ "California",
      symbol == "TXUR"    ~ "Texas",
      symbol == "NYUR"    ~ "New York"
    )
  )
# Wrangle
unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate = replace(
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    )
  ) |>
  ungroup()

unemp <- unemp |> 
  arrange(region, date) |> 
  group_by(region) |> 
  mutate(unemployment_rate_change = unemployment_rate - lag(unemployment_rate)) |> 
  ungroup()

unemp_recent <- unemp |> 
  filter(date >= as.Date("2015-01-01"))

# Summary
unemp_summary <- unemp |> 
  group_by(region) |> 
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )

# Recent Summary
unemp_recent_summary <- unemp_recent |>  
  group_by(region) |>  
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )

# Extremes
unemp_top_increases <- unemp |> 
  slice_max(unemployment_rate_change, n = 5, with_ties = TRUE)

unemp_top_decreases <- unemp |> 
  slice_min(unemployment_rate_change, n = 5, with_ties = TRUE)


# Extremes without 2020
unemp_no_2020 <- unemp |> 
  filter(format(date, "%Y") != "2020")

unemp_top_increases_no_covid <- unemp_no_2020 |> 
  slice_max(unemployment_rate_change, n = 5, with_ties = TRUE)

unemp_top_decreases_no_covid <- unemp_no_2020 |> 
  slice_min(unemployment_rate_change, n = 5, with_ties = TRUE)

## Part 5
library(ggplot2)
library(forcats)

p1 <- ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Monthly Unemployment Rates (1976-2025)",
    x = "Date",
    y = "Unemployment Rate (%)",
    color = "Region"
  ) +
  theme_minimal()

print(p1)

p2 <- ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ region) +
  labs(
    title = "Unemployment Rates by Region",
    x = "Date",
    y = "Unemployment Rate (%)",
    color = "Region"
  ) +
  theme_minimal() + theme(legend.position = "none")
 
print(p2)

p3 <- ggplot(unemp_summary, aes(x = fct_reorder(region, mean_rate), y = mean_rate, fill = region)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Average Unemployment Rate by Region (Full Sample)",
    x = "Region",
    y = "Mean Unemployment Rate (%)"
  ) +
  theme_minimal()

print(p3)

p4 <- ggplot(unemp_recent, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Recent Unemployment Trends (2015–2025)",
    x = "Date",
    y = "Unemployment Rate (%)",
    color = "Region"
  ) +
  theme_minimal()

print(p4)

