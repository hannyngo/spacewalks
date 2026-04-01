# Hanny Ngo
# 20260331
# These codes generate graphs showing the time spent in space vs. time

# https://data.nasa.gov/resource/eva.json (with modifications)
input_file = 'eva-data.json' #raw data
output_file = 'eva-data.csv' 
graph_file = 'cumulative_eva_graph.png'

#files
library(tidyverse) #tidyverse "contains" ggplot2
library(jsonlite)
library(lubridate)

# 1) Read JSON array into a table
input_file  <- "./eva-data.json"
output_file <- "./eva-data.csv"
graph_file  <- "./cumulative_eva_graph.png"

# 2) Convert + write to CSV
eva_tbl <- jsonlite::fromJSON(input_file) |>
  as_tibble()

subset=c('duration','date')
eva_tbl <- eva_tbl |>
  mutate(
    eva  = as.numeric(eva),
    date = ymd_hms(date, quiet = TRUE)
  ) |>
  filter(!is.na(duration), duration != "", !is.na(date))

# 3) convert to csv file
readr::write_csv(eva_tbl, output_file)

# 4) sort by date
eva_tbl <- eva_tbl |>
  arrange(date)

# 5) duration_hours + cumulative_time
eva_tbl <- eva_tbl |>
  mutate(
    duration_hours = {
      parts <- str_split(duration, ":", n = 2, simplify = TRUE)
      as.numeric(parts[, 1]) + as.numeric(parts[, 2]) / 60
    },
    cumulative_time = cumsum(duration_hours)
  )

# 6) Plot and save
p <- ggplot(eva_tbl, aes(x = date, y = cumulative_time)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Year",
    y = "Total time spent in space to date (hours)"
  ) +
  theme_minimal()

ggsave(graph_file, plot = p, width = 9, height = 5, dpi = 300)
print(p)