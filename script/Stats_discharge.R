
library(dplyr)
library(lubridate)

# Load the data
data <- read.csv("data_rhone/Tables/discharge_rhone_1999-2019.csv")
colnames(data) <- c("Date",1:67)

# Convert and define hydrological year (starts on September 21)
data <- data %>%
  mutate(Date = as.Date(Date),
         HydroYear = if_else(
           month(Date) > 9 | (month(Date) == 9 & day(Date) >= 21),
           year(Date) + 1,
           year(Date)
         ))

# Compute statistics by hydrological year
hydro_stats <- data %>%
  group_by(HydroYear) %>%
  summarise(across(
    where(is.numeric),
    list(
      Min = ~min(.x, na.rm = TRUE),
      Mean = ~mean(.x, na.rm = TRUE),
      Median = ~median(.x, na.rm = TRUE),
      Max = ~max(.x, na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  ungroup()

# Compute global statistics (for all years)
total_stats <- data %>%
  summarise(across(
    where(is.numeric),
    list(
      Min = ~min(.x, na.rm = TRUE),
      Mean = ~mean(.x, na.rm = TRUE),
      Median = ~median(.x, na.rm = TRUE),
      Max = ~max(.x, na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  mutate(HydroYear = "Total") %>%
  select(HydroYear, everything())

# Combine yearly and total stats
final_stats <- bind_rows(hydro_stats %>% mutate(HydroYear = as.character(HydroYear)), total_stats)

# View the result
print(final_stats)

# # save final_stats to CSV
# write.csv(final_stats, "data_rhone/Tables/hydrological_year_statistics.csv")


long_stats <- final_stats %>%
  pivot_longer(-HydroYear, names_to = "VarStat", values_to = "Value") %>%
  separate(VarStat, into = c("Variable", "Statistic"), sep = "_")

colnames(long_stats) <- c("HY", "reach_ID", "Stat", "runoff")

# # save final_stats to CSV
# write.csv(long_stats, "data_rhone/Tables/hydrological_year_statistics_long.csv", row.names = F)
