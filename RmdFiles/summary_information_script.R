# Jiahang Wu
# Summary information script for Final Project Info 201

library(tidyverse)
library(tidycensus)

# Load all datasets from acs
total_2019 <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C01_001E",
  year = 2019
)

total_2017 <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C01_001E",
  year = 2017
)

black_2019 <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C01_003E",
  year = 2019
)

black_2017 <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C01_003E",
  year = 2017
)

white_2019 <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C01_010E",
  year = 2019
)

white_2017 <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C01_010E",
  year = 2017
)

median_income_2017 <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C03_001E",
  year = 2017
)

median_income_2019 <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C03_001E",
  year = 2019
)

# Calculate values using simple functions and operations
change_white_households <- sum(white_2019$estimate, na.rm = T) -
  sum(white_2017$estimate, na.rm = T)

change_black_households <- sum(black_2019$estimate, na.rm = T) -
  sum(black_2017$estimate, na.rm = T)

change_median_income <- round(
  mean(median_income_2019$estimate, na.rm = T) -
    mean(median_income_2017$estimate, na.rm = T),
  digit = 4
)

change_black_proportion <- paste0(
  round(
    100 * ((sum(black_2019$estimate, na.rm = T) /
      sum(total_2019$estimate, na.rm = T)) -
      (sum(black_2017$estimate, na.rm = T) /
        sum(total_2017$estimate, na.rm = T))),
    digits = 4
  ),
  "%"
)

change_white_proportion <- paste0(
  round(
    100 * ((sum(white_2019$estimate, na.rm = T) /
      sum(total_2019$estimate, na.rm = T)) -
      (sum(white_2017$estimate, na.rm = T) /
        sum(total_2017$estimate, na.rm = T))),
    digits = 4
  ),
  "%"
)

# Store variables in a list
summary_info <- list()
summary_info$change_black_households <- change_black_households
summary_info$change_white_households <- change_white_households
summary_info$change_median_income <- change_median_income
summary_info$change_black_proportion <- change_black_proportion
summary_info$change_white_proportion <- change_white_proportion
