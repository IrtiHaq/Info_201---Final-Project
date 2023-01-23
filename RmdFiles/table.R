library(tidyverse)
library(tidycensus)
library(kableExtra)

# Loading Data From ACS -----------------------------------------------------

# Median Income
medincome_king <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C03_001E",
  year = 2019
) %>% select(GEOID, NAME, median_income = estimate)

# Num of Households
num_household <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C01_001E",
  year = 2019
) %>% select(GEOID, num_household = estimate)

# Num of Black Households
num_black <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C01_003E",
  year = 2019
) %>% select(GEOID, num_black = estimate)

# Num of Native American Households
num_native <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C01_004E",
  year = 2019
) %>% select(GEOID, num_native = estimate)

# Num of Asian Households
num_asian <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C01_005E",
  year = 2019
) %>% select(GEOID, num_asian = estimate)

# Num of Native Hawians and Pacific Islander Households
num_nhopi <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C01_006E",
  year = 2019
) %>% select(GEOID, num_nhopi = estimate)

# Num Other Race Households
num_other <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C01_007E",
  year = 2019
) %>% select(GEOID, num_other = estimate)

# Num Mixed Race Households
num_mixed <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C01_008E",
  year = 2019
) %>% select(GEOID, num_mixed = estimate)

# Num Latin Households
num_latin <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C01_009E",
  year = 2019
) %>% select(GEOID, num_latin = estimate)

# Num White Households
num_white <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C01_010E",
  year = 2019
) %>% select(GEOID, num_white = estimate)


# Data Processing ----------------------------------------------------------

# Round Median Income to Nearest 10,000 Dollars
medincome_king_rounded <- medincome_king %>% mutate(
  median_income =
    round(median_income,
      digits = -4
    )
)

## Data Frames --------------------------------------------------------------

# Combining All Data Frames Into One & Calc Percentages for Race
merdged_king_stats <- left_join(medincome_king_rounded, num_household,
  by = "GEOID"
) %>%
  left_join(num_black, by = "GEOID") %>%
  left_join(num_native, by = "GEOID") %>%
  left_join(num_other, by = "GEOID") %>%
  left_join(num_mixed, by = "GEOID") %>%
  left_join(num_asian, by = "GEOID") %>%
  left_join(num_nhopi, by = "GEOID") %>%
  left_join(num_latin, by = "GEOID") %>%
  left_join(num_white, by = "GEOID") %>%
  mutate(
    perc_black = (num_black / num_household) * 100,
    perc_native = (num_native / num_household) * 100,
    perc_other = (num_other / num_household) * 100,
    perc_mixed = (num_mixed / num_household) * 100,
    perc_asian = (num_asian / num_household) * 100,
    perc_nhopi = (num_nhopi / num_household) * 100,
    perc_latin = (num_latin / num_household) * 100,
    perc_white = (num_white / num_household) * 100
  )


# Summary Data Frame
king_county_summary <- merdged_king_stats %>%
  group_by(median_income) %>%
  summarise(
    num_tracts = n(),
    num_household = prettyNum(sum(num_household),
      big.mark = ",",
      scientific = FALSE
    ),
    avg_perc_black = mean(perc_black),
    avg_perc_native = mean(perc_native),
    avg_perc_other = mean(perc_other),
    avg_perc_mixed = mean(perc_mixed),
    avg_perc_asian = mean(perc_asian),
    avg_perc_nhopi = mean(perc_nhopi),
    avg_perc_latin = mean(perc_latin),
    avg_perc_white = mean(perc_white)
  ) %>%
  mutate(income_bracket = paste0(
    "$", prettyNum((median_income - 5000),
                   big.mark = ",", scientific = FALSE
    ), " - ",
    "$", prettyNum((median_income + 4000),
                   big.mark = ",", scientific = FALSE
    )
  ))

# Final Cleaned and Rounded Data Frame
king_county_summary_cleaned <- king_county_summary %>%
  select(
    income_bracket,
    num_tracts,
    num_household,
    avg_perc_white,
    avg_perc_black,
    avg_perc_asian,
    avg_perc_latin,
    avg_perc_native,
    avg_perc_nhopi,
    avg_perc_mixed,
    avg_perc_other,
  ) %>%
  mutate(across(4:11, round, 2))

# Table

# Summary Table
summary_table <- king_county_summary_cleaned %>%
  kbl(col.names = c(
    "Median Income Brackets ($)",
    "Number of Census Tracts",
    "Number of Households",
    "White",
    "Black",
    "Asian",
    "Latinx",
    "Native American",
    "Native Hawaiian & Other Pacific Islander ",
    "Multiracial Household",
    "Other Race Household"
  )) %>%
  add_header_above(c(" " = 3, "Percent of Households (%)" = 8)) %>%
  kable_paper(c("striped", "hover", "responsive"),
    fixed_thead = T,
    full_width = F, position = "center"
  )
