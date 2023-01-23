library(tidyverse)
library(tidycensus)
options(tigris_use_cache = TRUE)
library(leaflet)
library(priceR)

# Loading Data From ACS -----------------------------------------------------

## Median Income Data --------------------------------------------------------
# 2019 Data
medincome_king_2019 <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C03_001E",
  year = 2019,
  geometry = TRUE
) %>% select(-moe, -variable)


# 2010 - 2016 Data
years_income <- lst(2010, 2011, 2012, 2013, 2014, 2015, 2016)

medincome_king_2010_2016 <- map_dfr(
  years_income,
  ~ get_acs(
    geography = "tract",
    state = "WA",
    county = "King County",
    variables = "S1903_C02_001E",
    year = .x,
    survey = "acs5"
  ),
  .id = "year"
)


# 2016 - 2018 Data
years_income_2 <- lst(2017, 2018)

medincome_king_2017_2018 <- map_dfr(
  years_income_2,
  ~ get_acs(
    geography = "tract",
    state = "WA",
    county = "King County",
    variables = "S1903_C03_001E",
    year = .x,
    survey = "acs5"
  ),
  .id = "year"
)

# Adjusting for Inflation Base Year 2019
medincome_2017_2018_adj_infl <- medincome_king_2017_2018 %>%
  mutate(median_income = round(adjust_for_inflation(estimate,
    year,
    "US",
    to_date = 2019
  ))) %>%
  select(-moe, -estimate, -variable) %>%
  pivot_wider(names_from = year, values_from = median_income)



medincome_2010_2016_adj_infl <- medincome_king_2010_2016 %>%
  mutate(median_income = round(adjust_for_inflation(estimate,
    year,
    "US",
    to_date = 2019
  ))) %>%
  select(-moe, -estimate, -variable) %>%
  pivot_wider(names_from = year, values_from = median_income)



# Combined 2010 - 2018
medincome_2010_2018_adj_infl <- left_join(
  medincome_2010_2016_adj_infl,
  medincome_2017_2018_adj_infl
)


# Final Combined DF (2010 - 2019)
medincome_king_complete <- medincome_king_2019 %>%
  rename("2019" = estimate) %>%
  right_join(medincome_2010_2018_adj_infl, by = c("GEOID", "NAME")) %>%
  relocate("2019", .after = "2018")

## Median Home Value Data --------------------------------------------------


# 2019 Data
home_value__2019 <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "B25077_001",
  year = 2019,
  geometry = TRUE
) %>%
  select(-moe, -variable) %>%
  rename("2019" = estimate)



# 2010 - 2018 Data
home_value_years <- lst(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)

home_value_2010_2018 <- map_dfr(
  home_value_years,
  ~ get_acs(
    geography = "tract",
    state = "WA",
    county = "King County",
    variables = "B25077_001",
    year = .x,
    survey = "acs5"
  ),
  .id = "year"
) %>%
  mutate(median_income = round(adjust_for_inflation(estimate,
    year,
    "US",
    to_date = 2019
  ))) %>%
  select(-moe, -estimate, -variable) %>%
  pivot_wider(names_from = year, values_from = median_income)

home_value_2010_2019_adj_infl <- home_value__2019 %>%
  right_join(home_value_2010_2018, by = c("GEOID", "NAME")) %>%
  relocate("2019", .after = "2018")





## Percent Race --------------------------------------------------

# Loading Data From ACS
all_years <- lst(2017, 2018, 2019)


# Num of Households
num_household <- map_dfr(
  all_years,
  ~ get_acs(
    geography = "tract",
    state = "WA",
    county = "King County",
    variables = "S1903_C01_001E",
    year = .x
  ),
  .id = "year"
) %>% select(year, GEOID, num_household = estimate)

# Num of Black Households
num_black <- map_dfr(
  all_years,
  ~ get_acs(
    geography = "tract",
    state = "WA",
    county = "King County",
    variables = "S1903_C01_003E",
    year = .x
  ),
  .id = "year"
) %>% select(year, GEOID, num_black = estimate)


# Num of Asian Households
num_asian <- map_dfr(
  all_years,
  ~ get_acs(
    geography = "tract",
    state = "WA",
    county = "King County",
    variables = "S1903_C01_005E",
    year = .x
  ),
  .id = "year"
) %>% select(year, GEOID, num_asian = estimate)


# Num Latin Households
num_latin <- map_dfr(
  all_years,
  ~ get_acs(
    geography = "tract",
    state = "WA",
    county = "King County",
    variables = "S1903_C01_009E",
    year = .x
  ),
  .id = "year"
) %>% select(year, GEOID, num_latin = estimate)

# Num White Households
num_white <- map_dfr(
  all_years,
  ~ get_acs(
    geography = "tract",
    state = "WA",
    county = "King County",
    variables = "S1903_C01_010E",
    year = .x
  ),
  .id = "year"
) %>% select(year, GEOID, num_white = estimate)


# Geodata
geodata <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C01_010E",
  year = 2019,
  geometry = TRUE
) %>% select(GEOID, geometry)



### Data Frames --------------------------------------------------------------

# Combining All Data Frames Into One & Calc Percentages for Race
merdged_race_stats <- num_household %>%
  left_join(num_black, by = c("year", "GEOID")) %>%
  left_join(num_asian, by = c("year", "GEOID")) %>%
  left_join(num_latin, by = c("year", "GEOID")) %>%
  left_join(num_white, by = c("year", "GEOID")) %>%
  mutate(
    perc_black = (num_black / num_household) * 100,
    perc_asian = (num_asian / num_household) * 100,
    perc_latin = (num_latin / num_household) * 100,
    perc_white = (num_white / num_household) * 100
  ) %>%
  select(-num_black, -num_asian, -num_latin, -num_white)

merged_race_plus_geo <- geodata %>%
  right_join(merdged_race_stats, by = "GEOID")


# Creating Map --------------------------------------------------------------

get_king_map <- function(get_year, get_dataset, get_race) {
  select_df <- if (get_dataset == "a") {
    get_label <<- paste0(
      " Estimated Median Income </br> in ", get_year,
      " (2019 Dollars)"
    )

    medincome_king_complete %>%
      select(toString(get_year), geometry) %>%
      rename(estimate = 1)
  } else if (get_dataset == "b") {
    get_label <<- paste0(
      " Median Housing Value </br> in ", get_year,
      " (2019 Dollars)"
    )


    home_value_2010_2019_adj_infl %>%
      select(toString(get_year), geometry) %>%
      rename(estimate = 1)
  } else {
    get_label <- paste0(" % Selected Race")

    merged_race_plus_geo %>%
      filter(year == get_year) %>%
      select(get_race, geometry) %>%
      rename(estimate = 1)
  }


  get_tooltip <- if (get_dataset == "a") {
    paste0("Median Income $", prettyNum(select_df$estimate, big.mark = ","))
  } else if (get_dataset == "b") {
    paste0("Housing Value $", prettyNum(select_df$estimate, big.mark = ","))
  } else {
    paste0(round(select_df$estimate, digits = 2), "%")
  }




  # Setup Color Pallet
  pal <- colorNumeric(
    palette = "magma",
    domain = select_df$estimate
  )


  # Create Map

  king_map <- leaflet() %>%
    addProviderTiles(providers$Stamen.TonerLite) %>%
    setView(-122.335167, 47.508013, 10.5) %>%
    addPolygons(
      data = select_df,
      color = ~ pal(estimate),
      weight = 0.5,
      smoothFactor = 0.2,
      fillOpacity = 0.5,
      label = get_tooltip
    ) %>%
    addLegend(
      position = "bottomleft",
      pal = pal,
      values = select_df$estimate,
      title = get_label
    )
}
