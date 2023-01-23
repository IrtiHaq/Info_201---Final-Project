# Jiahang Wu
# Chart 1 for Final Project Info 201

library(tidyverse)
library(tidycensus)

# Load datasets from acs
total_2019 <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C01_001E",
  year = 2019
)

black_2019 <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C01_003E",
  year = 2019
)

white_2019 <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C01_010E",
  year = 2019
)

median_income_2019 <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C03_001E",
  year = 2019
)

# Add a column to distinguish data
black_2019 <- black_2019 %>%
  mutate(race = "Black")

white_2019 <- white_2019 %>%
  mutate(race = "White")

# Combine two datasets for preperation of plotting
both <- rbind(white_2019, black_2019)

# Join the population dataframes
combined <- both %>%
  left_join(total_2019, by = "NAME")

# Calculate for proportion of household
combined <- combined %>%
  mutate(proportion = estimate.x / estimate.y) %>%
  select(NAME, proportion, race)

# Join the income dataframe
combined_income <- combined %>%
  left_join(median_income_2019, by = "NAME")

# Plot the scatterplot
chart1 <- ggplot(data = combined_income) +
  geom_point(
    mapping = aes(x = proportion, y = estimate, color = race)
  ) +
  labs(
    title = "Proportion of Households vs. Median Income (2016 Dollars) in 2019",
    x = "Proportion of Households",
    y = "Median Income (2016 Dollars)",
    color = "Race"
  )
