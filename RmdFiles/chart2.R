# Timothy Kidd
# Chart 2 for Final Project Info 201

# Load Libraries
library(tidyverse)
library(tidycensus)
library(plotly)

# Set API key for census data
census_api_key("12ad784a12bd28d651be30ca71abc7955ef0882b")

# Construct vectors for use of extracting
# and labeling the appropriate census data
dg_code <- c(paste0("S1903_C01_00", 3:9, "E"), "S1903_C01_010E")
dg_race <- c(
  "Black or African American",
  "American Indian and Alaska Native",
  "Asian", "Native Hawaiian and Other Pacific Islander",
  "Some other race",
  "Two or more races",
  "Hispanic or Latino origin",
  "White alone, not Hispanic or Latino"
)

# Load Demographic data and organize accordingly
kcdemographics_2017 <- get_acs(
  geography = "county",
  state = "WA",
  county = "King County",
  variables = dg_code,
  year = 2017
) %>%
  summarize(
    year = "2017", household_race = dg_race, estimate,
    percentage = round(estimate / sum(estimate), digits = 3) * 100
  ) %>%
  arrange(desc(estimate))

kcdemographics_2018 <- get_acs(
  geography = "county",
  state = "WA",
  county = "King County",
  variables = dg_code,
  year = 2018
) %>%
  summarize(
    year = "2018", household_race = dg_race, estimate,
    percentage = round(estimate / sum(estimate), digits = 3) * 100
  ) %>%
  arrange(desc(estimate))

kcdemographics_2019 <- get_acs(
  geography = "county",
  state = "WA",
  county = "King County",
  variables = dg_code,
  year = 2019
) %>%
  summarize(
    year = "2019", household_race = dg_race, estimate,
    percentage = round(estimate / sum(estimate), digits = 3) * 100
  ) %>%
  arrange(desc(estimate))

# Combine the data frames into one set to be used in the chart
kcdemographics_datasets <- rbind(
  kcdemographics_2017,
  kcdemographics_2018,
  kcdemographics_2019
)

# Create and plot the bar chart
king_county_demographic <- ggplot(
  kcdemographics_datasets,
  aes(
    x = year,
    y = percentage,
    fill = reorder(household_race, estimate)
  )
) +
  geom_bar(position = "fill", stat = "identity", width = .9) +
  ggtitle("Estimated Household Demographic (by percentage)
           of King County 2017-19") +
  xlab("Year") +
  ylab("Percentage of King County") +
  scale_fill_discrete(name = "Household Race")

# Make the chart interactive and display only the percentage when hovering
king_county_demographic <- ggplotly(king_county_demographic, tooltip = "y")
