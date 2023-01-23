## Timothy Kidd, Base from Irti
library(ggthemes)
# Loading Data From ACS -----------------------------------------------------

# Median Income 2019
medincome_king_2019 <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C03_001E",
  year = 2019
) %>%
  select(GEOID, NAME, median_income = estimate) %>%
  mutate(year = "3")

# Median Income 2015
medincome_king_2015 <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C02_001E",
  year = 2015
) %>%
  select(GEOID, NAME, median_income = estimate) %>%
  mutate(year = "2")

# Median Income 2010
medincome_king_2010 <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C02_001E",
  year = 2010
) %>%
  select(GEOID, NAME, median_income = estimate) %>%
  mutate(year = "1")

all_years <- c(2010, 2015, 2019)

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

medincome_king <- rbind(
  medincome_king_2010,
  medincome_king_2015,
  medincome_king_2019
)


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
merged_king_stats <- left_join(medincome_king_rounded, num_household,
  by = c("GEOID", "year")
)


# Summary Data Frame
king_county_income_summary <- merged_king_stats %>%
  group_by(year) %>%
  group_by(median_income, .add = TRUE) %>%
  summarise(
    year = year,
    num_tracts = n(),
    num_household = sum(num_household)
  ) %>%
  mutate(income_bracket = paste0(
    "$", prettyNum((median_income - 5000),
      big.mark = ",", scientific = FALSE
    ), " - ",
    "$", prettyNum((median_income + 4000),
      big.mark = ",", scientific = FALSE
    )
  )) %>%
  na.omit() %>%
  unique()

create_income_chart <- function(data) {
  year_labs <- c("2010", "2015", "2019")
  names(year_labs) <- c("1", "2", "3")

  chart <- ggplot(
    data,
    aes(
      x = reorder(income_bracket, num_household),
      y = num_household,
      fill = reorder(income_bracket, median_income),
      text = paste0(
        "# of Households: ",
        prettyNum(num_household, big.mark = ",", scientific = FALSE),
        "\nMedian Income: ",
        prettyNum(median_income, big.mark = ",", scientific = FALSE),
        "\n", income_bracket
      )
    )
  ) +
    geom_bar(stat = "identity", width = .9) +
    ggtitle("Household Income (by brackets) of King County") +
    xlab("Income Bracket") +
    ylab("Number of Households in King County") +
    scale_fill_discrete(name = "Income Bracket") +
    theme_solarized() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    facet_wrap(
      ~year,
      labeller = labeller(year = year_labs)
    )

  # Make the chart interactive and display only the percentage when hovering
  chart <- ggplotly(chart, tooltip = "text")

  return(chart)
}
