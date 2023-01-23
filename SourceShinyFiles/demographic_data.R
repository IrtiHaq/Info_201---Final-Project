## Timothy kidd

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

create_demographic_by_year <- function(year) {
  data <- get_acs(
    geography = "county",
    state = "WA",
    county = "King County",
    variables = dg_code,
    year = year
  ) %>%
    summarize(
      year = year, household_race = dg_race, estimate,
      percentage = round(estimate / sum(estimate), digits = 3) * 100
    ) %>%
    arrange(desc(estimate))

  return(data)
}

kcdemographic <- create_demographic_by_year(2017)
for (num in 2018:2019) {
  kcdemographic <- rbind(kcdemographic, create_demographic_by_year(num))
}

create_demographics_chart <- function(data) {
  chart <- ggplot(
    data,
    aes(
      y = percentage,
      x = year,
      fill = reorder(household_race, estimate),
      text = paste0(
        "Percentage: ",
        percentage,
        "\nEstimated Count: ",
        prettyNum(estimate, big.mark = ",", scientific = FALSE)
      )
    )
  ) +
    geom_bar(position = "fill", stat = "identity", width = .9) +
    scale_x_continuous(breaks = 2017:2019) +
    ggtitle("Estimated Household Demographic (by percentage)
           of King County 2017-19") +
    xlab("Year") +
    ylab("Percentage of King County") +
    scale_fill_discrete(name = "Household Race")

  # Make the chart interactive and display only the percentage when hovering
  chart <- ggplotly(chart, tooltip = "text")

  return(chart)
}
