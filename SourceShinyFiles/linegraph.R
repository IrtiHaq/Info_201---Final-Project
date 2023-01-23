library(plotly)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidycensus)


MicrosoftPrice <- read.csv("data/MicrosoftandAmazonAnnualRevenue.csv")


ggplotly <- ggplot(data = MicrosoftPrice) +
  geom_line(mapping = aes(x = Year, y = Microsoft.Annual.Revenue..Millions.of.US...)) +
  labs(
    x = "Year", y = "Microsoft Annual Revenue", title =
      "Microsoft Annual Revenue vs Years"
  )


# x axis = year
# y axis = amazon stock price




Home_Value <- get_acs(
  geography = "county",
  state = "WA",
  county = "King County",
  variables = "B25077_001",
  year = 2019
)


years <- lst(2010, 2011, 2012, 2013, 2014, 2015, 2016)



medincome_king_2010_2016 <- map_dfr(
  years,
  ~ get_acs(
    geography = "county",
    state = "WA",
    county = "King County",
    variables = "S1903_C02_001E",
    year = .x,
    survey = "acs5"
  ),
  .id = "year"
)

years_2 <- lst(2017, 2018, 2019)


medincome_king_2017_2018 <- map_dfr(
  years_2,
  ~ get_acs(
    geography = "county",
    state = "WA",
    county = "King County",
    variables = "S1903_C03_001E",
    year = .x,
    survey = "acs5"
  ),
  .id = "year"
)


# binded_df <- bind_r(medincome_king_2010_2016, medincome_king_2017_2018, MicrosoftPrice)

mutated_df <- MicrosoftPrice %>% arrange((Year))


binded_df_beta <- rbind(medincome_king_2010_2016, medincome_king_2017_2018)

names(binded_df_beta)[1] <- "Year"

class(binded_df_beta[[1]]) <- "integer"


VARIABLE <- left_join(binded_df_beta, MicrosoftPrice, by = "Year")


ggplotly_1 <- ggplot(data = VARIABLE) +
  geom_point(mapping = aes(x = Microsoft.Annual.Revenue..Millions.of.US..., y = estimate)) +
  labs(
    x = "Microsoft Annual Revenue", y = " Median Income", title =
      "Microsoft Annual Revenue vs Median Income"
  ) #+
# geom_text(aes(x = Microsoft.Annual.Revenue..Millions.of.US..., y = estimate,
#               label = Year),nudge_x = -200, nudge_y = 20000, check_overlap = TRUE,
#           size = 2.25)
