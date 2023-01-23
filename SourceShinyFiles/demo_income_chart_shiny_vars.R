## Timothy Kidd

library(shiny)
library(shinythemes)
library(shinyjs)
library(dplyr)
library(plotly)
library(ggplot2)
library(tidycensus)
library(tidyverse)
library(kableExtra)

census_api_key("12ad784a12bd28d651be30ca71abc7955ef0882b")

source("./my_server.R")

conditional_demographic_widget <- conditionalPanel(
  condition = "input.data_select == 'demo'",
  checkboxGroupInput(
    "race_select",
    label = "Select Race(s) to Display",
    choices = unique(kcdemographic$household_race),
    selected = unique(kcdemographic$household_race)
  )
)

conditional_income_widget <- conditionalPanel(
  condition = "input.data_select == 'income'",
  checkboxGroupInput(
    "year_select",
    label = "Select Year to Display",
    choices = c(
      "2010" = "1",
      "2015" = "2",
      "2019" = "3"
    ),
    selected = c("1", "2", "3")
  ),
  checkboxGroupInput(
    "income_select",
    label = "Select Income Bracket to Display",
    choices = unique(king_county_income_summary$income_bracket),
    selected = unique(king_county_income_summary$income_bracket)
  )
)

bar_chart_sidebar_content <- sidebarPanel(
  useShinyjs(),
  radioButtons(
    "data_select",
    label = "Select Data Type",
    choices = c(
      "Demographics" = "demo",
      "Income" = "income"
    ),
    selected = "demo"
  ),
  conditional_demographic_widget,
  conditional_income_widget
)

bar_chart_main_content <- mainPanel(
  plotlyOutput("bar_chart", height = "45vh"),
  h1("Findings"),
  conditionalPanel(
    condition = "input.data_select == 'demo'",
    p(
      "The largest count of household races in king county is White alone, not
      Hispanic or Latino in all three years shown. Though the percentage of
      White households has been decreasing with each year, it's more accurate
      to say, the growth of the non-White households is more than the growth of
      the White households, such that it results in a decreasing percentage
      trend of White households on our chart."
    )
  ),
  conditionalPanel(
    condition = "input.data_select == 'income'",
    p(
      "In 2010 the income bracket highest in count among the King County
      households is the $65k - $74k bracket with $55k - $64k coming as a close
      second place. For 2015, it was the same bracket at the top but second
      went to the $75k - $84k bracket. By 2019, these brackets shot up in count.
      The income bracket with the highest count in households was $95k - $104k,
      $105k - $114k was just bit lower in second place."
    )
  )
)

bar_chart_tab <- tabPanel(
  "Demographic/Income Charts",
  fluidRow(
    titlePanel("King County Demographic/Income"),
    sidebarLayout(
      bar_chart_sidebar_content,
      bar_chart_main_content
    )
  )
)
