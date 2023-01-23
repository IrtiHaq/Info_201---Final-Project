library(shiny)
library(plotly)
library(shinyWidgets)
library(shinyjs)
# source("map.R")

# source("page_2_data.R")
# source("SourceShinyFiles/demo_income_chart_shiny_vars.R")

load("data_to_load_in.RData")

plot_main <- mainPanel(
  plotlyOutput(outputId = "ggplotly", height = "45vh"),
  plotlyOutput(outputId = "ggplotly_1", height = "45vh")
)

plot_sidebar <- sidebarPanel(
  h4("Line Graph: Annual Revenue Over Time"),
  select_amazon <- selectInput(
    inputId = "select_amazon",
    label = "Select Company",
    choice = list(
      "Microsoft Annual Revenue" = "Microsoft.Annual.Revenue..Millions.of.US...",
      "Amazon Annual Revenue" = "Amazon.Annual.Revenue..Millions.of.US..."
    ),
    selected = "Microsoft.Annual.Revenue..Millions.of.US..."
  ), h4("Scatter Plot: Median Income vs Annual Revenue"),
  select_amazon_alpha <- selectInput(
    "select_amazon_scatter",
    label = "Select Company",
    choice = list(
      "Microsoft Annual Revenue" = "Microsoft.Annual.Revenue..Millions.of.US...",
      "Amazon Annual Revenue" = "Amazon.Annual.Revenue..Millions.of.US..."
    ),
    selected = "Microsoft.Annual.Revenue..Millions.of.US..."
  )
)


plot_tab <- tabPanel(
  "Big Tech and Income & Housing",
  fluidRow(
    sidebarLayout(
      sidebarPanel = plot_sidebar,
      mainPanel = plot_main
    )
  )
)

BubbleSidebar <- sidebarPanel(
  h4("Plot 1"),
  year_input <- sliderInput(
    "year",
    label = "Select Year", min = 2010, max = 2019,
    value = 2019, sep = "", animate =
      animationOptions(interval = 600, loop = F)
  ),
  income_input <- sliderInput(
    "income",
    label = "Filter By Median Income", min = 5000,
    max = 251000,
    value = c(5000, 251000)
  ),
  price_input <- sliderInput(
    "price",
    label = "Filter Median House Price",
    min = 22000, max = 2000010, 
    value = c(22000, 2000010)
  ),
  color_input <- sliderInput(
    "base_year",
    label = "Select Year For Color Encoding",
    min = 2010, max = 2019, value = 2010, sep = ""
  ),
  h4("Plot 2:"),
  year_input <- sliderInput(
    "year2",
    label = "Select Year", min = 2017, max = 2019,
    value = 2019, sep = "", animate =
      animationOptions(interval = 1000, loop = F)
  ),
  race_input <- radioButtons(
    "race",
    label = "Select Race",
    choices = list(
      "Black" = "Black.Percentage",
      "American Indian and Alaska Native" = "AIAN.Percentage",
      "Asian" = "Asian.Percentage",
      "Native Hawaiian and Other Pacific Islander" = "NHOPI.Percentage",
      "Some other race" = "SOR.Percentage",
      "Two or more races" = "TOM.Percentage",
      "Hispanic or Latino origin" = "HL.Percentage",
      "White" = "White.Percentage"
    ),
    selected = "Black.Percentage"
  ),
  display_input <- selectInput(
    "display",
    label = "Select variable",
    choices = list(
      "Median Income" = "Median.Income",
      "House Price" = "House.Price"
    ),
    selected = "Median.Income"
  ),
  color2_input <- sliderInput(
    "base_year_2",
    label = "Select Year For Color Encoding",
    min = 2017, max = 2019, value = 2017, sep = ""
  ), 
)

BubbleMain <- mainPanel(
  plotlyOutput(outputId = "BubblePlot", height = "45vh"),
  plotlyOutput(outputId = "BubblePlot2", height = "45vh")
)

BubblePlotTab <- tabPanel(
  "Income,  Home Value  & Race",
  fluidRow(
    sidebarLayout(
      BubbleSidebar,
      BubbleMain
    )
  )
)

intro_main_panel <- column(
  9, h1("Introduction"),
  p(
  "\n\n  Over the last decade, we have seen unprecedented levels of economic
  growth in the Seattle Metro Area. We have seen how the growth of large tech
  corporations like Amazon and Microsoft have driven levels of income sky high
  and have attracted talent from around the world. However, this growth has
  fuelled a major Housing Affordability crisis in King County Washington.
  Rising levels of income have fluid a sharp rise in the cost's homes. This
  increase in house price has displaced lower-income residents and have
  particularly affected people of colour. Leading to gentrification and fueling
  stark divides in Inequality."
  ),
  p(
  "\n\n  Through this project, we aim to quantity and visualise these changes
  over the last few years, in hopes to better highlight this pressing issue.
  Through this project we aim to answer three main questions:\n"
  ),
  tags$ol(
    tags$li(
  "How have Median Income and Median house prices changed over time in areas 
  around King County?"
  ),
  tags$li(
  "How has increases in Median House prices affected Diversity in an Area?"
  ),
  tags$li(
  "Has the growth of Large Tech companies like Amazon and Microsoft 
  contributed to these changes?"
  )
  ),
  h3("Data:"),
  p(
  "\n\n  For our project, our data mainly comes American Community Survey from
  the Census Bureau. The ACS is a an annually survey that ask a sample of
  resident's key questions. Using the ACS and the Tidy Census package we can
  get detailed information about the composition of individual Census Tracts.
  Census Tracts are like zip codes in a sense that they aim to subdivide a
  city into small units. The size of Census tracts are similar to the size
  of zip codes. Using the ACS, we were able to get the Median income,
  number of Households, Racial demographics, and the Median Value of homes
  for each individual tract. Aside from the Census are also using both
  Macrotrends.net the get the annual revenue for both Amazon and Microsoft.
  We will mostly be using Data from the last 10 years for our analysis."
  )
)

IntroductionTab <- tabPanel(
  "Introduction",
  includeCSS("styles.css"),
  HTML(' <div class="header">
  <h1 > Housing, Race & Income in King County </h1>
  <p>A Look Into The Growth of Big Tech & Housing </p>
</div>'),
  column(1),
  intro_main_panel,
  column(2)
)


MapTab <- tabPanel(
  "King County Map",
    leafletOutput("mymap", width = "100%", height = "94vh"),
    absolutePanel(shinyjs::useShinyjs(),
      dropdownButton(
        selectInput(
          inputId = "dataset_selct",
          label = "Display:",
          choices = list(
            "Median Income" = "a",
            "Median Home Value" = "b",
            "Percent Race" = "c"
          ),
          selected = "a",
          multiple = FALSE,
        ),
        conditionalPanel(
          condition = "input.dataset_selct == 'c'",
          radioButtons(
            inputId = "race_selct",
            label = "Displayed Race:",
            choices = list(
              "White" = "perc_white",
              "Asian" = "perc_asian",
              "Latinx" = "perc_latin",
              "Black" = "perc_black"
            ),
            selected = "perc_white"
          )
        ),
        sliderInput(
          inputId = "year_selct",
          label = "Select Year",
          min = 2010,
          max = 2019,
          value = 2019,
          sep = "",
          step = 1,
          animate = animationOptions(interval = 600),
          width = "95%"
        ),
        circle = TRUE,
        status = "danger",
        size = "default",
        icon = icon("gear"),
        right = TRUE,
        up = FALSE,
        width = NULL,
        margin = "10px",
        inline = FALSE,
        inputId = NULL
      ),
      style = "z-index:500;",
      class = "panel panel-default",
      draggable = FALSE,
       top = "7vh",
      right = "2%",
      fixed = TRUE,
      
    )
  )


sum_main_panel <- column(
  9, h1("Conclusion"),
  p(
  "\n\nFrom looking at median income and average house prices in different tracts in
  King County, we can clearly see that the median income hasn't increased much
  over the last decade. However, while the highest median home value was around
  a million dollars in 2010, the overall home values began increasing in 2015, 
  speeding up in 2019. Since we adjusted the values to account for inflation, 
  this increase in house prices demonstrates a rise in real home values and
  illustrates the housing affordability crisis in King county. Over the last
  6 - 7 years the price of homes and thus their values have skyrocketed."
  ),
  p(
  "\n\nWe can also find a relationship between Race and the median income in King
  County. In terms of demographics King county is mostly composed of white
  households, they are the majority in most tracts, and we can find that nearly
  all areas with a high median income are white-dominated. In contrast, the
  tracts with a relatively high concentration of minorities, especially black
  and Latin households, usually have a lower median income. These trends don't
  vary much over time and such doesn't seem to be either
  improving or worsening."
  ),
  p(
  "\n\nThis trend is even more clear when we look at the map, areas, where median
  income levels are the lowest, are all almost exclusively dominated by Black
  and Latin residents. We can see large concentrations of Black and Latin
  residents in South Seattle, where income levels are also some of the Lowest.
  Whereas some of the high-income areas have the least diverse populations and
  are mostly dominated by Black residents. While the divide for Median Income
  is already pretty stark when we toggle Median home price the divide widens
  even further."
  ),
  p(
  "\n\nThe line plot and scatter plot reflect the impact of the growth of Big Tech
  on the housing affordability crisis and gentrification in King county. From
  the line graph, we can see the rapid growth of these large tech corporations
  over the last decade. This pattern applies to both Microsoft and Amazon."
  ),
  p(
  "\n\nThe scatter plot shows a strong correlation between the growth of these
  large companies and the rise in median household income in King county.
  While the scatter plot stopped in the year 2020, it would be fair to assume
  that the rise would not likely go away anytime soon. Similar to the line plot,
  this is irrespective of which company it was, as Amazon and Microsoft both
  showed the same pattern of causing median household income and home values
  to increase as well around the same time, their annual revenue goes up.
  It's worth noting that this data is focused on King County, Washington and
  both of these Tech Corporations have their headquarters in Washington."
  )
)

SummaryTab <- tabPanel(
  "Conclusion",
  includeCSS("styles.css"),
  HTML(' <div class="header">
  <h1 > Summary </h1>
  <hr>
  <p>Housing, Race & Income in King County</p>
</div>'),
  column(1),
  sum_main_panel,
  column(2)
)

table_tab <- tabPanel(
  "Demographics & Income Table",
  fluidRow(
    h1("Income and Demographic Characteristics of Area within King County"),
    h3("Table Showing an Area's Median Income and its Average Racial Composition"),
    reactableOutput("f_summary_table")
  )
)


ui <- navbarPage(
  theme = shinytheme("flatly"),
  "Housing, Race & Income in King County",
  IntroductionTab,
  table_tab,
  bar_chart_tab,
  BubblePlotTab,
  plot_tab,
  MapTab,
  SummaryTab
)
