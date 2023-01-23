# MicrosoftPrice <- read.csv("data/MicrosoftandAmazonAnnualRevenue.csv")

# library(plotly)
# library(ggplot2)
# library(shiny)
# library(reactable)

# source("SourceShinyFiles/page_2_data.R")
# source("SourceShinyFiles/map.R")
# source("SourceShinyFiles/linegraph.R")
# source("SourceShinyFiles/table_interactive.R")
# source("SourceShinyFiles/demographic_data.R")
# source("SourceShinyFiles/income_data.R")

# ggplotly <- ggplot(data = MicrosoftPrice) +
#   geom_line(mapping = aes(x = Year, y = Microsoft.Annual.Revenue..Millions.of.US...)) +
#   labs(x = "Year", y = "Microsoft Annual Revenue", title =
#          "Microsoft Annual Revenue vs Years")

server <- function(input, output) {
  output$ggplotly <- renderPlotly({
    label_r <- if (input$select_amazon == "Microsoft.Annual.Revenue..Millions.of.US...") {
      paste("Microsoft Revenue Over Time")
    } else {
      paste("Amazon Revenue Over Time")
    }
    my_plot <- ggplot(data = MicrosoftPrice) +
      geom_line(mapping = aes(x = Year, y = !!as.name(input$select_amazon))) +
      labs(
        x = "Year", y = "Revenue [$]", title =
          label_r
      )

    my_plotly_plot <- ggplotly(my_plot)
    return(my_plotly_plot)
  })
  output$ggplotly_1 <- renderPlotly({
    label_y <- if (input$select_amazon_scatter == "Microsoft.Annual.Revenue..Millions.of.US...") {
      paste("Microsoft Revenue Over Time")
    } else {
      paste("Amazon Revenue Over Time")
    }
    Microsoft_List <- list(
      "Microsoft.Annual.Revenue..Millions.of.US..." = "Microsoft Revenue",
      "Amazon.Annual.Revenue..Millions.of.US..." = "Amazon Revenue"
    )
    my_plot_1 <- ggplot(data = VARIABLE) +
      geom_point(mapping = aes_string(x = input$select_amazon_scatter, y = VARIABLE$estimate)) +
      labs(
        x = "Revenue [$]", y = " Median Income", title =
          label_y
      ) #+
    # geom_text(aes(x = Microsoft.Annual.Revenue..Millions.of.US..., y = estimate,
    #               label = Year),nudge_x = -200, nudge_y = 20000, check_overlap = TRUE,
    #           size = 2.25)

    my_plotly_plot_1 <- ggplotly(my_plot_1)
    return(my_plotly_plot_1)
  })


  output$BubblePlot <- renderPlotly({
    this_year <- together %>% filter(year == input$year)
    base_year <- together %>% filter(year == input$base_year)
    my_plot <- ggplot(
      data = this_year,
      aes(
        x = Median.Income, y = House.Price,
        size = log(Total.Households)
      )
    ) +
      geom_point(aes_string(color = base_year$House.Price), alpha = 0.5) +
      labs(
        x = "Median Income", y = "House Price",
        color = paste("House Price\nin", input$base_year),
        title = paste(
          "King County Median Income vs. House Price in", input$year
        )
      ) +
      scale_x_continuous(limits = input$income) +
      scale_y_continuous(limits = input$price) +
      scale_size(range = c(1, 3), name = "Number of Households")

    my_plotly_plot <- ggplotly(my_plot)

    return(my_plotly_plot)
  })

  output$BubblePlot2 <- renderPlotly({
    displaying_label <- list(
      "Black.Percentage" = "Black Households",
      "AIAN.Percentage" = "American Indian and Alaska Native Households",
      "Asian.Percentage" = "Asian Households",
      "NHOPI.Percentage" = "Native Hawaiian and Other Pacific Islander Households",
      "SOR.Percentage" = "Some Other Race Households",
      "TOM.Percentage" = "Two or More Race Households",
      "HL.Percentage" = "Hispanic or Latino Households",
      "White.Percentage" = "White Households"
    )
    y_axis_label <- list(
      "Median.Income" = "Median Income",
      "House.Price" = "House Price"
    )
    base_year <- together %>% filter(year == input$base_year_2)
    base_percentage <- input$race
    my_plot2 <- ggplot(
      data = together %>% filter(year == input$year2),
      aes_string(
        x = input$race, y = input$display,
        color = base_year[[base_percentage]]
      )
    ) +
      geom_point(aes(size = log(Total.Households)), alpha = 0.5) +
      labs(
        x = paste(displaying_label[input$race], "Percentage"),
        y = paste(y_axis_label[input$display]),
        color = paste(
          "Percentage in \nKing County",
          input$base_year_2
        ),
        title = (paste(
          displaying_label[input$race], "Percentage vs.",
          y_axis_label[input$display],
          "in", input$year2
        ))
      ) +
      xlim(0, 1) +
      scale_size(range = c(1, 3), name = "Number of Households")
    my_plotly_plot2 <- ggplotly(my_plot2)

    return(my_plotly_plot2)
  })

  # Map: --------------------------------------------------------------
  min_year <- 2010
  observeEvent(input$dataset_selct, {
    get_min_year <- function(a) {
      if (a == "c") {
        min_year <<- 2017
      } else {
        min_year <<- 2010
      }
    }

    get_min_year(input$dataset_selct)

    updateSliderInput(inputId = "year_selct", min = min_year)
  })

  output$mymap <- renderLeaflet({
    get_king_map(
      input$year_selct,
      input$dataset_selct,
      input$race_selct
    )
  })

  ## Timothy Kidd demo/income chart page 1 Start
  observe({
    if (length(input$year_select) == 1) {
      shinyjs::disable(
        selector = paste0(
          "#year_select input[value='",
          input$year_select,
          "']"
        )
      )
    }
    if (length(input$year_select) > 1) {
      shinyjs::enable(
        selector = paste0(
          "#year_select input[value='",
          input$year_select,
          "']"
        )
      )
    }

    output$bar_chart <- renderPlotly({
      if (input$data_select == "demo") {
        data <- kcdemographic %>%
          filter(household_race %in% input$race_select)

        chart <- create_demographics_chart(data)
      } else {
        data <- king_county_income_summary %>%
          filter(income_bracket %in% input$income_select) %>%
          filter(year %in% input$year_select)

        chart <- create_income_chart(data)
      }

      return(chart)
    })
  })
  # Summary Table
  output$f_summary_table <- renderReactable({
    df_summary_table
  })


  ## Timothy Kidd End
}
