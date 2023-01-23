library(tidyverse)
library(tidycensus)
options(tigris_use_cache = TRUE)
library(leaflet)

# Loading Data From ACS -----------------------------------------------------
medincome_king <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C03_001E",
  year = 2019,
  geometry = TRUE
)

# Creating Map --------------------------------------------------------------

# Setup Color Pallet
pal <- colorNumeric(
  palette = "magma",
  domain = medincome_king$estimate
)


# Create Map

 

income_map <- leaflet() %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  addPolygons(
    data = medincome_king,
    color = ~ pal(estimate),
    weight = 0.5,
    smoothFactor = 0.2,
    fillOpacity = 0.5,
    label = ~estimate
  ) %>%
  addLegend(
    position = "bottomleft",
    pal = pal,
    values = medincome_king$estimate,
    title = " Estimated Median Income </br> In 2019 (2016 Dollars)"
  )
