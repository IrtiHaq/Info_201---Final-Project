library(tidyverse)
library(tidycensus)
library(ggplot2)
library(plotly)
library(priceR)

# House price data from 2010 to 2019
for (i in 1:10) {
  name <- paste0("price_201", (i - 1))
  assign(name, get_acs(
    geography = "tract",
    state = "WA",
    county = "King County",
    variables = "B25077_001",
    year = as.numeric(paste0("201", (i - 1)))
  ) %>% mutate(year = paste0("201", (i - 1))))
}

years <- lst(2010, 2011, 2012, 2013, 2014, 2015, 2016)

median_income_2010_2016 <- map_dfr(
  years,
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



# Income data from 2017 to 2019
for (i in 7:9) {
  name <- paste0("median_income_201", i)
  assign(name, get_acs(
    geography = "tract",
    state = "WA",
    county = "King County",
    variables = "S1903_C03_001E",
    year = as.numeric(paste0("201", i))
  ) %>% mutate(year = paste0("201", i)))
}

# Total Households from 2017 to 2019
for (i in 0:9) {
  name <- paste0("pop_201", i)
  assign(name, get_acs(
    geography = "tract",
    state = "WA",
    county = "King County",
    variables = "S1903_C01_001E",
    year = as.numeric(paste0("201", i))
  ) %>% mutate(year = paste0("201", i)))
}

# Black Households from 2017 to 2019
for (i in 7:9) {
  name <- paste0("black_201", i)
  assign(name, get_acs(
    geography = "tract",
    state = "WA",
    county = "King County",
    variables = "S1903_C01_003E",
    year = as.numeric(paste0("201", i))
  ) %>% mutate(year = paste0("201", i)))
}

# American Indian and Alaska Native Households from 2017 to 2019
for (i in 7:9) {
  name <- paste0("aian_201", i)
  assign(name, get_acs(
    geography = "tract",
    state = "WA",
    county = "King County",
    variables = "S1903_C01_004E",
    year = as.numeric(paste0("201", i))
  ) %>% mutate(year = paste0("201", i)))
}

# Asian Households from 2017 to 2019
for (i in 7:9) {
  name <- paste0("asian_201", i)
  assign(name, get_acs(
    geography = "tract",
    state = "WA",
    county = "King County",
    variables = "S1903_C01_005E",
    year = as.numeric(paste0("201", i))
  ) %>% mutate(year = paste0("201", i)))
}

# Native Hawaiian and Other Pacific Islander Households from 2017 to 2019
for (i in 7:9) {
  name <- paste0("nhopi_201", i)
  assign(name, get_acs(
    geography = "tract",
    state = "WA",
    county = "King County",
    variables = "S1903_C01_006E",
    year = as.numeric(paste0("201", i))
  ) %>% mutate(year = paste0("201", i)))
}

# Some other race Households from 2017 to 2019
for (i in 7:9) {
  name <- paste0("sor_201", i)
  assign(name, get_acs(
    geography = "tract",
    state = "WA",
    county = "King County",
    variables = "S1903_C01_007E",
    year = as.numeric(paste0("201", i))
  ) %>% mutate(year = paste0("201", i)))
}

# Two or more races Households from 2017 to 2019
for (i in 7:9) {
  name <- paste0("tom_201", i)
  assign(name, get_acs(
    geography = "tract",
    state = "WA",
    county = "King County",
    variables = "S1903_C01_008E",
    year = as.numeric(paste0("201", i))
  ) %>% mutate(year = paste0("201", i)))
}

# Hispanic or Latino Households from 2017 to 2019
for (i in 7:9) {
  name <- paste0("hl_201", i)
  assign(name, get_acs(
    geography = "tract",
    state = "WA",
    county = "King County",
    variables = "S1903_C01_009E",
    year = as.numeric(paste0("201", i))
  ) %>% mutate(year = paste0("201", i)))
}

# White Households from 2017 to 2019
for (i in 7:9) {
  name <- paste0("white_201", i)
  assign(name, get_acs(
    geography = "tract",
    state = "WA",
    county = "King County",
    variables = "S1903_C01_010E",
    year = as.numeric(paste0("201", i))
  ) %>% mutate(year = paste0("201", i)))
}


all_price <- rbind(
  price_2010, price_2011,
  price_2012, price_2013,
  price_2014, price_2015,
  price_2016, price_2017,
  price_2018, price_2019
)

all_price <- all_price %>%
  select(GEOID, NAME, estimate, year) %>%
  rename(House.Price = estimate)

all_income <- rbind(
  median_income_2010_2016, median_income_2017,
  median_income_2018, median_income_2019
) %>%
  mutate(median_income = round(adjust_for_inflation(estimate,
    year,
    "US",
    to_date = 2019
  ))) %>%
  select(GEOID, NAME, median_income, year) %>%
  rename(Median.Income = median_income)


all_pop <- rbind(
  pop_2010, pop_2011, pop_2012,
  pop_2013, pop_2014, pop_2015, pop_2016,
  pop_2017, pop_2018, pop_2019
) %>%
  select(GEOID, NAME, estimate, year) %>%
  rename(Total.Households = estimate)

all_black <- rbind(black_2017, black_2018, black_2019) %>%
  select(GEOID, NAME, estimate, year) %>%
  rename(Black.Households = estimate)

all_aian <- rbind(aian_2017, aian_2018, aian_2019) %>%
  select(GEOID, NAME, estimate, year) %>%
  rename(American.Indian.and.Alaska.Native.Households = estimate)

all_asian <- rbind(asian_2017, asian_2018, asian_2019) %>%
  select(GEOID, NAME, estimate, year) %>%
  rename(Asian.Households = estimate)

all_nhopi <- rbind(nhopi_2017, nhopi_2018, nhopi_2019) %>%
  select(GEOID, NAME, estimate, year) %>%
  rename(Native.Hawaiian.and.Other.Pacific.Islander.Households = estimate)

all_sor <- rbind(sor_2017, sor_2018, sor_2019) %>%
  select(GEOID, NAME, estimate, year) %>%
  rename(Some.Other.Race.Households = estimate)

all_tom <- rbind(tom_2017, tom_2018, tom_2019) %>%
  select(GEOID, NAME, estimate, year) %>%
  rename(Two.or.More.Race.Households = estimate)

all_hl <- rbind(hl_2017, hl_2018, hl_2019) %>%
  select(GEOID, NAME, estimate, year) %>%
  rename(Hispanic.or.Latino.Households = estimate)

all_white <- rbind(white_2017, white_2018, white_2019) %>%
  select(GEOID, NAME, estimate, year) %>%
  rename(White.Households = estimate)

together <- all_income %>%
  left_join(all_price, by = c("GEOID", "NAME", "year")) %>%
  left_join(all_pop, by = c("GEOID", "NAME", "year")) # %>%
# na.omit() # Drop NA if wanted

together <- together %>%
  left_join(all_black, by = c("GEOID", "NAME", "year")) %>%
  left_join(all_aian, by = c("GEOID", "NAME", "year")) %>%
  left_join(all_asian, by = c("GEOID", "NAME", "year")) %>%
  left_join(all_nhopi, by = c("GEOID", "NAME", "year")) %>%
  left_join(all_sor, by = c("GEOID", "NAME", "year")) %>%
  left_join(all_tom, by = c("GEOID", "NAME", "year")) %>%
  left_join(all_hl, by = c("GEOID", "NAME", "year")) %>%
  left_join(all_white, by = c("GEOID", "NAME", "year")) %>%
  mutate(
    Black.Percentage = Black.Households / Total.Households,
    AIAN.Percentage = American.Indian.and.Alaska.Native.Households / Total.Households,
    Asian.Percentage = Asian.Households / Total.Households,
    NHOPI.Percentage = Native.Hawaiian.and.Other.Pacific.Islander.Households / Total.Households,
    SOR.Percentage = Some.Other.Race.Households / Total.Households,
    TOM.Percentage = Two.or.More.Race.Households / Total.Households,
    HL.Percentage = Hispanic.or.Latino.Households / Total.Households,
    White.Percentage = White.Households / Total.Households
  )

min_income <- min(together$Median.Income, na.rm = T)
max_income <- max(together$Median.Income, na.rm = T)
min_price <- min(together$House.Price, na.rm = T)
max_price <- max(together$House.Price, na.rm = T)


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
my_plot2 <- ggplot(data = together %>% filter(year == 2019), aes_string(x = "Black.Percentage", y = "Median.Income")) +
  geom_point(aes(size = log(Total.Households)), alpha = 0.5) +
  labs(
    x = displaying_label["Black.Percentage"],
    y = y_axis_label["Median.Income"],
    title = "2019"
  ) #+
# scale_size(range = c(1, 5), name="Number of Households")
my_plotly_plot2 <- ggplotly(my_plot2)
