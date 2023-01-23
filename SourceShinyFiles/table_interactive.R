library(tidyverse)
library(tidycensus)
library(kableExtra)
library(reactable)


# Loading Data From ACS -----------------------------------------------------

# Median Income
tbl_medincome_king <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C03_001E",
  year = 2019
) %>% select(GEOID, NAME, median_income = estimate)

# Num of Households
tbl_num_household <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C01_001E",
  year = 2019
) %>% select(GEOID, num_household = estimate)

# Num of Black Households
tbl_num_black <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C01_003E",
  year = 2019
) %>% select(GEOID, num_black = estimate)

# Num of Native American Households
tbl_num_native <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C01_004E",
  year = 2019
) %>% select(GEOID, num_native = estimate)

# Num of Asian Households
tbl_num_asian <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C01_005E",
  year = 2019
) %>% select(GEOID, num_asian = estimate)

# Num of Native Hawians and Pacific Islander Households
tbl_num_nhopi <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C01_006E",
  year = 2019
) %>% select(GEOID, num_nhopi = estimate)

# Num Other Race Households
tbl_num_other <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C01_007E",
  year = 2019
) %>% select(GEOID, num_other = estimate)

# Num Mixed Race Households
tbl_num_mixed <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C01_008E",
  year = 2019
) %>% select(GEOID, num_mixed = estimate)

# Num Latin Households
tbl_num_latin <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C01_009E",
  year = 2019
) %>% select(GEOID, num_latin = estimate)

# Num White Households
tbl_num_white <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = "S1903_C01_010E",
  year = 2019
) %>% select(GEOID, num_white = estimate)


# Data Processing ----------------------------------------------------------

# Round Median Income to Nearest 10,000 Dollars
tbl_medincome_king_rounded <- tbl_medincome_king %>% mutate(
  median_income =
    round(median_income,
      digits = -4
    )
)

## Data Frames --------------------------------------------------------------

# Combining All Data Frames Into One & Calc Percentages for Race
tbl_merdged_king_stats <- left_join(tbl_medincome_king_rounded, 
                                    tbl_num_household,
  by = "GEOID"
) %>%
  left_join(tbl_num_black, by = "GEOID") %>%
  left_join(tbl_num_native, by = "GEOID") %>%
  left_join(tbl_num_other, by = "GEOID") %>%
  left_join(tbl_num_mixed, by = "GEOID") %>%
  left_join(tbl_num_asian, by = "GEOID") %>%
  left_join(tbl_num_nhopi, by = "GEOID") %>%
  left_join(tbl_num_latin, by = "GEOID") %>%
  left_join(tbl_num_white, by = "GEOID") %>%
  mutate(
    perc_black = (num_black / num_household) * 100,
    perc_native = (num_native / num_household) * 100,
    perc_other = (num_other / num_household) * 100,
    perc_mixed = (num_mixed / num_household) * 100,
    perc_asian = (num_asian / num_household) * 100,
    perc_nhopi = (num_nhopi / num_household) * 100,
    perc_latin = (num_latin / num_household) * 100,
    perc_white = (num_white / num_household) * 100
  )


# Summary Data Frame
tbl_king_county_summary <- tbl_merdged_king_stats %>%
  group_by(median_income) %>%
  summarise(
    num_tracts = n(),
    num_household = sum(num_household),
    avg_perc_black = mean(perc_black),
    avg_perc_native = mean(perc_native),
    avg_perc_other = mean(perc_other),
    avg_perc_mixed = mean(perc_mixed),
    avg_perc_asian = mean(perc_asian),
    avg_perc_nhopi = mean(perc_nhopi),
    avg_perc_latin = mean(perc_latin),
    avg_perc_white = mean(perc_white)
  ) %>%
  mutate(
    income_bracket_min = median_income - 5000,
    income_bracket_max = median_income + 4000
  )

# Final Cleaned and Rounded Data Frame
tbl_king_county_summary_cleaned <- tbl_king_county_summary %>%
  select(
    income_bracket_min,
    income_bracket_max,
    num_tracts,
    num_household,
    avg_perc_white,
    avg_perc_black,
    avg_perc_asian,
    avg_perc_latin,
    avg_perc_native,
    avg_perc_nhopi,
    avg_perc_mixed,
    avg_perc_other,
  ) %>%
  mutate_all(round, 2) %>%
  head(-1)

# Table


options(reactable.theme = reactableTheme(
  color = "hsl(233, 9%, 87%)",
  backgroundColor = "hsl(233, 9%, 19%)",
  borderColor = "hsl(233, 9%, 22%)",
  stripedColor = "hsl(233, 12%, 22%)",
  highlightColor = "hsl(233, 12%, 24%)",
  inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
))




tb_headers <- list(
  income_bracket_min =
    colDef(
      name = "Median",
      align = "right",
      format = colFormat(
        prefix = "$",
        suffix = " -",
        separators = TRUE,
        digits = 0
      ),
    ),
  income_bracket_max = colDef(
    name = "Income Brackets ($)",
    align = "left",
    format = colFormat(
      prefix = "$",
      separators = TRUE,
      digits = 0
    )
  ),
  num_tracts = colDef(name = "Number of Census Tracts"),
  num_household = colDef(
    name = "Number of Households",
    format = colFormat(separators = TRUE)
  ),
  avg_perc_white = colDef(
    name = "% White",
    format = colFormat(
      separators = TRUE,
      suffix = "%", digits = 2
    )
  ),
  avg_perc_black = colDef(
    name = "% Black",
    format = colFormat(
      separators = TRUE,
      suffix = "%", digits = 2
    )
  ),
  avg_perc_asian = colDef(
    name = "% Asian",
    format = colFormat(
      separators = TRUE,
      suffix = "%", digits = 2
    )
  ),
  avg_perc_latin = colDef(name = "% Latinx", format = colFormat(
    separators = TRUE, suffix = "%", digits = 2
  )),
  avg_perc_native = colDef(
    name = "% Native American",
    format = colFormat(
      separators = TRUE, suffix = "%", digits = 2
    )
  ),
  avg_perc_nhopi = colDef(
    name = "% Native Hawaiian & Other Pacific Islander ",
    format = colFormat(
      separators = TRUE,
      suffix = "%", digits = 2
    )
  ),
  avg_perc_mixed = colDef(
    name = "% Multiracial Household",
    format = colFormat(
      separators = TRUE, suffix = "%",
      digits = 2
    )
  ),
  avg_perc_other = colDef(
    name = "% Other Race Household",
    format = colFormat(
      separators = TRUE,
      suffix = "%", digits = 2
    )
  )
)
# Summary Table
df_summary_table <- reactable(tbl_king_county_summary_cleaned,
  columns = tb_headers,
  pagination = FALSE,
  compact = TRUE
)

df_summary_table
