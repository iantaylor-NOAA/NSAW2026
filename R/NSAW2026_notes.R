# Data downloaded from StockSMART
# https://apps-st.fisheries.noaa.gov/stocksmart?app=download-data
# criteria was to select the following:
# - Select Data to Download: Assessment Time Series Data
# - Select stocks to include in the report: "Pacific Coast Groundfish" under management plan,
# - Select All Stocks
# - Download

library(ggplot2)
library(tidyr)
library(dplyr)

# read the timeseries info downloaded from StockSMART:
dat <- readxl::read_excel(
    "data-raw/Assessment_TimeSeries_Data.xlsx",
    col_names = FALSE
)
dat[1:10, 1:5]
# # A tibble: 10 x 5
#    ...1             ...2  ...3                                ...4                                ...5
#    <chr>            <chr> <chr>                               <chr>                               <chr>
#  1 Stock Name       NA    Arrowtooth flounder - Pacific Coast Arrowtooth flounder - Pacific Coast Arrowtooth flounder - Pacific Coast
#  2 Stock ID         NA    10005                               10005                               10005
#  3 Assessment ID    NA    11637                               11637                               11637
#  4 Assessment Year  NA    2021                                2021                                2021
#  5 Assessment Month NA    9                                   9                                   9
#  6 Parameter        Year  Catch                               Fmort                               Recruitment
#  7 Description      NA    Modeled Total Catch                 Fishing Intensity                   Abundance - Age 0
#  8 Unit             NA    Metric Tons                         1 - SPR                             Thousand Recruits
#  9 NA               1875  NA                                  NA                                  NA
# 10 NA               1876  NA                                  NA                                  NA

# pivot the data to make it easier to work
# rows should be years, columns should be stocks/parameters

# transpose
dat2 <- dat |> t()
# get year values to fill in first row
dat2[1, -(1:8)] <- dat2[2, -(1:8)]
dat2 <- as.data.frame(dat2, stringsAsFactors = FALSE)
names(dat2) <- dat2[1, ]
dat2 <- dat2[-(1:2), ] # remove first two rows now included as column names

# pivot longer to get a long format, preserving first 8 columns of metadata
# and mapping rownames to new "Year" column
dat_long <- dat2 |>
    pivot_longer(
        cols = -(1:8),
        names_to = "Year",
        values_to = "Value"
    )

# clean up columns and remove ID columns
dat_long <- dat_long |>
    select(
        stock = `Stock Name`,
        assessment_year = `Assessment Year`,
        parameter = Parameter,
        description = Description,
        unit = Unit,
        year = Year,
        value = Value
    ) |>
    mutate(
        year = as.integer(year),
        value = as.numeric(value)
    )

# remove two stocks:
# 1. Pacific Hake (fishery is very distinct)
# 2. Pacific grenadier (no longer in FMP)
dat_long <- dat_long |>
    filter(
        !grepl("hake|grenadier", tolower(stock))
    )

# remove oldest assessments (obsolete black rockfish, shortbelly, and starry flounder)
dat_long <- dat_long |>
    filter(
        assessment_year > 2010
    )
# remove 2019 "Black rockfish - California" as it's been superseded by
# 2023 assessments for different parts of California
dat_long <- dat_long |>
    filter(stock != "Black rockfish - California")

# manually update Yellowtail Rockfish info using the 2025 assessment
source("R/update_info.R")

# remove old yellowtail data and add in new rows
dat_long <- dat_long |> filter(!grepl("yellowtail", tolower(stock)))
dat_long <- rbind(
    dat_long,
    update_info(
        stock_name = "Yellowtail rockfish - Northern Pacific Coast",
        ss3files = "https://raw.githubusercontent.com/pfmc-assessments/yellowtail_2025/refs/heads/main/Model_Runs/5.09_no_extra_SE",
        spawn_output_units = "Number x 1,000,000,000,000",
        assessment_year = 2025
    )
)

# remove old chilipepper data and add in new rows
dat_long <- dat_long |> filter(!grepl("chilipepper", tolower(stock)))
dat_long <- rbind(
    dat_long,
    update_info(
        stock_name = "Chilipepper - Southern Pacific Coast",
        ss3files = "https://raw.githubusercontent.com/EJDick-NOAA/Chilipepper-Assessment-2025/refs/heads/main/models/Model%20167a%2C%20FINAL%20base%20model%2C%20with%20updated%20forecast%20buffers",
        spawn_output_units = "Number x 1,000,000,000",
        assessment_year = 2025
    )
)

# remove old widow data and add in new rows
dat_long <- dat_long |> filter(!grepl("widow", tolower(stock)))
dat_long <- rbind(
    dat_long,
    update_info(
        stock_name = "Widow rockfish - Pacific Coast",
        ss3files = "https://raw.githubusercontent.com/mcgoodman/widow_rockfish_2025/refs/heads/main/models/2025%20base%20model",
        spawn_output_units = "Number x 1,000,000,000",
        assessment_year = 2025
    )
)

# create a species group for each stock
get_group <- function(stock) {
    case_when(
        # rockfish
        grepl("rockfish", stock, ignore.case = TRUE) ~ "Rockfish",
        grepl("Bocaccio", stock, ignore.case = TRUE) ~ "Rockfish",
        grepl("Chilipepper", stock, ignore.case = TRUE) ~ "Rockfish",
        grepl("Cowcod", stock, ignore.case = TRUE) ~ "Rockfish",
        grepl("scorpionfish", stock, ignore.case = TRUE) ~ "Rockfish",
        grepl("thornyhead", stock, ignore.case = TRUE) ~ "Rockfish",
        grepl("Pacific ocean perch", stock, ignore.case = TRUE) ~ "Rockfish",
        # flatfish
        grepl("flounder", stock, ignore.case = TRUE) ~ "Flatfish",
        grepl("sole", stock, ignore.case = TRUE) ~ "Flatfish",
        grepl("sanddab", stock, ignore.case = TRUE) ~ "Flatfish",
        # elasmobranchs
        grepl("skate", stock, ignore.case = TRUE) ~ "Elasmobranch",
        grepl("dogfish", stock, ignore.case = TRUE) ~ "Elasmobranch",
        # roundfish
        grepl("Cabezon", stock, ignore.case = TRUE) ~ "Roundfish",
        grepl("Cabezon", stock, ignore.case = TRUE) ~ "Roundfish",
        grepl("Kelp greenling", stock, ignore.case = TRUE) ~ "Roundfish",
        grepl("Lingcod", stock, ignore.case = TRUE) ~ "Roundfish",
        grepl("Pacific cod", stock, ignore.case = TRUE) ~ "Roundfish",
        grepl("Sablefish", stock, ignore.case = TRUE) ~ "Roundfish",
        # other (should be empty)
        TRUE ~ "Other"
    )
}

# remove area specification to get species name only
dat_long <- dat_long |>
    mutate(
        group = get_group(stock),
        species = sub(" - .*", "", stock)
    )


# aggregate across years within each species
dat_long |>
    filter(parameter == "Catch") |>
    group_by(species) |>
    summarize(
        total_catch = sum(value, na.rm = TRUE)
    ) |>
    arrange(desc(total_catch))

#  1 Dover sole              842651.
#  2 Sablefish               623774.
#  3 Widow rockfish          379485.
#  4 Yellowtail rockfish     265674.
#  5 Arrowtooth flounder     257308.
#  6 Lingcod                 227610.
#  7 Petrale sole            214367.
#  8 Spiny dogfish           196530.
#  9 Bocaccio                186379
# 10 Pacific ocean perch     149962.
# # i 27 more rows
# # i Use `print(n = ...)` to see more rows

# find the top n rockfish by total catch
top_n_rockfish <- dat_long |>
    filter(group == "Rockfish") |>
    filter(parameter == "Catch") |>
    group_by(species) |>
    summarize(
        total_catch = sum(value, na.rm = TRUE)
    ) |>
    arrange(desc(total_catch)) |>
    slice_head(n = 8) |> # currently top 8
    pull(species)

top_n_rockfish |> as_tibble()
# # A tibble: 8 x 1
#   value
#   <chr>
# 1 Widow rockfish
# 2 Yellowtail rockfish
# 3 Bocaccio
# 4 Pacific ocean perch
# 5 Canary rockfish
# 6 Chilipepper
# 7 Shortspine thornyhead
# 8 Longspine thornyhead

# add colors for of the top n rockfish species using the okabe-ito palette
# but assigned to vaguely match fish colors in a few cases
dat_long$color <- case_when(
    dat_long$species == "Widow rockfish" ~ "#000000",
    dat_long$species == "Yellowtail rockfish" ~ "#009E73",
    dat_long$species == "Bocaccio" ~ "#56B4E9",
    dat_long$species == "Pacific ocean perch" ~ "#D55E00",
    dat_long$species == "Canary rockfish" ~ "#E69F00",
    dat_long$species == "Chilipepper" ~ "#CC79A7",
    dat_long$species == "Shortspine thornyhead" ~ "#F0E442",
    dat_long$species == "Longspine thornyhead" ~ "#0072B2",
    TRUE ~ "gray70"
)

# group all rockfish outside the top n into "Other rockfish"
dat_long <- dat_long |>
    mutate(
        species_with_other = factor(
            ifelse(
                species %in% top_n_rockfish,
                species,
                "Other rockfish"
            ),
            levels = c(
                setdiff(top_n_rockfish, "Other rockfish"),
                "Other rockfish"
            )
        )
    )

# plot catch for all groundfish (except hake and grenadier which were removed earlier)
dat_long |>
    filter(parameter == "Catch") |>
    filter(year <= 2022) |>
    ggplot(aes(x = year, y = value, fill = group)) +
    geom_bar(stat = "identity") +
    labs(
        title = "Total Catch by Stock Over Time",
        x = "Year",
        y = "Catch (metric tons)"
    ) +
    theme_minimal()

# just plot the rockfish with "Other rockfish" grouping
dat_long |>
    filter(group == "Rockfish") |>
    filter(parameter == "Catch") |>
    filter(year <= 2022) |>
    filter(!is.na(value) & year > 1920) |>
    ggplot(aes(x = year, y = value, fill = species_with_other)) +
    scale_fill_manual(
        values = setNames(dat_long$color, dat_long$species_with_other)
    ) +
    geom_bar(stat = "identity") +
    labs(
        #title = "Total Catch by Rockfish Species Over Time",
        x = "Year",
        y = "Catch (metric tons)"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(fill = "")
ggsave("figures/NSAW2026_groundfish_catch_rockfish.png", width = 8, height = 6)

#TODO: consider using GEMM to update recent catch for bocaccio, POP, and longspine

# calculate Abundance plots relative to first non-NA value of abundance
#dat_long <- dat_long_backup
for (i in unique(dat_long$stock)) {
    # get abundance data for this stock
    dat_sub <- dat_long |>
        filter(stock == i) |>
        filter(parameter == "Abundance") |>
        arrange(year)
    # get first non-NA value for abundance
    first_value <- first(na.omit(dat_sub$value))
    # modify dat_sub to create Abundance_ratio
    dat_sub <- dat_sub |>
        mutate(
            value = value / first_value,
            parameter = "Abundance_ratio",
            description = paste0(
                description,
                " (relative to first non-NA value)"
            )
        )
    # rbind the modified rows back to dat_long
    dat_long <- dat_long |>
        rbind(dat_sub)
}

dat_long |>
    filter(group == "Rockfish") |>
    filter(parameter == "Abundance_ratio") |>
    filter(species_with_other != "Other rockfish") |>
    filter(!is.na(value) & year > 1920 & year <= 2025) |>
    ggplot(aes(
        x = year,
        y = value,
        color = species_with_other #,
        #group = species
    )) +
    #scale_color_identity() +
    scale_color_manual(values = setNames(dat_long$color, dat_long$species_with_other)) +
    geom_line(linewidth = 1.3) +
    geom_hline(
        yintercept = c(0, 0.25, 0.4, 1.0),
        #linetype = "dashed",
        color = "gray50"
    ) +
    annotate(
        "text",
        x = -Inf,
        y = c(0.25, 0.4),
        label = c("Minimum biomass threshold", "Biomass target"),
        hjust = 0,
        vjust = -0.5,
        size = 3,
        color = "gray50"
    ) +
    labs(
        #title = "Relative Rockfish Abundance Over Time",
        x = "Year",
        y = "Fraction of unfished spawning output"
    ) +
    theme_minimal() +
    scale_y_continuous(breaks = c(0, 0.25, 0.4, 1.0)) +
    theme(panel.grid.major.x = element_line(color = "gray90"),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank())

ggsave(
    "figures/NSAW2026_groundfish_abundance_rockfish.png",
    width = 8,
    height = 6
)
