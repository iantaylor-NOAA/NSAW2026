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

# load a bunch of files associated with POP 2017 assessment
load(url(
    "https://github.com/chantelwetzel-noaa/POP_2017/raw/refs/heads/master/r4ss/SS_output.RData"
))
# remove old POP data and add in new rows
dat_long <- dat_long |> filter(!grepl("perch", tolower(stock)))
dat_long <- rbind(
    dat_long,
    update_info(
        stock_name = "Pacific ocean perch - Pacific Coast",
        model = mod1,
        spawn_output_units = "Number x 1,000,000",
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
        grepl("scorpionfish", stock, ignore.case = TRUE) ~ "Roundfish",
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

# check if top_n_rockfish have multiple stocks per species (answer: no)
dat_long |>
    filter(species %in% c(top_n_rockfish)) |>
    select(stock, species) |>
    distinct() |>
    arrange(species) |>
    group_by(species) |>
    summarize(n_stocks = n()) |>
    filter(n_stocks > 1)


# get GEMM data to fill in recent years for some/all stocks
if (!exists("gemm")) {
    # try loading from the data-raw folder first
    if (file.exists("data-raw/GEMM_data/gemm_data.rdata")) {
        load("data-raw/GEMM_data/gemm_data.rdata")
    } else {
        gemm <- nwfscSurvey::pull_gemm(dir = "data-raw/GEMM_data/")
    }
}
gemm_summary <- gemm |>
    group_by(year, species) |>
    summarize(
        total_dead = sum(
            total_discard_with_mort_rates_applied_and_landings_mt
        )
    ) |>
    arrange(species, year)

gemm_summary |>
    filter(year >= 2017) |>
    group_by(species) |>
    summarize(avg_dead_2017plus = mean(total_dead)) |>
    arrange(desc(avg_dead_2017plus))

# # A tibble: 837 x 2
#    species             avg_dead_2017plus
#    <chr>                           <dbl>
#  1 Pacific Hake                  281221.
#  2 Market Squid                   41894.
#  3 Pink Shrimp                    24944.
#  4 Dungeness Crab                 23887.
#  5 Widow Rockfish                  9841.
#  6 Northern Anchovy                6310.
#  7 Albacore Tuna                   5936.
#  8 Sablefish                       5670.
#  9 Dover Sole                      5051.
# 10 Yellowtail Rockfish             3362.
# # i 827 more rows
# # i Use `print(n = ...)` to see more rows

# match gemm with dat_long stocks/species

# count rockfish species in dat_long
dat_long |>
    filter(group == "Rockfish") |>
    pull(species) |>
    unique() |>
    length()
# [1] 27

# count stocks in dat_long
dat_long |>
    filter(group == "Rockfish") |>
    pull(stock) |>
    unique() |>
    length()
# [1] 39

# check thornyhead catches in gemm
gemm |>
    filter(grepl("Thornyhead", species)) |>
    group_by(species) |>
    summarize(
        total_catch = sum(
            total_discard_with_mort_rates_applied_and_landings_mt
        )
    )
#   species                         total_catch
#   <chr>                                 <dbl>
# 1 Longspine Thornyhead                 19698.
# 2 Shortspine Thornyhead                21564.
# 3 Shortspine/Longspine Thornyhead       1446.

# change species for the top 8 rockfish (and a few others) in gemm to match dat_long species names
gemm_rock <- gemm |>
    filter(grepl("Rockfish", species) | grepl("Thornyhead", species)) |>
    mutate(
        species = case_when(
            grepl("Pacific Ocean Perch Rockfish", species) ~
                "Pacific ocean perch",
            grepl("Bocaccio Rockfish", species) ~ "Bocaccio",
            grepl("Chilipepper Rockfish", species) ~ "Chilipepper",
            grepl("Cowcod", species) ~ "Cowcod",
            grepl("Shortspine Thornyhead", species) ~ "Shortspine thornyhead",
            grepl("Longspine Thornyhead", species) ~ "Longspine thornyhead", # this probably groups the thornyhead mixes too
            grepl(
                "Rougheye",
                species
            ) ~ "Pacific Coast Blackspotted and Rougheye Rockfish Complex",
            TRUE ~ gsub("Rockfish", "rockfish", species)
        )
    )

all(top_n_rockfish %in% gemm_rock$species)
# [1] TRUE

dat_long_rockfish_species <- dat_long |>
    filter(group == "Rockfish") |>
    pull(species) |>
    unique()
gemm_rock_species <- gemm_rock$species |>
    unique()
length(dat_long_rockfish_species)
# [1] 27
length(gemm_rock_species)
# [1] 69

dat_long_rockfish_species[
    which(
        !dat_long_rockfish_species %in% gemm_rock_species
    )
]
# [1] "California Blue and Deacon Rockfish Complex"                    "Northern California Gopher / Black-and-Yellow Rockfish Complex"
# [3] "Oregon Blue and Deacon Rockfish Complex"                        "Vermilion rockfish and Sunset rockfish Complex"
gemm_rock_species[
    which(
        !gemm_rock_species %in% dat_long_rockfish_species
    )
]
#  [1] "Shortbelly rockfish"                    "Greenstriped rockfish"                  "Redstripe rockfish"                     "rockfish Unid"
#  [5] "Rosethorn rockfish"                     "Silvergray rockfish"                    "Stripetail rockfish"                    "Redbanded rockfish"
#  [9] "Shortraker rockfish"                    "Splitnose rockfish"                     "Yellowmouth rockfish"                   "Dusky rockfish"
# [13] "Speckled rockfish"                      "Bank rockfish"                          "Shelf rockfish Unid"                    "Slope rockfish Unid"
# [17] "Blue/Deacon rockfish"                   "Nearshore rockfish Unid"                "Quillback rockfish (California)"        "Quillback rockfish (Washington/Oregon)"
# [21] "Black and Yellow rockfish"              "Gopher rockfish"                        "Grass rockfish"                         "Kelp rockfish"
# [25] "Olive rockfish"                         "Pygmy rockfish"                         "Bronzespotted rockfish"                 "Flag rockfish"
# [29] "Greenblotched rockfish"                 "Honeycomb rockfish"                     "Mexican rockfish"                       "Starry rockfish"
# [33] "Halfbanded rockfish"                    "Pink rockfish"                          "Pinkrose rockfish"                      "Rosy rockfish"
# [37] "Tiger rockfish"                         "Harlequin rockfish"                     "Chameleon rockfish"                     "Treefish rockfish"
# [41] "Calico rockfish"                        "Swordspine rockfish"                    "Freckled rockfish"                      "Spotted rockfish Unid"
# [45] "Puget Sound rockfish"                   "Whitespeckled rockfish"

# calculate fraction of total catch in gemm_rock for species that are or aren't in dat_long
z <- gemm_rock |>
    filter(year >= 2011) |>
    mutate(
        in_dat_long = ifelse(
            species %in% dat_long_rockfish_species,
            TRUE,
            FALSE
        )
    ) |>
    group_by(year, in_dat_long) |>
    summarize(
        total_dead = sum(
            total_discard_with_mort_rates_applied_and_landings_mt
        )
    )
(sum(z$total_dead[z$in_dat_long]) / sum(z$total_dead)) |> round(2)
# [1] 0.92

range(gemm_rock$year)
# [1] 2002 2024

# update the values with parameter == Catch in dat_long for the years 2011 onward with values from gemm_rock
# GEMM goes back to 2002, but estimates from assessments between 2002 and 2010 include discard estimates which may be more accurate
new_rows <- gemm_rock |>
    filter(species %in% dat_long_rockfish_species, year >= 2011) |>
    group_by(year, species) |>
    summarize(
        total_dead = sum(total_discard_with_mort_rates_applied_and_landings_mt)
    ) |>
    mutate(
        stock = "new rows from GEMM",
        assessment_year = NA,
        parameter = "Catch",
        description = "GEMM estimated total catch including discards with mortality and landings",
        unit = "Metric Tons",
        value = total_dead,
        group = "Rockfish"
    ) |>
    select(
        stock,
        assessment_year,
        parameter,
        description,
        unit,
        year,
        value,
        group,
        species
    )

# add new rows to dat_long after removing old rockfish catch data from 2002 onward
dat_with_gemm <- dat_long |>
    filter(!(parameter == "Catch" & year >= 2011 & group == "Rockfish")) |>
    rbind(new_rows)


# add colors for of the top n rockfish species using the okabe-ito palette
# but assigned to vaguely match fish colors in a few cases
dat_with_gemm$color <- case_when(
    dat_with_gemm$species == "Widow rockfish" ~ "#000000",
    dat_with_gemm$species == "Yellowtail rockfish" ~ "#009E73",
    dat_with_gemm$species == "Bocaccio" ~ "#56B4E9",
    dat_with_gemm$species == "Pacific ocean perch" ~ "#D55E00",
    dat_with_gemm$species == "Canary rockfish" ~ "#E69F00",
    dat_with_gemm$species == "Chilipepper" ~ "#CC79A7",
    dat_with_gemm$species == "Shortspine thornyhead" ~ "#F0E442",
    dat_with_gemm$species == "Longspine thornyhead" ~ "#0072B2",
    TRUE ~ "gray70"
)

# group all rockfish outside the top n into "Other rockfish"
dat_with_gemm <- dat_with_gemm |>
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
dat_with_gemm |>
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
dat_with_gemm |>
    filter(group == "Rockfish") |>
    filter(parameter == "Catch") |>
    #filter(year <= 2022) |>
    filter(!is.na(value) & year > 1920) |>
    ggplot(aes(x = year, y = value, fill = species_with_other)) +
    scale_fill_manual(
        values = setNames(dat_with_gemm$color, dat_with_gemm$species_with_other)
    ) +
    scale_x_continuous(breaks = seq(1920, 2030, 10)) +
    geom_bar(stat = "identity") +
    labs(
        title = "Total U.S. West Coast Rockfish Catch",
        subtitle = "27 species included represent 90% of total rockfish catch since 2002",
        x = "Year",
        y = "Catch (metric tons)"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(fill = "")
ggsave("figures/NSAW2026_groundfish_catch_rockfish.png", width = 8, height = 5)


# calculate Abundance plots relative to first non-NA value of abundance
# dat_with_gemm <- dat_with_gemm_backup
for (i in unique(dat_with_gemm$stock)) {
    # get abundance data for this stock
    dat_sub <- dat_with_gemm |>
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
    # rbind the modified rows back to dat_with_gemm
    dat_with_gemm <- dat_with_gemm |>
        rbind(dat_sub)
}

dat_with_gemm |>
    filter(group == "Rockfish") |>
    filter(parameter == "Abundance_ratio") |>
    filter(species_with_other != "Other rockfish") |>
    filter(!is.na(value) & year > 1920 & year <= 2025) |>
    ggplot(aes(
        x = year,
        y = value,
        color = species_with_other # ,
        # group = species
    )) +
    # scale_color_identity() +
    scale_color_manual(
        values = setNames(dat_with_gemm$color, dat_with_gemm$species_with_other)
    ) +
    geom_line(linewidth = 1.3) +
    geom_hline(
        yintercept = c(0, 0.25, 0.4, 1.0),
        # linetype = "dashed",
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
        y = "Fraction of unfished spawning output",
        color = ""
    ) +
    theme_minimal() +
    scale_y_continuous(breaks = c(0, 0.25, 0.4, 1.0)) +
    theme(
        panel.grid.major.x = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()
    )

ggsave(
    "figures/NSAW2026_groundfish_abundance_rockfish.png",
    width = 8,
    height = 5
)


# calculate total catch in gemm_rock from 2017 to 2024 vs 2011 to 2016 
avgs <- gemm_rock |>
    filter(year >= 2011) |>
    mutate(
        period = ifelse(year <= 2016, "2011-2015", "2020-2024")
    ) |>
    group_by(period) |>
    summarize(
        total_dead = sum(
            total_discard_with_mort_rates_applied_and_landings_mt
        )
    )

#   period    total_dead
#   <chr>          <dbl>
# 1 2011-2015     45324.
# 2 2020-2024    159721.

159721 / 45324
# [1] 3.523983
