# read assessments for Canary, Widow, and Yellowtail rockfish

library(dplyr)
library(r4ss)

# read assessment outputs from GitHub repos and save locally as .rds files
if (!file.exists("data-raw/out_widow_2025.rds")) {
    out_widow <- SS_output(
        "https://raw.githubusercontent.com/mcgoodman/widow_rockfish_2025/refs/heads/main/models/2025%20base%20model",
        SpawnOutputLabel = "Spawning output (billions of eggs)",
        printstats = FALSE,
        verbose = FALSE
    )
    # save output as .rds file for faster loading later
    saveRDS(out_widow, file = "data-raw/out_widow_2025.rds")

    out_yt <- SS_output(
        "https://raw.githubusercontent.com/pfmc-assessments/yellowtail_2025/refs/heads/main/Model_Runs/5.09_no_extra_SE",
        SpawnOutputLabel = "Spawning output (trillions of eggs)",
        printstats = FALSE,
        verbose = FALSE
    )
    saveRDS(out_yt, file = "data-raw/out_yt_2025.rds")

    out_canary <- SS_output(
        "https://raw.githubusercontent.com/pfmc-assessments/canary_2023/refs/heads/main/models/7_3_5_reweight",
        SpawnOutputLabel = "Spawning output (millions of eggs)",
        printstats = FALSE,
        verbose = FALSE
    )
    saveRDS(out_canary, file = "data-raw/out_canary_2023.rds")
}

# read the rds files if they aren't already in workspace
if (!exists("out_widow")) {
    out_widow <- readRDS("data-raw/out_widow_2025.rds")
    out_yt <- readRDS("data-raw/out_yt_2025.rds")
    out_canary <- readRDS("data-raw/out_canary_2023.rds")
}

get_old_numbers <- function(out, ref_age) {
    out$natage_annual_2_with_fishery |>
        filter(Bio_Pattern == 1) |>
        select(-Bio_Pattern) |>
        pivot_longer(
            cols = -c(Sex, Yr),
            names_to = "Age",
            values_to = "Numbers"
        ) |>
        group_by(Yr) |>
        mutate(old = as.numeric(Age) >= ref_age) |>
        filter(old) |>
        summarize(N_old = 1000 * sum(Numbers))
}

ref_age <- 30
olds <- rbind(
    get_old_numbers(out_widow, ref_age = ref_age) |>
        mutate(species = "Widow rockfish"),
    get_old_numbers(out_yt, ref_age = ref_age) |>
        mutate(species = "Yellowtail rockfish"),
    get_old_numbers(out_canary, ref_age = ref_age) |>
        mutate(species = "Canary rockfish")
)

head(olds)
#      Yr    N_old species
#   <int>    <dbl> <chr>
# 1  1916 2285364. Widow rockfish
# 2  1917 2284002. Widow rockfish
# 3  1918 2281886. Widow rockfish
# 4  1919 2279453. Widow rockfish
# 5  1920 2277772. Widow rockfish
# 6  1921 2276052. Widow rockfish

olds$color <- case_when(
    olds$species == "Widow rockfish" ~ "#000000",
    olds$species == "Yellowtail rockfish" ~ "#009E73",
    olds$species == "Canary rockfish" ~ "#E69F00",
    TRUE ~ "gray70"
)

# calculate fraction of old fish in 2023 vs the first
# value in the time series for each species
olds <- olds |>
    group_by(species) |>
    mutate(
        first_N_old = first(N_old),
        frac_old = N_old / first_N_old
    ) |>
    select(-first_N_old)

# trim projections
olds <- olds |>
    filter(
        Yr <=
            if_else(
                species %in% c("Yellowtail rockfish", "Widow rockfish"),
                2025,
                2023
            )
    ) |>
    rename(fraction = frac_old)

# plot the N_old time series for each species
library(ggplot2)
ggplot(olds, aes(x = Yr, y = N_old / 1e6, color = species)) +
    geom_line(linewidth = 1) +
    labs(
        #title = "Estimated Numbers of Old Fish in Three Rockfish Stocks",
        x = "Year",
        y = glue::glue("Estimated numbers of fish age {ref_age}+ (millions)"),
        color = "species"
    ) +
    scale_color_manual(
        values = setNames(olds$color, olds$species)
    ) +
    theme_minimal(base_size = 14)
ggsave(
    "figures/old_fish_numbers.png",
    width = 7,
    height = 5,
    units = "in",
    dpi = 300
)


# plot the fraction time series for each species
ggplot(olds, aes(x = Yr, y = fraction, color = species)) +
    geom_line(linewidth = 1) +
    labs(
        #title = "Estimated Numbers of Old Fish in Three Rockfish Stocks",
        x = "Year",
        y = glue::glue(
            "Estimated numbers of fish age {ref_age}+\nrelative to unfished equlibrium"
        ),
        color = "species"
    ) +
    scale_color_manual(
        values = setNames(olds$color, olds$species)
    ) +
    theme_minimal(base_size = 14)
ggsave(
    "figures/old_fish_relative_numbers.png",
    width = 7,
    height = 5,
    units = "in",
    dpi = 300
)


# plot the fraction time series for each species
olds |>
    dplyr::filter(Yr >= 1980) |>
    ggplot(aes(x = Yr, y = fraction, color = species)) +
    geom_line(linewidth = 1) +
    labs(
        #title = "Estimated Numbers of Old Fish in Three Rockfish Stocks",
        x = "Year",
        y = glue::glue(
            "Estimated numbers of fish age {ref_age}+\nrelative to unfished equlibrium"
        ),
        color = "species"
    ) +
    scale_color_manual(
        values = setNames(olds$color, olds$species)
    ) +
    theme_minimal(base_size = 14)
ggsave(
    "figures/old_fish_relative_numbers_zoomed.png",
    width = 7,
    height = 5,
    units = "in",
    dpi = 300
)


# todo: compute expected old fish from previous assessments
# look at patterns in the data

SSplotComparisons(
    SSsummarize(list(
        out_widow,
        out_yt,
        out_canary
    )),
    legendlabels = c("Widow", "Yellowtail", "Canary"),
    subplots = 3,
    col = c("#000000", "#009E73", "#E69F00"),
    
)

#w19 <- r4ss::SS_read('models/2019 base model//Base_45')
