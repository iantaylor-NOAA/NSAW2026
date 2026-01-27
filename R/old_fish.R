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

# read in older model outputs for comparison
out_widow_old <- r4ss::SS_output(
    "C:/SS/Widow/Widow2019/WidowRF_2019_Update/2_base_model",
    printstats = FALSE,
    verbose = FALSE
)
out_yt_old <- SS_output(
    "https://raw.githubusercontent.com/pfmc-assessments/yellowtail_2025/refs/heads/main/Model_Runs/1.01_base_2017",
    printstats = FALSE,
    verbose = FALSE
)
out_canary_old <- r4ss::SS_output(
    'C:/SS/canary/CanaryRf_2015 base model_run',
    printstats = FALSE,
    verbose = FALSE
)


get_old_numbers <- function(out, ref_age) {
    out$natage_annual_2_with_fishery |>
        filter(Bio_Pattern == 1) |>
        select(-Bio_Pattern) |>
        rename(Sex = any_of("Gender"), Yr = any_of("Year")) |>
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
) |>
    mutate(assessment = "new")

# compare old fish numbers from older assessments
olds2 <- rbind(
    get_old_numbers(out_widow_old, ref_age = ref_age) |>
        mutate(species = "Widow rockfish"),
    get_old_numbers(out_yt_old, ref_age = ref_age) |>
        mutate(species = "Yellowtail rockfish"),
    get_old_numbers(out_canary_old, ref_age = ref_age) |>
        mutate(species = "Canary rockfish")
) |>
    mutate(assessment = "old")
olds <- rbind(olds, olds2)

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

# calculate frac_old of old fish in 2023 vs the first
# value in the time series for each species
olds <- olds |>
    group_by(species, assessment) |>
    mutate(
        first_N_old = first(N_old),
        frac_old = N_old / first_N_old
    ) |>
    select(-first_N_old)

# trim projections
olds <- olds |>
    filter(
        Yr <=
            case_when(
                assessment == "new" &
                    species %in%
                        c("Yellowtail rockfish", "Widow rockfish") ~ 2025,
                assessment == "new" & species == "Canary rockfish" ~ 2023,
                assessment == "old" & species == "Canary rockfish" ~ 2016,
                assessment == "old" &
                    species == "Yellowtail rockfish" ~ 2018,
                assessment == "old" & species == "Widow rockfish" ~ 2020
            )
    )

# plot the N_old time series for each species
library(ggplot2)
olds |>
    #filter(assessment == "new") |>
    ggplot(aes(
        x = Yr,
        y = N_old / 1e6,
        linetype = assessment,
        color = species
    )) +
    geom_line(linewidth = 1) +
    labs(
        title = glue::glue(
            "Estimated numbers of age {ref_age}+ fish\nrelative to unfished equlibrium"
        ),
        x = "Year",
        y = "Fraction of unfished equlibrium",
        color = "species"
        #linetype = "assessment"
    ) +
    scale_color_manual(
        values = setNames(olds$color, olds$species)
    ) +
    theme_minimal(base_size = 14)
ggsave(
    "figures/old_fish_numbers2.png",
    width = 7,
    height = 5,
    units = "in",
    dpi = 300
)


# plot the frac_old time series for each species
olds |>
    ggplot(aes(x = Yr, y = frac_old, color = species, linetype = assessment)) +
    geom_line(linewidth = 1) +
    labs(
        title = glue::glue(
            "Estimated numbers of age {ref_age}+ fish\nrelative to unfished equlibrium"
        ),
        x = "Year",
        y = "Fraction of unfished equlibrium",
        color = "species"
    ) +
    scale_color_manual(
        values = setNames(olds$color, olds$species)
    ) +
    theme_minimal(base_size = 14)
ggsave(
    "figures/old_fish_relative_numbers2.png",
    width = 7,
    height = 5,
    units = "in",
    dpi = 300
)


# plot the frac_old time series for each species
olds |>
    dplyr::filter(Yr >= 1980 & assessment == "new") |>
    ggplot(aes(x = Yr, y = frac_old, color = species)) + #, linetype = assessment)) +
    geom_line(linewidth = 1) +
    labs(
        title = glue::glue(
            "Estimated numbers of age {ref_age}+ fish\nrelative to unfished equlibrium"
        ),
        x = "Year",
        y = "Fraction of unfished equlibrium",
        color = "species"
    ) +
    scale_color_manual(
        values = setNames(olds$color, olds$species)
    ) +
    theme_minimal(base_size = 14)
ggsave(
    "figures/old_fish_relative_numbers_zoomed2.png",
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


# example CAAL plots for WCGBTS fleet by model to get rough estimate of which fish are expected
# to be large

# yt fleet 6 CAAL at length > 38, max agebin = 30
# canary fleet 28, CAAL at length > 40, max agebin = 35
# widow fleet 8, CAAL at length > 35, max agebin = 40
# averages out to about 70% of Linf

#    Linf <- out$Growth_Parameters |> filter(Sex == 1) |> pull(Linf)

SS_plots(out_canary, fleets = 28, plot = 18, png = FALSE, print = FALSE)
SS_plots(out_yt, fleets = 6, plot = 18, png = FALSE, print = FALSE)
SS_plots(out_widow, fleets = 8, plot = 18, png = FALSE, print = FALSE)

# get obs/expected proportion of ages in length bins > 70% of L_at_Amax
out_canary$Growth_Parameters |> filter(Sex == 1) |> pull(Linf)
# [1] 59.2228
out_widow$Growth_Parameters |> filter(Sex == 1) |> pull(Linf)
# [1] 49.4547
out_yt$Growth_Parameters |> filter(Sex == 1) |> pull(Linf)
# [1] 54.4546

get_marginals <- function(out) {
    fleet <- which(
        out$FleetNames %in%
            c("WCGBTS", "28_coastwide_NWFSC", "NWFSC", "NWFSCcombo")
    )
    # remove additive constant
    min <- out$condbase$Obs |> min(na.rm = TRUE)
    marginals <- out$condbase |>
        filter(Fleet == fleet & Sex == 1) |>
        group_by(Yr, Bin) |>
        summarize(
            Obs = sum(Nsamp_adj * (Obs - min)),
            Exp = sum(Nsamp_adj * (Exp - min)),
            Nsamp_adj = sum(Nsamp_adj)
        )
    # rescale within a year to sum to 1
    for (y in unique(marginals$Yr)) {
        marginals$Obs[marginals$Yr == y] <- marginals$Obs[marginals$Yr == y] /
            sum(marginals$Obs[marginals$Yr == y])
        marginals$Exp[marginals$Yr == y] <- marginals$Exp[marginals$Yr == y] /
            sum(marginals$Exp[marginals$Yr == y])
    }
    return(marginals)
}
# # plot Exp vs Bin with colors by year
# out_widow |> get_marginals() |> ggplot(aes(x = Bin, y = Obs, color = Yr)) + geom_point()

# proportion of CAAL data for females that are > 70% of Linf
get_old_prop <- function(out, ref_age = 20) {
    get_marginals(out) |>
        group_by(Yr) |>
        # get proportions above ref_age for obs and exp
        summarize(
            obs = sum(Obs[Bin >= ref_age]) / sum(Obs),
            exp = sum(Exp[Bin >= ref_age]) / sum(Exp)
        ) |>
        # put in long format better suited to ggplot
        pivot_longer(
            cols = c(obs, exp),
            names_to = "type",
            values_to = "prop_old"
        )
}

x <- out_widow |>
    get_marginals() |>
    ungroup() |>
    filter(Yr == 2005) |>
    select(Bin, Exp) |>
    plot()
x <- out_widow |>
    get_marginals() |>
    ungroup() |>
    filter(Yr == 2006) |>
    select(Bin, Exp) |>
    lines()
x <- out_widow |>
    get_marginals() |>
    ungroup() |>
    filter(Yr == 2007) |>
    select(Bin, Exp) |>
    lines()


out_widow |>
    get_old_prop() |>
    ggplot(aes(x = Yr, y = prop_old, color = type)) +
    geom_line() #+
# labs(
#     title = "Proportion of old fish in CAAL data",
#     y = glue::glue("Proportion of fish age {ref_age}+")
# )

ref_age <- 20
out_yt |>
    get_old_prop(ref_age = ref_age) |>
    ggplot(aes(x = Yr, y = prop_old, color = type)) +
    geom_line() +
    labs(
        title = "Proportion of old fish in CAAL data",
        y = glue::glue("Proportion of fish age {ref_age}+")
    )

out_yt |>
    get_old_prop(ref_age = 20) |>
    filter(Yr >= 2017, type == "exp") |>
    pull(prop_old) |>
    mean()

out_yt_old |>
    get_old_prop(ref_age = 20) |>
    filter(Yr >= 2014, type == "exp") |>
    pull(prop_old) |>
    mean()
