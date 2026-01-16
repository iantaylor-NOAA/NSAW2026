#' Update stock assessment information for a specific stock from SS3 model output
#'
#' @param stock_name Character string with the name of the stock
#' @param ss3files Path or URL to SS3 model output files
#' @param spawn_output_units Character string describing the units for spawning output
#' @param assessment_year Numeric year of the assessment
#'
#' @return A data frame with updated stock assessment information
#' @export
#'
#' @examples
#' \dontrun{
#' updated_data <- update_info(dat_long)
#' }
#'
update_info <- function(
    stock_name = "Yellowtail rockfish - Northern Pacific Coast",
    ss3files = "https://raw.githubusercontent.com/pfmc-assessments/yellowtail_2025/refs/heads/main/Model_Runs/5.09_no_extra_SE/",
    spawn_output_units = "Number x 1,000,000,000,000",
    assessment_year = 2025
) {
    model <- r4ss::SS_output(
        ss3files,
        verbose = FALSE,
        printstats = FALSE
    )
    model_catch <- model$catch |>
        group_by(Yr) |>
        summarize(value = sum(dead_bio)) |>
        mutate(
            stock = stock_name,
            assessment_year = assessment_year,
            parameter = "Catch",
            description = "Modeled Total Catch",
            unit = "Metric Tons",
            year = Yr
        ) |>
        select(
            stock,
            assessment_year,
            parameter,
            description,
            unit,
            year,
            value
        )
    model_abundance <- model$timeseries |>
        select(Yr, SpawnBio) |>
        mutate(
            stock = stock_name,
            assessment_year = assessment_year,
            parameter = "Abundance",
            description = "Spawning Output, Eggs (Mean)",
            unit = spawn_output_units,
            year = Yr,
            value = SpawnBio
        ) |>
        select(
            stock,
            assessment_year,
            parameter,
            description,
            unit,
            year,
            value
        )
    model_Fmort <- model$sprseries |>
        select(Yr, SPR) |>
        mutate(
            stock = stock_name,
            assessment_year = assessment_year,
            parameter = "Fmort",
            description = "1-SPR, Exploitable All (Mean)",
            unit = "Rate",
            year = Yr,
            value = 1 - SPR
        ) |>
        select(
            stock,
            assessment_year,
            parameter,
            description,
            unit,
            year,
            value
        )
    model_recruitment <- model$timeseries |>
        select(Yr, Recruit_0) |>
        mutate(
            stock = stock_name,
            assessment_year = assessment_year,
            parameter = "Recruitment",
            description = "Abundance, Age 0 (Mean)",
            unit = "Number x 1,000",
            year = Yr,
            value = Recruit_0
        ) |>
        select(
            stock,
            assessment_year,
            parameter,
            description,
            unit,
            year,
            value
        )

    # aggregate new rows
    new_rows <- rbind(
        model_catch,
        model_abundance,
        model_Fmort,
        model_recruitment
    ) |>
        dplyr::filter(year <= model$endyr + 1) # filter for one year beyond endyr to start of year current status

    cli::cli_alert_info(
        "Range of years for for {stock_name} in new rows {new_rows |> filter(stock == stock_name & !is.na(value)) |> pull(year) |> range() |> paste(collapse = ' to ')}"
    )
    return(new_rows)
}
