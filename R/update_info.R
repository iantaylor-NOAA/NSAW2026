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
    model = NULL,
    spawn_output_units = "Number x 1,000,000,000,000",
    assessment_year = 2025
) {
    if (!is.null(model)) {
        cli::cli_alert_info("Using supplied model object.")
    } else {
        cli::cli_alert_info("Reading SS3 model output from {ss3files}")
        model <- r4ss::SS_output(
            ss3files,
            verbose = FALSE,
            printstats = FALSE
        )
    }

    # get catch, abundance, Fmort, recruitment

    # get catch
    if ("kill_bio" %in% names(model$catch)) {
        # replace header in catch table for older versions of SS3 (e.g. in 2017 POP assessment)
        model$catch <- model$catch |> rename(dead_bio = kill_bio)
    }
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
    # get spawning output as abundance
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
    # get Fmort as 1-SPR
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
    # get recruitment
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
