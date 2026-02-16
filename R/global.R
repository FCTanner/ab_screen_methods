#' Identifiers of genotypes that are used as controls
control_genotypes <- c("WLD085", "Genesis090", "Howzat")

#' Metadata for each subset: Sowing date, thinning date, infection date, scopring date, number of pots
experiment_metadata <- list(
  "2020 Main" = list(
    sowing_date = as.Date("2020-06-13"),
    thinning_date = as.Date("2020-07-29"),
    infection_date = as.Date("2020-08-03"),
    fungicide_dates = NULL,
    scoring_date = as.Date("2020-09-28"),
    n_pots = 2096
  ),
  "2021 Main" =  list(
    sowing_date = as.Date("2021-06-16"),
    thinning_date = as.Date("2021-07-16"),
    infection_date = as.Date("2021-07-20"),
    fungicide_dates = NULL,
    scoring_date = as.Date("2021-09-20"),
    n_pots = 2520
  ),
  "2021 Fungicide" = list(
    sowing_date = as.Date("2021-06-16"),
    thinning_date = as.Date("2021-07-16"),
    infection_date = as.Date("2021-07-20"),
    fungicide_dates = as.Date(c("2021-08-11", "2021-09-01")),
    scoring_date = as.Date("2021-09-20"),
    n_pots = 180
  ),
  "2022 Main" = list(
    sowing_date = as.Date("2022-06-08"),
    thinning_date = as.Date("2022-07-14"),
    infection_date = as.Date("2022-07-19"),
    fungicide_dates = NULL,
    scoring_date = as.Date("2022-09-22"),
    n_pots = 2400
  ),
  "2022 Fungicide" = list(
    sowing_date = as.Date("2022-06-08"),
    thinning_date = as.Date("2022-07-14"),
    infection_date = as.Date("2022-07-19"),
    fungicide_dates = as.Date(c("2022-07-18", "2022-08-09", "2022-08-16", "2022-09-01")),
    scoring_date = as.Date("2022-09-22"),
    n_pots = 180
  )
)
