#' Reads and combines in situ traits into single `tibble`
#'
#' @return `tibble`
read_traits <- function() {
  data_path <- fs::path_package("abscreenmethods", "data-raw")

  subset_2020_main <- "2020 Main"
  traits_2020_main <- readr::read_csv(fs::path(data_path, "trait_data/2020_Main.csv")) |>
    dplyr::mutate(subset = .env$subset_2020_main,
                  pot = as.character(.data$pot),
                  dai = as.integer(difftime(.data$date, .env$experiment_metadata[[subset_2020_main]]$infection_date , units = "days")),
                  das = as.integer(difftime(.data$date, .env$experiment_metadata[[subset_2020_main]]$sowing_date , units = "days")))

  subset_2021_main <- "2021 Main"
  traits_2021_main <- readr::read_csv(fs::path(data_path, "trait_data/2021_Main.csv"))|>
    dplyr::mutate(subset = .env$subset_2021_main,
                  pot = as.character(.data$pot),
                  dai = as.integer(difftime(.data$date, .env$experiment_metadata[[subset_2021_main]]$infection_date , units = "days")),
                  das = as.integer(difftime(.data$date, .env$experiment_metadata[[subset_2021_main]]$sowing_date , units = "days")))

  subset_2021_fungicide <- "2021 Fungicide"
  traits_2021_fungicide <- readr::read_csv(fs::path(data_path, "trait_data/2021_fungicide.csv"))|>
    dplyr::mutate(subset = .env$subset_2021_fungicide,
                  pot = as.character(.data$pot),
                  main_unit = dplyr::case_when(.data$row <= 9 ~ .data$rep,
                                               .data$row >9 ~ .data$rep +3),
                  dai = as.integer(difftime(.data$date, .env$experiment_metadata[[subset_2021_fungicide]]$infection_date , units = "days")),
                  das = as.integer(difftime(.data$date, .env$experiment_metadata[[subset_2021_fungicide]]$sowing , units = "days")),
                  treatment = dplyr::if_else(.data$treatment == "Fungicide", "Fungicide-treated", .data$treatment)
                  )

  subset_2022_main <- "2022 Main"
  traits_2022_main <- readr::read_csv(fs::path(data_path, "trait_data/2022_Main.csv"))|>
    dplyr::mutate(subset = .env$subset_2022_main,
                  pot = as.character(.data$pot),
                  dai = as.integer(difftime(.data$date, .env$experiment_metadata[[subset_2022_main]]$infection_date , units = "days")),
                  das = as.integer(difftime(.data$date, .env$experiment_metadata[[subset_2022_main]]$sowing_date , units = "days")),
                  # Fix position mixup during imaging
                  pot = dplyr::case_when(.data$pot == "801" ~ "802",
                                         .data$pot == "802" ~ "801",
                                         TRUE ~ .data$pot))

  subset_2022_fungicide <- "2022 Fungicide"
  traits_2022_fungicide <- readr::read_csv(fs::path(data_path, "trait_data/2022_fungicide.csv"))|>
    dplyr::mutate(subset = .env$subset_2022_fungicide,
                  pot = as.character(.data$pot),
                  main_unit = dplyr::case_when(.data$row <= 9 ~ .data$rep,
                                               .data$row >9 ~ .data$rep +3),
                  dai = as.integer(difftime(.data$date, .env$experiment_metadata[[subset_2022_fungicide]]$infection_date , units = "days")),
                  das = as.integer(difftime(.data$date, .env$experiment_metadata[[subset_2022_fungicide]]$sowing_date , units = "days")),
                  # Fix position mixup during imaging
                  pot = dplyr::case_when(.data$pot == "20A" ~ "21A",
                                         .data$pot == "21A" ~ "20A",
                                         TRUE ~ .data$pot),
                  treatment = dplyr::if_else(.data$treatment == "Fungicide", "Fungicide-treated", .data$treatment)
    )


  dplyr::bind_rows(
    traits_2020_main, traits_2021_main, traits_2021_fungicide, traits_2022_main, traits_2022_fungicide
  )
}


#' Helper to retrieve trait data for an individual experiment
#'
#' @return `tibble` `traits` data filtered for a subset
get_subset_traits <- function(subset) {
  traits |>
    dplyr::filter(.data$subset == .env$subset)
}

#' Helper to retrieve trait data for 2020 Main experiment
#' @noRd
get_main_2020_traits <- \() get_subset_traits("2020 Main")

#' Helper to retrieve trait data for 2021 Main experiment
#' @noRd
get_main_2021_traits <- \() get_subset_traits("2021 Main")

#' Helper to retrieve trait data for 2022 Main experiment
#' @noRd
get_main_2022_traits <- \() get_subset_traits("2022 Main")

#' Helper to retrieve trait data for 2021 fungicide experiment
#' @noRd
get_fungicide_2021_traits <- \() get_subset_traits("2021 Fungicide")

#' Helper to retrieve trait data for 2022 fungicide experiment
#' @noRd
get_fungicide_2022_traits <- \() get_subset_traits("2022 Fungicide")
