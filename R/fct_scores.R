#' Reads and combines scores into single `tibble`
#'
#' @return `tibble` with columns
#'  * __pot__ `character` Identifier for each pot
#'  * __di__ `numeric` Disease Index as visually scored
#'  * __genotype_id__ `character` Identifier for genotype
#'  * __type__ `character` Genotype group
#'  * __subset__ `character` Experiment
#'  * __treatment__ `character` treatment of pot, either "Infected" or
#'    "Fungicide-treated"
#'  * __score_group__ `character` Category of disease severity, either "Great",
#'    "Ok" or "Reject"
#'  * __score_group_count__ `integer` Count of incidence of score group within
#'    each experiment subset
read_scores <- function() {
  data_path <- fs::path_package("abscreenmethods", "data-raw")

  design_2020 <- readr::read_csv(fs::path(
    data_path,
    "trait_data/2020_Main.csv"
  ))
  design_2021_main <- readr::read_csv(fs::path(
    data_path,
    "trait_data/2021_Main.csv"
  ))
  design_2021_fungicide <- readr::read_csv(fs::path(
    data_path,
    "trait_data/2021_fungicide.csv"
  ))
  design_2022_main <- readr::read_csv(fs::path(
    data_path,
    "trait_data/2022_Main.csv"
  ))
  design_2022_fungicide <- readr::read_csv(fs::path(
    data_path,
    "trait_data/2022_fungicide.csv"
  ))

  scores_2020 <- readr::read_csv(fs::path(
    data_path,
    "score_data/scores_2020.csv"
  )) |>
    dplyr::mutate(pot = stringr::str_replace(pot, ":00 AM", " A"))
  scores_2021 <- readr::read_csv(fs::path(
    data_path,
    "score_data/scores_2021.csv"
  ))
  scores_2022 <- readr::read_csv(fs::path(
    data_path,
    "score_data/scores_2022.csv"
  ))

  scores_2020_matched <- scores_2020 |>
    dplyr::left_join(
      design_2020 |>
        dplyr::distinct(pot, type, genotype_id)
    ) |>
    dplyr::mutate(subset = "2020 Main", treatment = "Infected")

  scores_2021_Main_matched <- scores_2021 |>
    dplyr::left_join(
      design_2021_main |>
        dplyr::mutate(pot = as.character(pot)) |>
        dplyr::distinct(pot, type, genotype_id)
    ) |>
    dplyr::mutate(subset = "2021 Main", treatment = "Infected") |>
    dplyr::filter(stringr::str_detect(pot, "A", negate = TRUE))

  scores_2021_fungicide_matched <- scores_2021 |>
    dplyr::left_join(
      design_2021_fungicide |>
        dplyr::mutate(pot = as.character(pot)) |>
        dplyr::distinct(pot, type, genotype_id, treatment)
    ) |>
    dplyr::mutate(subset = "2021 Subset") |>
    dplyr::filter(!is.na(type))

  scores_2022_Main_matched <- scores_2022 |>
    dplyr::left_join(
      design_2022_main |>
        dplyr::mutate(pot = as.character(pot)) |>
        dplyr::distinct(pot, type, genotype_id)
    ) |>
    dplyr::mutate(subset = "2022 Main", treatment = "Infected") |>
    dplyr::filter(stringr::str_detect(pot, "A", negate = TRUE))

  scores_2022_fungicide_matched <- scores_2022 |>
    dplyr::left_join(
      design_2022_fungicide |>
        dplyr::mutate(pot = as.character(pot)) |>
        dplyr::distinct(pot, type, genotype_id, treatment)
    ) |>
    dplyr::mutate(subset = "2022 Subset") |>
    dplyr::filter(!is.na(type))

  scores <- dplyr::bind_rows(
    scores_2020_matched,
    scores_2021_fungicide_matched,
    scores_2021_Main_matched,
    scores_2022_fungicide_matched,
    scores_2022_Main_matched
  ) |>
    dplyr::filter(!is.na(type)) |>
    dplyr::mutate(
      score_group = dplyr::case_when(
        di > 80 ~ "Reject",
        di > 40 & di <= 80 ~ "Ok",
        di <= 40 ~ "Great"
      ),
      score_group = as.factor(score_group),
      treatment = dplyr::if_else(
        .data$treatment == "Fungicide",
        "Fungicide-treated",
        .data$treatment
      )
    )

  return(scores)
}


#' Get summary metrics for scores of control genotypes for all experiments
#'
#' @param `scores` `tibble` return value from [read_scores()]
#' @return `tibble`
get_control_genotype_score_averages <- function(scores) {
  scores |>
    dplyr::filter(
      .data$genotype_id %in% .env$control_genotypes
    ) |>
    dplyr::group_by(genotype_id, subset, treatment) |>
    dplyr::summarise(n = dplyr::n(), mean_di = mean(di), sd_di = sd(di)) |>
    dplyr::ungroup()
}


#' Get summary metrics for treatments for all experiments
#'
#' @param `scores` `tibble` return value from [read_scores()]
#' @return `tibble`
get_treatment_score_averages <- function(scores) {
  scores |>
    dplyr::filter(!is.na(di)) |>
    dplyr::group_by(subset, treatment) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean_di = mean(di, na.rm = TRUE),
      sd_di = sd(di, na.rm = TRUE)
    ) |>
    dplyr::ungroup()
}


#' Read repeated scores from two exports in 2021 trial
#'
#'
read_repeated_scores <- function() {
  data_path <- fs::path_package(
    "abscreenmethods",
    "data-raw",
    "score_data",
    "rescoring_2021.csv"
  )

  readr::read_csv(data_path) |>
    dplyr::mutate(
      score_group = dplyr::case_when(
        di > 80 ~ "Reject",
        di > 40 & di <= 80 ~ "Ok",
        di <= 40 ~ "Great"
      ),
      score_group = as.factor(score_group)
    )
}


#' Calculates metrics for repeatability of scoring
#'
#' Reads data from two rounds of scoring in 2021 of two expert scorers and
#' calculates repeatability metrics accuracy and Cohen's kappa.
#'
#' @return `tibble`
#'  * __accuracy__ `numeric` Accuracy of prediction
#'  * __kap__ `numeric` Cohen's kappa
#'  * __kind__ `character` either "within" or "between" scorer comparison
#'  * __truth__ `character` ground truth for metric calculation
#'  * __estimate__ `character`  identifier for the predicted results
get_scoring_repeatability <- function() {
  repeated_scores <- read_repeated_scores()

  class_matrix <- repeated_scores |>
    dplyr::mutate(
      scoring_group = paste0("scorer_", scorer, "_round_", round)
    ) |>
    tidyr::pivot_wider(
      id_cols = c(pot),
      names_from = scoring_group,
      values_from = score_group
    )

  # Helper function for formatting results
  fmt_yardstick_metrics <- function(yardstick_metrics, kind, truth, estimate) {
    yardstick_metrics |>
      tidyr::pivot_wider(names_from = .metric, values_from = .estimate) |>
      dplyr::mutate(kind = kind, truth = truth, estimate = estimate)
  }

  # TODO: discrepancy regarding Scorer B round 2! Maybe mistake in original data transcription?

  # Within Scorer

  a1_a2 <- yardstick::metrics(
    data = class_matrix,
    truth = scorer_A_round_1,
    estimate = scorer_A_round_2
  ) |>
    fmt_yardstick_metrics("Within", "Scorer A, Round 1", "Scorer A, Round 2")
  b1_b2 <- yardstick::metrics(
    data = class_matrix,
    truth = scorer_B_round_1,
    estimate = scorer_B_round_2
  ) |>
    fmt_yardstick_metrics("Within", "Scorer B, Round 1", "Scorer B, Round 2")

  # Between Scorer

  a1_b1 <- yardstick::metrics(
    data = class_matrix,
    truth = scorer_A_round_1,
    estimate = scorer_B_round_1
  ) |>
    fmt_yardstick_metrics("Between", "Scorer A, Round 1", "Scorer B, Round 1")
  a1_b2 <- yardstick::metrics(
    data = class_matrix,
    truth = scorer_A_round_1,
    estimate = scorer_B_round_2
  ) |>
    fmt_yardstick_metrics("Between", "Scorer A, Round 1", "Scorer B, Round 2")
  a2_b1 <- yardstick::metrics(
    data = class_matrix,
    truth = scorer_A_round_2,
    estimate = scorer_B_round_1
  ) |>
    fmt_yardstick_metrics("Between", "Scorer A, Round 2", "Scorer B, Round 1")
  a2_b2 <- yardstick::metrics(
    data = class_matrix,
    truth = scorer_A_round_2,
    estimate = scorer_B_round_2
  ) |>
    fmt_yardstick_metrics("Between", "Scorer A, Round 2", "Scorer B, Round 2")

  out <- dplyr::bind_rows(a1_a2, b1_b2, a1_b1, a1_b2, a2_b1, a2_b2) |>
    dplyr::select(-.estimator)

  return(out)
}
