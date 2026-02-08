#' Removes pots where maximum FGCC does not exceed 0.05
#'
#' @param data `tibble` `traits` or subset of `traits`
#'
#' @return `tibble` without the pots where maximum Canon FGCC never exceeds 0.05
remove_non_germinated_pots <- function(data) {

  data |>
    dplyr::group_by(.data$pot, .data$subset) |>
    dplyr::mutate(avg_fgcc = mean(.data$canon_fgcc),
                  max_fgcc = max(.data$canon_fgcc)) |>
    dplyr::filter(.data$max_fgcc > 0.05) |>
    dplyr::ungroup()
}


#' Helper to count pots in each subset that are removed before modelling
#'
#' @return `tibble` with information about number of pots that are removed due
#'  to non-germination criterion
count_non_germinated_pots <- function() {

  subsets <- unique(traits$subset)
  # without_non_germinated <- remove_non_germinated_pots()
  info_pot_removal <- subsets |> purrr::map(\(x) {
    subset <- get_subset_traits(x)
    n_all_pots <- length(unique(subset$pot))
    without_non_germinated <- remove_non_germinated_pots(subset)
    n_pots_germinated <- length(unique(without_non_germinated$pot))

    tibble::tibble(
      experiment = x,
      all_pots= n_all_pots,
      n_pots_germinated = n_pots_germinated,
      removed_pots = all_pots - n_pots_germinated
    )
  }) |>
    dplyr::bind_rows()

  return(info_pot_removal)
}


#' Create `TP` object from trait data
#'
#' @param subset `character(1L)` Name of the experiment, one of `traits$subset`
#'
#' @seealso [statgenHTP::createTimePoints()]
#'
#' @return `TP` object
make_statgenhtp_timepoints <- function(subset) {

  rlang::arg_match(subset, names(experiment_metadata))

  data <- get_subset_traits(subset) |>
    remove_non_germinated_pots()

  if (subset %in% c("2020 Main", "2021 Main", "2022 Main")) {
    # Kyabra not included in main parts
    checkGenotypes <- c("Howzat", "Genesis090")
    repId <- "rep"

  } else {
    checkGenotypes <- c("Howzat", "Genesis090", "Kyabra")
    repId <- "main_unit"
  }

  statgenHTP::createTimePoints(dat = data,
                               experimentName = subset,
                               genotype = "genotype_id",
                               timePoint = "date",
                               timeFormat = "%Y-%m-%d",
                               plotId = "pot",
                               repId = repId,
                               rowNum = "column",
                               colNum = "row",
                               checkGenotypes = checkGenotypes)
}


#' Removes single timepoint outliers
#'
#' @param tp_obj `TP` object
#' @param trait `character(1L)`
#' @param subset `character(1L)`
#'
#' @return `TP` object
remove_single_timepoint_outliers <- function(tp_obj, trait, subset) {

  rlang::arg_match(
    trait,
    c("n_lesions", "size_lesions", "canon_fgcc", "rededge_fgcc", "ndvi",
      "endvi", "rendvi", "ngrdi", "gndvi"
    )
  )

  confIntSize <- dplyr::case_when(
    subset %in% c("2021 Fungicide", "2022 Fungicide") ~ 4.0,
    subset == "2022 Main" && trait %in% c("n_lesions", "size_lesions") ~ 3.5,
    TRUE ~ 2.5,
  )

  detected <- statgenHTP::detectSingleOut(TP = tp_obj,
                                          trait = trait,
                                          confIntSize = confIntSize,
                                          nnLocfit = 0.8)


  # For size lesions and number of zeros, do not remove those with consistent zero
  if (trait %in% c("n_lesions", "size_lesions")) {
    threshold <- if (trait == "n_lesions") 1 else 0.0001
    detected <- detected |>
      dplyr::mutate(outlier = dplyr::case_when(.data[[trait]] == 0 & outlier == 1 & lwr < threshold ~ 0,
                                               TRUE~ outlier))
    attr(detected, "trait") <- trait
  }

  statgenHTP::removeSingleOut(tp_obj, detected)
}



#' Completely removes timepoints with less than 75% valid observations
discard_full_timepoints <- function(tp_obj, trait, subset) {

  n_germinated <- get_subset_traits(subset) |>
    remove_non_germinated_pots() |>
    dplyr::distinct(.data$pot) |>
    dplyr::count() |>
    dplyr::pull(n)

  minimum_valid <- round(n_germinated * 0.75)

  valid_dates_trait <- statgenHTP::countValid(tp_obj, trait)

  dates_rm <- names(valid_dates_trait[valid_dates_trait < minimum_valid])

  if (!rlang::has_length(dates_rm)) {
   return(tp_obj)
  }

  logger::log_info("Removing timepoints due to insufficient observations: {dates_rm}")
  statgenHTP::removeTimePoints(tp_obj, timePoints = dates_rm)
}



#' Get predicted values
#'
#' @return `tibble` with predicted values
fit_model <- function(tp_obj, trait, subset) {


  is_subset <- subset %in% c("2021 Fungicide", "2022 Fungicide")

  if (is_subset) {
    geno.decomp <- "treatment"
    knots <- 10L
  } else {
    geno.decomp <- "type"
    knots <- 5L
  }

  if (trait %in% c("n_lesions", "size_lesions")) {
    # Outliers where Size or Number of lesions is zero are kept, if size of number
    # of lesions is constantly zero, introduce minimal noise for model convergence
    withr::local_seed(123)
    for(i in 1:length(tp_obj)){
      df <- tp_obj[[i]]
      trait_sum <- sum(df[[trait]], na.rm = TRUE)
      if (trait_sum == 0){
        df[[trait]] <- 0.01 * runif(nrow(df))
      }
      tp_obj[[i]] <- df
    }
  }

  tp_obj_dates_removed <- discard_full_timepoints(tp_obj, trait, subset)

  spatial_model <- statgenHTP::fitModels(tp_obj_dates_removed, trait = trait, geno.decomp = geno.decomp)
  corrected_values <- statgenHTP::getCorrected(spatial_model)
  spline <- statgenHTP::fitSpline(corrected_values, trait = trait, knots = knots)
  spline$predDat
}


#' Runs spatiotemporal models for all experimental subsets
#'
#' Costly operation!
#'
#' @return `list` with results for each subset
run_spatiotemporal_modelling <- function(test_run = FALSE) {

  confirm_continue <- \() {
    cli::cli_alert("Running the spatiotemporal models is a costly operation (~60min)")
    repeat {
      answer <- tolower(readline("Do you want to continue? (y/n): "))
      if (answer %in% c("y", "n")) return(answer == "y")
      cat("Please enter 'y' or 'n'.\n")
    }
  }

  continue <- confirm_continue()

  if (!continue) {
    cat("Models are not run")
    return(invisible(NULL))
  }

  subsets <- unique(traits$subset)

  if (test_run) subsets <- c("2020 Main")
  n_subsets <- length(subsets)
  counter_subset <- 1L

  traits_main <- c("n_lesions", "size_lesions", "canon_fgcc")
  traits_subsets <- c(traits_main, "rededge_fgcc", "ndvi", "endvi", "rendvi", "ngrdi", "gndvi")

  out_subsets <- subsets |>
    purrr::set_names() |>
    purrr::map(\(s) {
      logger::log_info("Run models for subset {s} ({counter_subset} / {n_subsets} subsets)")

      tp_obj <- make_statgenhtp_timepoints(s)

      if (s %in% c("2020 Main", "2021 Main", "2022 Main")) {
        traits <- traits_main
      } else {
        traits <- c(traits_main, traits_subsets)
      }

      if (test_run) traits <- "n_lesions"

      n_traits <- length(traits)
      counter_trait <- 1L

      out_traits <- traits |>
        purrr::set_names() |>
        purrr::map(\(t) {
        logger::log_info("Model trait {t} ({counter_trait} / {n_traits} traits)")
        removed <- remove_single_timepoint_outliers(tp_obj, trait = t, subset = s)
        counts <- statgenHTP::countValid(removed, t)
        predicted_values <- fit_model(removed, t, s)

        mean_na_rm <- \(x) mean(x, na.rm = TRUE)

        daily_values <- predicted_values |>
          dplyr::mutate(
            day = as.Date(lubridate::floor_date(timePoint, "days"))
          ) |>
          dplyr::group_by(plotId, day) |>
          dplyr::summarise(mean_pred = mean(.data$pred.value, na.rm = TRUE)) |>
          dplyr::ungroup()

        trait_results <- list(
          predicted_values = daily_values,
          valid_observations = counts
        )

        counter_trait <<- counter_trait + 1L

        return(trait_results)
      })

      counter_subset <<- counter_subset + 1L

      return(out_traits)
    })

  return(out_subsets)
}
