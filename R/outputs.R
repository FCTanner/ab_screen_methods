#' Builds table for score overview
#'
#' @param scores `tibble` return value of [read_scores()]
#'
#' @return reactable object
make_score_table <- function(scores) {
  control_genotype_averages <- get_control_genotype_score_averages(scores)
  treatment_averages <- get_treatment_score_averages(scores) |>
    dplyr::mutate(genotype_id = "All pots")

  control_genotype_averages |>
    dplyr::bind_rows(treatment_averages) |>
    dplyr::ungroup() |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2)),
                  val = paste0(mean_di, " ± ", sd_di, " (", n, ")"),
                  val = tidyr::replace_na(val , "—")) |>
    dplyr::select(-mean_di, -sd_di, -n) |>
    tidyr::pivot_wider(names_from = subset, values_from = val) |>
    dplyr::rename("Genotype" = genotype_id,
                  "Treatment" = treatment) |>
    reactable::reactable()
}



#' Plots a histogram of the scores for one experimental subset
#'
#' @param scores `tibble` return value of [read_scores()]
#' @param experiment_subset `character(1L)` one of `scores$subset`
#'
#' @return `gg` object
make_score_histogram <- function(scores, experiment_subset){

  color_palette <- paletteer::paletteer_d(`"basetheme::clean"`, 5)
  color_palette_infection <- color_palette[1:2]

  p_title <- paste0(experiment_subset, ": DI count")

  p <- ggplot2::ggplot() +
    ggplot2::geom_vline(xintercept = 40, linetype = 2, color = "grey60") +
    ggplot2::geom_vline(xintercept = 80, linetype = 2, color = "grey60") +
    ggplot2::geom_histogram(data = scores |> dplyr::filter(subset == experiment_subset),
                            ggplot2::aes(di, fill = treatment, color = treatment),
                   alpha = 0.5, position = "identity") +
    ggplot2::scale_color_manual(values = c("Fungicide" = color_palette_infection[1], "Infected" = color_palette_infection[2])) +
    ggplot2::scale_fill_manual(values = c("Fungicide" = color_palette_infection[1], "Infected" = color_palette_infection[2])) +
    ggplot2::scale_x_continuous(breaks = c(0,20,40,60,80,100)) +
    ggplot2::labs(x = "Disease Index", subtitle = p_title) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
          legend.title = ggplot2::element_blank(),
          legend.position = "bottom",
          axis.title.y = ggplot2::element_blank())
  return(p)
}



#' Plots a dotplot of the scores of the control genotypes
#'
#' @param scores `tibble` return value of [read_scores()]
#'
#' @return `gg` object
make_control_genotypes_score_dotplot <- function(scores){

  color_palette <- paletteer::paletteer_d(`"basetheme::clean"`, 5)
  color_palette_genotypes <- color_palette[3:5]

  p <- scores |>
    dplyr::filter(.data$genotype_id %in% .env$control_genotypes) |>
    dplyr::mutate(grouping_var = stringr::str_c(subset, ": ", substr(.data$treatment, 1,3), "."),
           grouping_var = factor(grouping_var, levels = c("2022 Subset: Inf.",
                                                          "2022 Subset: Fun.",
                                                          "2021 Subset: Inf.",
                                                          "2021 Subset: Fun.",
                                                          "2022 Main: Inf.",
                                                          "2021 Main: Inf.",
                                                          "2020 Main: Inf."
           ))) |>
    ggplot2::ggplot(ggplot2::aes(x = di, y = grouping_var, color= genotype_id, shape = genotype_id)) +
    ggplot2::geom_jitter(height = 0.2, width = 0, alpha = 0.7) +
    ggplot2::scale_color_manual(values = color_palette_genotypes) +
    ggplot2::scale_x_continuous(breaks = c(0,20,40,60,80,100)) +
    ggplot2::labs(x = "Disease Index", subtitle= "Control genotypes: DI per pot") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
          legend.position = "bottom",
          axis.title.y = ggplot2::element_blank())

  return(p)
}



#' Builds table for scoring repeatability
#'
#' @return reactable object
make_scoring_repeatability_table <- function() {
  repeatability <- get_scoring_repeatability()

  repeatability |>
    dplyr::select(dplyr::all_of(c("kind", "truth", "estimate", "accuracy", "kap"))) |>
    dplyr::mutate(kap = round(.data$kap, 2)) |>
    dplyr::rename("Truth" = truth,
                  "Kind" = kind,
                  "Estimate" = estimate,
                  "Agreement" = accuracy,
                  "κ" = kap) |>
    reactable::reactable(
      columns = list(
        Agreement = reactable::colDef(
          format = reactable::colFormat(percent = TRUE, digits = 2)
        )
      )
    )
}
