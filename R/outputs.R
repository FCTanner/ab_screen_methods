#' Builds table for score overview
#'
#' @param scores `tibble` return value of [read_scores()]
#'
#' @return reactable object
make_score_table <- function(scores = scores) {
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
#' @param add_fill_legend_when_only_infected `logical(1L)` Should a color fill
#'  legend be added to the plot if only Infected plants are included.
#'
#' @return `gg` object
make_score_histogram <- function(
    scores = scores,
    experiment_subset,
    add_fill_legend_when_only_infected = TRUE
){

  color_palette <- paletteer::paletteer_d(`"basetheme::clean"`, 5)
  color_palette_infection <- color_palette[1:2]

  p_title <- paste0(experiment_subset, ": DI count")

  data_subset <- scores |> dplyr::filter(subset == experiment_subset)
  has_only_infected <- all(unique(data_subset$treatment) == "Infected")

  p <- ggplot2::ggplot() +
    ggplot2::geom_vline(xintercept = 40, linetype = 2, color = "grey60") +
    ggplot2::geom_vline(xintercept = 80, linetype = 2, color = "grey60") +
    ggplot2::geom_histogram(data = data_subset,
                            ggplot2::aes(di, fill = treatment, color = treatment),
                            alpha = 0.5, position = "identity") +
    ggplot2::scale_color_manual(values = c("Fungicide-treated" = color_palette_infection[1], "Infected" = color_palette_infection[2])) +
    ggplot2::scale_fill_manual(values = c("Fungicide-treated" = color_palette_infection[1], "Infected" = color_palette_infection[2])) +
    ggplot2::scale_x_continuous(breaks = c(0,20,40,60,80,100)) +
    ggplot2::labs(x = "Disease Index", subtitle = p_title) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank(),
                   legend.position = "bottom",
                   axis.title.y = ggplot2::element_blank())

  if (!add_fill_legend_when_only_infected && has_only_infected) {
    p <- p +
      ggplot2::guides(colour = "none", fill = "none")
  }
  return(p)
}



#' Plots a dotplot of the scores of the control genotypes
#'
#' @param scores `tibble` return value of [read_scores()]
#'
#' @return `gg` object
make_control_genotypes_score_dotplot <- function(scores = scores){

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


#' Builds table for distribution of genotypes in each experiment
#'
#' @return `reactable` Object
make_genotype_distribution_table <- function(scores = scores) {
  summarise_genotype_distribution() |>
    tidyr::pivot_wider(id_cols = subset, names_from = type, values_from = n) |>
    dplyr::rename("Subset" = subset) |>
    reactable::reactable()
}


#' Wraps the plots displaying the scores into a single plot
#'
#' @return `patchwork` Object
save_score_patchwork_plot <- function() {

  subsets <- unique(scores$subset)
  control_genotypes_score_dotplot <- make_control_genotypes_score_dotplot(scores)

  score_histograms <- subsets |>
    purrr::map(\(s) {
      p <- make_score_histogram(scores, s, add_fill_legend_when_only_infected = FALSE)
      print(p)
      p
    })

  plots <- c(list(control_genotypes_score_dotplot), score_histograms)
  p <- patchwork::wrap_plots(plots) +
    patchwork::plot_layout(
      guides = "collect",
      design = "
        AABB
        CCDD
        EEFF
      "
    ) &
    ggplot2::theme(legend.position = "bottom")

  ggplot2::ggsave(filename = "out/scores_patchwork.pdf", plot = p,
                  units = "cm", width =12, height = 10, limitsize = F,
                  device = "pdf", scale = 1.5)
}


#' Tabulate info about pot removal due to non-germination
#'
#' @return `reactable` Object
make_pot_removal_info_table <- function() {
  counts <- count_non_germinated_pots()
  counts |>
    reactable::reactable(
      columns = list(
        experiment = reactable::colDef("Experiment"),
        all_pots = reactable::colDef("All pots"),
        n_pots_germinated  = reactable::colDef("Germinated pots"),
        removed_pots  = reactable::colDef("Non-germinated pots")
      )
    )
}


#' Visualize experimental design
#'
#' @return `list` of `ggplot2` objects of experimental design
make_statgenhtp_layout_plots <- function() {
  subsets <- unique(traits$subset)
  subsets |>
    purrr::map(\(x){
      is_main <- stringr::str_detect(x, "Main")
      tp_obj <- make_statgenhtp_timepoints(x)
      first_timepoint <- attr(tp_obj, "timePoints")$timePoint[1]
      x <- plot(tp_obj,
                plotType = "layout",
                timePoints = first_timepoint,
                showGeno = FALSE,
                traits = "canon_fgcc")

      return(invisible(x))
    })
}


#' Count pots after outlier removal at individual timepoints
#'
#' TODO: move into spatiotemporal_model pipeline, return outputs
#'
#' @param trait `character(1L)` trait to be checked for outliers
#'
#' @return `list`
make_counts_of_removed_pots_at_single_timepoints <- function(trait) {
  subsets <- unique(traits$subset)
  valid_pots <- subsets |>
    purrr::map(\(x) {
      tp_obj <- make_statgenhtp_timepoints(x)
      removed <- remove_single_timepoint_outliers(tp_obj, trait = trait, subset = x)
      valid <- statgenHTP::countValid(removed, trait)
    })

  valid_pots <- stats::setNames(valid_pots, subsets)

  return(valid_pots)
}



#' Count pots after outlier removal at individual timepoints
#'
#' @return `list`
make_pot_single_timepoint_removal_tables <- function(counts) {
  purrr::imap(counts, \(valid, subset_name) {

    total_pots <- experiment_metadata[[subset_name]]$n_pots

    data <- data.frame(valid) |>
      tibble::rownames_to_column("date") |>
      dplyr::mutate(date= as.Date(.data$date),
                    valid_ratio = .data$valid / .env$total_pots,
                    timepoint_removed = .data$valid_ratio < 0.75) |>
      dplyr::left_join(traits |> dplyr::select(date, dai, das) |> dplyr::distinct(), by = "date")

    table <- reactable::reactable(data, columns = list(
      valid = reactable::colDef("Pots remaining"),
      date = reactable::colDef(
        name = "Imaging date",
        format = reactable::colFormat(date = TRUE, locales = "en-US")
      ),
      valid_ratio = reactable::colDef(
        name = "Valid pots (%)",
        format = reactable::colFormat(percent = TRUE, digits = 2)
      ),
      timepoint_removed = reactable::colDef(
        name = "Timepoint kept (> 75% valid)",
        align = "center",
        cell = function(value) {
          if (!isTRUE(value)) {
            htmltools::span("✔", style = "color: green; font-weight: bold;")
          } else {
            htmltools::span("✖", style = "color: red; font-weight: bold;")
          }
        }
      ),
      dai = reactable::colDef(
        name = "Days after inoculation"
      ),
      das = reactable::colDef(
        name = "Days after sowing"
      )
    ))

    htmltools::tagList(
      htmltools::tags$h4(subset_name),
      table
    )
  })
}
