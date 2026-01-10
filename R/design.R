#' Get count of individual genotypes in each experiment subset
#'
#' @return `data.frame` with columns:
#'  * __subset__ `character` Experimental subset
#'  * __type__ `character` Type of genotype
#'  * __n__ `integer` Count of genotypes
summarise_genotype_distribution <- function() {
  scores |>
    dplyr::distinct(subset, type, genotype_id) |>
    dplyr::group_by(subset, type) |>
    dplyr::count() |>
    dplyr::ungroup()
}
