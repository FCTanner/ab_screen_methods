#' Reads raw trait data from CSVs and standardizes column
#'
#' Reads in all processed raw trait data, standardize column names and data
#' types
#'
#' @return `list` with `tibble` for each experiment, main parts, subsets and
#'  data from automated phenotyping system:
#'  * __date__ `date` Imaging date
#'  * __pot__ `character` Identifier for pot
#'  * __n_lesions__ `integer` Number of detected lesions on image
#'  * __size_lesions__ `numeric` Total size of detected lesions
#'  * __genotype_id__ `character` Identifier of genotype
#'  * __type__ `character` Genotype group
#'  * __rep__ `integer` Number of replicate
#'  * __row__ `integer` Position of pot (row)
#'  * __column__ `integer` Position of pot (column)
#'  * __canon_fgcc__ `numeric` FGCC extracted from RGB images
#'  * __treatment__ `character` Treatment of pot
#'  * __rededge_fgcc__ `numeric` (only for subsets): FGCC extracted from
#'    multispectral images
#'  * __ndvi__ `numeric` (only for subsets): NDVI extracted from multispectral
#'    images
#'  * __endvi__ `numeric` (only for subsets): ENDVI extracted from multispectral
#'    images
#'  * __rendvi__ `numeric` (only for subsets): RENDVI extracted from multispectral
#'    images
#'  * __ngrdi__ `numeric` (only for subsets): NGRDI extracted from multispectral
#'    images
#'  * __gndvi__ `numeric` (only for subsets): GNDVI extracted from multispectral
#'    images
#'  * __lemna_fgcc__ `numeric` (only for lemna_data_2021): FGCC extracted from
#'    images from automated phenotyping system
#'  * __hyperspec__ `logical` (only for lemna_data_2021): Flag, whether
#'    hyperspectral imaging was performed in automated phenotyping system
#'  * __projected_shoot_area_pixels__ `numeric` (only for lemna_data_2021):
#'    Projected Shoot Area extracted from images from automated phenotyping
#'    system
read_raw_traits <- function() {

  data_path <- fs::path_package("abscreenmethods", "data-raw")

  parse_columns <- function(x) {
    x |>
      dplyr::mutate(
        pot = as.character(.data$pot),
        dplyr::across(
          dplyr::any_of(
            c("n_lesions", "rep", "row", "column")),
          ~as.integer(.x)
        )
      )
  }

  main_2020 <- readr::read_csv(fs::path(data_path, "trait_data", "2020_main.csv"))|>
    dplyr::rename(canon_fgcc = fgcc) |>
    parse_columns()
  main_2021 <- readr::read_csv(fs::path(data_path, "trait_data", "2021_main.csv")) |>
    dplyr::rename(canon_fgcc = fgcc) |>
    parse_columns()
  main_2022 <- readr::read_csv(fs::path(data_path, "trait_data", "2022_main.csv")) |>
    parse_columns()

  subset_2021 <- readr::read_csv(fs::path(data_path, "trait_data", "2021_fungicide.csv")) |>
    dplyr::rename(rededge_fgcc = fgcc) |>
    parse_columns()
  subset_2022 <- readr::read_csv(fs::path(data_path, "trait_data", "2022_fungicide.csv")) |>
    dplyr::rename(rededge_fgcc = fgcc) |>
    parse_columns()

  lemna_data_2021 <- readr::read_csv(fs::path(data_path, "trait_data", "2021_lemna_fgcc.csv")) |>
    dplyr::mutate(hyperspec = dplyr::case_when(
      .data$hyperspec == "Yes" ~ TRUE,
      .data$hyperspec == "Yes" ~ FALSE
    ))

  raw_traits <- list(
    main_2020 = main_2020,
    main_2021 = main_2021,
    main_2022 = main_2022,
    subset_2021 = subset_2021,
    subset_2022 = subset_2022,
    lemna_data_2021 = lemna_data_2021
  )

  return(raw_traits)
}
