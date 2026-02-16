scores <- read_scores()

usethis::use_data(scores, overwrite = TRUE)

traits <- read_traits()

usethis::use_data(traits, overwrite = TRUE)

models <- run_spatiotemporal_modelling(test_run = TRUE)
# usethis::use_data(models, overwrite = TRUE)

