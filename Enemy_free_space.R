#----------------------------------------------------------#
#
#
#                   Cascading diversity
#
#                   Enemy-free space
#
#
#                 M. Libra,  O. Mottl
#                         2024
#
#----------------------------------------------------------#

# How would the diversity of parasitoids change if the caterpillars were
# not limited to single locality



#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

# Load necessary libraries
library(tidyverse)
library(dplyr)

#----------------------------------------------------------#
# 1. Load data -----
#----------------------------------------------------------#

data_master <-
  readr::read_delim(
    here::here(
      "DATA/MASTER.csv"
    ),
    delim = ";"
  ) %>%
  tibble::as_tibble()


#----------------------------------------------------------#
# 2. Data wrangling -----
#----------------------------------------------------------#

data_work <-
  data_master %>%
  dplyr::filter(
    is.na(Par_remove) |
      Par_remove == "0"
  ) %>%
  dplyr::filter(remove_cat == "0") %>%
  dplyr::select(
    locality,
    plant,
    CAT_sp,
    PAR_sp
  )

# Summary of data
data_locality_summary <-
  data_work %>%
  dplyr::group_by(locality) %>%
  dplyr::summarise(
    num_plant_species = dplyr::n_distinct(plant, na.rm = TRUE),
    num_caterpillar_species = dplyr::n_distinct(CAT_sp, na.rm = ),
    num_parasitoid_species = dplyr::n_distinct(PAR_sp, na.rm = TRUE)
  ) %>%
  dplyr::mutate(
    abundance_caterpillars = purrr::map_dbl(
      .x = locality,
      .f = ~ data_work %>%
        dplyr::filter(locality == .x) %>%
        tidyr::drop_na(CAT_sp) %>%
        dplyr::select(CAT_sp) %>%
        nrow()
    ),
    abundance_parasitoids = purrr::map_dbl(
      .x = locality,
      .f = ~ data_work %>%
        dplyr::filter(locality == .x) %>%
        tidyr::drop_na(PAR_sp) %>%
        dplyr::select(PAR_sp) %>%
        nrow()
    )
  )


#----------------------------------------------------------#
# 3. Subset the data -----
#----------------------------------------------------------#
get_subset_number_of_parasitoids <- function(sel_locatily = NULL) {
  # get all caterpillar species
  vec_all_cat_species <-
    data_work %>%
    dplyr::filter(
      locality == sel_locatily
    ) %>%
    dplyr::distinct(CAT_sp) %>%
    dplyr::pull(CAT_sp)

  res <-
    # for each caterpillar species
    tibble::tibble(
      CAT_sp = vec_all_cat_species
    ) %>%
    dplyr::mutate(
      # get the number of parasitoid species
      n_par = purrr::map_dbl(
        .x = CAT_sp,
        .f = ~ data_work %>%
          dplyr::filter(
            locality == sel_locatily &
              CAT_sp == .x
          ) %>%
          tidyr::drop_na(PAR_sp) %>%
          dplyr::distinct(PAR_sp) %>%
          nrow()
      ),
      # get the number of parasitoid species after subsampling
      # This is done so that the abundance of parasitoids is the same as the
      # abundance of caterpillars in the original data.
      n_par_subsample = purrr::map_dbl(
        .progress = FALSE,
        .x = CAT_sp,
        .f = ~ {
          # get the data for the caterpillar species
          data_sel_cat <-
            data_work %>%
            dplyr::filter(
              CAT_sp == .x
            )

          # get the number of caterpillars
          n_abundance_cat <-
            data_work %>%
            dplyr::filter(
              locality == sel_locatily &
                CAT_sp == .x
            ) %>%
            tidyr::drop_na(CAT_sp) %>%
            dplyr::select(CAT_sp) %>%
            nrow()

          # get the number of parasitoids
          n_sp_par <-
            data_sel_cat %>%
            dplyr::sample_n(
              size = n_abundance_cat,
              replace = TRUE
            ) %>%
            tidyr::drop_na(PAR_sp) %>%
            dplyr::distinct(PAR_sp) %>%
            dplyr::pull(PAR_sp) %>%
            length()

          return(n_sp_par)
        }
      )
    )

  return(res)
}

# reproducibility SET SEED
set.seed(1234)
data_subset_parasitoids <-
  data_work %>%
  dplyr::distinct(locality) %>%
  dplyr::pull(locality) %>%
  rlang::set_names() %>%
  purrr::map(
    .progress = TRUE,
    .f = ~ get_subset_number_of_parasitoids(sel_locatily = .)
  ) %>%
  dplyr::bind_rows(
    .id = "locality"
  ) %>%
  dplyr:::mutate(
    diff = n_par - n_par_subsample
  )

View(data_subset_parasitoids)


#----------------------------------------------------------#
# 4. Visuaisation -----
#----------------------------------------------------------#
data_subset_parasitoids %>%
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = locality,
      y = diff
    )
  ) +
  ggplot2::geom_violin() +
  ggplot2::geom_boxplot(
    width = 0.2
  )




