# Load necessary libraries
library(tidyverse)
library(dplyr)
library(writexl)
library(ggplot2)

#----------------------------------------------------------#
# 1. Load data -----
#----------------------------------------------------------#

data_master <-
  readr::read_delim(
    here::here(
      "DATA/MASTER.csv"
    )
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


interaction_summary <- data_work %>%
  dplyr::group_by(CAT_sp) %>%
  # Total localities where the caterpillar is found
  dplyr::summarize(
    total_localities = dplyr::n_distinct(locality),
    # Localities where the caterpillar is parasitized (PAR_sp is not NA or empty)
    cat_parasitised = dplyr::n_distinct(locality[!is.na(PAR_sp) & PAR_sp != ""]),
    # Localities where the caterpillar is detected but not parasitized
    detected_no_par_localities = dplyr::n_distinct(locality) - dplyr::n_distinct(locality[!is.na(PAR_sp) & PAR_sp != ""]),
    # Localities where the caterpillar is not parasitized (PAR_sp is NA or empty)
    cat_available_without_par = dplyr::n_distinct(locality[is.na(PAR_sp) | PAR_sp == ""])
  ) 



# Handle caterpillars without parasitoids
no_parasitoid_summary <-
  data_work %>%
  dplyr::filter(is.na(PAR_sp)) %>% # keep only rows where parasiotid is missing
  dplyr::group_by(CAT_sp) %>%
  dplyr::summarise(
    num_localities = dplyr::n_distinct(locality, na.rm = TRUE), # Number of distinct localities where no parasitoid was found
    ) %>%
  dplyr::mutate(PAR_sp = NA) # Indicate that no parasitoid was found

# Combine the two tables (with and without parasitoids)
final_summary <-
  dplyr::bind_rows(interaction_summary, no_parasitoid_summary)

# View the final result
#View(final_summary)

# Step 1: Merge data_locality_summary with the full data

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

merged_data <-
  data_work %>%
  dplyr::left_join(
    data_locality_summary,
    by = "locality"
  )

# Step 1: Function to subsample data and compute interactions
subsample_data <- function(data_community, data_locality, replace = TRUE) {
  subsampled_data <-
    data_locality %>%
    dplyr::select(locality, abundance_caterpillars) %>%
    dplyr::mutate(
      data_subsampled = purrr::map(
        .x = abundance_caterpillars,
        .f = ~ dplyr::sample_n(
          data_community,
          size = .x,
          replace = replace
        ) %>%
          dplyr::select(-locality)
      )
    ) %>%
    tidyr::unnest(data_subsampled)
  
  return(subsampled_data)
}


group_subsample_data <- function(data_source) {
  # Summarize subsampled interactions (including caterpillars without parasitoids)
  subsampled_interaction_summary <-
    data_source %>%
    dplyr::filter(!is.na(PAR_sp)) %>% # Filter out rows where parasitoid is missing
    dplyr::group_by(CAT_sp, PAR_sp) %>%
    dplyr::summarise(
      .groups = "drop",
      num_localities = dplyr::n_distinct(locality, na.rm = TRUE),
      total_interactions = n()
    )
  
  # Handle caterpillars without parasitoids
  subsampled_no_parasitoid_summary <-
    data_source %>%
    dplyr::filter(is.na(PAR_sp)) %>%
    dplyr::group_by(CAT_sp) %>%
    dplyr::summarise(
      .groups = "drop",
      num_localities = dplyr::n_distinct(locality, na.rm = TRUE),
      total_interactions = n()
    ) %>%
    dplyr::mutate(PAR_sp = NA)
  
  # Combine the interaction summaries (with and without parasitoids)
  final_subsampled_summary <-
    dplyr::bind_rows(
      subsampled_interaction_summary,
      subsampled_no_parasitoid_summary
    )
  
  return(final_subsampled_summary)
}

