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
   dplyr::select(
   dplyr::locality,
    plant,
    CAT_sp,
    PAR_sp
  )



# Count the number of unique localities for each caterpillar species

locality_counts <- data_work %>%
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


interaction_counts_PAR <- data_work %>%
  dplyr::group_by(CAT_sp, PAR_sp) %>%
  dplyr::summarize(
    # Total localities where the caterpillar-parasitoid interaction is detected
    total_localities = dplyr::n_distinct(locality),
    # Localities where the interaction is parasitized (PAR_sp is not NA or empty)
    parasitized_localities = dplyr::n_distinct(locality[!is.na(PAR_sp) & PAR_sp != ""]),
    # Localities where the interaction is not parasitized (PAR_sp is NA or empty)
    non_parasitized_localities = dplyr::n_distinct(locality[is.na(PAR_sp) | PAR_sp == ""])
  ) 

# View the result
print(interaction_counts_PAR)


# Number of caterpillar species for each locality + total
caterpillar_counts <- data_work %>%
  dplyr::group_by(locality) %>%
  dplyr::summarize(num_caterpillars = n_distinct(CAT_sp)) 

total_caterpillars <- data_work %>%
  dplyr::summarize(total_unique_caterpillars = n_distinct(CAT_sp))

# Number of caterpillars for each locality + total
caterpillar_abundance <- data_work %>%
  dplyr::group_by(locality) %>%
  dplyr::summarize(abundance = n())






