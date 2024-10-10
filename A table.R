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
    locality,
    plant,
    CAT_sp,
    PAR_sp
  )





# Load the dataset

# Count the number of unique localities for each caterpillar species
locality_count <- data_work %>%
  group_by(CAT_sp) %>%
  summarize(num_localities = n_distinct(locality)) %>%
  arrange(desc(num_localities))

# View the result
print(locality_count)

locality_counts <- data %>%
  group_by(CAT_sp) %>%
  # Total localities where the caterpillar is found
  summarize(
    total_localities = n_distinct(locality),
    # Localities where the caterpillar is parasitized (PAR_sp is not NA or empty)
    parasitized_localities = n_distinct(locality[!is.na(PAR_sp) & PAR_sp != ""]),
    # Localities where the caterpillar is not parasitized (PAR_sp is NA or empty)
    non_parasitized_localities = n_distinct(locality[is.na(PAR_sp) | PAR_sp == ""]),
    # Localities where the caterpillar is detected but not parasitized
    detected_non_parasitized_localities = n_distinct(locality) - n_distinct(locality[!is.na(PAR_sp) & PAR_sp != ""])
  ) %>%
  arrange(desc(total_localities))

# View the result
print(locality_counts)





interaction_counts_PAR <- data %>%
  group_by(CAT_sp, PAR_sp) %>%
  # Total localities where the caterpillar-parasitoid interaction is detected
  summarize(
    total_localities = n_distinct(locality),
    # Localities where the interaction is parasitized (PAR_sp is not NA or empty)
    parasitized_localities = n_distinct(locality[!is.na(PAR_sp) & PAR_sp != ""]),
    # Localities where the interaction is not parasitized (PAR_sp is NA or empty)
    non_parasitized_localities = n_distinct(locality[is.na(PAR_sp) | PAR_sp == ""])
  ) %>%
  arrange(desc(total_localities))

# View the result
print(interaction_counts_PAR)


# Number of caterpillar species for each locality
caterpillar_counts <- data_work %>%
  group_by(locality) %>%
  summarize(num_caterpillars = n_distinct(CAT_sp)) %>%
  arrange(desc(num_caterpillars))

# View the result
print(caterpillar_counts)

total_caterpillars <- data_work %>%
  summarize(total_unique_caterpillars = n_distinct(CAT_sp))

# View the result
print(total_caterpillars)

caterpillar_abundance <- data_work %>%
  group_by(locality) %>%
  summarize(abundance = n()) %>%
  arrange(locality, desc(abundance))

# View the result
print(caterpillar_abundance)




