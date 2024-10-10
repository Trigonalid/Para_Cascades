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

# Load necessary libraries
library(dplyr)
library(tidyr)

# Load the dataset
# data_work <- read.csv("your_data_file.csv") # Modify this to match your actual file path if needed

# Define the number of subsamples
n_subsamples <- 999

# Function to perform subsampling
subsample_data <- function(data) {
  # Count the number of records for each CAT_sp in each locality
  counts <- data %>%
    group_by(locality, CAT_sp) %>%
    summarize(count = n(), .groups = 'drop')
  
  # Now subsample based on these counts
  subsampled <- counts %>%
    group_by(CAT_sp) %>%
    do(slice_sample(., n = nrow(.), replace = TRUE)) %>%
    ungroup()
  
  # Join back to the original data
  subsampled_data <- data %>%
    semi_join(subsampled, by = c("locality", "CAT_sp"))
  
  return(subsampled_data)
}

set.seed(1234)  # Ensure reproducibility

# Store results for each subsample
subsampled_results <- replicate(n_subsamples, subsample_data(data_work), simplify = FALSE)

# Combine the subsampled datasets into one
combined_subsamples <- bind_rows(subsampled_results)

# Count metrics for the original data
original_counts <- data_work %>%
  group_by(CAT_sp) %>%
  summarize(
    original_localities = n_distinct(locality),
    parasitized_localities = n_distinct(locality[!is.na(PAR_sp) & PAR_sp != ""]),
    non_parasitized_localities = n_distinct(locality[is.na(PAR_sp) | PAR_sp == ""]),
    detected_non_parasitized_localities = n_distinct(locality[!is.na(PAR_sp) & PAR_sp == ""]), # New column
    .groups = 'drop'
  )

# Count metrics for the subsampled data
subsample_counts <- combined_subsamples %>%
  group_by(CAT_sp) %>%
  summarize(
    mean_subsampled_localities = n_distinct(locality),
    mean_parasitized_localities = n_distinct(locality[!is.na(PAR_sp) & PAR_sp != ""]),
    mean_non_parasitized_localities = n_distinct(locality[is.na(PAR_sp) | PAR_sp == ""]),
    .groups = 'drop'
  )

# Combine original counts with subsampled counts
final_summary <- original_counts %>%
  left_join(subsample_counts, by = "CAT_sp")

# View the final result
print(final_summary)
