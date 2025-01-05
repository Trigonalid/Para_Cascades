# Load necessary libraries
library(dplyr)
library(vegan)
library(tidyr)
library(tibble)
library(CommEcol)
library(readxl)
library(betapart)

# Load your data
MASTER <- read_excel("DATA/MASTER.xlsx") %>% as_tibble()

# Filter data for parasitoids
parasitoid_data <- MASTER %>%
  filter(guild == "PAR", Par_remove == "0", locality %in% c("Ohu1", "Ohu2")) %>%
  select(locality, PAR_sp)

# Determine the size of the smaller locality
small_locality_size <- parasitoid_data %>%
  group_by(locality) %>%
  dplyr::summarize(count = n()) %>%
  dplyr::summarize(min(count)) %>%
  pull()

# Identify the bigger locality
locality_sizes <- parasitoid_data %>%
  group_by(locality) %>%
  dplyr::summarize(count = n())

bigger_locality <- locality_sizes %>%
  filter(count == max(count)) %>%
  pull(locality)

# Identify the smaller locality
small_locality <- locality_sizes %>%
  filter(count == min(count)) %>%
  pull(locality)

# Function to resample and calculate Sorensen dissimilarity
resample_and_calculate <- function(data, small_locality_size, bigger_locality, small_locality) {
  set.seed(sample(1:10000, 1)) # Set a random seed for reproducibility
  
  # Resample data from the bigger locality
  subsampled_data <- data %>%
    filter(locality == bigger_locality) %>%
    sample_n(small_locality_size)
  
  # Combine the subsampled data with the smaller locality data
  combined_data <- bind_rows(subsampled_data, data %>% filter(locality == small_locality))
  
  # Create a species count table
  species_counts <- combined_data %>%
    group_by(locality, PAR_sp) %>%
    tally() %>%
    spread(key = locality, value = n, fill = 0) %>%
    as.data.frame()
  
  # Prepare the species matrix for Sorensen dissimilarity
  species_matrix <- species_counts %>%
    column_to_rownames(var = "PAR_sp") %>%
    t() %>%
    as.matrix()
  
  # Convert to presence-absence
  species_matrix[species_matrix > 0] <- 1
  
  # Calculate Sorensen dissimilarity using beta.pair
  sorensen <- beta.pair(species_matrix)
  return(as.matrix(sorensen$beta.sor))
}

# Perform 999 resampling iterations and calculate Sorensen dissimilarity
sorensen_results_PAR <- lapply(1:999, function(x) {
  resample_and_calculate(parasitoid_data, small_locality_size, bigger_locality, small_locality)
})

# Extract Sorensen values from the results
sorensen_values_PAR <- sapply(sorensen_results_PAR, function(mat) mat[1, 2])

# Calculate mean and standard deviation
mean_sorensen_PAR <- mean(sorensen_values_PAR)
sd_sorensen_PAR <- sd(sorensen_values_PAR)

# Print results
print(mean_sorensen_PAR)
print(sd_sorensen_PAR)


########################################################################################################
# Load necessary libraries
library(dplyr)
library(vegan)
library(tidyr)
library(tibble)
library(CommEcol)
library(readxl)

# Load your data
MASTER <- read_excel("DATA/MASTER.xlsx") %>% as_tibble()

# Filter data for caterpillars
caterpillar_data <- MASTER %>%
  filter(guild == "CAT", locality %in% c("Ohu1", "Ohu2")) %>%
  select(locality, CAT_sp)

# Determine the size of the smaller locality
small_locality_size <- caterpillar_data %>%
  group_by(locality) %>%
  dplyr::summarize(count = n()) %>%
  dplyr::summarize(min(count)) %>%
  pull()

# Identify the bigger locality
locality_sizes <- caterpillar_data %>%
  group_by(locality) %>%
  dplyr::summarize(count = n())

bigger_locality <- locality_sizes %>%
  filter(count == max(count)) %>%
  pull(locality)

# Identify the smaller locality
small_locality <- locality_sizes %>%
  filter(count == min(count)) %>%
  pull(locality)

# Function to resample and calculate Sorensen dissimilarity
resample_and_calculate <- function(data, small_locality_size, bigger_locality, small_locality) {
  set.seed(sample(1:10000, 1)) # Set a random seed for reproducibility
  
  # Resample data from the bigger locality
  subsampled_data <- data %>%
    filter(locality == bigger_locality) %>%
    sample_n(small_locality_size)
  
  # Combine the subsampled data with the smaller locality data
  combined_data <- bind_rows(subsampled_data, data %>% filter(locality == small_locality))
  
  # Create a species count table
  species_counts <- combined_data %>%
    group_by(locality, CAT_sp) %>%
    tally() %>%
    spread(key = locality, value = n, fill = 0) %>%
    as.data.frame()
  
  # Prepare the species matrix for Sorensen dissimilarity
  species_matrix <- species_counts %>%
    column_to_rownames(var = "CAT_sp") %>%
    t() %>%
    as.matrix()
  
  # Convert to presence-absence
  species_matrix[species_matrix > 0] <- 1
  
  # Calculate Sorensen dissimilarity using beta.pair
  sorensen <- beta.pair(species_matrix)
  return(as.matrix(sorensen$beta.sor))
}

# Perform 999 resampling iterations and calculate Sorensen dissimilarity
sorensen_results_CAT <- lapply(1:999, function(x) {
  resample_and_calculate(caterpillar_data, small_locality_size, bigger_locality, small_locality)
})

# Extract Sorensen values from the results
sorensen_values_CAT <- sapply(sorensen_results_CAT, function(mat) mat[1, 2])

# Calculate mean and standard deviation
mean_sorensen_CAT <- mean(sorensen_values_CAT)
sd_sorensen_CAT <- sd(sorensen_values_CAT)

# Print results
print(mean_sorensen_CAT)
print(sd_sorensen_CAT)
