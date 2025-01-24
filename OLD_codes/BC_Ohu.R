# Load necessary libraries
library(dplyr)
library(vegan)
library(tidyr)
library(tibble)

# Read data
MASTER <- read_excel("DATA/MASTER.xlsx") %>% as_tibble()
parasitoid_data <- MASTER %>%
  filter(`guild` == "PAR", Par_remove == "0", locality %in% c("Ohu1", "Ohu2")) %>% 
  select(locality, PAR_sp)

#### Bray-Curtis Calculation

# Create a species counts table for each locality
species_counts_orig_para <- parasitoid_data %>%
  group_by(locality, PAR_sp) %>%
  tally() %>%
  spread(key = locality, value = n, fill = 0) %>%
  as.data.frame()

# Replace NA with 0
species_counts_orig_para[is.na(species_counts_orig_para)] <- 0

# Prepare data for Bray-Curtis calculation
species_matrix_orig_para <- species_counts_orig_para %>%
  column_to_rownames(var = "PAR_sp") %>%
  as.matrix()

# Calculate Bray-Curtis dissimilarity
bray_curtis_orig_para <- vegdist(species_matrix_orig_para, method = "bray")

# View the Bray-Curtis dissimilarity matrix
print(as.matrix(bray_curtis_orig_para))

#### Resampling and Calculating Bray-Curtis Dissimilarity

# Determine the size of the smaller locality
small_locality_size <- parasitoid_data %>% 
  group_by(locality) %>% 
  summarize(count = n()) %>% 
  summarize(min(count)) %>% 
  pull()

# Identify the bigger locality
locality_sizes <- parasitoid_data %>% 
  group_by(locality) %>% 
  summarize(count = n())

bigger_locality <- locality_sizes %>% 
  filter(count == max(count)) %>% 
  pull(locality)

# Function for resampling and calculating Bray-Curtis
resample_and_calculate_bray <- function(data, small_locality_size, bigger_locality, small_locality) {
  set.seed(sample(1:10000, 1)) # Set a random seed for reproducibility
  subsampled_data <- data %>%
    filter(locality == bigger_locality) %>%
    sample_n(small_locality_size)
  
  combined_data <- bind_rows(subsampled_data, data %>% filter(locality == small_locality))
  
  species_counts <- combined_data %>%
    group_by(locality, PAR_sp) %>%
    tally() %>%
    spread(key = locality, value = n, fill = 0) %>%
    as.data.frame()
  
  species_matrix <- species_counts %>%
    column_to_rownames(var = "PAR_sp") %>%
    as.matrix()
  
  bray_curtis <- vegdist(species_matrix, method = "bray")
  return(as.matrix(bray_curtis))
}

# Resampling and calculating Bray-Curtis dissimilarity 999 times
bray_curtis_results_PAR <- lapply(1:999, function(x) {
  resample_and_calculate_bray(parasitoid_data, small_locality_size, bigger_locality, small_locality)
})

# Extract values and calculate mean and standard deviation
bray_curtis_values_PAR <- sapply(bray_curtis_results_PAR, function(mat) mat[1, 2])
mean_bray_curtis_PAR <- mean(bray_curtis_values_PAR)
sd_bray_curtis_PAR <- sd(bray_curtis_values_PAR)

# Print the results
print(mean_bray_curtis_PAR) # Mean Bray-Curtis dissimilarity
print(sd_bray_curtis_PAR) # Standard deviation







#####################################################
# Load necessary libraries
library(dplyr)
library(vegan)
library(tidyr)
library(tibble)

# Extract caterpillar data
caterpillar_data <- MASTER %>%
  filter(guild == "CAT", remove_cat == "0", locality %in% c("Ohu1", "Ohu2")) %>% 
  select(locality, CAT_sp)

# Determine the size of the smaller locality
small_locality_size_cat <- caterpillar_data %>% 
  group_by(locality) %>% 
  summarize(count = n()) %>% 
  summarize(min(count)) %>% 
  pull()

# Identify the bigger locality
locality_sizes_cat <- caterpillar_data %>% 
  group_by(locality) %>% 
  summarize(count = n())

bigger_locality_cat <- locality_sizes_cat %>% 
  filter(count == max(count)) %>% 
  pull(locality)

# Randomly subsample from the bigger locality
set.seed(123) # Set seed for reproducibility
subsampled_data_cat <- caterpillar_data %>%
  filter(locality == bigger_locality_cat) %>%
  sample_n(small_locality_size_cat)

# Combine subsampled data with the smaller locality data
small_locality_cat <- locality_sizes_cat %>% 
  filter(count == min(count)) %>% 
  pull(locality)

combined_data_cat <- bind_rows(subsampled_data_cat, caterpillar_data %>% filter(locality == small_locality_cat))

# Verify the size of combined data
combined_data_cat %>% group_by(locality) %>% summarize(count = n())

# Create a table with species counts for each locality
species_counts_cat <- combined_data_cat %>%
  group_by(locality, CAT_sp) %>%
  tally() %>%
  spread(key = locality, value = n, fill = 0) %>%
  as.data.frame()

# Replace NA with 0
species_counts_cat[is.na(species_counts_cat)] <- 0

# Prepare data for Bray-Curtis dissimilarity
species_matrix_cat <- species_counts_cat %>%
  column_to_rownames(var = "CAT_sp") %>%
  as.matrix()

# Calculate Bray-Curtis dissimilarity
bray_curtis_cat <- vegdist(species_matrix_cat, method = "bray")

# View the Bray-Curtis dissimilarity matrix
print(as.matrix(bray_curtis_cat))

# Function for resampling and calculating Bray-Curtis dissimilarity
resample_and_calculate_bray_cat <- function(data, small_locality_size_cat, bigger_locality_cat, small_locality_cat) {
  set.seed(sample(1:10000, 1)) # Set a random seed for reproducibility
  
  # Filter and resample data from the bigger locality
  subsampled_data <- data %>%
    filter(locality == bigger_locality_cat) %>%
    sample_n(small_locality_size_cat, replace = TRUE)
  
  # Combine the resampled data with the data from the smaller locality
  combined_data_cat <- bind_rows(subsampled_data, data %>% filter(locality == small_locality_cat))
  
  # Calculate species counts
  species_counts_cat <- combined_data_cat %>%
    group_by(locality, CAT_sp) %>%
    tally() %>%
    spread(key = locality, value = n, fill = 0) %>%
    as.data.frame()
  
  # Convert to species matrix
  species_matrix_cat <- species_counts_cat %>%
    column_to_rownames(var = "CAT_sp") %>%
    as.matrix()
  
  # Calculate Bray-Curtis dissimilarity
  bray_curtis_cat <- vegdist(species_matrix_cat, method = "bray")
  return(as.matrix(bray_curtis_cat))
}

# Run the function 999 times and store the results
bray_curtis_results_CAT <- lapply(1:999, function(x) {
  resample_and_calculate_bray_cat(caterpillar_data, small_locality_size_cat, bigger_locality_cat, small_locality_cat)
})

# Extract values and calculate mean and standard deviation
bray_curtis_values_CAT <- sapply(bray_curtis_results_CAT, function(mat) mat[1, 2])
mean_bray_curtis_CAT <- mean(bray_curtis_values_CAT)
sd_bray_curtis_CAT <- sd(bray_curtis_values_CAT)

# Print the results
print(mean_bray_curtis_CAT) # Mean Bray-Curtis dissimilarity
print(sd_bray_curtis_CAT)   # Standard deviation

