# Load necessary libraries
library(dplyr)
library(vegan)
library(tidyr)
library(tibble)
library(CommEcol)
library(readxl)

# Load the data
MASTER <- read_excel("DATA/MASTER.xlsx") %>% as_tibble()

# Filter data for parasitoids
parasitoid_data <- MASTER %>%
  filter(guild == "PAR", Par_remove == "0", locality %in% c("Ohu1", "Ohu2")) %>%
  select(locality, PAR_sp)

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

# Identify the smaller locality
small_locality <- locality_sizes %>%
  filter(count == min(count)) %>%
  pull(locality)

# Function for resampling and calculating Chao-Sorensen
resample_and_calculate <- function(data, small_locality_size, bigger_locality, small_locality) {
  # Resample data
  subsampled_data <- data %>%
    filter(locality == bigger_locality) %>%
    sample_n(small_locality_size)
  
  combined_data <- bind_rows(subsampled_data, data %>% filter(locality == small_locality))
  
  # Create species count table
  species_counts <- combined_data %>%
    group_by(locality, PAR_sp) %>%
    tally() %>%
    spread(key = locality, value = n, fill = 0) %>%
    as.data.frame()
  
  # Prepare species matrix
  species_matrix <- species_counts %>%
    column_to_rownames(var = "PAR_sp") %>%
    t() %>%
    as.matrix()
  
  # Calculate Chao-Sorensen dissimilarity
  chao_sorensen <- dis.chao(species_matrix, index = "sorensen", version = "rare", freq = NULL)
  return(as.matrix(chao_sorensen))
}

# Perform resampling and calculate Chao-Sorensen 999 times
set.seed(9999) # Consistent seed
chao_sorensen_results_PAR <- lapply(1:999, function(x) {
  resample_and_calculate(parasitoid_data, small_locality_size, bigger_locality, small_locality)
})

# Extract dissimilarity values and compute statistics
chao_sorensen_values_PAR <- sapply(chao_sorensen_results_PAR, function(mat) mat[1, 2])
mean_chao_sorensen_PAR <- mean(chao_sorensen_values_PAR)
sd_chao_sorensen_PAR <- sd(chao_sorensen_values_PAR)

# Print results
print(mean_chao_sorensen_PAR)
print(sd_chao_sorensen_PAR)




########################################################################################################

## CATERPILLARS

# Load necessary libraries
library(dplyr)
library(vegan)
library(tidyr)
library(tibble)
library(CommEcol)
library(readxl)

# Load the data
MASTER <- read_excel("DATA/MASTER.xlsx") %>% as_tibble()

# Filter data for caterpillars
caterpillar_data <- MASTER %>%
  filter(guild == "CAT",locality %in% c("Ohu1", "Ohu2")) %>%
  select(locality, CAT_sp)

# Determine the size of the smaller locality
small_locality_size <- caterpillar_data %>%
  group_by(locality) %>%
  summarize(count = n()) %>%
  summarize(min(count)) %>%
  pull()

# Identify the bigger locality
locality_sizes <- caterpillar_data %>%
  group_by(locality) %>%
  summarize(count = n())

bigger_locality <- locality_sizes %>%
  filter(count == max(count)) %>%
  pull(locality)

# Identify the smaller locality
small_locality <- locality_sizes %>%
  filter(count == min(count)) %>%
  pull(locality)

# Function for resampling and calculating Chao-Sorensen
resample_and_calculate <- function(data, small_locality_size, bigger_locality, small_locality) {
  # Resample data
  subsampled_data <- data %>%
    filter(locality == bigger_locality) %>%
    sample_n(small_locality_size)
  
  combined_data <- bind_rows(subsampled_data, data %>% filter(locality == small_locality))
  
  # Create species count table
  species_counts <- combined_data %>%
    group_by(locality, CAT_sp) %>%
    tally() %>%
    spread(key = locality, value = n, fill = 0) %>%
    as.data.frame()
  
  # Prepare species matrix
  species_matrix <- species_counts %>%
    column_to_rownames(var = "CAT_sp") %>%
    t() %>%
    as.matrix()
  
  # Calculate Chao-Sorensen dissimilarity
  chao_sorensen <- dis.chao(species_matrix, index = "sorensen", version = "rare", freq = NULL)
  return(as.matrix(chao_sorensen))
}

# Perform resampling and calculate Chao-Sorensen 999 times
set.seed(9999) # Consistent seed
chao_sorensen_results_CAT <- lapply(1:999, function(x) {
  resample_and_calculate(caterpillar_data, small_locality_size, bigger_locality, small_locality)
})

# Extract dissimilarity values and compute statistics
chao_sorensen_values_CAT <- sapply(chao_sorensen_results_CAT, function(mat) mat[1, 2])
mean_chao_sorensen_CAT <- mean(chao_sorensen_values_CAT)
sd_chao_sorensen_CAT <- sd(chao_sorensen_values_CAT)

# Print results
print(mean_chao_sorensen_CAT)
print(sd_chao_sorensen_CAT)




# Load ggplot2 library
library(ggplot2)

# Create a data frame for Bray-Curtis dissimilarity values
dissimilarity_data <- data.frame(
  Guild = c(rep("Parasitoids", length(bray_curtis_values_PAR)), 
            rep("Caterpillars", length(bray_curtis_values_CAT))),
  Dissimilarity = c(bray_curtis_values_PAR, bray_curtis_values_CAT)
)

# Create the boxplot
ggplot(dissimilarity_data, aes(x = Guild, y = Dissimilarity, fill = Guild)) +
  geom_boxplot() +
  labs(title = "Bray-Curtis Dissimilarity Boxplot for Parasitoids and Caterpillars",
       x = "Guild",
       y = "Dissimilarity Index") +
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", "lightgreen")) +
  theme(legend.position = "none")

