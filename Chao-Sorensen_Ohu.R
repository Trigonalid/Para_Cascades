# Load necessary libraries
library(dplyr)
library(vegan)
library(tidyr)
library(tibble)
library(CommEcol)
MASTER <- read.csv("~/ownCloud/000_/000_R_stat/Cascading_diversity/DATA/MASTER.csv", sep=";") %>% as_tibble() #
parasitoid_data<-  MASTER  %>%
  filter(`guild` == "PAR", Par_remove == "0", locality %in% c("Ohu1", "Ohu2")) %>% 
  select(locality, PAR_sp)#
#### Original B-C
species_counts_orig_para <- parasitoid_data %>%
  group_by(locality, PAR_sp) %>%
  tally() %>%
  spread(key = locality, value = n, fill = 0) %>%
  as.data.frame()

# Replace NA with 0 for species that do not appear in a locality
species_counts_orig_para[is.na(species_counts_orig_para)] <- 0

# View the species counts table
print(species_counts_orig_para)

# Prepare data for Chao-Sorensen dissimilarity
species_matrix_orig_para <- t(species_counts_orig_para)
names(species_matrix_orig_para) <- as.matrix(species_matrix_orig_para[1, ])
species_matrix_orig_para <- species_matrix_orig_para[-1, ]
species_matrix_orig_para <-as.data.frame(apply(species_matrix_orig_para,2,  as.numeric)) %>%  as.matrix()

#CS_par_res_mantel <- mantel(CS_par_res, Distance , method="spear"); CS_par_res_mantel



# Calculate Chao-Sorensen dissimilarity
chao_sorensen_orig_para <- dis.chao(species_matrix_orig_para, index="sorensen", version="rare", freq=NULL)


# View the Chao-Sorensen dissimilarity matrix
print(as.matrix(chao_sorensen_orig_para)) # 0.1345022



# Assume your data frame is called 'parasitoid_data'
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

# Randomly subsample from the bigger locality
set.seed(9999) # Set seed for reproducibility
subsampled_data <- parasitoid_data %>%
  filter(locality == bigger_locality) %>%
  sample_n(small_locality_size)

# Combine subsampled data with the smaller locality data
small_locality <- locality_sizes %>% 
  filter(count == min(count)) %>% 
  pull(locality)

combined_data <- bind_rows(subsampled_data, parasitoid_data %>% filter(locality == small_locality))

# Verify the size of combined data
combined_data %>% group_by(locality) %>% summarize(count = n())

# Create a table with species counts for each locality
species_counts <- combined_data %>%
  group_by(locality, PAR_sp) %>%
  tally() %>%
  spread(key = locality, value = n, fill = 0) %>%
  as.data.frame()

# Replace NA with 0 for species that do not appear in a locality
species_counts[is.na(species_counts)] <- 0

# View the species counts table
print(species_counts)

# Prepare data for Chao-Sorensen dissimilarity
species_matrix <- t(species_counts)
names(species_matrix) <- as.matrix(species_matrix[1, ])
species_matrix <- species_matrix[-1, ]
species_matrix <-as.data.frame(apply(species_matrix,2,  as.numeric))


# Calculate Chao-Sorensen dissimilarity
chao_sorensen <- dis.chao(species_matrix, index="sorensen", version="rare", freq=NULL)

# View the Chao-Sorensen dissimilarity matrix
print(as.matrix(chao_sorensen))


# Function for repetition
resample_and_calculate <- function(data, small_locality_size, bigger_locality, small_locality) {
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
    t() %>%
    as.matrix()
  
  chao_sorensen <- dis.chao(species_matrix, index="sorensen", version="rare", freq=NULL)
  return(as.matrix(chao_sorensen))
}


chao_sorensen_results <- lapply(1:999, function(x) {
  resample_and_calculate(parasitoid_data, small_locality_size, bigger_locality, small_locality)
})

# View the results
print(chao_sorensen_results)

chao_sorensen_values <- sapply(chao_sorensen_results, function(mat) mat[1, 2])
mean_chao_sorensen <- mean(chao_sorensen_values)
sd_chao_sorensen_PAR <-sd(chao_sorensen_values) #0.01525228

# Print the mean Chao-Sorensen dissimilarity
print(mean_chao_sorensen) #0.379075



########################################################################################################

## CATERPILLARS

caterpillar_data<-  MASTER  %>%
  filter(guild == "CAT",  remove_cat == "0", locality %in% c("Ohu1", "Ohu2")) %>% 
  select(locality, CAT_sp)#


# Assume your data frame is called 'caterpillar_data'
# Determine the size of the smaller locality
small_locality_size_cat <- caterpillar_data %>% 
  group_by(locality) %>% 
  summarize(count = n()) %>% 
  summarize(min(count)) %>% 
  pull()

# Identify the bigger locality
locality_sizes_cat <- caterpillar_data %>% 
  group_by(locality) %>% 
  summarize(count = n()) ;locality_sizes_cat

bigger_locality_cat <- locality_sizes_cat %>% 
  filter(count == max(count)) %>% 
  pull(locality); bigger_locality_cat

# Randomly subsample from the bigger locality
set.seed(123) # Set seed for reproducibility
subsampled_data_cat <- caterpillar_data %>%
  filter(locality == bigger_locality_cat) %>%
  sample_n(small_locality_size_cat); subsampled_data_cat

# Combine subsampled data with the smaller locality data
small_locality_cat <- locality_sizes_cat %>% 
  filter(count == min(count)) %>% 
  pull(locality);small_locality_cat

combined_data_cat <- bind_rows(subsampled_data_cat, caterpillar_data %>% filter(locality == small_locality_cat))

# Verify the size of combined data
combined_data_cat %>% group_by(locality) %>% summarize(count = n())

# Create a table with species counts for each locality
species_counts_cat <- combined_data_cat %>%
  group_by(locality, CAT_sp) %>%
  tally() %>%
  spread(key = locality, value = n, fill = 0) %>%
  as.data.frame()

# Replace NA with 0 for species that do not appear in a locality
species_counts_cat[is.na(species_counts_cat)] <- 0

# View the species counts table
print(species_counts_cat)

# Prepare data for Chao-Sorensen dissimilarity
species_matrix_cat <- t(species_counts_cat)
names(species_matrix_cat) <- as.matrix(species_matrix_cat[1, ])
species_matrix_cat <- species_matrix_cat[-1, ]
species_matrix_cat <-as.data.frame(apply(species_matrix_cat,2,  as.numeric))


# Calculate Chao-Sorensen dissimilarity
chao_sorensen_cat <- dis.chao(species_matrix_cat, index="sorensen", version="rare", freq=NULL)
# View the Chao-Sorensen dissimilarity matrix
print(as.matrix(chao_sorensen_cat))


resample_and_calculate_cat <- function(data, small_locality_size_cat, bigger_locality_cat, small_locality_cat) {
  set.seed(sample(1:10000, 1)) # Set a random seed for reproducibility
  
  # Filter and resample data from the bigger locality
  subsampled_data <- data %>%
    filter(locality == bigger_locality_cat) %>%
    sample_n(small_locality_size_cat, replace = TRUE) # Ensure replace is appropriate for your data
  
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
    t() %>%
    as.matrix()
  
  # Calculate Chao-Sorensen distance
  chao_sorensen_cat <- dis.chao(species_matrix_cat, index="sorensen", version="rare", freq=NULL)
  return(as.matrix(chao_sorensen_cat))
}

# Run the function 999 times and store the results
chao_sorensen_results_cat <- lapply(1:999, function(x) {
  resample_and_calculate_cat(caterpillar_data, small_locality_size_cat, bigger_locality_cat, small_locality_cat)
})

# View the results
print(chao_sorensen_results_cat)


chao_sorensen_values_cat <- sapply(chao_sorensen_results_cat, function(mat) mat[1, 2])
mean_chao_sorensen_cat <- mean(chao_sorensen_values_cat); mean_chao_sorensen_cat 
sd_chao_sorensen_CAT <-sd(chao_sorensen_values_cat) # 0.007957034

# Print the mean Chao-Sorensen dissimilarity
print(mean_chao_sorensen_cat) # 0.2295633





