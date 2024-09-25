# Load necessary libraries
library(dplyr)
library(vegan)
library(tidyr)
library(tibble)
library(CommEcol)
MASTER <- read.csv("~/ownCloud/000_/000_R_stat/Cascading_diversity/DATA/MASTER.csv", sep=";") %>% as_tibble() #
parasitoid_data<-  MASTER  %>%
  filter(guild == "PAR", Par_remove == "0", locality %in% c("Ohu1", "Ohu2")) %>% 
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

# Prepare data for Sorensen dissimilarity
species_matrix_orig_para <- t(species_counts_orig_para)
names(species_matrix_orig_para) <- as.matrix(species_matrix_orig_para[1, ])
species_matrix_orig_para <- species_matrix_orig_para[-1, ]

species_matrix_orig_para <-as.data.frame(apply(species_matrix_orig_para,2,  as.numeric)) %>%  as.matrix()
species_matrix_orig_para [species_matrix_orig_para > 0] <- 1 # converts from abundance to P/A - for Sorensen index only

# Calculate Sorensen dissimilarity
sorensen_orig_para<-beta.pair(species_matrix_orig_para)#calculate Sorensen dissimilarity

sorensen_orig_para<-(as.matrix(sorensen_orig_para$beta.sor)) #pick sorensen index


# View the Sorensen dissimilarity matrix
print(as.matrix(sorensen_orig_para)) # 0.1345022



# Assume your data frame is called 'parasitoid_data'
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
combined_data %>% group_by(locality) %>% dplyr::summarize(count = n())

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

# Prepare data for Sorensen dissimilarity
species_matrix <- t(species_counts)
names(species_matrix) <- as.matrix(species_matrix[1, ])
species_matrix <- species_matrix[-1, ]
species_matrix <-as.data.frame(apply(species_matrix,2,  as.numeric))
species_matrix[species_matrix > 0] <- 1 # converts from abundance to P/A - for Sorensen index only


# Calculate Sorensen dissimilarity

sorensen<-beta.pair(species_matrix)#calculate Sorensen dissimilarity

sorensen<-(as.matrix(sorensen$beta.sor)) 

# View the Sorensen dissimilarity matrix
print(as.matrix(sorensen))


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
  
  sorensen <- dis.chao(species_matrix, index="sorensen", version="rare", freq=NULL)
  return(as.matrix(sorensen))
}


sorensen_results <- lapply(1:999, function(x) {
  resample_and_calculate(parasitoid_data, small_locality_size, bigger_locality, small_locality)
})

# View the results
print(sorensen_results)

sorensen_values <- sapply(sorensen_results, function(mat) mat[1, 2])
mean_sorensen <- mean(sorensen_values)


sd_sorensen_PAR <-sd(sorensen_values) #0.01525228

# Print the mean Sorensen dissimilarity
print(mean_sorensen) #0.379075



########################################################################################################

## CATERPILLARS

caterpillar_data<-  MASTER  %>%
  filter(guild == "CAT",  remove_cat == "0", locality %in% c("Ohu1", "Ohu2")) %>% 
  select(locality, CAT_sp)#


# Assume your data frame is called 'caterpillar_data'
# Determine the size of the smaller locality
small_locality_size_cat <- caterpillar_data %>% 
  group_by(locality) %>% 
  dplyr:: summarize(count = n()) %>% 
  dplyr:: summarize(min(count)) %>% 
  pull()

# Identify the bigger locality
locality_sizes_cat <- caterpillar_data %>% 
  group_by(locality) %>% 
  dplyr:: summarize(count = n()) ;locality_sizes_cat

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

# Prepare data for Sorensen dissimilarity
species_matrix_cat <- t(species_counts_cat)
names(species_matrix_cat) <- as.matrix(species_matrix_cat[1, ])
species_matrix_cat <- species_matrix_cat[-1, ]
species_matrix_cat <-as.data.frame(apply(species_matrix_cat,2,  as.numeric))
species_matrix_cat[species_matrix_cat > 0] <- 1 # converts from abundance to P/A - for Sorensen index only



# Calculate Sorensen dissimilarity
sorensen_cat<-beta.pair(species_matrix_cat)#calculate Sorensen dissimilarity

sorensen_cat<-(as.matrix(sorensen_cat$beta.sor)) 
# View the Sorensen dissimilarity matrix
print(as.matrix(sorensen_cat))


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
  
  # Calculate Sorensen distance
  
  sorensen_cat <- beta.pair(species_matrix_cat)
  sorensen_cat <- as.matrix(sorense_cat$beta.sor)
  return(as.matrix(sorensen_cat))
}

# Run the function 999 times and store the results
sorensen_results_cat <- lapply(1:999, function(x) {
  resample_and_calculate_cat(caterpillar_data, small_locality_size_cat, bigger_locality_cat, small_locality_cat)
})

# View the results
print(sorensen_results_cat)


sorensen_values_cat <- sapply(sorensen_results_cat, function(mat) mat[1, 2])
mean_sorensen_cat <- mean(sorensen_values_cat); mean_sorensen_cat 
sd_sorensen_CAT <-sd(sorensen_values_cat) # 0.007957034

# Print the mean Sorensen dissimilarity
print(mean_sorensen_cat) # 0.2295633





