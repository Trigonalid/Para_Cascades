# Load necessary libraries
library(tidyr)
library(dplyr)
library(vegan)

# Provided data
data <- data.frame(
  locality = rep("Ohu1", 30),
  CAT_sp = c(
    "Choreutis lutescens", "Choreutis lutescens", "Talanga les", "Choreutis lutescens", "Brenthia 1",
    "Brenthia 1", "Brenthia 4", "Brenthia 4", "Brenthia 1", "Brenthia 2",
    "Choreutis lutescens", "Choreutis lutescens", "Talanga les", "Choreutis lutescens", "Brenthia 1",
    "Brenthia 1", "Brenthia 4", "Brenthia 4", "Brenthia 1", "Brenthia 2",
    "Choreutis lutescens", "Choreutis lutescens", "Talanga les", "Choreutis lutescens", "Brenthia 1",
    "Brenthia 1", "Brenthia 4", "Brenthia 4", "Brenthia 1", "Brenthia 2"
  )
)

# Create table C
table_C <- data.frame(
  locality = rep("Ohu1", 17),
  CAT_sp = c(
    "Choreutis lutescens", "Choreutis lutescens", "Talanga les", "Choreutis lutescens", "Brenthia 1",
    "Brenthia 1", "Brenthia 4", "Brenthia 4", "Brenthia 1", "Brenthia 2",
    "Choreutis lutescens", "Choreutis lutescens", "Talanga les", "Choreutis lutescens", "Brenthia 1",
    "Brenthia 1", "Brenthia 4"
  )
)

# Function to create a wide-format summary
create_wide_summary <- function(df) {
  df %>%
    group_by(locality, CAT_sp) %>%
    summarise(count = n(), .groups = 'drop') %>%
    pivot_wider(names_from = CAT_sp, values_from = count, values_fill = 0)
}

# Create wide-format summary for table C
wide_C <- create_wide_summary(table_C)

# Set seed for reproducibility
set.seed(123)

# Function to perform one random subsampling and calculate Bray-Curtis dissimilarity
calculate_bray_curtis <- function(data, wide_C) {
  sampled_data <- data[sample(nrow(data), 10), ]
  wide_sample <- create_wide_summary(sampled_data)
  
  # Ensure both data frames have the same columns
  all_columns <- union(names(wide_C), names(wide_sample))
  wide_C_filled <- wide_C %>% select(all_of(all_columns)) %>% replace(is.na(.), 0)
  wide_sample_filled <- wide_sample %>% select(all_of(all_columns)) %>% replace(is.na(.), 0)
  
  # Calculate Bray-Curtis dissimilarity
  dissimilarity <- vegdist(rbind(as.matrix(wide_C_filled[,-1]), as.matrix(wide_sample_filled[,-1])), method = "bray")
  
  return(dissimilarity)
}

# Perform random subsampling 100 times and calculate Bray-Curtis dissimilarity
bray_curtis_values <- replicate(100, calculate_bray_curtis(data, wide_C))

# Compute the mean of the Bray-Curtis dissimilarities
mean_bray_curtis <- mean(bray_curtis_values)

# Print the mean Bray-Curtis dissimilarity
cat("The mean Bray-Curtis dissimilarity is:", mean_bray_curtis, "\n")
