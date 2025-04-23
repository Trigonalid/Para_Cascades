#----------------------------------------------------------#
#
#
#                   Cascading diversity
#
#                Bray-Curtis dissimilarity
#
#
#                 M. Libra,  O. Mottl
#                         2024
#
#----------------------------------------------------------#

# Estimate the Bray-Curtis dissimilarity between two localities for
#   parasitoids and catepillars.


#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

# Load necessary libraries
library(tidyverse)
library(here)
library(vegan)


#----------------------------------------------------------#
# 1. Load data -----
#----------------------------------------------------------#

data_master <-
  readr::read_delim(
    here::here(
      "DATA/MASTER.csv"
    ),
    delim = ";"
  ) %>%
  tibble::as_tibble()


#----------------------------------------------------------#
# 2. Helper functions -----
#----------------------------------------------------------#

get_speciec_counts <- function(data_source) {
  data_source %>%
    dplyr::group_by(locality, sp) %>%
    dplyr::count() %>%
    tidyr::pivot_wider(
      names_from = locality,
      values_from = n,
      values_fill = 0
    ) %>%
    as.data.frame() %>%
    return()
}

get_species_matrix <- function(data_source) {
  data_source %>%
    tibble::column_to_rownames(var = "sp") %>%
    t() %>%
    as.matrix()
}

get_bray_curtis <- function(data_source) {
  bray_curtis <-
    data_source %>%
    vegan::vegdist(method = "bray") %>%
    as.matrix()
  
  res <-
    bray_curtis[1, 2]
  
  return(res)
}

get_smaller_locality_size <- function(data_source) {
  data_source %>%
    dplyr::group_by(locality) %>%
    dplyr::summarize(count = n()) %>%
    dplyr::summarize(min(count)) %>%
    dplyr::pull()
}

get_locality_name_by_size <- function(data_source, type = c("min", "max")) {
  type <- match.arg(type)
  data_summed <-
    data_source %>%
    dplyr::group_by(locality) %>%
    dplyr::summarize(count = n())
  
  if (
    type == "max"
  ) {
    res <-
      dplyr::filter(data_summed, count == max(count))
  } else {
    res <-
      dplyr::filter(data_summed, count == min(count))
  }
  
  res %>%
    dplyr::pull(locality) %>%
    return()
}

resample_and_calculate_bray_curtis <- function(
    data_source,
    small_locality_size,
    bigger_locality_name,
    small_locality_name,
    resample = TRUE) {
  subsampled_data <-
    data_source %>%
    dplyr::filter(locality == bigger_locality_name) %>%
    dplyr::sample_n(small_locality_size, replace = resample)
  
  combined_data <-
    dplyr::bind_rows(
      subsampled_data,
      data_source %>%
        dplyr::filter(locality == small_locality_name)
    )
  
  species_counts <-
    get_speciec_counts(combined_data)
  
  species_matrix <-
    get_species_matrix(species_counts)
  
  res <-
    get_bray_curtis(species_matrix)
  
  return(res)
}

#----------------------------------------------------------#
# 3. Parasitoids -----
#----------------------------------------------------------#

data_parasitoids <-
  data_master %>%
  dplyr::filter(
    guild == "PAR",
    Par_remove == "0",
    locality %in% c("Ohu1", "Ohu2")
  ) %>%
  dplyr::select(
    locality,
    sp = PAR_sp
  )

data_parasitoids_sp_count <-
  get_speciec_counts(data_parasitoids)

# View the species counts table
print(head(data_parasitoids_sp_count))

# Prepare data for Bray-Curtis dissimilarity
data_parasitoids_sp_count_matrix <-
  get_species_matrix(data_parasitoids_sp_count)

# Calculate Bray-Curtis dissimilarity
parasitoids_bray_curtis <-
  get_bray_curtis(data_parasitoids_sp_count_matrix)

# View the Bray-Curtis dissimilarity matrix
print(parasitoids_bray_curtis) # 0.4143921

set.seed(9999) # Set seed for reproducibility
parasitoids_bray_curtis_resampled <-
  purrr::map_dbl(
    .progress = TRUE,
    .x = 1:999,
    .f = ~ resample_and_calculate_bray_curtis(
      data_source = data_parasitoids,
      small_locality_size = get_smaller_locality_size(data_parasitoids),
      bigger_locality_name = get_locality_name_by_size(data_parasitoids, "max"),
      small_locality_name = get_locality_name_by_size(data_parasitoids, "min")
    )
  )

# View the results
print(summary(parasitoids_bray_curtis_resampled))

parasitoids_bray_curtis_resampled_mean <-
  mean(parasitoids_bray_curtis_resampled)
parasitoids_bray_curtis_resampled_sd <-
  sd(parasitoids_bray_curtis_resampled) # 0.01525228

# Print the mean Bray-Curtis dissimilarity
print(parasitoids_bray_curtis_resampled_mean) # 0.379075

# Violin plot with boxplot, jittered points, and observed value
ggplot(bray_data, aes(x = type, y = value)) +
  geom_violin(data = subset(bray_data, type == "Resampled"),
              fill = "#69b3a2", alpha = 0.6, width = 0.8, trim = FALSE) +
  geom_boxplot(data = subset(bray_data, type == "Resampled"),
               width = 0.1, outlier.shape = NA, alpha = 0.8) +
  geom_jitter(data = subset(bray_data, type == "Resampled"),
              width = 0.1, alpha = 0.3, color = "black", size = 1) +
  geom_point(data = subset(bray_data, type == "Observed"),
             aes(x = type, y = value),
             color = "red", size = 3) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Bray-Curtis Dissimilarity: Observed vs. Resampled",
    subtitle = "Observed value (red), resampled distribution (violin, box, scatter)",
    x = NULL,
    y = "Bray-Curtis Dissimilarity"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12))





#----------------------------------------------------------#
# 4. Catepillars -----
#----------------------------------------------------------#

data_catepillars <-
  data_master %>%
  dplyr::filter(
    guild == "CAT",
    remove_cat == "0",
    locality %in% c("Ohu1", "Ohu2")
  ) %>%
  dplyr::select(
    locality,
    sp = CAT_sp
  )

data_catepillars_sp_count <-
  get_speciec_counts(data_catepillars)

# View the species counts table
print(head(data_catepillars_sp_count))

# Prepare data for Bray-Curtis dissimilarity
data_catepillars_sp_count_matrix <-
  get_species_matrix(data_catepillars_sp_count)

# Calculate Bray-Curtis dissimilarity
catepillars_bray_curtis <-
  get_bray_curtis(data_catepillars_sp_count_matrix)

# View the Bray-Curtis dissimilarity matrix
print(catepillars_bray_curtis) # 0.3057603


set.seed(9999) # Set seed for reproducibility
catepillars_bray_curtis_resampled <-
  purrr::map_dbl(
    .progress = TRUE,
    .x = 1:999,
    .f = ~ resample_and_calculate_bray_curtis(
      data_source = data_catepillars,
      small_locality_size = get_smaller_locality_size(data_catepillars),
      bigger_locality_name = get_locality_name_by_size(data_catepillars, "max"),
      small_locality_name = get_locality_name_by_size(data_catepillars, "min")
    )
  )

# View the results
print(summary(catepillars_bray_curtis_resampled))

catepillars_bray_curtis_resampled_mean <-
  mean(catepillars_bray_curtis_resampled)
catepillars_bray_curtis_resampled_sd <-
  sd(catepillars_bray_curtis_resampled)

# Print the mean Bray-Curtis dissimilarity
print(catepillars_bray_curtis_resampled_mean)

# 4.1 Resampling for Parasitoids within Ohu2 -----

resample_and_calculate_bray_curtis_within_ohu2 <- function(
    data_source,
    size,
    bigger_locality_name,
    small_locality_name,
    resample = TRUE) {
  subsampled_data_bigger <-
    data_source %>%
    dplyr::filter(locality == bigger_locality_name) %>%
    dplyr::sample_n(size, replace = resample)
  
  subsampled_data_smaller <-
    data_source %>%
    dplyr::filter(locality == small_locality_name) %>%
    dplyr::sample_n(size, replace = resample)
  
  combined_data <-
    dplyr::bind_rows(
      subsampled_data_bigger,
      subsampled_data_smaller
    )
  
  species_counts <-
    get_speciec_counts(combined_data)
  
  species_matrix <-
    get_species_matrix(species_counts)
  
  res <-
    get_bray_curtis(species_matrix)
  
  return(res)
}

set.seed(9999) # Set seed for reproducibility
catepillars_bray_curtis_resampled_for_para_ohu2 <-
  purrr::map_dbl(
    .progress = TRUE,
    .x = 1:999,
    .f = ~ resample_and_calculate_bray_curtis_within_ohu2(
      data_catepillars,
      size = get_smaller_locality_size(data_parasitoids),
      bigger_locality_name = get_locality_name_by_size(data_parasitoids, "max"),
      small_locality_name = get_locality_name_by_size(data_parasitoids, "min")
    )
  )

# View the results
print(summary(catepillars_bray_curtis_resampled_for_para_ohu2))

catepillars_bray_curtis_resampled_for_para_ohu2_mean <-
  mean(catepillars_bray_curtis_resampled_for_para_ohu2)
catepillars_bray_curtis_resampled_for_para_ohu2_sd <-
  sd(catepillars_bray_curtis_resampled_for_para_ohu2)

# Print the mean Bray-Curtis dissimilarity
print(catepillars_bray_curtis_resampled_for_para_ohu2_mean)




##### Plot 
# Prepare combined data
bray_data_parasitoids <- data.frame(
  type = "Resampled",
  value = parasitoids_bray_curtis_resampled,
  guild = "Parasitoids"
)

bray_data_parasitoids <- rbind(
  bray_data_parasitoids,
  data.frame(
    type = "Observed",
    value = parasitoids_bray_curtis,
    guild = "Parasitoids"
  )
)

bray_data_caterpillars <- data.frame(
  type = "Resampled",
  value = catepillars_bray_curtis_resampled,
  guild = "Caterpillars"
)

bray_data_caterpillars <- rbind(
  bray_data_caterpillars,
  data.frame(
    type = "Observed",
    value = catepillars_bray_curtis,
    guild = "Caterpillars"
  )
)

# Combine both into one dataframe
bray_data_all <- rbind(bray_data_parasitoids, bray_data_caterpillars)

# Plot
ggplot(bray_data_all, aes(x = type, y = value)) +
  geom_violin(data = subset(bray_data_all, type == "Resampled"),
              fill = "#a6cee3", alpha = 0.6, width = 0.8, trim = FALSE) +
  geom_boxplot(data = subset(bray_data_all, type == "Resampled"),
               width = 0.1, outlier.shape = NA, alpha = 0.8) +
  geom_jitter(data = subset(bray_data_all, type == "Resampled"),
              width = 0.1, alpha = 0.3, color = "gray30", size = 1) +
  geom_point(data = subset(bray_data_all, type == "Observed"),
             aes(x = type, y = value),
             color = "red", size = 3) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    x = NULL,
    y = "Bray-Curtis dissimilarity"
  ) +
  facet_wrap(~guild) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 12))

