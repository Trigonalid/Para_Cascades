

Enemy free space where caterpillars without parasitoids in origin dataset are still present.


```{r Data_prep_A}
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
  dplyr::filter(
    is.na(Par_remove) |
      Par_remove == "0"
  ) %>%
  dplyr::filter(remove_cat == "0") %>%
  dplyr::select(
    locality,
    plant,
    CAT_sp,
    PAR_sp
  )



# Create  summary table


interaction_summary <- data_work %>%
  dplyr::group_by(CAT_sp,PAR_sp) %>%
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

no_parasitoid_summary <-
  data_work %>%
  dplyr::filter(is.na(PAR_sp)) %>% # keep only rows where parasiotid is missing
  dplyr::group_by(CAT_sp, PAR_sp) %>%
  dplyr::summarise(
    # Number of distinct localities where no parasitoid was found
    total_localities = dplyr::n_distinct(locality, na.rm = TRUE), 
    # Localities where the caterpillar is parasitized (PAR_sp is not NA or empty)
    cat_parasitised = dplyr::n_distinct(locality[!is.na(PAR_sp) & PAR_sp != ""]),
    # Localities where the caterpillar is detected but not parasitized
    cat_available_without_par = dplyr::n_distinct(locality[is.na(PAR_sp) | PAR_sp == ""]),
    # Localities where the caterpillar is not parasitized (PAR_sp is NA or empty)
    detected_no_par_localities = dplyr::n_distinct(locality) - dplyr::n_distinct(locality[!is.na(PAR_sp) & PAR_sp != ""])
  ) %>%
  dplyr::mutate(PAR_sp = NA) # Indicate that no parasitoid was found


# Combine the two tables (with and without parasitoids)
final_summary <-
  dplyr::bind_rows(interaction_summary, no_parasitoid_summary)



# Step 1: Merge data_locality_summary with the full data

# Summary of data
data_locality_summary <-
  data_work %>%
  dplyr::group_by(locality) %>%
  dplyr::summarise(
    num_plant_species = dplyr::n_distinct(plant, na.rm = TRUE),
    num_caterpillar_species = dplyr::n_distinct(CAT_sp, na.rm = ),
    num_parasitoid_species = dplyr::n_distinct(PAR_sp, na.rm = TRUE)
  ) %>%
  dplyr::mutate(
    abundance_caterpillars = purrr::map_dbl(
      .x = locality,
      .f = ~ data_work %>%
        dplyr::filter(locality == .x) %>%
        tidyr::drop_na(CAT_sp) %>%
        dplyr::select(CAT_sp) %>%
        nrow()
    ),
    abundance_parasitoids = purrr::map_dbl(
      .x = locality,
      .f = ~ data_work %>%
        dplyr::filter(locality == .x) %>%
        tidyr::drop_na(PAR_sp) %>%
        dplyr::select(PAR_sp) %>%
        nrow()
    )
  )

merged_data <-
  data_work %>%
  dplyr::left_join(
    data_locality_summary,
    by = "locality"
  )

# Step 1: Function to subsample data and compute interactions
subsample_data <- function(data_community, data_locality, replace = TRUE) {
  subsampled_data <-
    data_locality %>%
    dplyr::select(locality, abundance_caterpillars) %>%
    dplyr::mutate(
      data_subsampled = purrr::map(
        .x = abundance_caterpillars,
        .f = ~ dplyr::sample_n(
          data_community,
          size = .x,
          replace = replace
        ) %>%
          dplyr::select(-locality)
      )
    ) %>%
    tidyr::unnest(data_subsampled)
  
  return(subsampled_data)
}


group_subsample_data <- function(data_source) {
  # Summarize subsampled interactions (including caterpillars without parasitoids)
  subsampled_interaction_summary <-
    data_source %>%
    dplyr::filter(!is.na(PAR_sp)) %>% # Filter out rows where parasitoid is missing
    dplyr::group_by(CAT_sp, PAR_sp) %>%
    dplyr::summarise(
      .groups = "drop",
      # Number of distinct localities where no parasitoid was found
      total_localities = dplyr::n_distinct(locality, na.rm = TRUE), 
      # Localities where the caterpillar is parasitized (PAR_sp is not NA or empty)
      cat_parasitised = dplyr::n_distinct(locality[!is.na(PAR_sp) & PAR_sp != ""]),
      # Localities where the caterpillar is detected but not parasitized
      cat_available_without_par = dplyr::n_distinct(locality[is.na(PAR_sp) | PAR_sp == ""]),
      # Localities where the caterpillar is not parasitized (PAR_sp is NA or empty)
      detected_no_par_localities = dplyr::n_distinct(locality) - dplyr::n_distinct(locality[!is.na(PAR_sp) & PAR_sp != ""])
    )
  
  
  # Handle caterpillars without parasitoids
  subsampled_no_parasitoid_summary <-
    data_source %>%
    dplyr::filter(is.na(PAR_sp)) %>%
    dplyr::group_by(CAT_sp, PAR_sp) %>%
    dplyr::summarise(
      .groups = "drop",
      # Number of distinct localities where no parasitoid was found
      total_localities = dplyr::n_distinct(locality, na.rm = TRUE), 
      # Localities where the caterpillar is parasitized (PAR_sp is not NA or empty)
      cat_parasitised = dplyr::n_distinct(locality[!is.na(PAR_sp) & PAR_sp != ""]),
      # Localities where the caterpillar is detected but not parasitized
      cat_available_without_par = dplyr::n_distinct(locality[is.na(PAR_sp) | PAR_sp == ""]),
      # Localities where the caterpillar is not parasitized (PAR_sp is NA or empty)
      detected_no_par_localities = dplyr::n_distinct(locality) - dplyr::n_distinct(locality[!is.na(PAR_sp) & PAR_sp != ""])
    )
  
  # Combine the interaction summaries (with and without parasitoids)
  final_subsampled_summary <-
    dplyr::bind_rows(
      subsampled_interaction_summary,
      subsampled_no_parasitoid_summary
    )
  
  return(final_subsampled_summary)
}
```

# replacement TRUE
```{r Subsampling A}
# Step 2: Repeat subsampling 99 times and store results
set.seed(1234)
list_subsampled <-
  purrr::map(
    .progress = TRUE,
    .x = 1:999,
    .f = ~ subsample_data(data_work, data_locality_summary)
  )

subsampled_results <-
  purrr::map(
    .progress = TRUE,
    .x = list_subsampled,
    .f = ~ group_subsample_data(.x)
  ) %>%
  dplyr::bind_rows(.id = "iteration")

# Step 4: Calculate the mean interactions across all 99 iterations
mean_subsampled_interactions <-
  subsampled_results %>%
  dplyr::group_by(CAT_sp, PAR_sp) %>%     ###################### BEZ PAR!
  dplyr::summarise(
    .groups = "drop",
    mean_total_sub_localities = mean(total_localities, na.rm = TRUE), # Mean number of localities
    mean_sub_cat_parasitised = mean(cat_parasitised, na.rm = TRUE),
    mean_sub_without_origin = mean (cat_available_without_par, na.rm = TRUE),
    mean_sub_detected_no_par_localitites = mean(detected_no_par_localities, na.rm= TRUE)
  )

# Step 5: Add the mean interactions to the original table
final_merged_summary <-
  final_summary %>%
  dplyr::full_join(
    mean_subsampled_interactions,
    by = c("CAT_sp","PAR_sp")
  )
# View the final result
#print(final_merged_summary)

writexl::write_xlsx(
  final_merged_summary,
  here::here("output", "final_merged_summary_replace_TRUE.xlsx")
)
# Step 6: Summarize data for each caterpillar species (regardless of parasitoid)
summary_caterpillar <-
  final_merged_summary %>%
  dplyr::group_by(CAT_sp) %>%
  dplyr::summarise(
    .groups = "drop",
    localitites_orig = mean(total_localities, na.rm = TRUE),
    parasitised_orig = mean (cat_parasitised, na.rm = TRUE),
    enemy_free_orig = mean (detected_no_par_localities, na.rm = TRUE),
    localities_sub = mean(mean_total_sub_localities, na.rm = TRUE),
    parasitised_sub = mean(mean_sub_cat_parasitised, na.rm = TRUE),
    enemy_free_sub = mean(mean_sub_detected_no_par_localitites, na.rm = TRUE)
  )


# View the summary for each caterpillar species
#View(summary_caterpillar)

# Reshape the data into long format for easier plotting
summary_long <-
  summary_caterpillar %>%
  tidyr::pivot_longer(
    cols = c(localitites_orig, parasitised_orig, enemy_free_orig,localities_sub,parasitised_sub,enemy_free_sub),
    names_to = "group",
    values_to = "mean_localities"
  )

# Create the boxplot with y-axis limits set to 0-8
summary_long$group <- factor(summary_long$group, levels = c("localitites_orig","localities_sub", "parasitised_orig","parasitised_sub", "enemy_free_orig","enemy_free_sub"))
p1 <-
  ggplot(summary_long, aes(x = group, y = mean_localities, fill = group)) +
  geom_violin() +
  geom_boxplot(width = 0.2, fill = "white") +
  geom_jitter() +
  scale_fill_manual(values = c( "#E69F00",  "#009E73","#D55E00","#56B4E9", "#999999", "#CC79A7" )) + # Colorblind-friendly colors
  labs(
    x = "Group",
    y = "Mean Number of Localities",
    title = "Enemy free space for caterpillars"
  ) +
  coord_cartesian(ylim = c(0, 9)) + # Set y-axis limits
  scale_y_continuous(
    breaks = seq(1, 9, 1)
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot2::ggsave(
  plot = p1,
  filename = here::here("output", "mean_localities_boxplot_replace_TRUE.png"),
  bg = "white"
)
```



##### Caterpillar species without parasitoids are removed from dataset


```{r CATS with PAR only}
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
  dplyr::filter(
    is.na(Par_remove) |
      Par_remove == "0"
  ) %>%
  dplyr::filter(remove_cat == "0") %>%
  dplyr::select(
    locality,
    plant,
    CAT_sp,
    PAR_sp
  )



# Create  summary table


interaction_summary <- data_work %>%
  dplyr::group_by(CAT_sp, PAR_sp) %>%
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

# Prepare tables only with caterpillars, which have parasitoid

# Function to create a table of caterpillar species with and without parasitoids
create_caterpillar_parasitoid_table <- function(interaction_summary) {
  # Create a summary table
  table <- interaction_summary %>%
    group_by(CAT_sp) %>%
    summarize(has_parasitoid = ifelse(any(!is.na(PAR_sp) & PAR_sp != ""), "Yes", "No")) %>%
    arrange(desc(has_parasitoid)) # Sort by whether or not they have a parasitoid
  
  return(table)
}
caterpillar_parasitoid_table <- create_caterpillar_parasitoid_table(interaction_summary)
print(caterpillar_parasitoid_table)



# Filter rows in interaction_summary based on the "YES" condition in caterpillar_parasitoid_table
final_summary_B <- interaction_summary %>%
  filter(CAT_sp %in% caterpillar_parasitoid_table$CAT_sp[caterpillar_parasitoid_table$has_parasitoid == "Yes"])

# Print the filtered table
print(final_summary_B)

# Making dataset B containing all the data about caterpillars with parasitoids
data_work_B <- data_work %>% 
  filter(CAT_sp %in% caterpillar_parasitoid_table$CAT_sp[caterpillar_parasitoid_table$has_parasitoid == "Yes"])



# Step 1: Merge data_locality_summary_B with the full data

# Summary of data
data_locality_summary_B <-
  data_work_B %>%
  dplyr::group_by(locality) %>%
  dplyr::summarise(
    num_plant_species = dplyr::n_distinct(plant, na.rm = TRUE),
    num_caterpillar_species = dplyr::n_distinct(CAT_sp, na.rm = TRUE),
    num_parasitoid_species = dplyr::n_distinct(PAR_sp, na.rm = TRUE)
  ) %>%
  dplyr::mutate(
    abundance_caterpillars = purrr::map_dbl(
      .x = locality,
      .f = ~ data_work_B %>%
        dplyr::filter(locality == .x) %>%
        tidyr::drop_na(CAT_sp) %>%
        dplyr::select(CAT_sp) %>%
        nrow()
    ),
    abundance_parasitoids = purrr::map_dbl(
      .x = locality,
      .f = ~ data_work_B %>%
        dplyr::filter(locality == .x) %>%
        tidyr::drop_na(PAR_sp) %>%
        dplyr::select(PAR_sp) %>%
        nrow()
    )
  )

merged_data_B <-
  data_work_B %>%
  dplyr::left_join(
    data_locality_summary_B,
    by = "locality"
  )

# Step 1: Function to subsample data and compute interactions
subsample_data_B <- function(data_community, data_locality_summary_B, replace = TRUE) {
  subsampled_data_B <-
    data_locality_summary_B %>%
    dplyr::select(locality, abundance_caterpillars) %>%
    dplyr::mutate(
      data_subsampled = purrr::map(
        .x = abundance_caterpillars,
        .f = ~ dplyr::sample_n(
          data_community,
          size = .x,
          replace = replace
        ) %>%
          dplyr::select(-locality)
      )
    ) %>%
    tidyr::unnest(data_subsampled)
  
  return(subsampled_data_B)
}


group_subsample_data_B <- function(data_source) {
  # Summarize subsampled interactions (including caterpillars without parasitoids)
  subsampled_interaction_summary_B <-
    data_source %>%
    dplyr::group_by(CAT_sp, PAR_sp) %>%
    dplyr::summarise(
      .groups = "drop",
      # Number of distinct localities
      total_localities = dplyr::n_distinct(locality, na.rm = TRUE), 
      # Localities where the caterpillar is parasitized (PAR_sp is not NA or empty)
      cat_parasitised = dplyr::n_distinct(locality[!is.na(PAR_sp) & PAR_sp != ""]),
      # Localities where the caterpillar is not parasitized (PAR_sp is NA or empty)
      cat_available_without_par = dplyr::n_distinct(locality[is.na(PAR_sp) | PAR_sp == ""]),
      # Localities where the caterpillar is detected but not parasitized
      detected_no_par_localities = dplyr::n_distinct(locality) - dplyr::n_distinct(locality[!is.na(PAR_sp) & PAR_sp != ""])
    )
  
  return(subsampled_interaction_summary_B)
}


# Step 2: Repeat subsampling 99 times and store results
set.seed(1234)
list_subsampled_B <-
  purrr::map(
    .progress = TRUE,
    .x = 1:999,
    .f = ~ subsample_data_B(data_work_B, data_locality_summary_B)
  )

subsampled_results_B <-
  purrr::map(
    .progress = TRUE,
    .x = list_subsampled_B,
    .f = ~ group_subsample_data_B(.x)
  ) %>%
  dplyr::bind_rows(.id = "iteration")

# Step 4: Calculate the mean interactions across all 99 iterations
mean_subsampled_interactions_B <-
  subsampled_results_B %>%
  dplyr::group_by(CAT_sp, PAR_sp) %>%     ###################### BEZ PAR!
  dplyr::summarise(
    .groups = "drop",
    mean_total_sub_localities = mean(total_localities, na.rm = TRUE), # Mean number of localities
    mean_sub_cat_parasitised = mean(cat_parasitised, na.rm = TRUE),
    mean_sub_without_origin = mean (cat_available_without_par, na.rm = TRUE),
    mean_sub_detected_no_par_localitites = mean(detected_no_par_localities, na.rm= TRUE)
  )

# Step 5: Add the mean interactions to the original table
final_merged_summary_B <-
  final_summary_B %>%
  dplyr::full_join(
    mean_subsampled_interactions_B,
    by = c("CAT_sp", "PAR_sp")
  )

# View the final result
writexl::write_xlsx(
  final_merged_summary_B,
  here::here("output", "final_merged_summary_B.xlsx")
)

# Step 6: Summarize data for each caterpillar species (regardless of parasitoid)
summary_caterpillar_B <-
  final_merged_summary_B %>%
  dplyr::group_by(CAT_sp) %>%
  dplyr::summarise(
    .groups = "drop",
    localitites_orig = mean(total_localities, na.rm = TRUE),
    parasitised_orig = mean (cat_parasitised, na.rm = TRUE),
    enemy_free_orig = mean (detected_no_par_localities, na.rm = TRUE),
    localities_sub = mean(mean_total_sub_localities, na.rm = TRUE),
    parasitised_sub = mean(mean_sub_cat_parasitised, na.rm = TRUE),
    enemy_free_sub = mean(mean_sub_detected_no_par_localitites, na.rm = TRUE)
  )

# Reshape the data into long format for easier plotting
summary_long_B <-
  summary_caterpillar_B %>%
  tidyr::pivot_longer(
    cols = c(localitites_orig,  parasitised_orig, enemy_free_orig,localities_sub,parasitised_sub,enemy_free_sub),
    names_to = "group",
    values_to = "mean_localities"
  )
summary_long_B$group <- factor(summary_long_B$group, levels = c("localitites_orig","localities_sub", "parasitised_orig","parasitised_sub", "enemy_free_orig","enemy_free_sub"))


# Create the boxplot with y-axis limits set to 0-8
### Plot
p1_B <-
  ggplot(summary_long_B, aes(x = group, y = mean_localities, fill = group)) +
  geom_violin() +
  geom_boxplot(width = 0.2, fill = "white") +
  geom_jitter() +
  scale_fill_manual(values = c( "#E69F00",  "#009E73","#D55E00","#56B4E9", "#999999", "#CC79A7" )) + # Colorblind-friendly colors
  labs(
    x = "Group",
    y = "Mean Number of Localities",
    title = "Enemy free space for caterpillars only with parasitoids"
  ) +
  coord_cartesian(ylim = c(0, 9)) + # Set y-axis limits
  scale_y_continuous(
    breaks = seq(1, 9, 1)
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot2::ggsave(
  plot = p1_B,
  filename = here::here("output", "mean_localities_boxplot_replace_TRUE_B.png"),
  bg = "white"
)
```




# replacement FALSE
```{r Subsampling A False }
# Step 2: Repeat subsampling 99 times and store results
set.seed(1234)
list_subsampled <-
  purrr::map(
    .progress = TRUE,
    .x = 1:999,
    .f = ~ subsample_data(data_work, data_locality_summary, replace = FALSE)
  )

subsampled_results <-
  purrr::map(
    .progress = TRUE,
    .x = list_subsampled,
    .f = ~ group_subsample_data(.x)
  ) %>%
  dplyr::bind_rows(.id = "iteration")

# Step 4: Calculate the mean interactions across all 99 iterations
mean_subsampled_interactions <-
  subsampled_results %>%
  dplyr::group_by(CAT_sp, PAR_sp) %>%     ###################### BEZ PAR!
  dplyr::summarise(
    .groups = "drop",
    mean_total_sub_localities = mean(total_localities, na.rm = TRUE), # Mean number of localities
    mean_sub_cat_parasitised = mean(cat_parasitised, na.rm = TRUE),
    mean_sub_without_origin = mean (cat_available_without_par, na.rm = TRUE),
    mean_sub_detected_no_par_localitites = mean(detected_no_par_localities, na.rm= TRUE)
  )

# Step 5: Add the mean interactions to the original table
final_merged_summary <-
  final_summary %>%
  dplyr::full_join(
    mean_subsampled_interactions,
    by = c("CAT_sp","PAR_sp")
  )
# View the final result
#print(final_merged_summary)

writexl::write_xlsx(
  final_merged_summary,
  here::here("output", "final_merged_summary_replace_TRUE.xlsx")
)
# Step 6: Summarize data for each caterpillar species (regardless of parasitoid)
summary_caterpillar <-
  final_merged_summary %>%
  dplyr::group_by(CAT_sp) %>%
  dplyr::summarise(
    .groups = "drop",
    total_mean_localities_original = mean(total_localities, na.rm = TRUE),
    total_cat_parasitised_original = mean (cat_parasitised, na.rm = TRUE),
    total_detected_not_par_original = mean (detected_no_par_localities, na.rm = TRUE),
    total_sub_localities = mean(mean_total_sub_localities, na.rm = TRUE),
    total_sub_cat_parasitised = mean(mean_sub_cat_parasitised, na.rm = TRUE),
    total_sub_detected_not_par = mean(mean_sub_detected_no_par_localitites, na.rm = TRUE)
  )


# View the summary for each caterpillar species
#View(summary_caterpillar)

# Reshape the data into long format for easier plotting
summary_long <-
  summary_caterpillar %>%
  tidyr::pivot_longer(
    cols = c(total_mean_localities_original,  total_cat_parasitised_original, total_detected_not_par_original,total_sub_localities,total_sub_cat_parasitised,total_sub_detected_not_par),
    names_to = "group",
    values_to = "mean_localities"
  )

# Create the boxplot with y-axis limits set to 0-8
p1_FALSE <-
  ggplot(summary_long, aes(x = group, y = mean_localities, fill = group)) +
  geom_violin() +
  geom_boxplot(width = 0.2, fill = "white") +
  geom_jitter() +
  scale_fill_manual(values = c( "#E69F00",  "#009E73","#D55E00","#56B4E9", "#999999", "#CC79A7" )) + # Colorblind-friendly colors
  labs(
    x = "Group",
    y = "Mean Number of Localities",
    title = "Enemy free space for caterpillars"
  ) +
  coord_cartesian(ylim = c(0, 9)) + # Set y-axis limits
  scale_y_continuous(
    breaks = seq(1, 9, 1)
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot2::ggsave(
  plot = p1_FALSE,
  filename = here::here("output", "mean_localities_boxplot_replace_FALSE.png"),
  bg = "white"
)
```