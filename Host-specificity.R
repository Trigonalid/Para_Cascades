# Assuming your dataset is called 'data'
library(dplyr)

data <- data_master <-
  readr::read_delim(
    here::here(
      "DATA/MASTER.csv"
    )
  ) %>%
  tibble::as_tibble()

##### Host-specificity of parasitoids

# Step 1: Filter parasitoid species with more than 5 records
filtered_data <- data %>%
  group_by(PAR_sp) %>%
  filter(`guild` == "PAR") %>%
  filter(n() > 5) %>%
  ungroup()

# Step 2: Count unique caterpillar (host) species per parasitoid species
hosts_per_parasitoid <- filtered_data %>%
  group_by(PAR_sp) %>%
  summarise(num_hosts = n_distinct(CAT_sp))

# Step 3: Calculate the mean and standard deviation of host species per parasitoid species
mean_sd_hosts_per_parasitoid <- hosts_per_parasitoid %>%
  summarise(mean_num_hosts = mean(num_hosts),
            sd_num_hosts = sd(num_hosts))

# View results
mean_sd_hosts_per_parasitoid


##### Host-specificity of Caterpillars

# Step 1: Filter parasitoid species with more than 5 records
filtered_data_CAT <- data %>%
  group_by(CAT_sp) %>%
  filter(n() > 5) %>%
  ungroup()

# Step 2: Count unique caterpillar (host) species per parasitoid species
hosts_per_caterpillar <- filtered_data_CAT %>%
  group_by(CAT_sp) %>%
  summarise(num_hosts = n_distinct(PLANT_sp))

# Step 3: Calculate the mean and standard deviation of host species per parasitoid species
mean_sd_hosts_per_caterpillars <- hosts_per_caterpillar %>%
  summarise(mean_num_hosts = mean(num_hosts),
            sd_num_hosts = sd(num_hosts))

# View results
mean_sd_hosts_per_caterpillars



#############################    Host-specificity over locality for parasitoids


library(dplyr)
library(readr)
library(here)

# Load the data


##### Host-specificity of parasitoids

# Step 1: Filter parasitoid species with more than 5 records
filtered_data <- data %>%
  filter(`guild` == "PAR") %>%
  select(locality, CAT_sp, PLANT_sp, PAR_sp) %>%
  group_by(PAR_sp) %>%
  filter(n() > 5) %>%
  ungroup()

# Step 2: Count unique caterpillar (host) species per locality
hosts_per_locality <- filtered_data %>%
  group_by(locality, PAR_sp) %>%  # Group by both locality and parasitoid species
  summarise(num_hosts = n_distinct(CAT_sp), .groups = "drop")


# Step 3: Calculate the mean number of hosts per locality
mean_hosts_per_locality <- hosts_per_locality %>%
  group_by(locality) %>%  # Group by locality
  summarise(mean_num_hosts = mean(num_hosts),  # Calculate mean of num_hosts
            sd_num_hosts = sd(num_hosts),      # Calculate standard deviation of num_hosts
            .groups = "drop")

# View results
mean_hosts_per_locality


# Now calculate the overall mean and standard deviation for mean_num_hosts and sd_num_hosts
overall_mean_num_hosts <- mean(mean_hosts_per_locality$mean_num_hosts, na.rm = TRUE)
overall_sd_num_hosts <- mean(mean_hosts_per_locality$sd_num_hosts, na.rm = TRUE)

overall_sd_mean_hosts <- sd(mean_hosts_per_locality$mean_num_hosts, na.rm = TRUE)
overall_sd_sd_hosts <- sd(mean_hosts_per_locality$sd_num_hosts, na.rm = TRUE)

# Create a data frame to hold the overall summary
overall_summary_PAR <- data.frame(
  overall_mean_num_hosts = overall_mean_num_hosts,
  overall_sd_num_hosts = overall_sd_num_hosts
)

# View overall summary
overall_summary_PAR





#############################    Host-specificity over locality for caterpillars
##### Host-specificity of caterpillars 

# Step 1: Filter parasitoid species with more than 5 records
filtered_data <- data %>%
  select(locality, CAT_sp, PLANT_sp, PAR_sp) %>%
  group_by(PAR_sp) %>%
  filter(n() > 5) %>%
  ungroup()


# Step 2: Count unique caterpillar (host) species per locality and parasitoid species
hosts_per_locality <- filtered_data %>%
  group_by(locality, CAT_sp) %>%  # Group by both locality and parasitoid species
  summarise(num_hosts = n_distinct(PLANT_sp), .groups = "drop")

# Step 3: Calculate the mean number of hosts per locality
mean_hosts_per_locality <- hosts_per_locality %>%
  group_by(locality) %>%  # Group by locality
  summarise(mean_num_hosts = mean(num_hosts),  # Calculate mean of num_hosts
            sd_num_hosts = sd(num_hosts),      # Calculate standard deviation of num_hosts
            .groups = "drop")

# View results
mean_hosts_per_locality


# Now calculate the overall mean and standard deviation for mean_num_hosts and sd_num_hosts
overall_mean_num_hosts <- mean(mean_hosts_per_locality$mean_num_hosts, na.rm = TRUE)
overall_sd_num_hosts <- mean(mean_hosts_per_locality$sd_num_hosts, na.rm = TRUE)

overall_sd_mean_hosts <- sd(mean_hosts_per_locality$mean_num_hosts, na.rm = TRUE)
overall_sd_sd_hosts <- sd(mean_hosts_per_locality$sd_num_hosts, na.rm = TRUE)

# Create a data frame to hold the overall summary
overall_summary_CAT <- data.frame(
  overall_mean_num_hosts = overall_mean_num_hosts,
  overall_sd_num_hosts = overall_sd_num_hosts
)

# View overall summary
overall_summary_CAT
