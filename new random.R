# Load necessary libraries
library(ggplot2)
library(dplyr)

# Calculate distributions of incidences
# 1. Parasitoids
parasitoid_incidence <- data_testing %>%
  filter(guild == "PAR") %>%
  group_by(PAR_sp) %>%
  summarise(N = n(), .groups = "drop")

# 2. Caterpillars
caterpillar_incidence <- data_testing %>%
  filter(guild == "CAT") %>%
  group_by(CAT_sp) %>%
  summarise(N = n(), .groups = "drop")

# 3. Subsampled Caterpillars (example using one iteration of resampled data)
# You can replace `cat_randomly_resampled` with any iteration of your subsampled data
subsampled_caterpillar_incidence <- cat_randomly_resampled %>%
  group_by(CAT_sp) %>%
  summarise(N = n(), .groups = "drop")

# Plotting the distributions
# Plot a) Parasitoids
plot_parasitoids <- ggplot(parasitoid_incidence, aes(x = N)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Parasitoid Incidences",
       x = "Incidence (N)",
       y = "Frequency") +
  theme_minimal()

# Plot b) Caterpillars
plot_caterpillars <- ggplot(caterpillar_incidence, aes(x = N)) +
  geom_histogram(binwidth = 1, fill = "forestgreen", color = "black") +
  labs(title = "Distribution of Caterpillar Incidences",
       x = "Incidence (N)",
       y = "Frequency") +
  theme_minimal()

# Plot c) Subsampled Caterpillars
plot_subsampled <- ggplot(subsampled_caterpillar_incidence, aes(x = N)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "black") +
  labs(title = "Distribution of Subsampled Caterpillar Incidences",
       x = "Incidence (N)",
       y = "Frequency") +
  theme_minimal()

# Arrange plots together
library(gridExtra)
grid.arrange(plot_parasitoids, plot_caterpillars, plot_subsampled, ncol = 1)




# Plot a) Parasitoids
plot_parasitoid_localities <- ggplot(parasitoid_locality_incidence, aes(x = n_localities)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  scale_x_continuous(breaks = seq(1, max(parasitoid_locality_incidence$n_localities), by = 1)) +
  labs(title = "Locality Incidence for Parasitoids",
       x = "Number of Localities",
       y = "Number of Species") +
  theme_minimal()

# Plot b) Caterpillars
plot_caterpillar_localities <- ggplot(caterpillar_locality_incidence, aes(x = n_localities)) +
  geom_histogram(binwidth = 1, fill = "forestgreen", color = "black") +
  scale_x_continuous(breaks = seq(1, max(caterpillar_locality_incidence$n_localities), by = 1)) +
  labs(title = "Locality Incidence for Caterpillars",
       x = "Number of Localities",
       y = "Number of Species") +
  theme_minimal()

# Plot c) Subsampled Caterpillars
plot_subsampled_localities <- ggplot(subsampled_locality_incidence, aes(x = n_localities)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "black") +
  scale_x_continuous(breaks = seq(1, max(subsampled_locality_incidence$n_localities), by = 1)) +
  labs(title = "Locality Incidence for Subsampled Caterpillars",
       x = "Number of Localities",
       y = "Number of Species") +
  theme_minimal()

# Arrange the plots together
library(gridExtra)
grid.arrange(plot_parasitoid_localities, plot_caterpillar_localities, plot_subsampled_localities, ncol = 1)
