# Load required libraries
library(tidyverse)
library(readxl)
library(here)
library(cheddar)

# Step 1: Read and filter your data
df <- read_excel(here("DATA/MASTER.xlsx")) %>%
  as_tibble() %>%
  filter(locality == "Elem", Par_remove == "0") %>%
  select(locality, PLANT_sp, CAT_sp, PAR_sp)




# Step 2: Create pairwise trophic links
link1 <- df %>%
  transmute(consumer = CAT_sp, resource = PLANT_sp)

link2 <- df %>%
  transmute(consumer = PAR_sp, resource = CAT_sp)

trophic_links <- bind_rows(link1, link2) %>%
  distinct()

# Step 3: Prepare nodes with cheddar-compatible 'category' field
all_species <- unique(c(trophic_links$consumer, trophic_links$resource))

nodes <- tibble(
  node = all_species,
  category = if_else(node %in% df$PLANT_sp, "producer", "consumer")
)

# Step 4: Create the cheddar community object
my_community <- Community(
  nodes = nodes,
  trophic.links = trophic_links,
  properties = list(title = "My Tritrophic Web")
)

# Step 5: Plot the food web
PlotWebByLevel(my_community)

# Step 6: (Optional) Trophic levels
TLs <- TrophicLevels(my_community)
head(TLs)


