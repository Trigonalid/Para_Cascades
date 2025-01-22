library(dplyr)

data <- data.frame(
  locality = c('Yapsiei', 'Yapsiei', 'Yapsiei', 'Yapsiei', 'Wanang', 'Wanang', 'Ohu2', 'Yapsiei', 'Ohu1', 'Ohu2',
               'Ohu1', 'Ohu1', 'Ohu2', 'Wanang', 'Wanang', 'Wanang', 'Elem', 'Yapsiei', 'Yapsiei', 'Yapsiei',
               'Yapsiei', 'Yapsiei', 'Yapsiei', 'Wanang', 'Ohu1', 'Wamangu', 'Wamangu', 'Wamangu', 'Wamangu', 'Morox'),
  plant = c('PYB', 'PYB', 'PYR', 'PYR', 'MAF', 'SYW', 'SRB', 'COP', 'COP', 'COP',
            'COP', 'COP', 'PHA', 'COP', 'COP', 'COP', 'COP', 'COP', 'COP', 'COP',
            'COP', 'COP', 'COP', 'COP', 'COP', 'COP', 'COP', 'COP', 'COP', 'COP'),
  CAT_sp = c('Meekiaria purpurea', 'Meekiaria purpurea', 'Nothomastix klossi', 'Meekiaria sp. BOLD:AAA3374', 'Mellea ordinaria complex', 'Loboschiza sp. AAI1072', 'Dudua n. sp. nr. aprobola', 'Choreutis lutescens', 'Choreutis lutescens', 'Choreutis lutescens',
             'Choreutis lutescens', 'Talanga excelsalis', 'Choreutis cf. anthorma', 'Choreutis lutescens', 'Choreutis lutescens', 'Choreutis lutescens', 'Choreutis lutescens', 'Choreutis lutescens', 'Choreutis lutescens', 'Choreutis lutescens',
             'Talanga excelsalis', 'Choreutis lutescens', 'Choreutis lutescens', 'Choreutis lutescens', 'Choreutis lutescens', 'Choreutis lutescens', 'Choreutis lutescens', 'Choreutis lutescens', 'Choreutis lutescens', 'Choreutis lutescens'),
  PAR_sp = c('AGAT001', 'AGAT001', 'AGAT001', 'AGAT001', 'AGAT010', NA, NA, NA, NA, NA,
             NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
             'AGAT022', 'AGAT022', 'AGAT022', 'AGAT022', 'AGAT022', 'AGAT022', 'AGAT022', 'AGAT022', 'AGAT022', 'AGAT022')
)

locality_summary <- data %>%
  group_by(locality) %>%
  summarise(
    num_plant_species = n_distinct(plant),
    num_caterpillar_species = n_distinct(CAT_sp),
    num_parasitoid_species = n_distinct(PAR_sp, na.rm = TRUE),
    abundance_plants = n(),
    abundance_caterpillars = n(),
    abundance_parasitoids = sum(!is.na(PAR_sp))
  )

print(locality_summary)

caterpillar_per_plant_locality <- data %>%
  group_by(locality, plant) %>%
  summarise(num_caterpillar_species = n_distinct(CAT_sp))

print(caterpillar_per_plant_locality)

set.seed(123) # For reproducibility

random_pick_data <- function(df) {
  df %>%
    group_by(locality, plant) %>%
    sample_n(size = n())
}

sampled_data <- random_pick_data(data)
print(sampled_data)

write.csv(sampled_data, "sampled_data.csv", row.names = FALSE)

summary_sampled <- sampled_data %>%
  group_by(locality, plant) %>%
  summarise(
    num_caterpillars = n_distinct(CAT_sp),
    num_parasitoids = sum(!is.na(PAR_sp)),
    mean_parasitoids_per_caterpillar = mean(!is.na(PAR_sp))
  )

print(summary_sampled)

results <- replicate(999, {
  sampled_data <- random_pick_data(data)
  sampled_summary <- sampled_data %>%
    group_by(locality, plant) %>%
    summarise(
      num_caterpillars = n_distinct(CAT_sp),
      num_parasitoids = sum(!is.na(PAR_sp)),
      mean_parasitoids_per_caterpillar = mean(!is.na(PAR_sp))
    )
  sampled_summary
}, simplify = FALSE)

results_df <- bind_rows(results)
write.csv(results_df, "results_999_subsamples.csv", row.names = FALSE)

unique_links <- data %>%
  filter(!is.na(PAR_sp)) %>%
  group_by(locality, plant) %>%
  summarise(unique_links = n_distinct(interaction(CAT_sp, PAR_sp)))

print(unique_links)




