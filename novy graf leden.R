par_fig_table_BC <- par_fig_table_BC %>% 
  add_column(indexy = "Bray-Curtis")
Cat_fig_table_BC <- Cat_fig_table_BC %>% 
  add_column(indexy = "Bray-Curtis")
par_fig_table_CS <- par_fig_table_CS %>% 
  add_column(indexy = "Chao-Sorensen")
Cat_fig_table_CS <- Cat_fig_table_CS %>% 
  add_column(indexy = "Chao-Sorensen")

par_fig_table_sor <- par_fig_table_sor %>% 
  add_column(indexy = "Sorensen")
cat_fig_table_sor <- cat_fig_table_sor %>% 
  add_column(indexy = "Sorensen")
Sorensen_fig_subsampled <- Sorensen_fig_subsampled %>% 
  add_column(indexy = "Sorensen")

CS_subsampled_figure_cat <- CS_subsampled_figure_cat %>% 
  add_column(indexy = "Chao-Sorensen")
BC_melt_subsampling_fig <- BC_melt_subsampling_fig %>% 
  add_column(indexy = "Bray-Curtis")


combined_table <- bind_rows(
  par_fig_table_BC,
  Cat_fig_table_BC,
  par_fig_table_CS,
  Cat_fig_table_CS,
  par_fig_table_sor,
  cat_fig_table_sor,
  Sorensen_fig_subsampled,
  CS_subsampled_figure_cat,
  BC_melt_subsampling_fig
)

ind_BC <- combined_table %>% 
  dplyr::filter(indexy == "Bray-Curtis")

ind_CS <- combined_table %>% 
  dplyr::filter(indexy == "Chao-Sorensen")

ind_Sor <- combined_table %>% 
  dplyr::filter(indexy == "Sorensen")

# Bray-Curtis
BC_FIG_1 <-ggplot(ind_BC, aes(x = distance, y = hodnota, 
                                color = guild, shape = guild, linetype = guild)) +
  geom_point(size = 3) +  # Adjust size as needed
  geom_smooth(method = "lm", se = FALSE, aes(group = guild), formula = y ~ x, fullrange = TRUE) +
  scale_color_manual(values = c("Parasitoids" = "darkgreen", 
                                "Caterpillars" = "darkorange", 
                                "Caterpillars_subsampled" = "#ffb681")) +
  scale_shape_manual(values = c("Parasitoids" = 17,  # Triangle
                                "Caterpillars" = 16,  # Full Circle
                                "Caterpillars_subsampled" = 1)) +  # Empty Circle
  scale_linetype_manual(values = c("Parasitoids" = "solid", 
                                   "Caterpillars" = "solid", 
                                   "Caterpillars_subsampled" = "dashed")) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_minimal() +
  labs(title = "Bray-Curtis",
       x = "",
       y = "",
       color = "Guild",   # Change legend title for color
       shape = "Guild",   # Change legend title for shape
       linetype = "Guild")  +
  theme_classic(base_size = 20)  ;BC_FIG_1 

CS_FIG_1 <-ggplot(ind_CS, aes(x = distance, y = hodnota, 
                                color = guild, shape = guild, linetype = guild)) +
  geom_point(size = 3) +  # Adjust size as needed
  geom_smooth(method = "lm", se = FALSE, aes(group = guild), formula = y ~ x, fullrange = TRUE) +
  scale_color_manual(values = c("Parasitoids" = "darkgreen", 
                                "Caterpillars" = "darkorange", 
                                "Caterpillars_subsampled" = "#ffb681")) +
  scale_shape_manual(values = c("Parasitoids" = 17,  # Triangle
                                "Caterpillars" = 16,  # Full Circle
                                "Caterpillars_subsampled" = 1)) +  # Empty Circle
  scale_linetype_manual(values = c("Parasitoids" = "solid", 
                                   "Caterpillars" = "solid", 
                                   "Caterpillars_subsampled" = "dashed")) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_minimal() +
  labs(title = "Chao-Sorensen",
       x = "",
       y = "",
       color = "Guild",   # Change legend title for color
       shape = "Guild",   # Change legend title for shape
       linetype = "Guild") +
  theme_classic(base_size = 20)  ;CS_FIG_1  


SOR_FIG_1 <-ggplot(ind_Sor, aes(x = distance, y = hodnota, 
                                  color = guild, shape = guild, linetype = guild)) +
  geom_point(size = 3) +  # Adjust size as needed
  geom_smooth(method = "lm", se = FALSE, aes(group = guild), formula = y ~ x, fullrange = TRUE) +
  scale_color_manual(values = c("Parasitoids" = "darkgreen", 
                                "Caterpillars" = "darkorange", 
                                "Caterpillars-subsampled" = "#ffb681")) +
  scale_shape_manual(values = c("Parasitoids" = 17,  # Triangle
                                "Caterpillars" = 16,  # Full Circle
                                "Caterpillars-subsampled" = 1)) +  # Empty Circle
  scale_linetype_manual(values = c("Parasitoids" = "solid", 
                                   "Caterpillars" = "solid", 
                                   "Caterpillars-subsampled" = "dashed")) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_minimal() +
  labs(title = "Sorensen",
       x = "",
       y = "β diversity",
       color = "Guild",   # Change legend title for color
       shape = "Guild",   # Change legend title for shape
       linetype = "Guild") +  # Change legend title for linetype
  theme_classic(base_size = 20) ; SOR_FIG_1


Fig_1_final_new <-ggarrange(SOR_FIG_1 , CS_FIG_1 , BC_FIG_1, common.legend = TRUE, legend = "bottom", nrow = 1)
Fig_1_final_new
ggsave("output/fig/Fig_1_final_new.tiff",
       Fig_1_final_new,
       height = 14,
       width = 29,
       units = "cm")   # save the plot as .tiff

