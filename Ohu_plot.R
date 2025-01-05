# Graf Ohu dissimilarity


sorensen_values_PAR
sorensen_values_CAT

chao_sorensen_values_PAR
chao_sorensen_values_CAT

bray_curtis_values_PAR
bray_curtis_values_CAT 

# Load ggplot2
library(ggplot2)

# Combine all datasets into a single data frame
dissimilarity_data <- data.frame(
  Guild = c(
    rep("Parasitoids", length(sorensen_values_PAR)),
    rep("Caterpillars", length(sorensen_values_CAT)),
    rep("Parasitoids", length(chao_sorensen_values_PAR)),
    rep("Caterpillars", length(chao_sorensen_values_CAT)),
    rep("Parasitoids", length(bray_curtis_values_PAR)),
    rep("Caterpillars", length(bray_curtis_values_CAT))
  ),
  Dissimilarity = c(
    sorensen_values_PAR,
    sorensen_values_CAT,
    chao_sorensen_values_PAR,
    chao_sorensen_values_CAT,
    bray_curtis_values_PAR,
    bray_curtis_values_CAT
  ),
  Index = c(
    rep("Sorensen", length(sorensen_values_PAR) + length(sorensen_values_CAT)),
    rep("Chao-Sorensen", length(chao_sorensen_values_PAR) + length(chao_sorensen_values_CAT)),
    rep("Bray-Curtis", length(bray_curtis_values_PAR) + length(bray_curtis_values_CAT))
  )
)

# Create the boxplot
ggplot(dissimilarity_data, aes(x = Index, y = Dissimilarity, fill = Guild)) +
  geom_boxplot() +
  labs(
    title = "Dissimilarity of parasitoids and caterpillars at Ohu locality",
    x = "Dissimilarity index",
    y = "Dissimilarity"
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_minimal() +
  scale_fill_manual(values = c("Parasitoids" = "skyblue", "Caterpillars" = "lightgreen")) +
  theme(
    legend.title = element_blank(),
    legend.position = "right"
  )



betaS_values_PC
betaOS_values_PC
betaWN_values_PC


betaS_values_CPL
betaOS_values_CPL
betaWN_values_CPL





# Load necessary libraries
library(ggplot2)
library(dplyr)

# Combine the beta values into one vector with an associated category and source
combined_data <- data.frame(
  beta_value = c(betaS_values_PC, betaOS_values_PC, betaWN_values_PC,
                 betaS_values_CPL, betaOS_values_CPL, betaWN_values_CPL),
  category = c(rep("betaS", length(betaS_values_PC)),
               rep("betaOS", length(betaOS_values_PC)),
               rep("betaWN", length(betaWN_values_PC)),
               rep("betaS", length(betaS_values_CPL)),
               rep("betaOS", length(betaOS_values_CPL)),
               rep("betaWN", length(betaWN_values_CPL))),
  source = c(rep("PC", length(betaS_values_PC)),
             rep("PC", length(betaOS_values_PC)),
             rep("PC", length(betaWN_values_PC)),
             rep("CPL", length(betaS_values_CPL)),
             rep("CPL", length(betaOS_values_CPL)),
             rep("CPL", length(betaWN_values_CPL)))
)

# Print to check the structure of combined_data
print(head(combined_data))

# Plot the boxplot using ggplot2
ggplot(combined_data, aes(x = category, y = beta_value, fill = source)) +
  geom_boxplot() +
  labs(title = "Food-Web Interaction Dissimilarity Boxplot",
       x = "Beta Value Category", y = "Dissimilarity") +
  coord_cartesian(ylim = c(0, 1)) +
  theme_minimal() +
  scale_fill_manual(values = c("PC" = "skyblue", "CPL" = "salmon")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))









#######



# Load necessary libraries
library(ggplot2)
library(dplyr)
library(gridExtra)  # For combining plots

# Combine dissimilarity data
dissimilarity_data <- data.frame(
  Guild = c(
    rep("Parasitoids", length(sorensen_values_PAR)),
    rep("Caterpillars", length(sorensen_values_CAT)),
    rep("Parasitoids", length(chao_sorensen_values_PAR)),
    rep("Caterpillars", length(chao_sorensen_values_CAT)),
    rep("Parasitoids", length(bray_curtis_values_PAR)),
    rep("Caterpillars", length(bray_curtis_values_CAT))
  ),
  Dissimilarity = c(
    sorensen_values_PAR,
    sorensen_values_CAT,
    chao_sorensen_values_PAR,
    chao_sorensen_values_CAT,
    bray_curtis_values_PAR,
    bray_curtis_values_CAT
  ),
  Index = c(
    rep("Sorensen", length(sorensen_values_PAR) + length(sorensen_values_CAT)),
    rep("Chao-Sorensen", length(chao_sorensen_values_PAR) + length(chao_sorensen_values_CAT)),
    rep("Bray-Curtis", length(bray_curtis_values_PAR) + length(bray_curtis_values_CAT))
  )
)

# Create the first plot (Dissimilarity of Parasitoids and Caterpillars)
plot1 <- ggplot(dissimilarity_data, aes(x = Index, y = Dissimilarity, fill = Guild)) +
  geom_boxplot() +
  labs(
    title = "Dissimilarity of parasitoids and caterpillars\nat Ohu locality",
    x = "Dissimilarity index",
    y = "Dissimilarity"
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_manual(name = "Community",
                    values = c("Parasitoids" = "darkgreen", "Caterpillars" = "darkorange")) +
  theme_classic(base_size = 16) +
  theme(
    legend.position = "bottom"
  )

# Combine the beta values into one vector with an associated category and source
combined_data <- data.frame(
  beta_value = c(betaS_values_PC, betaOS_values_PC, betaWN_values_PC,
                 betaS_values_CPL, betaOS_values_CPL, betaWN_values_CPL),
  category = c(rep("S", length(betaS_values_PC)),
               rep("OS", length(betaOS_values_PC)),
               rep("WN", length(betaWN_values_PC)),
               rep("S", length(betaS_values_CPL)),
               rep("OS", length(betaOS_values_CPL)),
               rep("WN", length(betaWN_values_CPL))),
  source = c(rep("PC", length(betaS_values_PC)),
             rep("PC", length(betaOS_values_PC)),
             rep("PC", length(betaWN_values_PC)),
             rep("CPL", length(betaS_values_CPL)),
             rep("CPL", length(betaOS_values_CPL)),
             rep("CPL", length(betaWN_values_CPL)))
)

# Ensure the 'source' column is a factor with the desired order
combined_data$source <- factor(combined_data$source, levels = c("PC", "CPL"))

# Create the second plot (Food-Web Interaction Dissimilarity)
plot2 <- ggplot(combined_data, aes(x = category, y = beta_value, fill = source)) +
  geom_boxplot() +
  labs(title = "Food-Web interaction dissimilarity\nat Ohu locality",
       x = "Components", y = "") +
  coord_cartesian(ylim = c(0, 1)) +
  theme_minimal() +
  scale_fill_manual(name = "Food-Web", 
                    values = c("PC" = "#1b9e77", "CPL" = "#d95f02"),
                    labels = c("PC" = "Parasitoid-Caterpillar", 
                               "CPL" = "Caterpillar-Plant")) +
  theme_classic(base_size = 16) +
  theme(
    legend.position = "bottom"
  )

# Combine both plots into one grid
grid.arrange(plot1, plot2, ncol = 2)



library(cowplot)

# Add labels to the plots
plot1_labeled <- ggdraw() +
  draw_plot(plot1) +
  draw_label("A", x = 0.03, y = 0.97, hjust = 0, vjust = 1, size = 15)

plot2_labeled <- ggdraw() +
  draw_plot(plot2) +
  draw_label("B", x = 0.03, y = 0.97, hjust = 0, vjust = 1, size = 15)

# Combine both plots into one grid
Fig_S2_final <- plot_grid(plot1_labeled, plot2_labeled, ncol = 2, align = "v")
print(Fig_S2_final)


Fig_S2_final
ggsave("output/fig/Fig_S2_final.tiff",
       Fig_S2_final,
       width = 11.7,      # Single column width in inches
       height = 6,       # Adjust as needed
       dpi = 300)   # save the plot as .tiff


