# Plot Pareto optimal scenarios in R 

library(ggplot2)
library(RColorBrewer)
library(gridExtra)

CMAP <- "Set1"

# Load the data
set1 <- read.csv("data/optimization_results/set1.csv", sep = " ")
set2 <- read.csv("data/optimization_results/set2.csv", sep = " ")
set3 <- read.csv("data/optimization_results/set3.csv", sep = " ")
set4 <- read.csv("data/optimization_results/set4.csv", sep = " ")

# Filter outliers
set1 <- set1[set1$V1 > 0, ]
set2 <- set2[set2$V1 > 0, ]
set3 <- set3[set3$V1 > 0, ]
set4 <- set4[set4$V1 > 0, ]

set1 <- set1[set1$V2 > 9.6, ]
set2 <- set2[set2$V2 > 9.6, ]
set3 <- set3[set3$V2 > 9.6, ]
set4 <- set4[set4$V2 > 9.6, ]

# Add scenario labels
set1$Scenario <- "Private, Passive garden"
set2$Scenario <- "Private, STEM garden"
set3$Scenario <- "Public, Passive garden"
set4$Scenario <- "Public, STEM garden"

# Combine all datasets
data <- rbind(set1, set2, set3, set4)
data$Scenario <- factor(data$Scenario)

# Rename columns for clarity
colnames(data)[1:3] <- c("Economic", "Biodiversity", "Health")

# Define color palette and markers
colors <- c("Private, Passive garden" = "blue", 
            "Private, STEM garden" = "orange", 
            "Public, Passive garden" = "green", 
            "Public, STEM garden" = "red")

# Create pairwise scatterplots for Pareto fronts
plot_pareto <- function(x, y, data, colors) {
  ggplot(data, aes_string(x = x, y = y, color = "Scenario")) +
    geom_point(alpha = 0.7, size = 3) +
  scale_color_brewer(palette = CMAP) +
    theme_minimal(base_size = 14) +
    theme_gray() +
    theme(legend.position = "none") +
    labs(x = x, y = y)
}

# Generate individual plots for each pair of objectives
plot_econ_bio <- plot_pareto("Economic", "Biodiversity", data, colors) +
  labs(x = "Economic (million VND)", y= "Biodiversity (million VND)") 
plot_econ_health <- plot_pareto("Economic", "Health", data, colors)+
  labs(x = "Economic (million VND)", y= "Health (million VND)") 
plot_bio_health <- plot_pareto("Biodiversity", "Health", data, colors)+
  labs(x = "Biodiversity (million VND)", y= "Health (million VND)") 

# Extract legend from one plot 
# Create a base plot to extract the legend
base_plot <- ggplot(data, aes(x = Economic, y = Biodiversity, color = Scenario)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_brewer(palette = CMAP) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")

# Extract the legend
legend <- cowplot::get_legend(base_plot)

# Arrange plots and legend
final_plot <- cowplot::plot_grid(
  plot_econ_bio + theme(axis.text.x = element_blank(),
                        axis.ticks.x = element_blank(),
                        axis.title.x = element_blank() ), 
  legend,
  plot_econ_health, 
  plot_bio_health + theme(axis.text.y = element_blank(),
                          axis.ticks.y = element_blank(),
                          axis.title.y = element_blank() ), 
  ncol = 2, 
  nrow = 2, 
  rel_widths = c(1, 1), 
  rel_heights = c(1, 1)
)

#final_plot
#ggsave(
#  "figures/pareto_new.png",
#  final_plot,
#  width = 10,
#  height = 7,
#)
