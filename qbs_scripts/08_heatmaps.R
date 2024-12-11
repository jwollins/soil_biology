
# Load necessary libraries
library(readxl)
library(ggplot2)
library(reshape2)

# Load the data
data <- all_dat_count

# Select species data from column 18 to the last column
species_data <- data[, 18:ncol(data)]

# Calculate the proportion of zeros for each column
zero_proportion <- colSums(species_data == 0) / nrow(species_data)

# Set a threshold, e.g., less than 90% zeros
threshold <- 0.75

# Filter out columns (species) that have 90% or more zeros
filtered_species_data <- species_data[, zero_proportion < threshold]

# Optionally, add relevant identifiers for rows, e.g., Year and Treatment
filtered_species_data$Year <- data$Year
filtered_species_data$Treatment <- data$Treatment

# Melt the data for use in ggplot
melted_data <- melt(filtered_species_data, id.vars = c("Year", "Treatment"))

# Create the heatmap with a continuous color scale
ggplot(melted_data, aes(x = variable, y = Year)) +
  geom_tile(aes(fill = value), color = "white") +
  scale_fill_viridis_c(option = "rocket", na.value = "grey90", direction = -1) +
  labs(x = "Species", y = "Year", fill = "Abundance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~ Treatment, scales = "free_y")

dir.create(path = "Plots/heatmap/")

ggsave(filename = "abundance_heatmap.png", 
       path = "Plots/heatmap/", 
       width = 5, 
       height = 5)
