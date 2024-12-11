## HEADER ####
## WHO: JOE COLLINS
## WHAT: Principal component analysis - QBS
## LAST EDIT: 25/01/2023
####



## 01 ALL YEARS - SCALE - LOOP ####

# Loop through each year
for (i in seq_along(years)) {
  year <- years[i]
  
  # Filter the dataset for the current year
  dat <- filter(.data = all_dat_count, Year == year)
  
  # Columns of interest
  qbs.soil <- dat[, c(18:ncol(dat))]
  
  # Remove columns with zero variance
  qbs.soil <- qbs.soil[, apply(qbs.soil, 2, var) != 0]
  
  # Run PCA
  count.pca <- prcomp(qbs.soil, center = TRUE, scale. = TRUE)
  
  # Save the PCA rotation matrix
  count.pca$rotation <- -1 * count.pca$rotation
  write.csv(x = count.pca$rotation, file = paste0("Statistics/PCA/pca_tax_order", year - 2021, ".principal.components.csv"))
  
  ### Scree Plot ###
  scree_plot <- fviz_eig(count.pca, addlabels = TRUE, ylim = c(0, 50), main = paste("Scree plot Year", year))
  
  # Create directory for scree plots
  dir.create("Plots/PCA/scree.plots/no_scale/", showWarnings = FALSE, recursive = TRUE)
  
  # Save scree plot
  ggsave(filename = paste0("Plots/PCA/scree.plots/no_scale/", year - 2021, ".scree.plot.png"), plot = scree_plot, width = 8, height = 6, dpi = 150)
  
  ### PCA's 1-2 Plot ###
  mean.pca <- aggregate(count.pca$x[,1:2], by = list(dat$Treatment), FUN = mean)
  sd.pca <- aggregate(count.pca$x[,1:2], by = list(dat$Treatment), FUN = sd)
  
  # PCA plot
  pca.plot <- ggbiplot(pcobj = count.pca, 
                       ellipse = TRUE, ellipse.fill = FALSE,
                       var.axes = FALSE, 
                       scale = 1,
                       groups = dat$Treatment, 
                       choices = c(1, 2),
                       varname.abbrev = TRUE,
                       varname.size = element_blank(), 
                       var.scale = 1, 
                       point.size = 2, 
                       labels.size = 4, 
                       varname.adjust = 1, 
                       repel.force = 10, 
                       pull.force = 1, 
                       text.padding = 10,
                       arrow.color = "black", 
                       linetype = "solid", 
                       alpha_arrow = 1, 
                       text.color = "black") + 
    theme_classic(base_size = 18) + 
    xlim(-4,4) +
    ylim(-4,4) +
    geom_point(aes(x = mean.pca[1,2], y = mean.pca[1,3]), colour = "black", fill = "turquoise3", size = 2, stroke = 1, pch = 21) + 
    geom_point(aes(x = mean.pca[2,2], y = mean.pca[2,3]), colour = "black", fill = "tomato2", size = 2, stroke = 1, pch = 21) +
    scale_color_manual(name = "Treatment", values = c("turquoise3", "tomato2")) +
    ggtitle(year)
  
  # Adding error bars to PCA plot
  pca.plot <- pca.plot + 
    geom_segment(aes(x = mean.pca[1,2] - sd.pca[1,2], 
                     y = mean.pca[1,3], 
                     xend = mean.pca[1,2] + sd.pca[1,2], 
                     yend = mean.pca[1,3]), 
                 color = "red", linetype = "dashed", linewidth = 0.25) +
    geom_segment(aes(x = mean.pca[1,2], 
                     y = mean.pca[1,3] - sd.pca[1,2], 
                     xend = mean.pca[1,2], 
                     yend = mean.pca[1,3] + sd.pca[1,2]), 
                 color = "red", linetype = "dashed", linewidth = 0.25) +
    geom_segment(aes(x = mean.pca[2,2] - sd.pca[2,2], 
                     y = mean.pca[2,3],
                     xend = mean.pca[2,2] + sd.pca[2,2], 
                     yend = mean.pca[2,3]), 
                 color = "red", linetype = "dashed", linewidth = 0.25) +
    geom_segment(aes(x = mean.pca[2,2],
                     y = mean.pca[2,3] - sd.pca[2,2], 
                     xend = mean.pca[2,2],
                     yend = mean.pca[2,3] + sd.pca[2,2]), 
                 color = "red", linetype = "dashed", linewidth = 0.25)
  
  # Store each plot in the list
  pca_plots[[i]] <- pca.plot
  
  # Create directory for PCA plots
  dir.create(paste0("Plots/PCA/no_scale_pca/"), showWarnings = FALSE, recursive = TRUE)
  
  # Save PCA plot
  ggsave(filename = paste0("Plots/PCA/no_scale_pca/", year - 2021, ".PCA.png"), 
         plot = pca.plot, 
         width = 8, 
         height = 6, 
         dpi = 150)
}

# Arrange and save all PCA plots in a single figure
combined_pca_plot <- ggarrange(plotlist = pca_plots, 
                               labels = c("A", "B", "C"), 
                               ncol = 2, 
                               nrow = 2, 
                               common.legend = TRUE, 
                               legend = "bottom")

combined_pca_plot

# Save the combined plot
ggsave(filename = "Plots/PCA/no_scale_pca/combined_pca_plot.png", 
       plot = combined_pca_plot, 
       width = 12, 
       height = 12, 
       dpi = 150)














## 06 ALL YEARS - SCALE ####

# Define the years of interest
years <- c(2022, 2023, 2024)

# Initialize a list to store each PCA plot
pca_plots <- list()


# Loop through each year
for (year in years) {
  
  # Filter the dataset for the current year
  dat <- filter(.data = all_dat_count, Year == year)
  
  # Columns of interest
  qbs.soil <- dat[, c(18:ncol(dat))]
  
  # Remove columns with zero variance
  qbs.soil <- qbs.soil[, apply(qbs.soil, 2, var) != 0]
  
  # Run PCA
  count.pca <- prcomp(qbs.soil, center = TRUE, scale. = TRUE)
  
  # Save the PCA rotation matrix
  count.pca$rotation <- -1 * count.pca$rotation
  write.csv(x = count.pca$rotation, file = paste0("Statistics/PCA/Y", year - 2021, ".principal.components.csv"))
  
  ### Scree Plot ###
  scree_plot <- fviz_eig(count.pca, addlabels = TRUE, ylim = c(0, 50), main = paste("Scree plot Year", year))
  
  # Create directory for scree plots
  dir.create("Plots/PCA/scree.plots", showWarnings = FALSE, recursive = TRUE)
  
  # Save scree plot
  ggsave(filename = paste0("Plots/PCA/scree.plots/Y", year - 2021, ".scree.plot.png"), plot = scree_plot, width = 8, height = 6, dpi = 150)
  
  ### PCA's 1-2 Plot ###
  mean.pca <- aggregate(count.pca$x[,1:2], by = list(dat$Treatment), FUN = mean)
  sd.pca <- aggregate(count.pca$x[,1:2], by = list(dat$Treatment), FUN = sd)
  
  # PCA plot
  pca.plot <- ggbiplot(pcobj = count.pca, 
                       ellipse = FALSE,
                       scale = 1,
                       groups = dat$Treatment, 
                       choices = c(1, 2),
                       varname.abbrev = FALSE,
                       varname.size = 4) + 
    theme_classic(base_size = 18) + 
    xlim(-4,4) +
    ylim(-4,4) +
    geom_point(aes(x = mean.pca[1,2], y = mean.pca[1,3]), colour = "black", fill = "#009E73", size = 5, stroke = 1, pch = 21) + 
    geom_point(aes(x = mean.pca[2,2], y = mean.pca[2,3]), colour = "black", fill = "#E69F00", size = 5, stroke = 1, pch = 21) +
    scale_color_manual(name = "Treatment", values = c("#009E73", "#E69F00")) +
    ggtitle(paste("QBS PCA Plot", year))
  
  # Adding error bars to PCA plot
  pca.plot <- pca.plot + 
    geom_segment(aes(x = mean.pca[1,2]-sd.pca[1,2], y = mean.pca[1,3], xend = mean.pca[1,2]+sd.pca[1,2], yend = mean.pca[1,3]), color = "red", linetype = "dashed", linewidth = 0.25) +
    geom_segment(aes(x = mean.pca[1,2], y = mean.pca[1,3]-sd.pca[1,2], xend = mean.pca[1,2], yend = mean.pca[1,3]+sd.pca[1,2]), color = "red", linetype = "dashed", linewidth = 0.25) +
    geom_segment(aes(x = mean.pca[2,2]-sd.pca[2,2], y = mean.pca[2,3], xend = mean.pca[2,2]+sd.pca[2,2], yend = mean.pca[2,3]), color = "red", linetype = "dashed", linewidth = 0.25) +
    geom_segment(aes(x = mean.pca[2,2], y = mean.pca[2,3]-sd.pca[2,2], xend = mean.pca[2,2], yend = mean.pca[2,3]+sd.pca[2,2]), color = "red", linetype = "dashed", linewidth = 0.25)
  
  
  # Store each plot in the list
  pca_plots[[i]] <- pca.plot
  
  
  # Create directory for PCA plots
  dir.create(paste0("Plots/PCA/scaled_pca"), showWarnings = FALSE, recursive = TRUE)
  
  # Save PCA plot
  ggsave(filename = paste0("Plots/PCA/scaled_pca", "/Y", year - 2021, ".PCA.png"), 
         plot = pca.plot, 
         width = 8, 
         height = 6, 
         dpi = 150)
}


# Arrange and save all PCA plots in a single figure
combined_pca_plot <- ggarrange(plotlist = pca_plots, 
                               labels = paste("Year", years), 
                               ncol = 3, 
                               nrow = 1, 
                               common.legend = TRUE, 
                               legend = "bottom")

combined_pca_plot

# Save the combined plot
ggsave(filename = "Plots/PCA/scaled_pca/Combined_PCA_Plots.png", 
       plot = combined_pca_plot, 
       width = 18, 
       height = 8, 
       dpi = 150)






## 07 SD TAX ORDER PCA'S ####

# Define the years of interest
years <- c(2022, 2023, 2024)

# Initialize a list to store each PCA plot
pca_plots <- list()

# Loop through each year
for (year in years) {
  
  # Filter the dataset for the current year
  dat <- filter(.data = tax_ord_dat, Year == year)
  
  # Columns of interest
  qbs.soil <- dat[, c(18:ncol(dat))]
  
  # Remove columns with zero variance
  qbs.soil <- qbs.soil[, apply(qbs.soil, 2, var) != 0]
  
  # Run PCA
  count.pca <- prcomp(qbs.soil, center = TRUE, scale. = TRUE)
  
  # Save the PCA rotation matrix
  count.pca$rotation <- -1 * count.pca$rotation
  write.csv(x = count.pca$rotation, file = paste0("Statistics/PCA/pca_tax_order", year - 2021, ".principal.components.csv"))
  
  ### Scree Plot ###
  scree_plot <- fviz_eig(count.pca, addlabels = TRUE, ylim = c(0, 50), main = paste("Scree plot Year", year))
  
  # Create directory for scree plots
  dir.create("Plots/PCA/scree.plots/pca_tax_order/", showWarnings = FALSE, recursive = TRUE)
  
  # Save scree plot
  ggsave(filename = paste0("Plots/PCA/scree.plots/pca_tax_order/", year - 2021, ".scree.plot.png"), plot = scree_plot, width = 8, height = 6, dpi = 150)
  
  ### PCA's 1-2 Plot ###
  mean.pca <- aggregate(count.pca$x[,1:2], by = list(dat$Treatment), FUN = mean)
  sd.pca <- aggregate(count.pca$x[,1:2], by = list(dat$Treatment), FUN = sd)
  
  # PCA plot
  pca.plot <- ggbiplot(pcobj = count.pca, 
                       ellipse = FALSE,
                       scale = 0.5,
                       groups = dat$Treatment, 
                       choices = c(1, 2),
                       varname.abbrev = FALSE,
                       varname.size = 4) + 
    theme_classic(base_size = 18) + 
    xlim(-4,4) +
    ylim(-4,4) +
    geom_point(aes(x = mean.pca[1,2], y = mean.pca[1,3]), colour = "black", fill = "turquoise3", size = 2, stroke = 1, pch = 21) + 
    geom_point(aes(x = mean.pca[2,2], y = mean.pca[2,3]), colour = "black", fill = "tomato2", size = 2, stroke = 1, pch = 21) +
    scale_color_manual(name = "Treatment", values = c("turquoise3", "tomato2")) +
    ggtitle(paste("QBS PCA Plot", year))
  
  # Adding error bars to PCA plot
  pca.plot <- pca.plot + 
    geom_segment(aes(x = mean.pca[1,2] - sd.pca[1,2], 
                     y = mean.pca[1,3], 
                     xend = mean.pca[1,2] + sd.pca[1,2], 
                     yend = mean.pca[1,3]), 
                 color = "red", linetype = "dashed", linewidth = 0.25) +
    geom_segment(aes(x = mean.pca[1,2], 
                     y = mean.pca[1,3] - sd.pca[1,2], 
                     xend = mean.pca[1,2], 
                     yend = mean.pca[1,3] + sd.pca[1,2]), 
                 color = "red", linetype = "dashed", linewidth = 0.25) +
    geom_segment(aes(x = mean.pca[2,2] - sd.pca[2,2], 
                     y = mean.pca[2,3],
                     xend = mean.pca[2,2] + sd.pca[2,2], 
                     yend = mean.pca[2,3]), 
                 color = "red", linetype = "dashed", linewidth = 0.25) +
    geom_segment(aes(x = mean.pca[2,2],
                     y = mean.pca[2,3]-sd.pca[2,2], 
                     xend = mean.pca[2,2],
                     yend = mean.pca[2,3]+sd.pca[2,2]), 
                 color = "red", linetype = "dashed", linewidth = 0.25)
  
  # Create directory for PCA plots
  dir.create(paste0("Plots/PCA/pca_tax_order/SD/"), showWarnings = FALSE, recursive = TRUE)
  
  # Save PCA plot
  ggsave(filename = paste0("Plots/PCA/pca_tax_order/SD/", year - 2021, ".PCA.png"), 
         plot = pca.plot, 
         width = 8, 
         height = 6, 
         dpi = 150)
}



## 08 SD COMBINED TAX ORD ####


# Define the years of interest
years <- c(2022, 2023, 2024)

# Initialize a list to store each PCA plot
pca_plots <- list()

# Loop through each year
for (i in seq_along(years)) {
  year <- years[i]
  
  # Filter the dataset for the current year
  dat <- filter(.data = tax_ord_dat, Year == year)
  
  # Columns of interest
  qbs.soil <- dat[, c(18:ncol(dat))]
  
  # Remove columns with zero variance
  qbs.soil <- qbs.soil[, apply(qbs.soil, 2, var) != 0]
  
  # Run PCA
  count.pca <- prcomp(qbs.soil, center = TRUE, scale. = TRUE)
  
  # Save the PCA rotation matrix
  count.pca$rotation <- -1 * count.pca$rotation
  write.csv(x = count.pca$rotation, file = paste0("Statistics/PCA/pca_tax_order", year - 2021, ".principal.components.csv"))
  
  ### Scree Plot ###
  scree_plot <- fviz_eig(count.pca, addlabels = TRUE, ylim = c(0, 50), main = paste("Scree plot Year", year))
  
  # Create directory for scree plots
  dir.create("Plots/PCA/scree.plots/pca_tax_order/", showWarnings = FALSE, recursive = TRUE)
  
  # Save scree plot
  ggsave(filename = paste0("Plots/PCA/scree.plots/pca_tax_order/", year - 2021, ".scree.plot.png"), plot = scree_plot, width = 8, height = 6, dpi = 150)
  
  ### PCA's 1-2 Plot ###
  mean.pca <- aggregate(count.pca$x[,1:2], by = list(dat$Treatment), FUN = mean)
  sd.pca <- aggregate(count.pca$x[,1:2], by = list(dat$Treatment), FUN = sd)
  
  # PCA plot
  pca.plot <- ggbiplot(pcobj = count.pca, 
                       ellipse = FALSE,
                       scale = 1,
                       groups = dat$Treatment, 
                       choices = c(1, 2),
                       varname.abbrev = FALSE,
                       varname.size = 4, 
                       var.scale = 1, 
                       point.size = 2, 
                       labels.size = 2, 
                       varname.adjust = 1, 
                       repel.force = 10, 
                       pull.force = 1, 
                       text.padding = 1,
                       arrow.color = "black", 
                       linetype = "solid", 
                       alpha_arrow = 1, 
                       text.color = "black") + 
    theme_classic(base_size = 18) + 
    xlim(-4,4) +
    ylim(-4,4) +
    geom_point(aes(x = mean.pca[1,2], y = mean.pca[1,3]), colour = "black", fill = "turquoise3", size = 2, stroke = 1, pch = 21) + 
    geom_point(aes(x = mean.pca[2,2], y = mean.pca[2,3]), colour = "black", fill = "tomato2", size = 2, stroke = 1, pch = 21) +
    scale_color_manual(name = "Treatment", values = c("turquoise3", "tomato2")) +
    ggtitle(year)
  
  # Adding error bars to PCA plot
  pca.plot <- pca.plot + 
    geom_segment(aes(x = mean.pca[1,2] - sd.pca[1,2], 
                     y = mean.pca[1,3], 
                     xend = mean.pca[1,2] + sd.pca[1,2], 
                     yend = mean.pca[1,3]), 
                 color = "red", linetype = "dashed", linewidth = 0.25) +
    geom_segment(aes(x = mean.pca[1,2], 
                     y = mean.pca[1,3] - sd.pca[1,2], 
                     xend = mean.pca[1,2], 
                     yend = mean.pca[1,3] + sd.pca[1,2]), 
                 color = "red", linetype = "dashed", linewidth = 0.25) +
    geom_segment(aes(x = mean.pca[2,2] - sd.pca[2,2], 
                     y = mean.pca[2,3],
                     xend = mean.pca[2,2] + sd.pca[2,2], 
                     yend = mean.pca[2,3]), 
                 color = "red", linetype = "dashed", linewidth = 0.25) +
    geom_segment(aes(x = mean.pca[2,2],
                     y = mean.pca[2,3] - sd.pca[2,2], 
                     xend = mean.pca[2,2],
                     yend = mean.pca[2,3] + sd.pca[2,2]), 
                 color = "red", linetype = "dashed", linewidth = 0.25)
  
  # Store each plot in the list
  pca_plots[[i]] <- pca.plot
  
  # Create directory for PCA plots
  dir.create(paste0("Plots/PCA/pca_tax_order/SD/"), showWarnings = FALSE, recursive = TRUE)
  
  # Save PCA plot
  ggsave(filename = paste0("Plots/PCA/pca_tax_order/SD/", year - 2021, ".PCA.png"), 
         plot = pca.plot, 
         width = 8, 
         height = 6, 
         dpi = 150)
}

# Arrange and save all PCA plots in a single figure
combined_pca_plot <- ggarrange(plotlist = pca_plots, 
                               labels = c("A", "B", "C"), 
                               ncol = 2, 
                               nrow = 2, 
                               common.legend = TRUE, 
                               legend = "bottom")

# Save the combined plot
ggsave(filename = "Plots/PCA/pca_tax_order/SD/Combined_PCA_Plots.png", 
       plot = combined_pca_plot, 
       width = 12, 
       height = 12, 
       dpi = 150)








## 08 SE TAX ORDER PCA ####


# Define the years of interest
years <- c(2022, 2023, 2024)

# Loop through each year
for (year in years) {
  
  # Filter the dataset for the current year
  dat <- filter(.data = tax_ord_dat, Year == year)
  
  # Columns of interest
  qbs.soil <- dat[, c(18:ncol(dat))]
  
  # Remove columns with zero variance
  qbs.soil <- qbs.soil[, apply(qbs.soil, 2, var) != 0]
  
  # Run PCA
  count.pca <- prcomp(qbs.soil, center = TRUE, scale. = TRUE)
  
  # Save the PCA rotation matrix
  count.pca$rotation <- -1 * count.pca$rotation
  write.csv(x = count.pca$rotation, file = paste0("Statistics/PCA/pca_tax_order", year - 2021, ".principal.components.csv"))
  
  ### Scree Plot ###
  scree_plot <- fviz_eig(count.pca, addlabels = TRUE, ylim = c(0, 50), main = paste("Scree plot Year", year))
  
  # Create directory for scree plots
  dir.create("Plots/PCA/scree.plots/pca_tax_order/", showWarnings = FALSE, recursive = TRUE)
  
  # Save scree plot
  ggsave(filename = paste0("Plots/PCA/scree.plots/pca_tax_order/", year - 2021, ".scree.plot.png"), plot = scree_plot, width = 8, height = 6, dpi = 150)
  
  ### PCA's 1-2 Plot ###
  mean.pca <- aggregate(count.pca$x[,1:2], by = list(dat$Treatment), FUN = mean)
  sd.pca <- aggregate(count.pca$x[,1:2], by = list(dat$Treatment), FUN = sd)
  
  # Calculate standard error
  n <- table(dat$Treatment)  # Sample size per treatment group
  se.pca <- sd.pca[, -1] / sqrt(n)  # Remove grouping column and calculate standard error
  
  # PCA plot
  pca.plot <- ggbiplot(pcobj = count.pca, 
                       ellipse = FALSE,
                       scale = 1,
                       groups = dat$Treatment, 
                       choices = c(1, 2),
                       varname.abbrev = FALSE,
                       varname.size = 4) + 
    theme_classic(base_size = 18) + 
    xlim(-4,4) +
    ylim(-4,4) +
    geom_point(aes(x = mean.pca[1,2], y = mean.pca[1,3]), colour = "black", fill = "turquoise3", size = 2, stroke = 1, pch = 21) + 
    geom_point(aes(x = mean.pca[2,2], y = mean.pca[2,3]), colour = "black", fill = "tomato2", size = 2, stroke = 1, pch = 21) +
    scale_color_manual(name = "Treatment", values = c("turquoise3", "tomato2")) +
    ggtitle(paste("QBS PCA Plot", year))
  
  # Adding standard error bars to PCA plot
  pca.plot <- pca.plot + 
    geom_segment(aes(x = mean.pca[1,2] - se.pca[1,1], 
                     y = mean.pca[1,3], 
                     xend = mean.pca[1,2] + se.pca[1,1], 
                     yend = mean.pca[1,3]), 
                 color = "red", linetype = "dashed", linewidth = 0.25) +
    geom_segment(aes(x = mean.pca[1,2], 
                     y = mean.pca[1,3] - se.pca[1,2], 
                     xend = mean.pca[1,2], 
                     yend = mean.pca[1,3] + se.pca[1,2]), 
                 color = "red", linetype = "dashed", linewidth = 0.25) +
    geom_segment(aes(x = mean.pca[2,2] - se.pca[2,1], 
                     y = mean.pca[2,3],
                     xend = mean.pca[2,2] + se.pca[2,1], 
                     yend = mean.pca[2,3]), 
                 color = "red", linetype = "dashed", linewidth = 0.25) +
    geom_segment(aes(x = mean.pca[2,2],
                     y = mean.pca[2,3] - se.pca[2,2], 
                     xend = mean.pca[2,2],
                     yend = mean.pca[2,3] + se.pca[2,2]), 
                 color = "red", linetype = "dashed", linewidth = 0.25)
  
  # Create directory for PCA plots
  dir.create(paste0("Plots/PCA/pca_tax_order/SE/"), showWarnings = FALSE, recursive = TRUE)
  
  # Save PCA plot
  ggsave(filename = paste0("Plots/PCA/pca_tax_order/SE/", year - 2021, ".PCA.png"), 
         plot = pca.plot, 
         width = 8, 
         height = 6, 
         dpi = 150)
}



