
## WHO: JOE COLLINS
## WHAT: Barplots - QBS
## LAST EDIT: 25/01/2023
####

## 01 BARPLOTS ####
  
  
  # Create a directory to save plots if it doesn't already exist
  if (!dir.exists("Plots/barplots/abundance/")) {
    dir.create("Plots/barplots/abundance/")
  }
  
  # Loop through each summary table in summary_list and create a plot for each
  for (col_name in names(summary_list)) {
    # Assign the summary table to a temporary variable
    p_sum <- summary_list[[col_name]]
    
    # Specify the title and y-axis title based on the column name
    title_exp <- paste("Mean and SE of", col_name, "abundance by Treatment and Year")
    y_title <- paste("Mean", col_name)
    
    # Create the plot
    p <- ggplot(data = p_sum, 
                aes(x = Treatment, 
                    y = mean, 
                    fill = Treatment)) + 
      geom_bar(stat = "identity", 
               color = "black", 
               position = "dodge") + 
      labs(
        x = "Treatment",
        y = y_title,
        subtitle = title_exp, 
        caption = "") +
      theme_bw() +
      scale_fill_manual(values = c("turquoise3", "tomato2"), 
                        name = "Treatment") +
      theme(strip.text.x = element_text(size = 12, 
                                        color = "black", 
                                        face = "bold.italic"), 
            legend.position = "bottom") +
      geom_errorbar(aes(ymin = mean - se, 
                        ymax = mean + se),
                    width = .2,                    # Width of the error bars
                    position = position_dodge(.9)) +
      facet_wrap(~ Year, 
                 ncol = 4, 
                 scales = 'free_x')
    
    # Save the plot to a file, using the column name in the file name
    file_name <- paste0("plots/barplots/abundance/", col_name, "_plot.png")
    ggsave(filename = file_name, plot = p, width = 8, height = 5)
  }
  





## 02 tax order ####


# Create a directory to save plots if it doesn't already exist
if (!dir.exists("Plots/barplots/tax_order/")) {
  dir.create("Plots/barplots/tax_order/")
}

# Loop through each summary table in summary_list and create a plot for each
for (col_name in names(tax_ord_list)) {
  # Assign the summary table to a temporary variable
  p_sum <- tax_ord_list[[col_name]]
  
  # Specify the title and y-axis title based on the column name
  title_exp <- paste("Mean and SE of", col_name, "abundance by Treatment and Year")
  y_title <- paste("Mean", col_name)
  
  # Create the plot
  p <- ggplot(data = p_sum, 
              aes(x = Treatment, 
                  y = mean, 
                  fill = Treatment)) + 
    geom_bar(stat = "identity", 
             color = "black", 
             position = "dodge") + 
    labs(
      x = "Treatment",
      y = y_title,
      subtitle = title_exp, 
      caption = "") +
    theme_bw() +
    scale_fill_manual(values = c("turquoise3", "tomato2"), 
                      name = "Treatment") +
    theme(strip.text.x = element_text(size = 12, 
                                      color = "black", 
                                      face = "bold.italic"), 
          legend.position = "bottom") +
    geom_errorbar(aes(ymin = mean - se, 
                      ymax = mean + se),
                  width = .2,                    # Width of the error bars
                  position = position_dodge(.9)) +
    facet_wrap(~ Year, 
               ncol = 4, 
               scales = 'free_x')
  
  # Save the plot to a file, using the column name in the file name
  file_name <- paste0("plots/barplots/tax_order/", col_name, "_plot.png")
  ggsave(filename = file_name, plot = p, width = 8, height = 5)
}






## 03 QBS-ar Score ####


# Create a directory to save plots if it doesn't already exist
if (!dir.exists("Plots/barplots/QBS_score/")) {
  dir.create("Plots/barplots/QBS_score/")
}

# Loop through each summary table in summary_list and create a plot for each
for (col_name in names(qbs_dat_list)) {
  # Assign the summary table to a temporary variable
  p_sum <- qbs_dat_list[[col_name]]
  
  # Specify the title and y-axis title based on the column name
  title_exp <- paste("QBS Score Mean and SE of", col_name, "by Treatment and Year")
  y_title <- paste("QBS Score Mean", col_name)
  
  # Create the plot
  p <- ggplot(data = p_sum, 
              aes(x = Treatment, 
                  y = mean, 
                  fill = Treatment)) + 
    geom_bar(stat = "identity", 
             color = "black", 
             position = "dodge") + 
    labs(
      x = "Treatment",
      y = y_title,
      subtitle = title_exp, 
      caption = "") +
    theme_bw() +
    scale_fill_manual(values = c("turquoise3", "tomato2"), 
                      name = "Treatment") +
    theme(strip.text.x = element_text(size = 12, 
                                      color = "black", 
                                      face = "bold.italic"), 
          legend.position = "bottom") +
    geom_errorbar(aes(ymin = mean - se, 
                      ymax = mean + se),
                  width = .2,                    # Width of the error bars
                  position = position_dodge(.9)) +
    facet_wrap(~ Year, 
               ncol = 4, 
               scales = 'free_x')
  
  # Save the plot to a file, using the column name in the file name
  file_name <- paste0("plots/barplots/QBS_score/", col_name, "_plot.png")
  ggsave(filename = file_name, plot = p, width = 8, height = 5)
}



## 04 QBS-e Score ####


# Create a directory to save plots if it doesn't already exist
if (!dir.exists("Plots/barplots/QBS_e_score/")) {
  dir.create("Plots/barplots/QBS_e_score/")
}

# Loop through each summary table in summary_list and create a plot for each
for (col_name in names(qbs_e_dat_list)) {
  # Assign the summary table to a temporary variable
  p_sum <- qbs_e_dat_list[[col_name]]
  
  # Specify the title and y-axis title based on the column name
  title_exp <- paste("QBS-e Score Mean and SE of", col_name, "by Treatment and Year")
  y_title <- paste("QBS-e Score Mean", col_name)
  
  # Create the plot
  p <- ggplot(data = p_sum, 
              aes(x = Treatment, 
                  y = mean, 
                  fill = Treatment)) + 
    geom_bar(stat = "identity", 
             color = "black", 
             position = "dodge") + 
    labs(
      x = "Treatment",
      y = y_title,
      subtitle = title_exp, 
      caption = "") +
    theme_bw() +
    scale_fill_manual(values = c("turquoise3", "tomato2"), 
                      name = "Treatment") +
    theme(strip.text.x = element_text(size = 12, 
                                      color = "black", 
                                      face = "bold.italic"), 
          legend.position = "bottom") +
    geom_errorbar(aes(ymin = mean - se, 
                      ymax = mean + se),
                  width = .2,                    # Width of the error bars
                  position = position_dodge(.9)) +
    facet_wrap(~ Year, 
               ncol = 4, 
               scales = 'free_x')
  
  # Save the plot to a file, using the column name in the file name
  file_name <- paste0("plots/barplots/QBS_e_score/", col_name, "_plot.png")
  ggsave(filename = file_name, plot = p, width = 8, height = 5)
}





## 05 Shannon Index ####

ggplot(data = shannon_sum, 
       aes(x = Treatment, 
           y = mean, 
           fill = Treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = "Treatment",
    y = "Shannon Index",
    subtitle = "Shannon Index", 
    caption = "") +
  theme_bw() +
  scale_fill_manual(values = c("turquoise3", "tomato2"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        legend.position = "bottom") +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se),
                width = .2,                    # Width of the error bars
                position = position_dodge(.9)) +
  facet_wrap(~ Year, 
             ncol = 4, 
             scales = 'free_x')

dir.create(path = "Plots/barplots/shannon_index/")

ggsave(filename = "shannon_index_barplot.png", 
       path = "Plots/barplots/shannon_index/", width = 8, height = 5)














