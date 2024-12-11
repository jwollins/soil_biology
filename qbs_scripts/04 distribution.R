# 04 Distribution ####
## WHO: JOE COLLINS
## WHAT: QBS data normality test and distribution plots
## LAST EDIT: 2024-10-16
####



##ABUNDANCE ####

### 01 SHAPIRO WILK ####

# Load the data
data <- all_dat_count

# Specify the columns of interest (columns 7 to 55 in this case)
columns_of_interest <- names(data)[7:ncol(data)]

# Initialize an empty data frame to store results
result_df <- data.frame()

# Loop over each year group and apply Shapiro-Wilk test to each column
for (year in unique(data$Year)) {
  # Subset the data for the current year
  data_subset <- data %>% filter(Year == year)
  
  # Apply Shapiro-Wilk test for each column of interest
  shapiro_test_results <- sapply(data_subset[columns_of_interest], function(x) {
    if (length(unique(x)) > 1) {
      shapiro.test(x)$p.value
    } else {
      NA  # Return NA if all values are identical
    }
  })
  
  # Create a temporary data frame to store results for the current year
  temp_df <- data.frame(
    Year = year,
    Column = names(shapiro_test_results),
    P_Value = shapiro_test_results
  )
  
  # Add a column indicating normality based on p-value
  temp_df$Significance <- ifelse(temp_df$P_Value > 0.05, 
                                 yes = "Normally distributed", 
                                 no = "Non-normal distribution")
  
  # Bind the temporary results to the main results data frame
  result_df <- rbind(result_df, temp_df)
}

# Save the results to a CSV file
if (!dir.exists("Statistics/normality/")) {
  dir.create("Statistics/normality/", recursive = TRUE)
}
write.csv(result_df, file = "Statistics/normality/abundance_shapiro_results_by_year.csv", 
          row.names = FALSE)










### 02 BARTLETT TEST ####

# Load the data
data <- all_dat_count

# Specify the columns of interest (columns 7 to 55 in this case)
columns_of_interest <- names(data)[7:ncol(data)]

# Initialize an empty data frame to store results
result_df <- data.frame()

# Loop over each year and each column to apply Bartlett test grouped by Treatment
for (year in unique(data$Year)) {
  # Subset the data for the current year
  data_year_subset <- data %>% filter(Year == year)
  
  # Loop through each column of interest
  for (col_name in columns_of_interest) {
    # Check if column exists and is numeric
    if (col_name %in% names(data) && is.numeric(data_year_subset[[col_name]])) {
      # Check if there are at least two distinct values for Treatment
      if (n_distinct(data_year_subset$Treatment) >= 2 && length(unique(data_year_subset[[col_name]])) >= 2) {
        # Apply Bartlett test with Treatment as the grouping factor within the current year
        bartlett_test <- bartlett.test(data_year_subset[[col_name]] ~ data_year_subset$Treatment)
        p_value <- bartlett_test$p.value
      } else {
        # Return NA if conditions are not met
        p_value <- NA
      }
      
      # Create a temporary data frame to store results for the current column and year
      temp_df <- data.frame(
        Year = year,
        Column = col_name,
        P_Value = p_value
      )
      
      # Bind the temporary results to the main results data frame
      result_df <- rbind(result_df, temp_df)
    }
  }
}

# Add a column indicating homogeneity based on p-value
result_df$Significance <- ifelse(result_df$P_Value > 0.05, 
                                 yes = "Homogeneity of variances", 
                                 no = "No homogeneity of variances")

# Save the results to a CSV file
if (!dir.exists("Statistics/normality/")) {
  dir.create("Statistics/normality/", recursive = TRUE)
}
write.csv(result_df, file = "Statistics/normality/abundance_bartlett_results_by_year_treatment.csv", 
          row.names = FALSE)




### 03 BOX PLOTS ####




### 04 QQ PLOTS ####

# Loop through columns 7 to 55 and create a histogram for each, faceted by Year
for (i in 7:ncol(all_dat_count)) {
  col_name <- names(all_dat_count)[i]
  
  # Create the histogram
  p <- ggplot(all_dat_count, 
              aes(sample = all_dat_count[[col_name]])) + 
    stat_qq() +
    facet_wrap(~ Year) +
    labs(title = paste("QQ Plot for", col_name), 
         x = "Theoretical Quantiles", 
         y = "Sample Quantiles") +
    theme_bw()
  
  # Save each plot with a unique filename
  file_name <- paste0("Plots/qqplots/abundance/", 
                      col_name, "_qqplot.png")
  ggsave(filename = file_name, 
         plot = p, 
         width = 8, 
         height = 5)
}




















### 05 HISTOGRAMS ####

# Create a directory to save histograms if it doesn't already exist
if (!dir.exists("Plots/histograms/")) {
  dir.create("Plots/histograms/")
}

# Loop through columns 7 to 55 and create a histogram for each numeric column, faceted by Year
for (i in 7:ncol(all_dat_count)) {
  col_name <- names(all_dat_count)[i]
  
  # Check if column name is NULL or if the column is not numeric
  if (!is.null(col_name) && is.numeric(all_dat_count[[col_name]])) {
    # Print the column name to debug and verify it is not NULL
    print(paste("Processing column:", col_name))
    
    # Create the histogram
    p <- ggplot(all_dat_count, 
                aes(x = all_dat_count[[col_name]])) + 
      geom_histogram(bins = 30, 
                     fill = "turquoise3", 
                     color = "black", 
                     alpha = 0.7) + 
      facet_wrap(~ Year) +
      labs(
        x = col_name,
        y = "Frequency",
        title = paste("Distribution of", col_name, "abundance by Year")
      ) +
      theme_bw()
    
    # Save each plot with a unique filename
    file_name <- paste0("Plots/histograms/abundance/", col_name, "_histogram.png")
    ggsave(filename = file_name, plot = p, width = 8, height = 5)
  } else {
    print(paste("Skipping column:", col_name, "- Not numeric or NULL"))
  }
}






# QBS SCORE ###################################################################


### 01 SHAPIRO WILK ####

# Load the data
data <- qbs_dat

# Specify the columns of interest (columns 7 to 55 in this case)
columns_of_interest <- names(data)[18:ncol(data)]

# Initialize an empty data frame to store results
result_df <- data.frame()

# Loop over each year group and apply Shapiro-Wilk test to each column
for (year in unique(data$Year)) {
  # Subset the data for the current year
  data_subset <- data %>% filter(Year == year)
  
  # Apply Shapiro-Wilk test for each column of interest
  shapiro_test_results <- sapply(data_subset[columns_of_interest], function(x) {
    if (length(unique(x)) > 1) {
      shapiro.test(x)$p.value
    } else {
      NA  # Return NA if all values are identical
    }
  })
  
  # Create a temporary data frame to store results for the current year
  temp_df <- data.frame(
    Year = year,
    Column = names(shapiro_test_results),
    P_Value = shapiro_test_results
  )
  
  # Add a column indicating normality based on p-value
  temp_df$Significance <- ifelse(temp_df$P_Value > 0.05, 
                                 yes = "Normally distributed", 
                                 no = "Non-normal distribution")
  
  # Bind the temporary results to the main results data frame
  result_df <- rbind(result_df, temp_df)
}

# Save the results to a CSV file
if (!dir.exists("Statistics/normality/")) {
  dir.create("Statistics/normality/", recursive = TRUE)
}
write.csv(result_df, file = "Statistics/normality/qbs_score_shapiro_results_by_year.csv", 
          row.names = FALSE)



### 02 BARTLETT TEST ####

# Load the data
data <- qbs_dat

# Specify the columns of interest (columns 7 to 55 in this case)
columns_of_interest <- names(data)[18:ncol(data)]

# Initialize an empty data frame to store results
result_df <- data.frame()

# Loop over each year and each column to apply Bartlett test grouped by Treatment
for (year in unique(data$Year)) {
  # Subset the data for the current year
  data_year_subset <- data %>% filter(Year == year)
  
  # Loop through each column of interest
  for (col_name in columns_of_interest) {
    # Check if column exists and is numeric
    if (col_name %in% names(data) && is.numeric(data_year_subset[[col_name]])) {
      # Check if there are at least two distinct values for Treatment
      if (n_distinct(data_year_subset$Treatment) >= 2 && length(unique(data_year_subset[[col_name]])) >= 2) {
        # Apply Bartlett test with Treatment as the grouping factor within the current year
        bartlett_test <- bartlett.test(data_year_subset[[col_name]] ~ data_year_subset$Treatment)
        p_value <- bartlett_test$p.value
      } else {
        # Return NA if conditions are not met
        p_value <- NA
      }
      
      # Create a temporary data frame to store results for the current column and year
      temp_df <- data.frame(
        Year = year,
        Column = col_name,
        P_Value = p_value
      )
      
      # Bind the temporary results to the main results data frame
      result_df <- rbind(result_df, temp_df)
    }
  }
}

# Add a column indicating homogeneity based on p-value
result_df$Significance <- ifelse(result_df$P_Value > 0.05, 
                                 yes = "Homogeneity of variances", 
                                 no = "No homogeneity of variances")

# Save the results to a CSV file
if (!dir.exists("Statistics/normality/")) {
  dir.create("Statistics/normality/", recursive = TRUE)
}
write.csv(result_df, file = "Statistics/normality/qbs_score_bartlett_results_by_year_treatment.csv", 
          row.names = FALSE)




### 03 BOX PLOTS ####




### 04 QQ PLOTS ####

# Loop through columns 7 to 55 and create a histogram for each, faceted by Year
for (i in 18:ncol(data)) {
  col_name <- names(data)[i]
  
  # Create the histogram
  p <- ggplot(data, 
              aes(sample = data[[col_name]])) + 
    stat_qq() +
    facet_wrap(~ Year) +
    labs(title = paste("QQ Plot for", col_name, "QBS score"), 
         x = "Theoretical Quantiles", 
         y = "Sample Quantiles") +
    theme_bw()
  
  # Save each plot with a unique filename
  file_name <- paste0("Plots/qqplots/qbs_score/", 
                      col_name, "_qqplot.png")
  ggsave(filename = file_name, 
         plot = p, 
         width = 8, 
         height = 5)
}









### 05 HISTOGRAMS ####

# Create a directory to save histograms if it doesn't already exist
if (!dir.exists("Plots/histograms/qbs_score/")) {
  dir.create("Plots/histograms/qbs_score/")
}

# Loop through columns 7 to 55 and create a histogram for each numeric column, faceted by Year
for (i in 7:ncol(data)) {
  col_name <- names(data)[i]
  
  # Check if column name is NULL or if the column is not numeric
  if (!is.null(col_name) && is.numeric(data[[col_name]])) {
    # Print the column name to debug and verify it is not NULL
    print(paste("Processing column:", col_name))
    
    # Create the histogram
    p <- ggplot(data, 
                aes(x = data[[col_name]])) + 
      geom_histogram(bins = 30, 
                     fill = "turquoise3", 
                     color = "black", 
                     alpha = 0.7) + 
      facet_wrap(~ Year) +
      labs(
        x = col_name,
        y = "Frequency",
        title = paste("Distribution of", col_name, "QBS Score by Year")
      ) +
      theme_bw()
    
    # Save each plot with a unique filename
    file_name <- paste0("Plots/histograms/qbs_score/", col_name, "_histogram.png")
    ggsave(filename = file_name, plot = p, width = 8, height = 5)
  } else {
    print(paste("Skipping column:", col_name, "- Not numeric or NULL"))
  }
}




# TAXONOMIC ORDER ###################################################################


### 01 SHAPIRO WILK ####

# Load the data
data <- tax_ord_dat

# Specify the columns of interest (columns 7 to 55 in this case)
columns_of_interest <- names(data)[18:ncol(data)]

# Initialize an empty data frame to store results
result_df <- data.frame()

# Loop over each year group and apply Shapiro-Wilk test to each column
for (year in unique(data$Year)) {
  # Subset the data for the current year
  data_subset <- data %>% filter(Year == year)
  
  # Apply Shapiro-Wilk test for each column of interest
  shapiro_test_results <- sapply(data_subset[columns_of_interest], function(x) {
    if (length(unique(x)) > 1) {
      shapiro.test(x)$p.value
    } else {
      NA  # Return NA if all values are identical
    }
  })
  
  # Create a temporary data frame to store results for the current year
  temp_df <- data.frame(
    Year = year,
    Column = names(shapiro_test_results),
    P_Value = shapiro_test_results
  )
  
  # Add a column indicating normality based on p-value
  temp_df$Significance <- ifelse(temp_df$P_Value > 0.05, 
                                 yes = "Normally distributed", 
                                 no = "Non-normal distribution")
  
  # Bind the temporary results to the main results data frame
  result_df <- rbind(result_df, temp_df)
}

# Save the results to a CSV file
if (!dir.exists("Statistics/normality/")) {
  dir.create("Statistics/normality/", recursive = TRUE)
}
write.csv(result_df, file = "Statistics/normality/tax_order_abundance_shapiro_results_by_year.csv", 
          row.names = FALSE)



### 02 BARTLETT TEST ####

# Specify the columns of interest (columns 7 to 55 in this case)
columns_of_interest <- names(data)[18:ncol(data)]

# Initialize an empty data frame to store results
result_df <- data.frame()

# Loop over each year and each column to apply Bartlett test grouped by Treatment
for (year in unique(data$Year)) {
  # Subset the data for the current year
  data_year_subset <- data %>% filter(Year == year)
  
  # Loop through each column of interest
  for (col_name in columns_of_interest) {
    # Check if column exists and is numeric
    if (col_name %in% names(data) && is.numeric(data_year_subset[[col_name]])) {
      # Check if there are at least two distinct values for Treatment
      if (n_distinct(data_year_subset$Treatment) >= 2 && length(unique(data_year_subset[[col_name]])) >= 2) {
        # Apply Bartlett test with Treatment as the grouping factor within the current year
        bartlett_test <- bartlett.test(data_year_subset[[col_name]] ~ data_year_subset$Treatment)
        p_value <- bartlett_test$p.value
      } else {
        # Return NA if conditions are not met
        p_value <- NA
      }
      
      # Create a temporary data frame to store results for the current column and year
      temp_df <- data.frame(
        Year = year,
        Column = col_name,
        P_Value = p_value
      )
      
      # Bind the temporary results to the main results data frame
      result_df <- rbind(result_df, temp_df)
    }
  }
}

# Add a column indicating homogeneity based on p-value
result_df$Significance <- ifelse(result_df$P_Value > 0.05, 
                                 yes = "Homogeneity of variances", 
                                 no = "No homogeneity of variances")

# Save the results to a CSV file
if (!dir.exists("Statistics/normality/")) {
  dir.create("Statistics/normality/", recursive = TRUE)
}
write.csv(result_df, file = "Statistics/normality/tax_order_abundance_bartlett_results_by_year_treatment.csv", 
          row.names = FALSE)




### 03 BOX PLOTS ####




### 04 QQ PLOTS ####

# Loop through columns 7 to 55 and create a histogram for each, faceted by Year
for (i in 18:ncol(data)) {
  col_name <- names(data)[i]
  
  # Create the histogram
  p <- ggplot(data, 
              aes(sample = data[[col_name]])) + 
    stat_qq() +
    facet_wrap(~ Year) +
    labs(title = paste("QQ Plot for", col_name, "Abundance"), 
         x = "Theoretical Quantiles", 
         y = "Sample Quantiles") +
    theme_bw()
  
  # Save each plot with a unique filename
  file_name <- paste0("Plots/qqplots/tax_order/", 
                      col_name, "_qqplot.png")
  ggsave(filename = file_name, 
         plot = p, 
         width = 8, 
         height = 5)
}









### 05 HISTOGRAMS ####

# Create a directory to save histograms if it doesn't already exist
if (!dir.exists("Plots/histograms/tax_order/")) {
  dir.create("Plots/histograms/tax_order/")
}

# Loop through columns 7 to 55 and create a histogram for each numeric column, faceted by Year
for (i in 7:ncol(data)) {
  col_name <- names(data)[i]
  
  # Check if column name is NULL or if the column is not numeric
  if (!is.null(col_name) && is.numeric(data[[col_name]])) {
    # Print the column name to debug and verify it is not NULL
    print(paste("Processing column:", col_name))
    
    # Create the histogram
    p <- ggplot(data, 
                aes(x = data[[col_name]])) + 
      geom_histogram(bins = 30, 
                     fill = "turquoise3", 
                     color = "black", 
                     alpha = 0.7) + 
      facet_wrap(~ Year) +
      labs(
        x = col_name,
        y = "Frequency",
        title = paste("Distribution of", col_name, "Abundance by Year")
      ) +
      theme_bw()
    
    # Save each plot with a unique filename
    file_name <- paste0("Plots/histograms/tax_order/", col_name, "_histogram.png")
    ggsave(filename = file_name, plot = p, width = 8, height = 5)
  } else {
    print(paste("Skipping column:", col_name, "- Not numeric or NULL"))
  }
}




# SHANNON INDEX ############################################

### 01 SHAPIRO WILK ####

# Load the data
data <- shannon_data

# Perform a Shapiro-Wilk test on Shannon Index
shapiro_test <- shapiro.test(data$Shannon_Index)

# View the test results
print(shapiro_test)

# Define the file path where you want to save the results
file_path <- "Statistics/normality/shannon_index_shapiro.txt"

# Save the test output to a file
sink(file_path)
print(shapiro_test)
sink()  # Close the file connection





### 02 BARTLETT TEST ####

# Load the data
data <- all_dat_count


# Perform Bartlett's test for homogeneity of variances across treatments
bartlett_test <- bartlett.test(Shannon_Index ~ Treatment, data = data)

# View the test results
print(bartlett_test)

# Define the file path where you want to save the results
file_path <- "Statistics/normality/shannon_index_bartlett.txt"

# Save the test output to a file
sink(file_path)
print(bartlett_test)
sink()  # Close the file connection




### 03 BOX PLOTS ####




### 04 QQ PLOTS ####


  
  # Create the histogram
  p <- ggplot(data = data, 
              aes(sample = Shannon_Index)) + 
    stat_qq() +
    facet_wrap(~ Year) +
    labs(title = paste("QQ Plot for", "Shannon Index"), 
         x = "Theoretical Quantiles", 
         y = "Sample Quantiles") +
    theme_bw()

p

dir.create(path = "Plots/qqplots/shannon_index/")
  
  # Save each plot with a unique filename
  file_name <- paste0("Plots/qqplots/shannon_index/", 
                      "Shannon Index", "_qqplot.png")
  ggsave(filename = file_name, 
         plot = p, 
         width = 8, 
         height = 5)







### 05 HISTOGRAMS ####

# Create a directory to save histograms if it doesn't already exist
if (!dir.exists("Plots/histograms/shannon_index/")) {
  dir.create("Plots/histograms/shannon_index/")
}

    
    # Create the histogram
    p <- ggplot(data = data, 
                aes(x = Shannon_Index)) + 
      geom_histogram(bins = 30, 
                     fill = "turquoise3", 
                     color = "black", 
                     alpha = 0.7) + 
      facet_wrap(~ Year) +
      labs(
        x = "Shannon Index",
        y = "Frequency",
        title = paste("Distribution of", "Shannon Index", "abundance by Year")
      ) +
      theme_bw()
    
    p
    
    # Save each plot with a unique filename
    file_name <- paste0("Plots/histograms/shannon_index/", "Shannon_Index", "_histogram.png")
    ggsave(filename = file_name, plot = p, width = 8, height = 5)




