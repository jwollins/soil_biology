# 04 Distribution ####
## WHO: JOE COLLINS
## WHAT: QBS data normality test and distribution plots
## LAST EDIT: 2024-10-16
####



##ABUNDANCE ####

### 01 SHAPIRO WILK ####

# Load the data
data <- all_dat

#rename column by name
data <- data %>% rename_at('year', ~'Year')
#rename column by name
data <- data %>% rename_at('treatment', ~'Treatment')

# Specify the columns of interest (columns 7 to 55 in this case)
columns_of_interest <- names(data)[6:ncol(data)]

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





### 04 QQ PLOTS ####

# Loop through columns 7 to 55 and create a histogram for each, faceted by Year
for (i in 7:ncol(data)) {
  col_name <- names(data)[i]
  
  # Create the histogram
  p <- ggplot(data, 
              aes(sample = data[[col_name]])) + 
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




















