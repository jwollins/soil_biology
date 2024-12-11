
# Define the function
check_variance_homogeneity <- function(data, columns_of_interest) {
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
        
        temp_df$Homogeneity <- ifelse(temp_df$P_Value > 0.05, "Homogeneity", "No_homogeneity")
        
        # Bind the temporary results to the main results data frame
        result_df <- rbind(result_df, temp_df)
      }
    }
  }
  # Optionally, return the result_df to the global environment
  assign("bartlett_result_df", result_df, envir = .GlobalEnv)
}

# Example usage
# Assuming 'data' is your dataset and 'columns_of_interest' is a vector of column names
# check_variance_homogeneity(data, columns_of_interest)
