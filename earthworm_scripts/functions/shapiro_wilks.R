# Define the function
check_normality <- function(data, columns_of_interest) {
  # Initialize an empty data frame to store results
  result_df <- data.frame()
  
  # Loop over each year group and apply Shapiro-Wilk test to each column
  for (year in unique(data$Year)) {
    # Subset the data for the current year
    data_subset <- data %>% filter(Year == year)
    
    # Apply Shapiro-Wilk test for each column of interest
    shapiro_test_results <- sapply(data_subset[columns_of_interest], function(x) {
      # Ensure that x is numeric before applying the Shapiro-Wilk test
      if (is.numeric(x) && length(unique(x)) > 1) {
        return(shapiro.test(x)$p.value)
      } else {
        return(NA)  # Return NA if the column is not numeric or all values are identical
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
  
  # Assign the result to a variable in the global environment
  assign("result_df", result_df, envir = .GlobalEnv)
}

# Example usage
# Assuming 'data' is your dataset and 'columns_of_interest' is a vector of column names
# check_normality(data, columns_of_interest)
