# Load necessary libraries
library(lmerTest)
library(broom.mixed)
library(emmeans)
library(dplyr)

# Define the function
run_lmer_and_pairwise <- function(data, columns_to_run_lmer) {
  # Initialize empty lists to store LME summaries and pairwise comparisons
  lmer_summaries <- list()
  pairwise_summaries <- list()
  
  # Loop through the specified columns and run LME + pairwise comparisons
  for (col_name in columns_to_run_lmer) {
    # Create the LME formula string
    formula_string <- as.formula(paste(col_name, "~ Treatment + (1 | Year)"))
    
    # Run the LME model
    lmer_model <- lmer(formula_string, data = data)
    
    # Create a tidy summary of the model using broom.mixed::tidy
    tidy_summary <- broom.mixed::tidy(lmer_model, effects = "fixed")
    
    # Add the column name to the summary
    tidy_summary <- tidy_summary %>%
      mutate(
        variable = col_name,
        Year = NA  # Since `Year` is a random effect, it doesn't have a fixed level in tidy_summary
      )
    
    # Round numeric columns to 2 decimal places, only if they exist
    tidy_summary <- tidy_summary %>%
      mutate(
        estimate = ifelse("estimate" %in% names(tidy_summary), round(estimate, 2), estimate),
        std.error = ifelse("std.error" %in% names(tidy_summary), round(std.error, 2), std.error),
        statistic = ifelse("statistic" %in% names(tidy_summary), round(statistic, 2), statistic),
        p.value = ifelse("p.value" %in% names(tidy_summary), round(p.value, 2), p.value)
      )
    
    # Add significance stars based on p-value, if p.value exists
    if ("p.value" %in% colnames(tidy_summary)) {
      tidy_summary <- tidy_summary %>%
        mutate(
          p.signif = case_when(
            p.value < 0.001 ~ "***",
            p.value < 0.01 ~ "**",
            p.value < 0.05 ~ "*",
            TRUE ~ ""
          )
        )
    }
    
    # Append the tidy summary to the list
    lmer_summaries[[col_name]] <- tidy_summary
    
    # Loop through each unique Year for pairwise comparisons
    for (year in unique(data$Year)) {
      # Filter data for the current Year
      data_year <- data %>% filter(Year == year)
      
      # Check if there are at least two levels of Treatment in this subset
      if (length(unique(data_year$Treatment)) > 1) {
        # Run the LME model for this specific year subset
        lmer_model_year <- lmer(formula_string, data = data_year)
        
        # Perform pairwise comparisons using emmeans for this specific year
        emmeans_res <- emmeans(lmer_model_year, pairwise ~ Treatment)
        pairwise_summary <- as.data.frame(summary(emmeans_res$contrasts)) # Get pairwise comparisons summary
        
        # Add the column name and year to the pairwise summary
        pairwise_summary <- pairwise_summary %>%
          mutate(
            variable = col_name,
            Year = as.character(year)
          )
        
        # Round numeric columns for pairwise comparisons
        pairwise_summary <- pairwise_summary %>%
          mutate(
            estimate = round(estimate, 2),
            SE = round(SE, 2),
            p.value = round(p.value, 2)
          )
        
        # Check for t.ratio or z.ratio and round them
        if ("t.ratio" %in% colnames(pairwise_summary)) {
          pairwise_summary$t.ratio <- round(pairwise_summary$t.ratio, 2)
        } else if ("z.ratio" %in% colnames(pairwise_summary)) {
          pairwise_summary$z.ratio <- round(pairwise_summary$z.ratio, 2)
        }
        
        # Add significance stars for pairwise comparisons
        pairwise_summary$p.signif <- case_when(
          pairwise_summary$p.value < 0.001 ~ "***",
          pairwise_summary$p.value < 0.01 ~ "**",
          pairwise_summary$p.value < 0.05 ~ "*",
          TRUE ~ ""
        )
        
        # Append the pairwise summary to the list
        pairwise_summaries[[paste(col_name, year, sep = "_")]] <- pairwise_summary
      } else {
        # If only one level of Treatment exists, skip pairwise comparison for this year
        message(paste("Skipping pairwise comparisons for", col_name, "in Year", year, "due to insufficient levels of Treatment"))
      }
    }
  }
  
  # Combine all LME summaries and pairwise comparisons into single dataframes
  lmer_summaries_df <- bind_rows(lmer_summaries, .id = "variable")
  pairwise_summaries_df <- bind_rows(pairwise_summaries, .id = "variable")
  
  # Assign dataframes to the global environment
  assign("lmer_summaries_df", lmer_summaries_df, envir = .GlobalEnv)
  assign("pairwise_summaries_df", pairwise_summaries_df, envir = .GlobalEnv)
}

# Example usage
# Assuming 'data' is your dataset and 'columns_to_run_lmer' is a vector of column names
# run_lmer_and_pairwise(data, columns_to_run_lmer)
# The dataframes lmer_summaries_df and pairwise_summaries_df will be available in the global environment.
