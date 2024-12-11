# Load necessary libraries
library(broom)
library(emmeans)

# Define the function
run_glm_and_pairwise <- function(data, columns_to_run_glm) {
  # Initialize empty lists to store GLM summaries and pairwise comparisons
  glm_summaries <- list()
  pairwise_summaries <- list()
  
  # Loop through the specified columns and run GLM + pairwise comparisons
  for (col_name in columns_to_run_glm) {
    # Create the GLM formula string
    formula_string <- paste(col_name, "~ Treatment * Year", sep = "")
    
    # Run the GLM model (assuming a quasipoisson family)
    glm_model <- glm(formula_string, data = data, family = quasipoisson)
    
    # Create a tidy summary of the model using broom::tidy
    tidy_summary <- broom::tidy(glm_model)
    
    # Add the column name to the summary
    tidy_summary$variable <- col_name
    
    # Round numeric columns to 2 decimal places
    tidy_summary$estimate <- round(tidy_summary$estimate, 2)
    tidy_summary$std.error <- round(tidy_summary$std.error, 2)
    tidy_summary$statistic <- round(tidy_summary$statistic, 2)
    tidy_summary$p.value <- round(tidy_summary$p.value, 2)
    
    # Add significance stars based on p-value
    tidy_summary$p.signif <- ifelse(tidy_summary$p.value < 0.001, "***",
                                    ifelse(tidy_summary$p.value < 0.01, "**",
                                           ifelse(tidy_summary$p.value < 0.05, "*", "")))
    
    # Append the tidy summary to the list
    glm_summaries[[col_name]] <- tidy_summary
    
    # Perform pairwise comparisons using emmeans
    emmeans_res <- emmeans(glm_model, pairwise ~ Treatment | Year)
    pairwise_summary <- as.data.frame(summary(emmeans_res$contrasts)) # Get pairwise comparisons summary
    
    # Add the column name to the pairwise summary
    pairwise_summary$variable <- col_name
    
    # Round numeric columns for pairwise comparisons
    pairwise_summary$estimate <- round(pairwise_summary$estimate, 2)
    pairwise_summary$SE <- round(pairwise_summary$SE, 2)
    
    # Check for t.ratio or z.ratio and round them
    if ("t.ratio" %in% colnames(pairwise_summary)) {
      pairwise_summary$t.ratio <- round(pairwise_summary$t.ratio, 2)
    } else if ("z.ratio" %in% colnames(pairwise_summary)) {
      pairwise_summary$z.ratio <- round(pairwise_summary$z.ratio, 2)
    }
    
    # Round p-values for pairwise comparisons and add significance stars
    pairwise_summary$p.value <- round(pairwise_summary$p.value, 2)
    pairwise_summary$p.signif <- ifelse(pairwise_summary$p.value < 0.001, "***",
                                        ifelse(pairwise_summary$p.value < 0.01, "**",
                                               ifelse(pairwise_summary$p.value < 0.05, "*", "")))
    
    # Append the pairwise summary to the list
    pairwise_summaries[[col_name]] <- pairwise_summary
  }
  
  # Optionally, return the lists to the global environment
  assign("glm_summaries_list", glm_summaries, envir = .GlobalEnv)
  assign("pairwise_summaries_list", pairwise_summaries, envir = .GlobalEnv)
  
  # Combine all GLM summaries and pairwise comparisons into single dataframes
  glm_summaries_df <- bind_rows(glm_summaries_list, .id = "variable")
  pairwise_summaries_df <- bind_rows(pairwise_summaries_list, .id = "variable")
  
  # Assign dataframes to the global environment
  assign("glm_summaries_df", glm_summaries_df, envir = .GlobalEnv)
  assign("pairwise_summaries_df", pairwise_summaries_df, envir = .GlobalEnv)
}

# Example usage
# Assuming 'data' is your dataset and 'columns_to_run_glm' is a vector of column names
# run_glm_and_pairwise(data, columns_to_run_glm)
