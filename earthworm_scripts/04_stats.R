### Earthworm data
### J Collins 
### 2024-06-01
###

## 04 STATS ####


## 04.1 PACKAGES ####

source("scripts/01_packages.R")


## 04.2 DATA ####

source("scripts/02_data.R")



source("scripts/functions/fun_lm_by_year.R")
source("scripts/functions/fun_glm_by_year.R")

# Load the data
data <- all_dat

#rename column by name
data <- data %>% rename_at('year', ~'Year')
#rename column by name
data <- data %>% rename_at('treatment', ~'Treatment')

# Specify the columns of interest (columns 7 to 55 in this case)
columns_of_interest <- names(data)[6:ncol(data)]



#run_glm_and_pairwise(data = data, columns_to_run_glm = columns_of_interest)

run_lmer_and_pairwise(data = data, columns_to_run_lmer = columns_of_interest)

write.csv(x = lmer_summaries_df, file = "Statistics/lmer_summaries_df.csv")
write.csv(x = pairwise_summaries_df, file = "Statistics/pairwise_summaries_df.csv")






### 04.3 GLM's ####


#### 2023 ####
columns_to_run_glm <- colnames(dat_2023[6:19])

# Create an empty list to store the GLM models
glm_models <- list()

# Iterate through the specified columns and run GLM
for (col_name in columns_to_run_glm) {
  formula_string <- paste(col_name, "~ treatment + block", sep = "")
  
  # Assuming a Gaussian family for this example, change the family argument accordingly
  glm_model <- glm(formula_string, data = dat_2023)
  
  # Save the model summary to a text file
  file_path <- file.path("stats/glm_outputs/2023/", paste0("glm_summary_", gsub(" ", "_", col_name), ".txt"))
  summary_text <- capture.output(summary(glm_model))
  cat(summary_text, file = file_path, sep = "\n")
}




#### 2024 ####

columns_to_run_glm <- colnames(dat_2024[6:19])

# Create an empty list to store the GLM models
glm_models <- list()

# Iterate through the specified columns and run GLM
for (col_name in columns_to_run_glm) {
  formula_string <- paste(col_name, "~ treatment + block", sep = "")
  
  # Assuming a Gaussian family for this example, change the family argument accordingly
  glm_model <- glm(formula_string, data = dat_2024)
  
  # Save the model summary to a text file
  file_path <- file.path("stats/glm_outputs/2024/",
                         paste0("glm_summary_", gsub(" ", "_", col_name), ".txt"))
  summary_text <- capture.output(summary(glm_model))
  cat(summary_text, file = file_path, sep = "\n")
}

