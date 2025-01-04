### Earthworm data
### J Collins 
### 2024-06-01
###
#*******************************************************************************
## 04 STATS ####
setwd(dir = "~/Documents/GitHub/soil_biology/")


#*******************************************************************************
### 02.1 PACKAGES ####

source(file = "earthworm_scripts/01_packages.R")


#*******************************************************************************
## 04.2 DATA ####

source(file = "~/Documents/GitHub/soil_biology/earthworm_scripts/02_data.R")

source("scripts/functions/fun_lm_by_year.R")
source("scripts/functions/fun_glm_by_year.R")
source(file = "~/Documents/GitHub/phd_tools/fun_glm_gamma_log_by_year.R")



setwd(dir = "~/OneDrive - Harper Adams University/Data/Soil/worms/")

# Load the data
dat <- all_dat

#rename column by name
dat <- dat %>% rename_at('year', ~'Year')
#rename column by name
dat <- dat %>% rename_at('treatment', ~'Treatment')


# Specify the columns of interest (columns 7 to 55 in this case)
columns_of_interest <- names(dat)[6:ncol(dat)]


#*******************************************************************************
# Models 

library(MVN)

colnames(data)

dat <- data[10:ncol(data)]

# Perform normality test
mvn_result <- mvn(
  data = dat, 
  mvnTest = "mardia",  # Mardia's test for multivariate normality
  multivariatePlot = "qq",  # QQ plot matrix
  univariateTest = "SW",  # Shapiro-Wilk test for univariate normality
  univariatePlot = TRUE   # Generate univariate diagnostic plots
)

# Print results
print(mvn_result$univariateNormality)

mvn(data = data, mvnTest = "mardia", multivariatePlot = "persp")










#*******************************************************************************







#run_glm_and_pairwise(data = data, columns_to_run_glm = columns_of_interest)

run_lmer_and_pairwise(data = data, columns_to_run_lmer = columns_of_interest)

write.csv(x = lmer_summaries_df, file = "Statistics/lmer_summaries_df.csv")
write.csv(x = pairwise_summaries_df, file = "Statistics/pairwise_summaries_df.csv")





#*******************************************************************************
### 04.3 GLMM's #### new

dir.create(path = "stats/glmm_outputs/")

# Fit a GLMM with Gamma distribution (for positively skewed data)
glmm_model <- glmer(shoots_m2 ~ treatment + (1 | block) + (1 | crop) + (1 | year), 
                    family = Gamma(link = "log"), 
                    data = dat)






columns_to_run_glm <- colnames(dat[,6:19])

# Create an empty list to store the GLM models
glmm_models <- list()

# Iterate through the specified columns and run GLM
for (col_name in columns_to_run_glm) {
  formula_string <- paste(col_name, "~ Treatment + (1 | block) + (1 | Year) + (1 | crop)", sep = "")
  
  # Assuming a Gaussian family for this example, change the family argument accordingly
  glmm_model <- glmer(formula_string, data = dat, family = Gamma(link = "log"))
  
  # Save the model summary to a text file
  file_path <- file.path("stats/glmm_outputs/", paste0("glmm_summary_", gsub(" ", "_", col_name), ".txt"))
  summary_text <- capture.output(summary(glmm_model))
  cat(summary_text, file = file_path, sep = "\n")
}



#*******************************************************************************
### 04.3 GLM's #### old


#### 2023 ####
columns_to_run_glm <- colnames(dat_2023[6:19])

# Create an empty list to store the GLM models
glm_models <- list()

# Iterate through the specified columns and run GLM
for (col_name in columns_to_run_glm) {
  formula_string <- paste(col_name, "~ treatment + (1 | block) + (1 | year) + (1 | crop)", sep = "")
  
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

