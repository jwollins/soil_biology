# 04 Distribution ####
## WHO: JOE COLLINS
## WHAT: QBS data normality test and distribution plots
## LAST EDIT: 2024-10-16
####

#*******************************************************************************
# DISTRIBUTIONS ####

setwd(dir = "~/Documents/GitHub/soil_biology/")


#*******************************************************************************
# PACKAGES ####

source(file = "earthworm_scripts/01_packages.R")



#*******************************************************************************
# LOAD DATA ####

source(file = "~/Documents/GitHub/soil_biology/earthworm_scripts/02_data.R")

source("scripts/functions/fun_lm_by_year.R")
source("scripts/functions/fun_glm_by_year.R")
source(file = "~/Documents/GitHub/phd_tools/fun_distribution_tests.R")
source(file = "scripts/functions/shapiro_wilks.R")
source(file = "scripts/functions/fun_bartlett_test.R")
source(file = "~/Documents/GitHub/phd_tools/fun_overdispersion_test.R")
source(file = "~/Documents/GitHub/phd_tools/fun_glm_diagnostic_plots.R")




setwd(dir = "~/OneDrive - Harper Adams University/Data/Soil/worms/")



#*******************************************************************************
# ABUNDANCE ####



## prep data ####

# Load the data
dat <- all_dat

#rename column by name
dat <- dat %>% rename_at('year', ~'Year')
#rename column by name
dat <- dat %>% rename_at('treatment', ~'Treatment')


dat <- filter(dat, Year == 2022 | Year == 2023)

# Specify the columns of interest (columns 7 to 55 in this case)
columns_of_interest <- names(dat)[6:ncol(dat)]

# Initialize an empty data frame to store results
result_df <- data.frame()


# qucik function checks of distributions 
check_guassian(data = dat, columns_to_check = columns_of_interest)
check_gamma_distribution(dat = data, columns_to_check = columns_of_interest)
check_exponential_distribution(dat = data, columns_to_check = columns_of_interest)
check_poisson_multiple(data = dat, columns = columns_of_interest)





# Specify column numbers to test for overdispersion
response_column_indices <- c(6:19)  # Replace with the indices of the columns to test
response_columns <- colnames(dat)[response_column_indices]  # Get the column names

# Specify explanatory variables
explanatory_columns <- c("Year", "crop", "Treatment")

# Initialize a results list
results_list <- lapply(response_columns, function(response_column) {
  test_overdispersion(dat, response_column, explanatory_columns)
})

# Convert results into a dataframe
results_df <- do.call(rbind, lapply(results_list, as.data.frame))

# Print results
print(results_df)









#*******************************************************************************
### 01 SHAPIRO WILK ####



check_normality(data = dat, columns_of_interest = columns_of_interest)

# Save the results to a CSV file
if (!dir.exists("stats/normality/")) {
  dir.create("stats/normality/", recursive = TRUE)
}

write.csv(result_df, file = "stats/normality/abundance_shapiro_results_by_year.csv", 
          row.names = FALSE)









#*******************************************************************************
### 02 BARTLETT TEST ####




check_variance_homogeneity(data = dat, columns_of_interest = columns_of_interest)

# Save the results to a CSV file
if (!dir.exists("stats/normality/")) {
  dir.create("stats/normality/", recursive = TRUE)
}
write.csv(result_df, file = "stats/normality/abundance_bartlett_results_by_year_treatment.csv", 
          row.names = FALSE)





#*******************************************************************************
# DIST CHECK ####

# NORMAILITY

# NO TRANS 

library(MVN)

colnames(dat)

dat_cols <- dat[,c(13:17, 19)]

# Perform normality test
mvn_result <- mvn(
  data = dat_cols, 
  mvnTest = "mardia",  # Mardia's test for multivariate normality
  multivariatePlot = "qq",  # QQ plot matrix
  univariateTest = "SW",  # Shapiro-Wilk test for univariate normality
  univariatePlot = TRUE   # Generate univariate diagnostic plots
)

# Print results
print(mvn_result$univariateNormality)

mvn_result <- as.data.frame(mvn_result$univariateNormality)

write.csv(x = mvn_result, file = "stats/normality/mvn_normality.csv")






# SQRT TRANS 

# Perform normality test
mvn_result <- mvn(
  data = dat_cols, 
  mvnTest = "mardia",  # Mardia's test for multivariate normality
  multivariatePlot = "qq",  # QQ plot matrix
  transform = "sqrt",
  univariateTest = "SW",  # Shapiro-Wilk test for univariate normality
  univariatePlot = TRUE   # Generate univariate diagnostic plots
)

# Print results
print(mvn_result$univariateNormality)








#*******************************************************************************
# DIST PLOTS ####


library(tidyr)
library(ggplot2)


# Loop through column names and print with column numbers
for (i in seq_along(colnames(dat))) {
  cat("Column", i, ":", colnames(dat)[i], "\n")
}



## standardise the data ####

# Standardize the data (z-scores)
dat_standardized <- as.data.frame(scale(dat[,c(13:17, 19)]))

# dat_standardized <- sqrt(dat_standardized)

# Reshape to long format
dat_long <- dat_standardized %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")




dist_plot <- ggplot(data = dat_long, 
                    aes(x = Value, 
                        color = Variable, 
                        fill = Variable)) +
  geom_density(alpha = 0.1) +
  labs(title = "Standardised Distributions", x = "Standardised Value", y = "Density") +
  theme_minimal()


# Plot with jittered histogram bars using position_dodge
hist_plot <- ggplot(data = dat_long, 
                    aes(x = Value, 
                        fill = Variable)) +
  geom_histogram(position = position_dodge(width = 0.5), 
                 bins = 10, alpha = 0.6) +
  labs(title = "Standardised Histogram", x = "Standardised Value", y = "Count") +
  theme_minimal()

# Create Q-Q plot
qq_plot <- ggplot(data = dat_long, 
                  aes(sample = Value, color = Variable)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Standardized Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()


ggarrange(dist_plot, hist_plot, qq_plot,
          ncol = 3, 
          common.legend = TRUE, 
          legend = "bottom", 
          labels = c("A", "B", "C"))

ggsave(filename = "plots/distribution_plots_worms.png", width = 10, height = 4)






#*******************************************************************************
# STATS - MODELS ####


# get normality results (to pick appropiate test)
print(mvn_result$univariateNormality)

table(dat$block)
table(dat$crop)
table(dat$Year)

#*******************************************************************************
##  EXAMPLE - MODEL WITH RANDOM EFFECTS ####

# Fit the model with Poisson and random effects using glmer
glmer_model <- glmer(anecic_m2 ~ Treatment + 
                       (1 | block) + 
                       (1 | crop) + 
                       (1 | Year),
                     family = poisson(link = "log"), 
                     data = dat)

isSingular(glmer_model)


summary(glmer_model)

diagnostic_plots_glm(model = glmer_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glmer_model, pairwise ~ Treatment)
# View the results of pairwise comparisons
summary(pairwise_comparisons)




#*******************************************************************************
## EXAMPLE - WITHOUT RANDOM EFFECTS ####



# Load lme4 package
library(lme4)

# Fit a quasi-Poisson GLM in base R
glm_model_quasi <- glm(anecic_m2 ~ Treatment, 
                       family = quasipoisson(link = "log"), 
                       data = dat, 
                       na.action = na.exclude)

# Check for overdispersion
summary(glm_model_quasi)

diagnostic_plots_glm(model = glm_model_quasi)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model_quasi, pairwise ~ Treatment)
# View the results of pairwise comparisons
summary(pairwise_comparisons)





#*******************************************************************************
# RUN MODELS ####

# get normality results (to pick appropiate test)
print(mvn_result$univariateNormality)

# make all integers for model assessment 
dat[,6:ncol(dat)] <- round(x = dat[,6:ncol(dat)], digits = 0)

# check for non-integers
table(dat[,6:ncol(dat)] %% 1 == 0)  # Check if any non-integer values exist

# TRUE = no non-integers




#*******************************************************************************
## total_worms_m2 ####

variab <- dat$total_worms_m2
var_name <- "total_worms_m2"

# Fit a quasi-Poisson GLM in base R
glm_model_quasi <- glm(variab ~ Treatment, 
                       family = quasipoisson(link = "log"), 
                       data = dat, 
                       na.action = na.exclude)

# Check for overdispersion
summary(glm_model_quasi)

diagnostic_plots_glm(model = glm_model_quasi)

ggsave(filename = paste0("plots/", var_name, ".png"), width = 10, height = 4)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model_quasi, pairwise ~ Treatment)
# View the results of pairwise comparisons
summary(pairwise_comparisons)

# Open a connection to a text file
sink(file = paste0("stats/glmm_outputs/", var_name, ".txt"))

# Save the summary of the GLM model
cat(paste0(var_name,"\n"))
cat("GLM Model Summary:\n")
summary(glm_model_quasi)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model_quasi, pairwise ~ Treatment)

# Save the results of pairwise comparisons
cat("\nPairwise Comparisons Summary:\n")
summary(pairwise_comparisons)

# Close the file connection
sink()




#*******************************************************************************
## mass_g_m2 ####


variab <- dat$mass_g_m2
var_name <- "mass_g_m2"

# Fit a quasi-Poisson GLM in base R
glm_model_quasi <- glm(variab ~ Treatment, 
                       family = quasipoisson(link = "log"), 
                       data = dat, 
                       na.action = na.exclude)

# Check for overdispersion
summary(glm_model_quasi)

diagnostic_plots_glm(model = glm_model_quasi)

ggsave(filename = paste0("plots/", var_name, ".png"), width = 10, height = 4)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model_quasi, pairwise ~ Treatment)
# View the results of pairwise comparisons
summary(pairwise_comparisons)

# Open a connection to a text file
sink(file = paste0("stats/glmm_outputs/", var_name, ".txt"))

# Save the summary of the GLM model
cat(paste0(var_name,"\n"))
cat("GLM Model Summary:\n")
summary(glm_model_quasi)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model_quasi, pairwise ~ Treatment)

# Save the results of pairwise comparisons
cat("\nPairwise Comparisons Summary:\n")
summary(pairwise_comparisons)

# Close the file connection
sink()






#*******************************************************************************
## epigeic_m2 ####

variab <- dat$epigeic_m2
var_name <- "epigeic_m2"

# Fit a quasi-Poisson GLM in base R
glm_model_quasi <- glm(variab ~ Treatment, 
                       family = quasipoisson(link = "log"), 
                       data = dat, 
                       na.action = na.exclude)

# Check for overdispersion
summary(glm_model_quasi)

diagnostic_plots_glm(model = glm_model_quasi)

ggsave(filename = paste0("plots/", var_name, ".png"), width = 10, height = 4)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model_quasi, pairwise ~ Treatment)
# View the results of pairwise comparisons
summary(pairwise_comparisons)

# Open a connection to a text file
sink(file = paste0("stats/glmm_outputs/", var_name, ".txt"))

# Save the summary of the GLM model
cat(paste0(var_name,"\n"))
cat("GLM Model Summary:\n")
summary(glm_model_quasi)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model_quasi, pairwise ~ Treatment)

# Save the results of pairwise comparisons
cat("\nPairwise Comparisons Summary:\n")
summary(pairwise_comparisons)

# Close the file connection
sink()





#*******************************************************************************
## endogeic_m2 ####

variab <- dat$endogeic_m2
var_name <- "endogeic_m2"

# Fit a quasi-Poisson GLM in base R
glm_model_quasi <- glm(variab ~ Treatment, 
                       family = quasipoisson(link = "log"), 
                       data = dat, 
                       na.action = na.exclude)

# Check for overdispersion
summary(glm_model_quasi)

diagnostic_plots_glm(model = glm_model_quasi)

ggsave(filename = paste0("plots/", var_name, ".png"), width = 10, height = 4)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model_quasi, pairwise ~ Treatment)
# View the results of pairwise comparisons
summary(pairwise_comparisons)

# Open a connection to a text file
sink(file = paste0("stats/glmm_outputs/", var_name, ".txt"))

# Save the summary of the GLM model
cat(paste0(var_name,"\n"))
cat("GLM Model Summary:\n")
summary(glm_model_quasi)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model_quasi, pairwise ~ Treatment)

# Save the results of pairwise comparisons
cat("\nPairwise Comparisons Summary:\n")
summary(pairwise_comparisons)

# Close the file connection
sink()





#*******************************************************************************
## anecic_m2 ####

variab <- dat$anecic_m2
var_name <- "anecic_m2"

# Fit a quasi-Poisson GLM in base R
glm_model_quasi <- glm(variab ~ Treatment, 
                       family = quasipoisson(link = "log"), 
                       data = dat, 
                       na.action = na.exclude)

# Check for overdispersion
summary(glm_model_quasi)

diagnostic_plots_glm(model = glm_model_quasi)

ggsave(filename = paste0("plots/", var_name, ".png"), width = 10, height = 4)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model_quasi, pairwise ~ Treatment)
# View the results of pairwise comparisons
summary(pairwise_comparisons)

# Open a connection to a text file
sink(file = paste0("stats/glmm_outputs/", var_name, ".txt"))

# Save the summary of the GLM model
cat(paste0(var_name,"\n"))
cat("GLM Model Summary:\n")
summary(glm_model_quasi)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model_quasi, pairwise ~ Treatment)

# Save the results of pairwise comparisons
cat("\nPairwise Comparisons Summary:\n")
summary(pairwise_comparisons)

# Close the file connection
sink()







#*******************************************************************************
## juveniles_m2 ####

variab <- dat$juveniles_m2
var_name <- "juveniles_m2"

# Fit a quasi-Poisson GLM in base R
glm_model_quasi <- glm(variab ~ Treatment, 
                       family = quasipoisson(link = "log"), 
                       data = dat, 
                       na.action = na.exclude)

# Check for overdispersion
summary(glm_model_quasi)

diagnostic_plots_glm(model = glm_model_quasi)

ggsave(filename = paste0("plots/", var_name, ".png"), width = 10, height = 4)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model_quasi, pairwise ~ Treatment)
# View the results of pairwise comparisons
summary(pairwise_comparisons)

# Open a connection to a text file
sink(file = paste0("stats/glmm_outputs/", var_name, ".txt"))

# Save the summary of the GLM model
cat(paste0(var_name,"\n"))
cat("GLM Model Summary:\n")
summary(glm_model_quasi)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model_quasi, pairwise ~ Treatment)

# Save the results of pairwise comparisons
cat("\nPairwise Comparisons Summary:\n")
summary(pairwise_comparisons)

# Close the file connection
sink()



#*******************************************************************************




#*******************************************************************************


#*******************************************************************************


#*******************************************************************************



#*******************************************************************************

#*******************************************************************************

#*******************************************************************************















