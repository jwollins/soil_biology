### Earthworm data
### J Collins 
### 2024-06-01
###
#*******************************************************************************
# STATS ####



#__________________________________________####
# Set wd ####

setwd(rstudioapi::getActiveProject())

getwd()




#__________________________________________####
# PACKAGES ####

source(file = "earthworm_scripts/01_packages.R")

library(lme4)



#__________________________________________####
# FUNCTIONS ####

source(file = "~/Documents/GitHub/phd_tools/fun_distribution_plots.R")

source(file = "~/Documents/GitHub/phd_tools/fun_glm_diagnostic_plots.R")

source(file = "~/Documents/GitHub/phd_tools/fun_overdispersion_test.R")




#__________________________________________####
# DATA ####

dat <- read_excel(path = "sym_link_soil_biology/Data/earthworm_data/earthworm_data.xlsx", sheet = 1) 

# remove NA's
dat[is.na(dat)] <- 0


dat$Year <- as.factor(dat$Year)
dat$Treatment <- as.factor(dat$Treatment)
dat$block <- as.factor(dat$block)


dat$Treatment <- factor(dat$Treatment, 
                             levels = c("Conservation","Conventional"))







#_________________________________________________####

# Test for overdispersion ####


# Specify column numbers to test for overdispersion
response_column_indices <- c(6:ncol(dat))  # Replace with the indices of the columns to test
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





#_________________________________________________####
# Variables  ####


# ~ Juveniles ####

names(dat)

# Calculates mean, sd, se and IC - block
summ <- 
  dat %>%
  group_by(Treatment, Year) %>%
  summarise( 
    n = n(),
    mean = mean(juveniles_m2, na.rm = TRUE),
    sd = sd(juveniles_m2, na.rm = TRUE)
  ) %>%
  mutate( se = sd/sqrt(n))  %>%
  mutate( ic = se * qt((1-0.05)/2 + .5, n-1)) %>% 
  arrange(Year)

summ

distribution_plots(data = dat, 
                   variable = dat$juveniles, 
                   colour = dat$juveniles)

ggsave(filename = "sym_link_soil_biology/Plots/distributions/dist_juveniles_m2.png", width = 10, height = 2.25)


# Find the smallest nonzero value in the dataset
small_const <- min(dat$juveniles[dat$juveniles > 0]) * 0.01  # 1% of the smallest value

# Add this small constant to avoid zeros
dat$juveniles_adj <- dat$juveniles + small_const


glm_model <- glmer(juveniles_adj ~ Treatment + (1 | Year), 
                   data = dat, 
                   family = Gamma(link = "log"), 
                   control = glmerControl(optimizer = "bobyqa"))



# View summary
summary(glm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ Treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)


diagnostic_plots_glm(model = glm_model)

ggsave(filename = "sym_link_soil_biology/Plots/model_diagnostics/model_diag_juveniles_m2.png", 
       width = 10, height = 3.5)






# ~ Epigeic ####

names(dat)

# Calculates mean, sd, se and IC - block
summ <- 
  dat %>%
  group_by(Treatment, Year) %>%
  summarise( 
    n = n(),
    mean = mean(epigeic_m2, na.rm = TRUE),
    sd = sd(epigeic_m2, na.rm = TRUE)
  ) %>%
  mutate( se = sd/sqrt(n))  %>%
  mutate( ic = se * qt((1-0.05)/2 + .5, n-1)) %>% 
  arrange(Year)

summ

distribution_plots(data = dat, 
                   variable = dat$epigeic_m2, 
                   colour = dat$epigeic_m2)

ggsave(filename = "sym_link_soil_biology/Plots/distributions/dist_epigeic_m2.png", width = 10, height = 2.25)


# Find the smallest nonzero value in the dataset
small_const <- min(dat$epigeic[dat$epigeic > 0]) * 0.01  # 1% of the smallest value

# Add this small constant to avoid zeros
dat$epigeic_adj <- dat$epigeic + small_const



# Fit the GLMM model
glm_model <- glmer(epigeic_adj ~ Treatment + (1 | Year), 
                   data = dat, 
                   family = Gamma(link = "log"), 
                   control = glmerControl(optimizer = "bobyqa"))



# View summary
summary(glm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ Treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)


diagnostic_plots_glm(model = glm_model)

ggsave(filename = "sym_link_soil_biology/Plots/model_diagnostics/model_diag_epigeic_m2.png", 
       width = 10, height = 3.5)







# ~ Endogeic ####


names(dat)

# Calculates mean, sd, se and IC - block
summ <- 
  dat %>%
  group_by(Treatment, Year) %>%
  summarise( 
    n = n(),
    mean = mean(endogeic_m2, na.rm = TRUE),
    sd = sd(endogeic_m2, na.rm = TRUE)
  ) %>%
  mutate( se = sd/sqrt(n))  %>%
  mutate( ic = se * qt((1-0.05)/2 + .5, n-1)) %>% 
  arrange(Year)

summ

distribution_plots(data = dat, 
                   variable = dat$endogeic_m2, 
                   colour = dat$endogeic_m2)

ggsave(filename = "sym_link_soil_biology/Plots/distributions/dist_endogeic_m2.png", width = 10, height = 2.25)


# Find the smallest nonzero value in the dataset
small_const <- min(dat$endogeic[dat$endogeic > 0]) * 0.01  # 1% of the smallest value

# Add this small constant to avoid zeros
dat$endogeic_adj <- dat$endogeic + small_const



# Fit the GLMM model
glm_model <- glmer(endogeic_adj ~ Treatment + (1 | Year), 
                   data = dat, 
                   family = Gamma(link = "log"), 
                   control = glmerControl(optimizer = "bobyqa"))



# View summary
summary(glm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ Treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)


diagnostic_plots_glm(model = glm_model)

ggsave(filename = "sym_link_soil_biology/Plots/model_diagnostics/model_diag_endogeic_m2.png", 
       width = 10, height = 3.5)







# ~ Anecic ####


names(dat)

# Calculates mean, sd, se and IC - block
summ <- 
  dat %>%
  group_by(Treatment, Year) %>%
  summarise( 
    n = n(),
    mean = mean(anecic_m2, na.rm = TRUE),
    sd = sd(anecic_m2, na.rm = TRUE)
  ) %>%
  mutate( se = sd/sqrt(n))  %>%
  mutate( ic = se * qt((1-0.05)/2 + .5, n-1)) %>% 
  arrange(Year)

summ

distribution_plots(data = dat, 
                   variable = dat$anecic_m2, 
                   colour = dat$anecic_m2)

ggsave(filename = "sym_link_soil_biology/Plots/distributions/dist_anecic_m2.png", width = 10, height = 2.25)


# Find the smallest nonzero value in the dataset
small_const <- min(dat$anecic[dat$anecic > 0]) * 0.01  # 1% of the smallest value

# Add this small constant to avoid zeros
dat$anecic_adj <- dat$anecic + small_const



# Fit the GLMM model
glm_model <- glmer(anecic_adj ~ Treatment + (1 | Year), 
                   data = dat, 
                   family = Gamma(link = "log"), 
                   control = glmerControl(optimizer = "bobyqa"))



# View summary
summary(glm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ Treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)


diagnostic_plots_glm(model = glm_model)

ggsave(filename = "sym_link_soil_biology/Plots/model_diagnostics/model_diag_endogeic_m2.png", 
       width = 10, height = 3.5)





# ~ Total worms ####


names(dat)

# Calculates mean, sd, se and IC - block
summ <- 
  dat %>%
  group_by(Treatment, Year) %>%
  summarise( 
    n = n(),
    mean = mean(total_worms_m2, na.rm = TRUE),
    sd = sd(total_worms_m2, na.rm = TRUE)
  ) %>%
  mutate( se = sd/sqrt(n))  %>%
  mutate( ic = se * qt((1-0.05)/2 + .5, n-1)) %>% 
  arrange(Year)

summ

distribution_plots(data = dat, 
                   variable = dat$total_worms_m2, 
                   colour = dat$total_worms_m2)

ggsave(filename = "sym_link_soil_biology/Plots/distributions/dist_total_worms_m2.png", width = 10, height = 2.25)


# Fit the GLMM model
glm_model <- glmer(total_worms_m2 ~ Treatment + (1 | Year), 
                   data = dat, 
                   family = Gamma(link = "log"), 
                   control = glmerControl(optimizer = "bobyqa"))



# View summary
summary(glm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ Treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)


diagnostic_plots_glm(model = glm_model)

ggsave(filename = "sym_link_soil_biology/Plots/model_diagnostics/model_diag_total_worms_m2.png", 
       width = 10, height = 3.5)








# #*******************************************************************************
# # Models 
# 
# library(MVN)
# 
# colnames(data)
# 
# dat <- data[10:ncol(data)]
# 
# # Perform normality test
# mvn_result <- mvn(
#   data = dat, 
#   mvnTest = "mardia",  # Mardia's test for multivariate normality
#   multivariatePlot = "qq",  # QQ plot matrix
#   univariateTest = "SW",  # Shapiro-Wilk test for univariate normality
#   univariatePlot = TRUE   # Generate univariate diagnostic plots
# )
# 
# # Print results
# print(mvn_result$univariateNormality)
# 
# mvn(data = data, mvnTest = "mardia", multivariatePlot = "persp")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #*******************************************************************************
# 
# 
# 
# 
# 
# 
# 
# #run_glm_and_pairwise(data = data, columns_to_run_glm = columns_of_interest)
# 
# run_lmer_and_pairwise(data = data, columns_to_run_lmer = columns_of_interest)
# 
# write.csv(x = lmer_summaries_df, file = "Statistics/lmer_summaries_df.csv")
# write.csv(x = pairwise_summaries_df, file = "Statistics/pairwise_summaries_df.csv")
# 
# 
# 
# 
# 
# #*******************************************************************************
# ### 04.3 GLMM's #### new
# 
# dir.create(path = "stats/glmm_outputs/")
# 
# # Fit a GLMM with Gamma distribution (for positively skewed data)
# glmm_model <- glmer(shoots_m2 ~ treatment + (1 | block) + (1 | crop) + (1 | year), 
#                     family = Gamma(link = "log"), 
#                     data = dat)
# 
# 
# 
# 
# 
# 
# columns_to_run_glm <- colnames(dat[,6:19])
# 
# # Create an empty list to store the GLM models
# glmm_models <- list()
# 
# # Iterate through the specified columns and run GLM
# for (col_name in columns_to_run_glm) {
#   formula_string <- paste(col_name, "~ Treatment + (1 | block) + (1 | Year) + (1 | crop)", sep = "")
#   
#   # Assuming a Gaussian family for this example, change the family argument accordingly
#   glmm_model <- glmer(formula_string, data = dat, family = Gamma(link = "log"))
#   
#   # Save the model summary to a text file
#   file_path <- file.path("stats/glmm_outputs/", paste0("glmm_summary_", gsub(" ", "_", col_name), ".txt"))
#   summary_text <- capture.output(summary(glmm_model))
#   cat(summary_text, file = file_path, sep = "\n")
# }
# 
# 
# 
# #*******************************************************************************
# ### 04.3 GLM's #### old
# 
# 
# #### 2023 ####
# columns_to_run_glm <- colnames(dat_2023[6:19])
# 
# # Create an empty list to store the GLM models
# glm_models <- list()
# 
# # Iterate through the specified columns and run GLM
# for (col_name in columns_to_run_glm) {
#   formula_string <- paste(col_name, "~ treatment + (1 | block) + (1 | year) + (1 | crop)", sep = "")
#   
#   # Assuming a Gaussian family for this example, change the family argument accordingly
#   glm_model <- glm(formula_string, data = dat_2023)
#   
#   # Save the model summary to a text file
#   file_path <- file.path("stats/glm_outputs/2023/", paste0("glm_summary_", gsub(" ", "_", col_name), ".txt"))
#   summary_text <- capture.output(summary(glm_model))
#   cat(summary_text, file = file_path, sep = "\n")
# }
# 
# 
# 
# 
# #### 2024 ####
# 
# columns_to_run_glm <- colnames(dat_2024[6:19])
# 
# # Create an empty list to store the GLM models
# glm_models <- list()
# 
# # Iterate through the specified columns and run GLM
# for (col_name in columns_to_run_glm) {
#   formula_string <- paste(col_name, "~ treatment + block", sep = "")
#   
#   # Assuming a Gaussian family for this example, change the family argument accordingly
#   glm_model <- glm(formula_string, data = dat_2024)
#   
#   # Save the model summary to a text file
#   file_path <- file.path("stats/glm_outputs/2024/",
#                          paste0("glm_summary_", gsub(" ", "_", col_name), ".txt"))
#   summary_text <- capture.output(summary(glm_model))
#   cat(summary_text, file = file_path, sep = "\n")
# }

