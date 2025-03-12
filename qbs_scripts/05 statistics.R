## 05 STATS ####
## WHO: JOE COLLINS
## WHAT: QBS statistics
## LAST EDIT: 25/01/2023
####


#__________________________________________####
# Set wd ####

setwd(rstudioapi::getActiveProject())

getwd()




#__________________________________________####
# PACKAGES ####

source(file = "qbs_scripts/02 packages.R")





#__________________________________________####
# FUNCTIONS ####

source(file = "~/Documents/GitHub/phd_tools/fun_distribution_plots.R")

source(file = "~/Documents/GitHub/phd_tools/fun_glm_diagnostic_plots.R")

source(file = "~/Documents/GitHub/phd_tools/fun_overdispersion_test.R")





#__________________________________________####
# Data ####


tax_dat <- read.csv(file = "sym_link_soil_biology/Data/processed_data/taxanomic_order_data.csv")

# remove NA's
tax_dat[is.na(tax_dat)] <- 0


tax_dat$Year <- as.factor(tax_dat$Year)
tax_dat$Treatment <- as.factor(tax_dat$Treatment)
tax_dat$Block <- as.factor(tax_dat$Block)


tax_dat$Treatment <- factor(tax_dat$Treatment, 
                            levels = c("Conservation","Conventional"))







#_________________________________________________####

# Test for overdispersion ####



# Specify column numbers to test for overdispersion
response_column_indices <- c(8:ncol(tax_dat))  # Replace with the indices of the columns to test
response_columns <- colnames(tax_dat)[response_column_indices]  # Get the column names

# Convert to numeric (ensure no conversion errors)
tax_dat[, response_column_indices] <- lapply(tax_dat[, response_column_indices], function(x) {
  if(is.character(x) | is.factor(x)) {
    as.numeric(as.character(x))
  } else {
    x
  }
})


# Specify explanatory variables
explanatory_columns <- c("Year", "Treatment")

# Initialize a results list
results_list <- lapply(response_columns, function(response_column) {
  test_overdispersion(tax_dat, response_column, explanatory_columns)
})

# Convert results into a dataframe
results_df <- do.call(rbind, lapply(results_list, as.data.frame))

# Print results
print(results_df)









#_________________________________________________####
# Variables  ####


# ~ Chelicerata ####

names(tax_dat)

dat <- tax_dat

# Calculates mean, sd, se and IC - block
summ <- 
  dat %>%
  group_by(Treatment, Year) %>%
  summarise( 
    n = n(),
    mean = mean(Chelicerata_m2, na.rm = TRUE),
    sd = sd(Chelicerata_m2, na.rm = TRUE)
  ) %>%
  mutate( se = sd/sqrt(n))  %>%
  mutate( ic = se * qt((1-0.05)/2 + .5, n-1)) %>% 
  arrange(Year)

summ

distribution_plots(data = dat, 
                   variable = dat$Chelicerata, 
                   colour = dat$Chelicerata)

ggsave(filename = "sym_link_soil_biology/Plots/distributions/dist_Chelicerata_m2.png", width = 10, height = 2.25)


quasi_model <- glm(Chelicerata ~ Treatment + as.factor(Year) + as.factor(Block), 
                   data = dat, 
                   family = quasipoisson(link = "log"))

# View summary
summary(quasi_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(quasi_model, pairwise ~ Treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)


diagnostic_plots_glm(model = quasi_model)

ggsave(filename = "sym_link_soil_biology/Plots/model_diagnostics/model_diag_Chelicerata.png", 
       width = 10, height = 2.25)



# ~ check Zero-Inflated Model ####

# dispersion_quasi <- summary(quasi_model)$dispersion
# print(dispersion_quasi)
# 
# boxplot(residuals(quasi_model), main = "Residuals Boxplot")
# 
# nb_zi_model <- glmmTMB(Chelicerata ~ Treatment + as.factor(Year) + as.factor(Block), 
#                        data = dat, 
#                        family = nbinom2(link = "log"), 
#                        ziformula = ~1)  
# summary(nb_zi_model)
# 
# # Run pairwise comparisons for the 'Treatment' factor
# pairwise_comparisons <- emmeans(nb_zi_model, pairwise ~ Treatment)
# 
# # View the results of pairwise comparisons
# summary(pairwise_comparisons)
# 
# 
# # Simulate residuals from the Zero-Inflated Model
# sim_res <- simulateResiduals(fittedModel = nb_zi_model, plot = FALSE)
# 
# # Extract residuals and fitted values
# residuals_df <- data.frame(
#   Residuals = residuals(sim_res),
#   Fitted = fitted(nb_zi_model),
#   SimulatedResiduals = sim_res$scaledResiduals
# )
# 
# ggplot(residuals_df, aes(x = Fitted, y = Residuals)) +
#   geom_point(alpha = 0.5) +  # Scatterplot of residuals
#   geom_smooth(method = "loess", se = FALSE, color = "red", linetype = "dashed") +  # LOESS trend line
#   theme_minimal() +
#   labs(title = "Residuals vs. Fitted Values",
#        x = "Fitted Values",
#        y = "Residuals") +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "blue")  # Horizontal reference line
# 
# 
# plotResiduals(sim_res, form = fitted(nb_zi_model))
# 
# qqnorm(residuals(sim_res), main = "Q-Q Plot of Residuals")
# qqline(residuals(sim_res))
# 
# ggplot(residuals_df, aes(sample = Residuals)) +
#   stat_qq() +  # Q-Q plot
#   stat_qq_line() +  # Reference line
#   theme_minimal() +
#   labs(title = "Q-Q Plot of Residuals",
#        x = "Theoretical Quantiles",
#        y = "Sample Quantiles")






# ~ Crustacea ####

names(tax_dat)

dat <- tax_dat

# Calculates mean, sd, se and IC - block
summ <- 
  dat %>%
  group_by(Treatment, Year) %>%
  summarise( 
    n = n(),
    mean = mean(Crustacea_m2, na.rm = TRUE),
    sd = sd(Crustacea_m2, na.rm = TRUE)
  ) %>%
  mutate( se = sd/sqrt(n))  %>%
  mutate( ic = se * qt((1-0.05)/2 + .5, n-1)) %>% 
  arrange(Year)

summ

distribution_plots(data = dat, 
                   variable = dat$Crustacea, 
                   colour = dat$Crustacea)

ggsave(filename = "sym_link_soil_biology/Plots/distributions/dist_Crustacea_m2.png", width = 10, height = 2.25)


# quasi_model <- glm(Crustacea ~ Treatment + as.factor(Year) + as.factor(Block), 
#                    data = dat, 
#                    family = quasipoisson(link = "log"))
# 
# # View summary
# summary(quasi_model)
# 
# # Run pairwise comparisons for the 'Treatment' factor
# pairwise_comparisons <- emmeans(quasi_model, pairwise ~ Treatment)
# 
# # View the results of pairwise comparisons
# summary(pairwise_comparisons)
# 
# 
# diagnostic_plots_glm(model = quasi_model)
# 
# ggsave(filename = "sym_link_soil_biology/Plots/model_diagnostics/model_diag_Chelicerata.png", 
#        width = 10, height = 2.25)





# ~ Myriapoda ####

names(tax_dat)

dat <- tax_dat

# Calculates mean, sd, se and IC - block
summ <- 
  dat %>%
  group_by(Treatment, Year) %>%
  summarise( 
    n = n(),
    mean = mean(Myriapoda_m2, na.rm = TRUE),
    sd = sd(Myriapoda_m2, na.rm = TRUE)
  ) %>%
  mutate( se = sd/sqrt(n))  %>%
  mutate( ic = se * qt((1-0.05)/2 + .5, n-1)) %>% 
  arrange(Year)

summ

distribution_plots(data = dat, 
                   variable = dat$Myriapoda, 
                   colour = dat$Myriapoda)

ggsave(filename = "sym_link_soil_biology/Plots/distributions/dist_Myriapoda_m2.png", width = 10, height = 2.25)


quasi_model <- glm(Myriapoda ~ Treatment + as.factor(Year) + as.factor(Block), 
                   data = dat, 
                   family = quasipoisson(link = "log"))

# View summary
summary(quasi_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(quasi_model, pairwise ~ Treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)


diagnostic_plots_glm(model = quasi_model)

ggsave(filename = "sym_link_soil_biology/Plots/model_diagnostics/model_diag_Myriapoda.png", 
       width = 10, height = 2.25)






# ~ Hexapoda ####

names(tax_dat)

dat <- tax_dat

# Calculates mean, sd, se and IC - block
summ <- 
  dat %>%
  group_by(Treatment, Year) %>%
  summarise( 
    n = n(),
    mean = mean(Hexapoda_m2, na.rm = TRUE),
    sd = sd(Hexapoda_m2, na.rm = TRUE)
  ) %>%
  mutate( se = sd/sqrt(n))  %>%
  mutate( ic = se * qt((1-0.05)/2 + .5, n-1)) %>% 
  arrange(Year)

summ

distribution_plots(data = dat, 
                   variable = dat$Hexapoda, 
                   colour = dat$Hexapoda)

ggsave(filename = "sym_link_soil_biology/Plots/distributions/dist_Hexapoda_m2.png", width = 10, height = 2.25)


quasi_model <- glm(Hexapoda ~ Treatment + as.factor(Year) + as.factor(Block), 
                   data = dat, 
                   family = quasipoisson(link = "log"))

# View summary
summary(quasi_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(quasi_model, pairwise ~ Treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)


diagnostic_plots_glm(model = quasi_model)

ggsave(filename = "sym_link_soil_biology/Plots/model_diagnostics/model_diag_Hexapoda.png", 
       width = 10, height = 2.25)







#__________________________________________####
# Old code ####

# # Check for overdispersion
# mean_count <- mean(qbs_dat$qbs_score)
# var_count <- var(qbs_dat$qbs_score)
# overdispersion <- var_count / mean_count
# print(overdispersion)  # If much greater than 1, overdispersion is present
# 
# 
# # my example 
# 
# # Fit the linear mixed-effects model
# lm <- lmer(formula = Shannon_Index ~ Treatment + (1 | Block) + (1 | Year), data = shannon_data)
# 
# # Get the summary which includes p-values
# summary(lm)
# 
# # You can also use anova to get Type III sums of squares and p-values
# anova(lm)
# 
# # Eds example
# 
# lme.mites <- lme(log(Acari.totnum) ~ treatment * factor(year), 
#                  random = ~1|block/year,
#                  data = biochar)
# 
# 
# hist(qbs_dat$qbs_score)
# 
# 
# test_lm <- lmer(formula = log(qbs_score) ~ Treatment * Year + (1|Block), data = qbs_dat)
# 
# summary(test_lm)
# 
# anova(test_lm)
# 
# 
# ### Test code 
# 
# 
# 
# 
# # test for over dispersrion
# 
# glmer_mod <- glmer(qbs_score ~ Treatment + (1|Year), family = poisson, data = qbs_dat)
# summary(glmer_mod)
# 
# # Extract residual deviance and degrees of freedom
# deviance <- deviance(glmer_mod)
# df <- df.residual(glmer_mod)
# 
# # Print the values
# cat("Residual Deviance:", deviance, "\n")
# cat("Degrees of Freedom:", df, "\n")
# 
# 
# # Calculate overdispersion factor
# overdispersion_factor <- deviance / df
# cat("Overdispersion Factor:", overdispersion_factor, "\n")
# # this model has massive over dispersion therefore quasipoisson is needed. 
# 
# 
# 
# 
# 
# # Fit a Quasi-Poisson GLM
# quasi_model <- glm(qbs_score ~ Treatment * Year, family = quasipoisson(link = "log"), data = qbs_dat)
# summary(quasi_model)
# 
# # Calculate EMMs for Treatment
# emm_results <- emmeans(quasi_model, ~ Treatment | Year)
# 
# # Pairwise comparisons
# pairwise_results <- pairs(emm_results)
# summary(pairwise_results)
# 
# # Pairwise comparisons with Tukey adjustment
# pairwise_results_tukey <- pairs(emm_results, adjust = "tukey")
# summary(pairwise_results_tukey)
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
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ### 01 ABUNDANCE ####
# 
# library(emmeans)
# library(broom)
# 
# dat <- all_dat_count
# 
# # Create a directory to save the model summaries and pairwise comparisons
# dir.create("Statistics/glm_outputs/abundance/", showWarnings = FALSE)
# 
# columns_to_run_glm <- colnames(dat[, 18:ncol(dat)])
# 
# # Create empty lists to store the GLM model summaries and pairwise comparisons
# glm_summaries <- list()
# pairwise_summaries <- list()
# 
# # Iterate through the specified columns and run GLM + pairwise comparisons
# for (col_name in columns_to_run_glm) {
#   formula_string <- paste(col_name, "~ Treatment * Year", sep = "")
#   
#   # Run the GLM model (assuming a quasipoisson family)
#   glm_model <- glm(formula_string, data = dat, family = quasipoisson)
#   
#   # Use broom::tidy() to create a tidy summary of the model
#   tidy_summary <- broom::tidy(glm_model)
#   
#   # Add a column for the variable name
#   tidy_summary$variable <- col_name
#   
#   # Round numeric columns to 2 decimal places
#   tidy_summary$estimate <- round(tidy_summary$estimate, 2)
#   tidy_summary$std.error <- round(tidy_summary$std.error, 2)
#   tidy_summary$statistic <- round(tidy_summary$statistic, 2)
#   tidy_summary$p.value <- round(tidy_summary$p.value, 2)
#   
#   # Add significance stars based on p-value
#   tidy_summary$p.signif <- ifelse(tidy_summary$p.value < 0.001, "***",
#                                   ifelse(tidy_summary$p.value < 0.01, "**",
#                                          ifelse(tidy_summary$p.value < 0.05, "*", "")))
#   
#   # Append the tidy summary to the list
#   glm_summaries[[col_name]] <- tidy_summary
#   
#   # Perform pairwise comparisons using emmeans
#   emmeans_res <- emmeans(glm_model, pairwise ~ Treatment | Year)
#   pairwise_summary <- as.data.frame(summary(emmeans_res$contrasts)) # Get pairwise comparisons summary
#   
#   # Check the names of the columns in pairwise_summary
#   print(colnames(pairwise_summary))  # To inspect available columns (e.g., t.ratio, z.ratio)
#   
#   # Add a column for the variable name in pairwise comparisons
#   pairwise_summary$variable <- col_name
#   
#   # Round numeric columns, checking for correct column names
#   pairwise_summary$estimate <- round(pairwise_summary$estimate, 2)
#   pairwise_summary$SE <- round(pairwise_summary$SE, 2)
#   
#   # Check for existence of t.ratio or z.ratio and handle accordingly
#   if ("t.ratio" %in% colnames(pairwise_summary)) {
#     pairwise_summary$t.ratio <- round(pairwise_summary$t.ratio, 2)
#   } else if ("z.ratio" %in% colnames(pairwise_summary)) {
#     pairwise_summary$z.ratio <- round(pairwise_summary$z.ratio, 2)
#   }
#   
#   # Round the p-values and add significance stars
#   pairwise_summary$p.value <- round(pairwise_summary$p.value, 2)
#   pairwise_summary$p.signif <- ifelse(pairwise_summary$p.value < 0.001, "***",
#                                       ifelse(pairwise_summary$p.value < 0.01, "**",
#                                              ifelse(pairwise_summary$p.value < 0.05, "*", "")))
#   
#   
#   # Append the pairwise summary to the list
#   pairwise_summaries[[col_name]] <- pairwise_summary
# }
# 
# # Combine all GLM summaries into one data frame
# combined_glm_summaries <- do.call(rbind, glm_summaries)
# 
# # Combine all pairwise summaries into one data frame
# combined_pairwise_summaries <- do.call(rbind, pairwise_summaries)
# 
# # Write the combined GLM summaries to a CSV file
# write.csv(combined_glm_summaries, file = "Statistics/glm_outputs/abundance/glm_summaries.csv", row.names = FALSE)
# 
# # Write the combined pairwise summaries to a separate CSV file
# write.csv(combined_pairwise_summaries, file = "Statistics/glm_outputs/abundance/pairwise_summaries.csv", row.names = FALSE)
# 
# 
# 
# #### 01.1 abundance model ####
# 
# 
# # Poisson regression model
# model_poisson <- glm(total_abundance ~ Treatment * Year, 
#                      family = quasipoisson(link = "log"), 
#                      data = total_abun_dat)
# summary(model_poisson)
# 
# # Pairwise comparisons
# emmeans(model_poisson, pairwise ~ Treatment | Year)
# 
# 
# # Predicted values from model
# total_abun_dat$predicted <- predict(model_poisson, type = "response")
# 
# ggplot(total_abun_dat, aes(x = Year, y = predicted, color = Treatment)) +
#   geom_line(aes(group = Treatment)) +
#   geom_point() +
#   theme_minimal() +
#   labs(title = "Predicted Microarthropod Counts by Treatment and Year", y = "Predicted Count")
# 
# 
# # Example ggplot code including observed data points with jittering and custom colors
# ggplot(total_abun_dat, aes(x = Year)) +
#   geom_line(aes(y = predicted, color = Treatment, group = Treatment), size = 1) +  # Predicted lines
#   geom_point(aes(y = total_abundance, color = Treatment), 
#              size = 2, 
#              position = position_jitter(width = 0.2, height = 0), size = 3) +  # Jittered observed points
#   theme_bw() +
#   labs(title = element_blank(),
#        y = "Soil micro arthropod abundance") +
#   scale_color_manual(values = c("Conservation" = "turquoise3", "Conventional" = "tomato2")) +  # Custom colors
#   theme(legend.title = element_blank(), 
#         legend.position = "bottom", 
#         text = element_text(size=15))  # Optional: Remove legend title for cleaner look
# 
# dir.create(path = "Plots/models/")
# 
# ggsave(filename = "total_abundance_glm_plot.png", 
#        path = "Plots/models/", 
#        width = 5, 
#        height = 5)
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
# 
#   
# 
# 
# ### 02 TAX ORDER ####
# 
# dat <- tax_ord_dat
# 
# # Create a directory to save the model summaries and pairwise comparisons
# dir.create("Statistics/glm_outputs/tax_order/", showWarnings = FALSE)
# 
# columns_to_run_glm <- colnames(dat[, 18:ncol(dat)])
# 
# # Create empty lists to store the GLM model summaries and pairwise comparisons
# glm_summaries <- list()
# pairwise_summaries <- list()
# 
# # Iterate through the specified columns and run GLM + pairwise comparisons
# for (col_name in columns_to_run_glm) {
#   formula_string <- paste(col_name, "~ Treatment * Year", sep = "")
#   
#   # Run the GLM model (assuming a quasipoisson family)
#   glm_model <- glm(formula_string, data = dat, family = quasipoisson)
#   
#   # Use broom::tidy() to create a tidy summary of the model
#   tidy_summary <- broom::tidy(glm_model)
#   
#   # Add a column for the variable name
#   tidy_summary$variable <- col_name
#   
#   # Round numeric columns to 2 decimal places
#   tidy_summary$estimate <- round(tidy_summary$estimate, 2)
#   tidy_summary$std.error <- round(tidy_summary$std.error, 2)
#   tidy_summary$statistic <- round(tidy_summary$statistic, 2)
#   tidy_summary$p.value <- round(tidy_summary$p.value, 2)
#   
#   # Add significance stars based on p-value
#   tidy_summary$p.signif <- ifelse(tidy_summary$p.value < 0.001, "***",
#                                   ifelse(tidy_summary$p.value < 0.01, "**",
#                                          ifelse(tidy_summary$p.value < 0.05, "*", "")))
#   
#   # Append the tidy summary to the list
#   glm_summaries[[col_name]] <- tidy_summary
#   
#   # Perform pairwise comparisons using emmeans
#   emmeans_res <- emmeans(glm_model, pairwise ~ Treatment | Year)
#   pairwise_summary <- as.data.frame(summary(emmeans_res$contrasts)) # Get pairwise comparisons summary
#   
#   # Check the names of the columns in pairwise_summary
#   print(colnames(pairwise_summary))  # To inspect available columns (e.g., t.ratio, z.ratio)
#   
#   # Add a column for the variable name in pairwise comparisons
#   pairwise_summary$variable <- col_name
#   
#   # Round numeric columns, checking for correct column names
#   pairwise_summary$estimate <- round(pairwise_summary$estimate, 2)
#   pairwise_summary$SE <- round(pairwise_summary$SE, 2)
#   
#   # Check for existence of t.ratio or z.ratio and handle accordingly
#   if ("t.ratio" %in% colnames(pairwise_summary)) {
#     pairwise_summary$t.ratio <- round(pairwise_summary$t.ratio, 2)
#   } else if ("z.ratio" %in% colnames(pairwise_summary)) {
#     pairwise_summary$z.ratio <- round(pairwise_summary$z.ratio, 2)
#   }
#   
#   # Round the p-values and add significance stars
#   pairwise_summary$p.value <- round(pairwise_summary$p.value, 2)
#   pairwise_summary$p.signif <- ifelse(pairwise_summary$p.value < 0.001, "***",
#                                       ifelse(pairwise_summary$p.value < 0.01, "**",
#                                              ifelse(pairwise_summary$p.value < 0.05, "*", "")))
#   
#   # Append the pairwise summary to the list
#   pairwise_summaries[[col_name]] <- pairwise_summary
# }
# 
# # Combine all GLM summaries into one data frame
# combined_glm_summaries <- do.call(rbind, glm_summaries)
# 
# # Combine all pairwise summaries into one data frame
# combined_pairwise_summaries <- do.call(rbind, pairwise_summaries)
# 
# # Write the combined GLM summaries to a CSV file
# write.csv(combined_glm_summaries, file = "Statistics/glm_outputs/tax_order/glm_summaries.csv", row.names = FALSE)
# 
# # Write the combined pairwise summaries to a separate CSV file
# write.csv(combined_pairwise_summaries, file = "Statistics/glm_outputs/tax_order/pairwise_summaries.csv", row.names = FALSE)
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
# 
# 
# 
# 
# 
# ### 03 QBS-ar Score ####
# 
# dat <- qbs_dat
# 
# # Create a directory to save the model summaries and pairwise comparisons
# dir.create("Statistics/glm_outputs/qbs/", showWarnings = FALSE)
# 
# columns_to_run_glm <- colnames(dat[, 18:ncol(dat)])
# 
# # Create empty lists to store the GLM model summaries and pairwise comparisons
# glm_summaries <- list()
# pairwise_summaries <- list()
# 
# # Iterate through the specified columns and run GLM + pairwise comparisons
# for (col_name in columns_to_run_glm) {
#   formula_string <- paste(col_name, "~ Treatment * Year", sep = "")
#   
#   # Run the GLM model (assuming a quasipoisson family)
#   glm_model <- glm(formula_string, data = dat, family = quasipoisson)
#   
#   # Use broom::tidy() to create a tidy summary of the model
#   tidy_summary <- broom::tidy(glm_model)
#   
#   # Add a column for the variable name
#   tidy_summary$variable <- col_name
#   
#   # Round numeric columns to 2 decimal places
#   tidy_summary$estimate <- round(tidy_summary$estimate, 2)
#   tidy_summary$std.error <- round(tidy_summary$std.error, 2)
#   tidy_summary$statistic <- round(tidy_summary$statistic, 2)
#   tidy_summary$p.value <- round(tidy_summary$p.value, 2)
#   
#   # Add significance stars based on p-value
#   tidy_summary$p.signif <- ifelse(tidy_summary$p.value < 0.001, "***",
#                                   ifelse(tidy_summary$p.value < 0.01, "**",
#                                          ifelse(tidy_summary$p.value < 0.05, "*", "")))
#   
#   # Append the tidy summary to the list
#   glm_summaries[[col_name]] <- tidy_summary
#   
#   # Perform pairwise comparisons using emmeans
#   emmeans_res <- emmeans(glm_model, pairwise ~ Treatment | Year)
#   pairwise_summary <- as.data.frame(summary(emmeans_res$contrasts)) # Get pairwise comparisons summary
#   
#   # Check the names of the columns in pairwise_summary
#   print(colnames(pairwise_summary))  # To inspect available columns (e.g., t.ratio, z.ratio)
#   
#   # Add a column for the variable name in pairwise comparisons
#   pairwise_summary$variable <- col_name
#   
#   # Round numeric columns, checking for correct column names
#   pairwise_summary$estimate <- round(pairwise_summary$estimate, 2)
#   pairwise_summary$SE <- round(pairwise_summary$SE, 2)
#   
#   # Check for existence of t.ratio or z.ratio and handle accordingly
#   if ("t.ratio" %in% colnames(pairwise_summary)) {
#     pairwise_summary$t.ratio <- round(pairwise_summary$t.ratio, 2)
#   } else if ("z.ratio" %in% colnames(pairwise_summary)) {
#     pairwise_summary$z.ratio <- round(pairwise_summary$z.ratio, 2)
#   }
#   
#   # Round the p-values and add significance stars
#   pairwise_summary$p.value <- round(pairwise_summary$p.value, 2)
#   pairwise_summary$p.signif <- ifelse(pairwise_summary$p.value < 0.001, "***",
#                                       ifelse(pairwise_summary$p.value < 0.01, "**",
#                                              ifelse(pairwise_summary$p.value < 0.05, "*", "")))
#   
#   # Append the pairwise summary to the list
#   pairwise_summaries[[col_name]] <- pairwise_summary
# }
# 
# # Combine all GLM summaries into one data frame
# combined_glm_summaries <- do.call(rbind, glm_summaries)
# 
# # Combine all pairwise summaries into one data frame
# combined_pairwise_summaries <- do.call(rbind, pairwise_summaries)
# 
# # Write the combined GLM summaries to a CSV file
# write.csv(combined_glm_summaries, file = "Statistics/glm_outputs/qbs/glm_summaries.csv", row.names = FALSE)
# 
# # Write the combined pairwise summaries to a separate CSV file
# write.csv(combined_pairwise_summaries, file = "Statistics/glm_outputs/qbs/pairwise_summaries.csv", row.names = FALSE)
# 
# 
# qbs_ar_pair_comp <- filter(.data = combined_pairwise_summaries, variable == "qbs_score")
# 
# write.csv(x = qbs_ar_pair_comp, file = "Statistics/glm_outputs/qbs/qbs_ar_pairwise_summaries.csv", row.names = FALSE)
# 
# 
# 
# 
# 
# #### 03.1 QBS-ar model ####
# 
# # Poisson regression model
# model_poisson <- glm(qbs_score ~ Treatment * Year, family = quasipoisson(link = "log"), data = qbs_dat)
# summary(model_poisson)
# 
# # Pairwise comparisons
# emmeans(model_poisson, pairwise ~ Treatment | Year)
# 
# 
# # Predicted values from model
# qbs_dat$predicted <- predict(model_poisson, type = "response")
# 
# ggplot(qbs_dat, aes(x = Year, y = predicted, color = Treatment)) +
#   geom_line(aes(group = Treatment)) +
#   geom_point() +
#   theme_minimal() +
#   labs(title = "Predicted Microarthropod Counts by Treatment and Year", y = "Predicted Count")
# 
# 
# # Example ggplot code including observed data points with jittering and custom colors
# ggplot(qbs_dat, aes(x = Year)) +
#   geom_line(aes(y = predicted, color = Treatment, group = Treatment), size = 1) +  # Predicted lines
#   geom_point(aes(y = qbs_score, color = Treatment), 
#              size = 2, 
#              position = position_jitter(width = 0.2, height = 0), size = 3) +  # Jittered observed points
#   theme_bw() +
#   labs(title = element_blank(),
#        y = "QBS-ar EMI Score") +
#   scale_color_manual(values = c("Conservation" = "turquoise3", "Conventional" = "tomato2")) +  # Custom colors
#   theme(legend.title = element_blank(), 
#         legend.position = "bottom", 
#         text = element_text(size=15))  # Optional: Remove legend title for cleaner look
# 
# dir.create(path = "Plots/models/")
# 
# ggsave(filename = "qbs_ar_glm_plot.png", 
#        path = "Plots/models/", 
#        width = 5, 
#        height = 5)
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
# ### 04 QBS-c Score ####
# 
# dat <- qbs_c
# 
# # Create a directory to save the model summaries and pairwise comparisons
# dir.create("Statistics/glm_outputs/qbs_c/", showWarnings = FALSE)
# 
# columns_to_run_glm <- colnames(dat[, 18:ncol(dat)])
# 
# # Create empty lists to store the GLM model summaries and pairwise comparisons
# glm_summaries <- list()
# pairwise_summaries <- list()
# 
# # Iterate through the specified columns and run GLM + pairwise comparisons
# for (col_name in columns_to_run_glm) {
#   formula_string <- paste(col_name, "~ Treatment * Year", sep = "")
#   
#   # Run the GLM model (assuming a quasipoisson family)
#   glm_model <- glm(formula_string, data = dat, family = quasipoisson)
#   
#   # Use broom::tidy() to create a tidy summary of the model
#   tidy_summary <- broom::tidy(glm_model)
#   
#   # Add a column for the variable name
#   tidy_summary$variable <- col_name
#   
#   # Round numeric columns to 2 decimal places
#   tidy_summary$estimate <- round(tidy_summary$estimate, 2)
#   tidy_summary$std.error <- round(tidy_summary$std.error, 2)
#   tidy_summary$statistic <- round(tidy_summary$statistic, 2)
#   tidy_summary$p.value <- round(tidy_summary$p.value, 2)
#   
#   # Add significance stars based on p-value
#   tidy_summary$p.signif <- ifelse(tidy_summary$p.value < 0.001, "***",
#                                   ifelse(tidy_summary$p.value < 0.01, "**",
#                                          ifelse(tidy_summary$p.value < 0.05, "*", "")))
#   
#   # Append the tidy summary to the list
#   glm_summaries[[col_name]] <- tidy_summary
#   
#   # Perform pairwise comparisons using emmeans
#   emmeans_res <- emmeans(glm_model, pairwise ~ Treatment | Year)
#   pairwise_summary <- as.data.frame(summary(emmeans_res$contrasts)) # Get pairwise comparisons summary
#   
#   # Check the names of the columns in pairwise_summary
#   print(colnames(pairwise_summary))  # To inspect available columns (e.g., t.ratio, z.ratio)
#   
#   # Add a column for the variable name in pairwise comparisons
#   pairwise_summary$variable <- col_name
#   
#   # Round numeric columns, checking for correct column names
#   pairwise_summary$estimate <- round(pairwise_summary$estimate, 2)
#   pairwise_summary$SE <- round(pairwise_summary$SE, 2)
#   
#   # Check for existence of t.ratio or z.ratio and handle accordingly
#   if ("t.ratio" %in% colnames(pairwise_summary)) {
#     pairwise_summary$t.ratio <- round(pairwise_summary$t.ratio, 2)
#   } else if ("z.ratio" %in% colnames(pairwise_summary)) {
#     pairwise_summary$z.ratio <- round(pairwise_summary$z.ratio, 2)
#   }
#   
#   # Round the p-values and add significance stars
#   pairwise_summary$p.value <- round(pairwise_summary$p.value, 2)
#   pairwise_summary$p.signif <- ifelse(pairwise_summary$p.value < 0.001, "***",
#                                       ifelse(pairwise_summary$p.value < 0.01, "**",
#                                              ifelse(pairwise_summary$p.value < 0.05, "*", "")))
#   
#   # Append the pairwise summary to the list
#   pairwise_summaries[[col_name]] <- pairwise_summary
# }
# 
# # Combine all GLM summaries into one data frame
# combined_glm_summaries <- do.call(rbind, glm_summaries)
# 
# # Combine all pairwise summaries into one data frame
# combined_pairwise_summaries <- do.call(rbind, pairwise_summaries)
# 
# # Write the combined GLM summaries to a CSV file
# write.csv(combined_glm_summaries, file = "Statistics/glm_outputs/qbs_c/glm_summaries.csv", row.names = FALSE)
# 
# # Write the combined pairwise summaries to a separate CSV file
# write.csv(combined_pairwise_summaries, file = "Statistics/glm_outputs/qbs_c/pairwise_summaries.csv", row.names = FALSE)
# 
# 
# 
# 
# 
# 
# ### 05 QBS-e Score ####
# 
# dat <- qbs_e_dat
# 
# # Create a directory to save the model summaries and pairwise comparisons
# dir.create("Statistics/glm_outputs/qbs_e/", showWarnings = FALSE)
# 
# columns_to_run_glm <- colnames(dat[, 7:ncol(dat)])
# 
# # Create empty lists to store the GLM model summaries and pairwise comparisons
# glm_summaries <- list()
# pairwise_summaries <- list()
# 
# # Iterate through the specified columns and run GLM + pairwise comparisons
# for (col_name in columns_to_run_glm) {
#   formula_string <- paste(col_name, "~ Treatment * Year", sep = "")
#   
#   # Run the GLM model (assuming a quasipoisson family)
#   glm_model <- glm(formula_string, data = dat, family = quasipoisson)
#   
#   # Use broom::tidy() to create a tidy summary of the model
#   tidy_summary <- broom::tidy(glm_model)
#   
#   # Add a column for the variable name
#   tidy_summary$variable <- col_name
#   
#   # Round numeric columns to 2 decimal places
#   tidy_summary$estimate <- round(tidy_summary$estimate, 2)
#   tidy_summary$std.error <- round(tidy_summary$std.error, 2)
#   tidy_summary$statistic <- round(tidy_summary$statistic, 2)
#   tidy_summary$p.value <- round(tidy_summary$p.value, 2)
#   
#   # Add significance stars based on p-value
#   tidy_summary$p.signif <- ifelse(tidy_summary$p.value < 0.001, "***",
#                                   ifelse(tidy_summary$p.value < 0.01, "**",
#                                          ifelse(tidy_summary$p.value < 0.05, "*", "")))
#   
#   # Append the tidy summary to the list
#   glm_summaries[[col_name]] <- tidy_summary
#   
#   # Perform pairwise comparisons using emmeans
#   emmeans_res <- emmeans(glm_model, pairwise ~ Treatment | Year)
#   pairwise_summary <- as.data.frame(summary(emmeans_res$contrasts)) # Get pairwise comparisons summary
#   
#   # Check the names of the columns in pairwise_summary
#   print(colnames(pairwise_summary))  # To inspect available columns (e.g., t.ratio, z.ratio)
#   
#   # Add a column for the variable name in pairwise comparisons
#   pairwise_summary$variable <- col_name
#   
#   # Round numeric columns, checking for correct column names
#   pairwise_summary$estimate <- round(pairwise_summary$estimate, 2)
#   pairwise_summary$SE <- round(pairwise_summary$SE, 2)
#   
#   # Check for existence of t.ratio or z.ratio and handle accordingly
#   if ("t.ratio" %in% colnames(pairwise_summary)) {
#     pairwise_summary$t.ratio <- round(pairwise_summary$t.ratio, 2)
#   } else if ("z.ratio" %in% colnames(pairwise_summary)) {
#     pairwise_summary$z.ratio <- round(pairwise_summary$z.ratio, 2)
#   }
#   
#   # Round the p-values and add significance stars
#   pairwise_summary$p.value <- round(pairwise_summary$p.value, 2)
#   pairwise_summary$p.signif <- ifelse(pairwise_summary$p.value < 0.001, "***",
#                                       ifelse(pairwise_summary$p.value < 0.01, "**",
#                                              ifelse(pairwise_summary$p.value < 0.05, "*", "")))
#   
#   # Append the pairwise summary to the list
#   pairwise_summaries[[col_name]] <- pairwise_summary
# }
# 
# # Combine all GLM summaries into one data frame
# combined_glm_summaries <- do.call(rbind, glm_summaries)
# 
# # Combine all pairwise summaries into one data frame
# combined_pairwise_summaries <- do.call(rbind, pairwise_summaries)
# 
# # Write the combined GLM summaries to a CSV file
# write.csv(combined_glm_summaries, file = "Statistics/glm_outputs/qbs_e/glm_summaries.csv", row.names = FALSE)
# 
# # Write the combined pairwise summaries to a separate CSV file
# write.csv(combined_pairwise_summaries, file = "Statistics/glm_outputs/qbs_e/pairwise_summaries.csv", row.names = FALSE)
# 
# 
# 
# 
# 
# ### 06 Shannon index ####
# 
# # Check for overdispersion
# mean_count <- mean(shannon_data$Shannon_Index)
# var_count <- var(shannon_data$Shannon_Index)
# overdispersion <- var_count / mean_count
# print(overdispersion)  # If much greater than 1, overdispersion is present
# 
# 
# 
# # Fit the linear mixed-effects model
# lm <- lmer(formula = Shannon_Index ~ Treatment * Year + (1 | Block), data = shannon_data)
# 
# # Get the summary which includes p-values
# summary(lm)
# 
# # You can also use anova to get Type III sums of squares and p-values
# anova(lm)
# 
# # Calculate EMMs for Treatment
# emm_results <- emmeans(lm, ~ Treatment | Year)
# 
# # Pairwise comparisons
# pairwise_results <- pairs(emm_results)
# summary(pairwise_results)
# 
# # Pairwise comparisons with Tukey adjustment
# pairwise_results_tukey <- pairs(emm_results, adjust = "tukey")
# summary(pairwise_results_tukey)
# 
# 
# dir.create(path = "Statistics/lm_outputs/")
# 
# # Capture the output of summary(lm_model)
# lm_output <- capture.output(summary(lm))
# 
# # Write the output to a text file
# writeLines(lm_output, "Statistics/lm_outputs/shannon_lm.txt")
# 
# 
# p_comp <- capture.output(pairwise_results_tukey)
# 
# # Write the output to a text file
# writeLines(p_comp, "Statistics/lm_outputs/shannon_pair_comp.txt")
# 









