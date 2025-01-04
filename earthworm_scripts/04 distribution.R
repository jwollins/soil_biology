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
columns_of_interest <- names(dat)[6:ncol(data)]

# Initialize an empty data frame to store results
result_df <- data.frame()


# qucik function checks of distributions 
check_guassian(data = dat, columns_to_check = columns_of_interest)
check_gamma_distribution(dat = data, columns_to_check = columns_of_interest)
check_exponential_distribution(dat = data, columns_to_check = columns_of_interest)
check_poisson_multiple(data = dat, columns = columns_of_interest)







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


# NO TRANS 

library(MVN)

colnames(dat)

dat_cols <- dat[10:ncol(data)]

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




## standardise the data ####

# Standardize the data (z-scores)
dat_standardized <- as.data.frame(scale(dat[,6:ncol(dat)]))

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




















