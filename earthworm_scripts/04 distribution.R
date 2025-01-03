# 04 Distribution ####
## WHO: JOE COLLINS
## WHAT: QBS data normality test and distribution plots
## LAST EDIT: 2024-10-16
####

# DISTRIBUTIONS ####

setwd(dir = "~/Documents/GitHub/soil_biology/")

# PACKAGES ####

source(file = "earthworm_scripts/01_packages.R")


# LOAD DATA ####

source(file = "~/Documents/GitHub/soil_biology/earthworm_scripts/02_data.R")

source("scripts/functions/fun_lm_by_year.R")
source("scripts/functions/fun_glm_by_year.R")
source(file = "~/Documents/GitHub/phd_tools/fun_distribution_tests.R")
source(file = "scripts/functions/shapiro_wilks.R")
source(file = "scripts/functions/fun_bartlett_test.R")



setwd(dir = "~/OneDrive - Harper Adams University/Data/Soil/worms/")


# ABUNDANCE ####

## prep data ####

# Load the data
data <- all_dat

#rename column by name
data <- data %>% rename_at('year', ~'Year')
#rename column by name
data <- data %>% rename_at('treatment', ~'Treatment')

# Specify the columns of interest (columns 7 to 55 in this case)
columns_of_interest <- names(data)[6:ncol(data)]

# Initialize an empty data frame to store results
result_df <- data.frame()


# qucik function checks of distributions 
check_guassian(data = data, columns_to_check = columns_of_interest)
check_gamma_distribution(data = data, columns_to_check = columns_of_interest)
check_exponential_distribution(data = data, columns_to_check = columns_of_interest)
check_poisson_multiple(data = data, columns = columns_of_interest)




### 01 SHAPIRO WILK ####



check_normality(data = data, columns_of_interest = columns_of_interest)

# Save the results to a CSV file
if (!dir.exists("stats/normality/")) {
  dir.create("stats/normality/", recursive = TRUE)
}

write.csv(result_df, file = "stats/normality/abundance_shapiro_results_by_year.csv", 
          row.names = FALSE)










### 02 BARTLETT TEST ####

check_variance_homogeneity(data = data, columns_of_interest = columns_of_interest)

# Save the results to a CSV file
if (!dir.exists("stats/normality/")) {
  dir.create("stats/normality/", recursive = TRUE)
}
write.csv(result_df, file = "stats/normality/abundance_bartlett_results_by_year_treatment.csv", 
          row.names = FALSE)

























