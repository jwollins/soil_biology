# DATA ####
## WHO: JOE COLLINS
## WHAT: Load QBS data
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


# Function to round numeric columns to 2 decimal places
round_numeric_columns <- function(df) {
  df[] <- lapply(df, function(x) if(is.numeric(x)) round(x, 2) else x)
  return(df)
}







#__________________________________________####
# LOAD DATA ####



all_dat_count <- read_excel(path = "sym_link_soil_biology/Data/all_years/qbs_data_all_years.xlsx", sheet = 1)

# remove NA's
all_dat_count[is.na(all_dat_count)] <- 0

# ~ Factors ####

all_dat_count$Year <- factor(all_dat_count$Year)
all_dat_count$Block <- factor(all_dat_count$Block)
all_dat_count$Treatment <- factor(all_dat_count$Treatment)








#__________________________________________####
#  Stats summary ####

# Define the column range for which you want to calculate summaries by column number
start_col <- 18  # starting column number
end_col <- ncol(all_dat_count)   # ending column number

# Initialize an empty list to store summaries for each column
summary_list <- list()

# Loop over each specified column and calculate summaries
for (i in start_col:end_col) {
  col_name <- names(all_dat_count)[i] # Get the column name based on its index
  
  count_sum <- all_dat_count %>%
    group_by(Treatment, Year) %>%
    summarise(
      n = n(),
      mean = mean(.data[[col_name]], na.rm = TRUE),
      sd = sd(.data[[col_name]], na.rm = TRUE),
      median = median(.data[[col_name]], na.rm = TRUE),
      min = min(.data[[col_name]], na.rm = TRUE),
      max = max(.data[[col_name]], na.rm = TRUE),
      range = max - min
    ) %>%
    mutate(
      se = sd / sqrt(n),
      ic = se * qt((1 - 0.05) / 2 + .5, n - 1)
    ) %>%
    mutate(column = col_name) # add column name for reference
  
  # Add the result to the summary list
  summary_list[[col_name]] <- count_sum
}

# Combine all summaries into a single data frame
final_summary <- bind_rows(summary_list)

final_summary <- round_numeric_columns(final_summary)

write.csv(x = final_summary, file = "sym_link_soil_biology/Statistics/summary.stats/abundance_summary_stats.csv", row.names = FALSE)







# ~ total abundance ####

total_abun_dat <- all_dat_count[,1:17]


# Assuming you want the row sums from columns 18 to the last column
total_abun_dat$total_abundance <- rowSums(all_dat_count[, 18:ncol(all_dat_count)])

# generate summary stats for the qbs score
total_abun_sum <- total_abun_dat %>%
  group_by(Treatment, Year) %>%
  summarise(
    n = n(),
    mean = mean(total_abundance, na.rm = TRUE),
    sd = sd(total_abundance, na.rm = TRUE),
    median = median(total_abundance, na.rm = TRUE),
    min = min(total_abundance, na.rm = TRUE),
    max = max(total_abundance, na.rm = TRUE),
    range = max - min
  ) %>%
  mutate(
    se = sd / sqrt(n),
    ic = se * qt((1 - 0.05) / 2 + .5, n - 1)
  ) 


total_abun_sum <- round_numeric_columns(total_abun_sum)

write.csv(x = total_abun_sum, file = "Statistics/summary.stats/total_abun_sum.csv", row.names = FALSE)






# ~ Taxanomic order ####


# calculate the sum of the taxonomic orders

tax_ord_dat <- all_dat_count[,1:17]


tax_ord_dat$Chelicerata <- all_dat_count$Aranae_larger_than_5mm + 
  all_dat_count$Aranae_smaller_than_5mm +
  all_dat_count$Mites + 
  all_dat_count$Pseudoscorpiones +
  all_dat_count$Scorpiones + 
  all_dat_count$Opiliones +
  all_dat_count$Palpigrades

tax_ord_dat$Crustacea <- all_dat_count$Isopods

tax_ord_dat$Myriapoda <- all_dat_count$Diplopods_larger_than_5mm +
  all_dat_count$Diplopods_smaller_than_5mm +
  all_dat_count$Pauropods +
  all_dat_count$Symphylans +
  all_dat_count$Chilopods_larger_than_5mm +
  all_dat_count$Chilopods_smaller_than_5mm


tax_ord_dat$Hexapoda <- all_dat_count$Colem_Epigeic +
  all_dat_count$Colem_Hemiedaphic +
  all_dat_count$Colem_Eudaphic + 
  all_dat_count$Proturans +
  all_dat_count$Diplurans + 
  all_dat_count$Mycrocoryphia +
  all_dat_count$Zygentoma + 
  all_dat_count$Dermaptera +
  all_dat_count$Orthoptera +
  all_dat_count$Orthoptera_juvenile +
  all_dat_count$Embioptera +
  all_dat_count$Isoptera +
  all_dat_count$Blattaria +
  all_dat_count$Psocoptera +
  all_dat_count$Hemiptera_Cicada_larvae +
  all_dat_count$Hemiptera_other +
  all_dat_count$Tysanoptera +
  all_dat_count$Coleoptera_larger_than_2mm +
  all_dat_count$Coleoptera_3_4 +
  all_dat_count$Coleoptera_2_4 +
  all_dat_count$Coleoptera_1_4 +
  all_dat_count$Coleoptera_0_4 +
  all_dat_count$Hymenoptera_ants +
  all_dat_count$Hymenoptera_other +
  all_dat_count$Diptera_adult +
  all_dat_count$Coleoptera_lavae +
  all_dat_count$Hymenoptera_lavae +
  all_dat_count$Diptera_adult +
  all_dat_count$Lepidoptera_larvae +
  all_dat_count$Other_holometabolos










# ~ tax ord summary table ####


# Define the column range for which you want to calculate summaries by column number
start_col <- 18  # starting column number
end_col <- ncol(tax_ord_dat)   # ending column number

# Initialize an empty list to store summaries for each column
tax_ord_list <- list()

# Loop over each specified column and calculate summaries
for (i in start_col:end_col) {
  col_name <- names(tax_ord_dat)[i] # Get the column name based on its index
  
  count_sum <- tax_ord_dat %>%
    group_by(Treatment, Year) %>%
    summarise(
      n = n(),
      mean = mean(.data[[col_name]], na.rm = TRUE),
      sd = sd(.data[[col_name]], na.rm = TRUE),
      median = median(.data[[col_name]], na.rm = TRUE),
      min = min(.data[[col_name]], na.rm = TRUE),
      max = max(.data[[col_name]], na.rm = TRUE),
      range = max - min
    ) %>%
    mutate(
      se = sd / sqrt(n),
      ic = se * qt((1 - 0.05) / 2 + .5, n - 1)
    ) %>%
    mutate(column = col_name) # add column name for reference
  
  # Add the result to the summary list
  tax_ord_list[[col_name]] <- count_sum
}

# Combine all summaries into a single data frame
tax_ord_summary <- bind_rows(tax_ord_list)

tax_ord_summary <- round_numeric_columns(tax_ord_summary)

write.csv(x = tax_ord_summary, file = "Statistics/summary.stats/tax_ord_sum_stats.csv", row.names = FALSE)










# ~ QBS-ar calculation ####

qbs_emi_values <- read.csv(file = "Data/qbs_emi_values.csv")



# =IF(COUNT_TABLE[@[Colem.Epigeic]]>0,'EMI Values'!$F$2,0)

# Initialize qbs_dat with the same dimensions as all_dat_count and set to 0 initially
qbs_dat <- data.frame(matrix(0, nrow = nrow(all_dat_count), ncol = ncol(all_dat_count)))
names(qbs_dat) <- names(all_dat_count)

qbs_dat[,1:17] <- all_dat_count[,1:17]

qbs_dat[,18:ncol(qbs_dat)] <- 0

qbs_dat$Colem_Epigeic <- if_else(condition = all_dat_count$Colem_Epigeic > 0, 
                                 true = 4, 
                                 false = 0)

qbs_dat$Colem_Hemiedaphic <- if_else(condition = all_dat_count$Colem_Hemiedaphic > 0, 
                                 true = 8, 
                                 false = 0)

qbs_dat$Colem_Eudaphic <- if_else(condition = all_dat_count$Colem_Eudaphic > 0, 
                                     true = 20, 
                                     false = 0)

qbs_dat$Pseudoscorpiones <- if_else(condition = all_dat_count$Pseudoscorpiones > 0, 
                                  true = 20, 
                                  false = 0)

qbs_dat$Scorpiones <- if_else(condition = all_dat_count$Scorpiones > 0, 
                                    true = 10, 
                                    false = 0)

qbs_dat$Palpigrades <- if_else(condition = all_dat_count$Palpigrades > 0, 
                              true = 20, 
                              false = 0)

qbs_dat$Opiliones<- if_else(condition = all_dat_count$Opiliones > 0, 
                               true = 20, 
                               false = 0)

qbs_dat$Aranae_larger_than_5mm <- if_else(condition = all_dat_count$Aranae_larger_than_5mm > 0, 
                               true = 1, 
                               false = 0)

qbs_dat$Aranae_smaller_than_5mm <- if_else(condition = all_dat_count$Aranae_smaller_than_5mm > 0, 
                    true = 5, 
                    false = 0)

qbs_dat$Mites <- if_else(condition = all_dat_count$Mites > 0, 
                    true = 20, 
                    false = 0)

qbs_dat$Isopods <- if_else(condition = all_dat_count$Isopods > 0, 
                    true = 10, 
                    false = 0)

qbs_dat$Diplopods_larger_than_5mm <- if_else(condition = all_dat_count$Diplopods_larger_than_5mm > 0, 
                    true = 10, 
                    false = 0)

qbs_dat$Diplopods_smaller_than_5mm <- if_else(condition = all_dat_count$Diplopods_smaller_than_5mm > 0, 
                    true = 20, 
                    false = 0)

qbs_dat$Pauropods <- if_else(condition = all_dat_count$Pauropods > 0, 
                    true = 20, 
                    false = 0)

qbs_dat$Symphylans <- if_else(condition = all_dat_count$Symphylans > 0, 
                    true = 10, 
                    false = 0)

qbs_dat$Chilopods_larger_than_5mm <- if_else(condition = all_dat_count$Chilopods_larger_than_5mm > 0, 
                    true = 10, 
                    false = 0)

qbs_dat$Chilopods_smaller_than_5mm <- if_else(condition = all_dat_count$Chilopods_smaller_than_5mm > 0, 
                    true = 20, 
                    false = 0)

qbs_dat$Proturans <- if_else(condition = all_dat_count$Proturans > 0, 
                    true = 20, 
                    false = 0)

qbs_dat$Diplurans <- if_else(condition = all_dat_count$Diplurans > 0, 
                    true = 20, 
                    false = 0)

qbs_dat$Mycrocoryphia <- if_else(condition = all_dat_count$Mycrocoryphia > 0, 
                    true = 10, 
                    false = 0)

qbs_dat$Zygentoma <- if_else(condition = all_dat_count$Zygentoma > 0, 
                    true = 10, 
                    false = 0)

qbs_dat$Dermaptera <- if_else(condition = all_dat_count$Dermaptera > 0, 
                    true = 1, 
                    false = 0)

qbs_dat$Orthoptera <- if_else(condition = all_dat_count$Orthoptera > 0, 
                    true = 1, 
                    false = 0)

qbs_dat$Orthoptera_juvenile <- if_else(condition = all_dat_count$Orthoptera_juvenile > 0, 
                    true = 20, 
                    false = 0)

qbs_dat$Embioptera <- if_else(condition = all_dat_count$Embioptera > 0, 
                    true = 10, 
                    false = 0)

qbs_dat$Isoptera <- if_else(condition = all_dat_count$Isoptera > 0, 
                    true = 10, 
                    false = 0)

qbs_dat$Blattaria <- if_else(condition = all_dat_count$Blattaria > 0, 
                    true = 5, 
                    false = 0)

qbs_dat$Psocoptera <- if_else(condition = all_dat_count$Psocoptera > 0, 
                    true = 1, 
                    false = 0)

qbs_dat$Hemiptera_Cicada_larvae <- if_else(condition = all_dat_count$Hemiptera_Cicada_larvae > 0, 
                    true = 10, 
                    false = 0)

qbs_dat$Hemiptera_other <- if_else(condition = all_dat_count$Hemiptera_other > 0, 
                    true = 1, 
                    false = 0)

qbs_dat$Tysanoptera <- if_else(condition = all_dat_count$Tysanoptera > 0, 
                    true = 1, 
                    false = 0)

qbs_dat$Coleoptera_larger_than_2mm <- if_else(condition = all_dat_count$Coleoptera_larger_than_2mm > 0, 
                    true = 1, 
                    false = 0)

qbs_dat$Coleoptera_3_4 <- if_else(condition = all_dat_count$Coleoptera_3_4 > 0, 
                    true = 5, 
                    false = 0)

qbs_dat$Coleoptera_2_4 <- if_else(condition = all_dat_count$Coleoptera_2_4 > 0, 
                    true = 10, 
                    false = 0)

qbs_dat$Coleoptera_1_4 <- if_else(condition = all_dat_count$Coleoptera_1_4 > 0, 
                    true = 15, 
                    false = 0)

qbs_dat$Coleoptera_0_4 <- if_else(condition = all_dat_count$Coleoptera_0_4 > 0, 
                    true = 20, 
                    false = 0)

qbs_dat$Hymenoptera_ants <- if_else(condition = all_dat_count$Hymenoptera_ants > 0, 
                    true = 5, 
                    false = 0)

qbs_dat$Hymenoptera_other <- if_else(condition = all_dat_count$Hymenoptera_other > 0, 
                    true = 1, 
                    false = 0)

qbs_dat$Diptera_adult <- if_else(condition = all_dat_count$Diptera_adult > 0, 
                    true = 1, 
                    false = 0)

qbs_dat$Coleoptera_lavae <- if_else(condition = all_dat_count$Coleoptera_lavae > 0, 
                    true = 10, 
                    false = 0)

qbs_dat$Hymenoptera_lavae <- if_else(condition = all_dat_count$Hymenoptera_lavae > 0, 
                    true = 10, 
                    false = 0)

qbs_dat$Diptera_lavae <- if_else(condition = all_dat_count$Diptera_lavae > 0, 
                    true = 10, 
                    false = 0)

qbs_dat$Lepidoptera_larvae <- if_else(condition = all_dat_count$Lepidoptera_larvae > 0, 
                    true = 10, 
                    false = 0)

qbs_dat$Other_holometabolos <- if_else(condition = all_dat_count$Other_holometabolos > 0, 
                    true = 1, 
                    false = 0)


# calculate the total qbs score
qbs_dat$qbs_score <- rowSums( qbs_dat[,18:ncol(qbs_dat)])


# generate summary stats for the qbs score
qbs_sum <- qbs_dat %>%
  group_by(Treatment, Year) %>%
  summarise(
    n = n(),
    mean = mean(qbs_score, na.rm = TRUE),
    sd = sd(qbs_score, na.rm = TRUE),
    median = median(qbs_score, na.rm = TRUE),
    min = min(qbs_score, na.rm = TRUE),
    max = max(qbs_score, na.rm = TRUE),
    range = max - min
  ) %>%
  mutate(
    se = sd / sqrt(n),
    ic = se * qt((1 - 0.05) / 2 + .5, n - 1)
  ) 


qbs_sum <- round_numeric_columns(qbs_sum)



write.csv(x = qbs_sum, file = "sym_link_soil_biology/Statistics/summary.stats/qbs_ar_summary.csv", row.names = FALSE)










# ~ QBS-ar summary stats loop ####

# Define the column range for which you want to calculate summaries by column number
start_col <- 18  # starting column number
end_col <- ncol(qbs_dat)   # ending column number

# Initialize an empty list to store summaries for each column
qbs_dat_list <- list()

# Loop over each specified column and calculate summaries
for (i in start_col:end_col) {
  col_name <- names(qbs_dat)[i] # Get the column name based on its index
  
  count_sum <- qbs_dat %>%
    group_by(Treatment, Year) %>%
    summarise(
      n = n(),
      mean = mean(.data[[col_name]], na.rm = TRUE),
      sd = sd(.data[[col_name]], na.rm = TRUE),
      median = median(.data[[col_name]], na.rm = TRUE),
      min = min(.data[[col_name]], na.rm = TRUE),
      max = max(.data[[col_name]], na.rm = TRUE),
      range = max - min
    ) %>%
    mutate(
      se = sd / sqrt(n),
      ic = se * qt((1 - 0.05) / 2 + .5, n - 1)
    ) %>%
    mutate(column = col_name) # add column name for reference
  
  # Add the result to the summary list
  qbs_dat_list[[col_name]] <- count_sum
}

# Combine all summaries into a single data frame
qbs_dat_summary <- bind_rows(qbs_dat_list)

qbs_dat_summary <- round_numeric_columns(qbs_dat_summary)

write.csv(x = qbs_dat_summary, file = "sym_link_soil_biology/Statistics/summary.stats/QBS_ar_score_summary_stats.csv", row.names = FALSE)










# ~ QBS-c calculation ####

qbs_c <- qbs_dat[,1:18]


qbs_c$Colem_Epigeic <- all_dat_count$Colem_Epigeic * 4

qbs_c$Colem_Hemiedaphic <- all_dat_count$Colem_Hemiedaphic * 8

qbs_c$Colem_Eudaphic <- all_dat_count$Colem_Eudaphic * 20

# calculate the total qbs score
qbs_c$qbs_c_score <- rowSums( qbs_c[,18:ncol(qbs_c)])


# generate summary stats for the qbs score
qbs_c_sum <- qbs_c %>%
  group_by(Treatment, Year) %>%
  summarise(
    n = n(),
    mean = mean(qbs_c_score, na.rm = TRUE),
    sd = sd(qbs_c_score, na.rm = TRUE)
  ) %>%
  mutate(
    se = sd / sqrt(n),
    ic = se * qt((1 - 0.05) / 2 + .5, n - 1)
  ) 


qbs_c_sum <- round_numeric_columns(qbs_c_sum)

write.csv(x = qbs_c_sum, file = "sym_link_soil_biology/Statistics/glm_outputs/qbs_c/qbs_c_summary_stats.csv", row.names = FALSE)





# Define the column range for which you want to calculate summaries by column number
start_col <- 18  # starting column number
end_col <- ncol(qbs_c)   # ending column number

# Initialize an empty list to store summaries for each column
qbs_c_dat_list <- list()

# Loop over each specified column and calculate summaries
for (i in start_col:end_col) {
  col_name <- names(qbs_c)[i] # Get the column name based on its index
  
  count_sum <- qbs_c %>%
    group_by(Treatment, Year) %>%
    summarise(
      n = n(),
      mean = mean(.data[[col_name]], na.rm = TRUE),
      sd = sd(.data[[col_name]], na.rm = TRUE),
      median = median(.data[[col_name]], na.rm = TRUE),
      min = min(.data[[col_name]], na.rm = TRUE),
      max = max(.data[[col_name]], na.rm = TRUE),
      range = max - min
    ) %>%
    mutate(
      se = sd / sqrt(n),
      ic = se * qt((1 - 0.05) / 2 + .5, n - 1)
    ) %>%
    mutate(column = col_name) # add column name for reference
  
  # Add the result to the summary list
  qbs_c_dat_list[[col_name]] <- count_sum
}

# Combine all summaries into a single data frame
qbs_c_dat_summary <- bind_rows(qbs_dat_list)

qbs_c_dat_summary <- round_numeric_columns(qbs_c_dat_summary)

write.csv(x = qbs_c_dat_summary, file = "sym_link_soil_biology/Statistics/summary.stats/QBS_c_score_summary_stats.csv", row.names = FALSE)












# ~ Earthworm data ####


worm_dat <- read_excel(path = "sym_link_soil_biology/Data/earthworm_data/earthworm_data.xlsx", sheet = 1) 

# remove NA's
worm_dat[is.na(worm_dat)] <- 0

# Define the column range for which you want to calculate summaries by column number
start_col <- 6  # starting column number
end_col <- ncol(worm_dat)   # ending column number

# Initialize an empty list to store summaries for each column
worm_dat_list <- list()

# Loop over each specified column and calculate summaries
for (i in start_col:end_col) {
  col_name <- names(worm_dat)[i] # Get the column name based on its index
  
  count_sum <- worm_dat %>%
    group_by(Treatment, Year) %>%
    summarise(
      n = n(),
      mean = mean(.data[[col_name]], na.rm = TRUE),
      sd = sd(.data[[col_name]], na.rm = TRUE),
      median = median(.data[[col_name]], na.rm = TRUE),
      min = min(.data[[col_name]], na.rm = TRUE),
      max = max(.data[[col_name]], na.rm = TRUE),
      range = max - min
    ) %>%
    mutate(
      se = sd / sqrt(n),
      ic = se * qt((1 - 0.05) / 2 + .5, n - 1)
    ) %>%
    mutate(column = col_name) # add column name for reference
  
  # Add the result to the summary list
  worm_dat_list[[col_name]] <- count_sum
}

# Combine all summaries into a single data frame
worm_dat_summary <- bind_rows(worm_dat_list)

worm_dat_summary <- round_numeric_columns(worm_dat_summary)

write.csv(x = worm_dat_summary, file = "sym_link_soil_biology/Statistics/summary.stats/earthworm_summary_stats.csv", row.names = FALSE)









# ~ QBS-e ####


# Initialize qbs_dat with the same dimensions as all_dat_count and set to 0 initially
qbs_e_dat <- data.frame(matrix(0, nrow = nrow(worm_dat), ncol = ncol(worm_dat[,1:5])))
names(qbs_e_dat) <- names(worm_dat[,1:5])

qbs_e_dat[,1:6] <- worm_dat[,1:5]


qbs_e_dat$Epigeic_qbs_e <- worm_dat$epigeic_m2 * 3

qbs_e_dat$Endogeic_qbs_e <- worm_dat$endogeic_m2 * 3.2

qbs_e_dat$Anecic_qbs_e <- worm_dat$anecic_m2 * 14.4


# calculate the total qbs score
qbs_e_dat$qbs_score <- rowSums( qbs_e_dat[,7:ncol(qbs_e_dat)])




# ~ QBS-e summary stats loop ####

# Define the column range for which you want to calculate summaries by column number
start_col <- 7  # starting column number
end_col <- ncol(qbs_e_dat)   # ending column number

# Initialize an empty list to store summaries for each column
qbs_e_dat_list <- list()

# Loop over each specified column and calculate summaries
for (i in start_col:end_col) {
  col_name <- names(qbs_e_dat)[i] # Get the column name based on its index
  
  count_sum <- qbs_e_dat %>%
    group_by(Treatment, Year) %>%
    summarise(
      n = n(),
      mean = mean(.data[[col_name]], na.rm = TRUE),
      sd = sd(.data[[col_name]], na.rm = TRUE),
      median = median(.data[[col_name]], na.rm = TRUE),
      min = min(.data[[col_name]], na.rm = TRUE),
      max = max(.data[[col_name]], na.rm = TRUE),
      range = max - min
    ) %>%
    mutate(
      se = sd / sqrt(n),
      ic = se * qt((1 - 0.05) / 2 + .5, n - 1)
    ) %>%
    mutate(column = col_name) # add column name for reference
  
  # Add the result to the summary list
  qbs_e_dat_list[[col_name]] <- count_sum
}

# Combine all summaries into a single data frame
qbs_e_dat_summary <- bind_rows(qbs_e_dat_list)

qbs_e_dat_summary <- round_numeric_columns(qbs_e_dat_summary)

write.csv(x = qbs_e_dat_summary, file = "Statistics/summary.stats/QBS_e_score_summary_stats.csv", row.names = FALSE)



# generate summary stats for the qbs-e score
qbs_e_sum <- qbs_e_dat %>%
  group_by(Treatment, Year) %>%
  summarise(
    n = n(),
    mean = mean(qbs_score, na.rm = TRUE),
    sd = sd(qbs_score, na.rm = TRUE)
  ) %>%
  mutate(
    se = sd / sqrt(n),
    ic = se * qt((1 - 0.05) / 2 + .5, n - 1)
  ) 


qbs_e_sum <- round_numeric_columns(qbs_e_sum)

qbs_e_sum <- write.csv(x = qbs_e_sum, file = "Statistics/summary.stats/qbs_e_sum.csv", row.names = FALSE)



# ~ Shannon Index ####

shannon_data <- all_dat_count

# Select the microarthropod count data (starting from column 18 to the last column)
count_data <- shannon_data[, 18:ncol(shannon_data)]

# Calculate the Shannon diversity index for each row
shannon_index <- diversity(count_data, index = "shannon")

# Add the Shannon index to the original data
shannon_data$Shannon_Index <- shannon_index

shannon_sum <- shannon_data %>%
  group_by(Treatment, Year) %>%
  summarise(
    n = n(),
    mean = mean(Shannon_Index, na.rm = TRUE),
    sd = sd(Shannon_Index, na.rm = TRUE),
    median = median(Shannon_Index, na.rm = TRUE),
    min = min(Shannon_Index, na.rm = TRUE),
    max = max(Shannon_Index, na.rm = TRUE),
    range = max - min
  ) %>%
  mutate(
    se = sd / sqrt(n),
    ic = se * qt((1 - 0.05) / 2 + .5, n - 1)
  ) 

write.csv(x = shannon_sum, file = "Statistics/summary.stats/shannon_index_summary.csv")








#__________________________________________####
# ~ Overdisperal ####


### abundance ####

data <- all_dat_count[,18:ncol(all_dat_count)]

# Initialize a list to store results
results <- list()

# Iterate through each column of the data frame
for (column_name in names(data)) {
  # Skip non-numeric columns if necessary
  if (is.numeric(data[[column_name]])) {
    
    # Fit the Poisson model
    rd <- glm(data[[column_name]] ~ ., data = data, family = poisson)
    
    # Check for overdispersion manually
    mean_count <- mean(data[[column_name]])
    var_count <- var(data[[column_name]])
    overdispersion <- var_count / mean_count  # Calculate overdispersion
    
    # Determine if overdispersed (overdispersion > 1 indicates overdispersion)
    is_overdispersed <- ifelse(overdispersion > 1, "Yes", "No")
    
    # Store the results
    results[[column_name]] <- c(
      column_name,
      mean_count,           # Mean of the column
      var_count,            # Variance of the column
      overdispersion,       # Overdispersion value
      is_overdispersed      # Overdispersion status ("Yes" or "No")
    )
  }
}

# Convert results list to a data frame
results_df <- as.data.frame(do.call(rbind, results))
  
colnames(results_df) <- c("Column", "Mean", "Var Count", "Overdispertion_ratio", "is_overdispersed")

# Save results to CSV
write.csv(results_df, file = "Statistics/overdispersion/abundance_overdispersion.csv", row.names = FALSE)








# ~ qbs-ar ####

data <- qbs_dat[,18:ncol(qbs_dat)]

# Initialize a list to store results
results <- list()

# Iterate through each column of the data frame
for (column_name in names(data)) {
  # Skip non-numeric columns if necessary
  if (is.numeric(data[[column_name]])) {
    
    # Fit the Poisson model
    rd <- glm(data[[column_name]] ~ ., data = data, family = poisson)
    
    # Check for overdispersion manually
    mean_count <- mean(data[[column_name]])
    var_count <- var(data[[column_name]])
    overdispersion <- var_count / mean_count  # Calculate overdispersion
    
    # Determine if overdispersed (overdispersion > 1 indicates overdispersion)
    is_overdispersed <- ifelse(overdispersion > 1, "Yes", "No")
    
    # Store the results
    results[[column_name]] <- c(
      column_name,
      mean_count,           # Mean of the column
      var_count,            # Variance of the column
      overdispersion,       # Overdispersion value
      is_overdispersed      # Overdispersion status ("Yes" or "No")
    )
  }
}

# Convert results list to a data frame
results_df <- as.data.frame(do.call(rbind, results))

colnames(results_df) <- c("Column", "Mean", "Var Count", "Overdispertion_ratio", "is_overdispersed")

# Save results to CSV
write.csv(results_df, file = "Statistics/overdispersion/qbs_ar_overdispersion.csv", row.names = FALSE)





# ~ qbs-c ####

data <- qbs_c[,18:ncol(qbs_c)]

# Initialize a list to store results
results <- list()

# Iterate through each column of the data frame
for (column_name in names(data)) {
  # Skip non-numeric columns if necessary
  if (is.numeric(data[[column_name]])) {
    
    # Fit the Poisson model
    rd <- glm(data[[column_name]] ~ ., data = data, family = poisson)
    
    # Check for overdispersion manually
    mean_count <- mean(data[[column_name]])
    var_count <- var(data[[column_name]])
    overdispersion <- var_count / mean_count  # Calculate overdispersion
    
    # Determine if overdispersed (overdispersion > 1 indicates overdispersion)
    is_overdispersed <- ifelse(overdispersion > 1, "Yes", "No")
    
    # Store the results
    results[[column_name]] <- c(
      column_name,
      mean_count,           # Mean of the column
      var_count,            # Variance of the column
      overdispersion,       # Overdispersion value
      is_overdispersed      # Overdispersion status ("Yes" or "No")
    )
  }
}

# Convert results list to a data frame
results_df <- as.data.frame(do.call(rbind, results))

colnames(results_df) <- c("Column", "Mean", "Var Count", "Overdispertion_ratio", "is_overdispersed")


# Save results to CSV
write.csv(results_df, file = "Statistics/overdispersion/qbs_c_overdispersion.csv", row.names = FALSE)









# ~ qbs-e ####

data <- qbs_e_dat[,7:ncol(qbs_e_dat)]

# Initialize a list to store results
results <- list()

# Iterate through each column of the data frame
for (column_name in names(data)) {
  # Skip non-numeric columns if necessary
  if (is.numeric(data[[column_name]])) {
    
    # Fit the Poisson model
    rd <- glm(data[[column_name]] ~ ., data = data, family = poisson)
    
    # Check for overdispersion manually
    mean_count <- mean(data[[column_name]])
    var_count <- var(data[[column_name]])
    overdispersion <- var_count / mean_count  # Calculate overdispersion
    
    # Determine if overdispersed (overdispersion > 1 indicates overdispersion)
    is_overdispersed <- ifelse(overdispersion > 1, "Yes", "No")
    
    # Store the results
    results[[column_name]] <- c(
      column_name,
      mean_count,           # Mean of the column
      var_count,            # Variance of the column
      overdispersion,       # Overdispersion value
      is_overdispersed      # Overdispersion status ("Yes" or "No")
    )
  }
}

# Convert results list to a data frame
results_df <- as.data.frame(do.call(rbind, results))

colnames(results_df) <- c("Column", "Mean", "Var Count", "Overdispertion_ratio", "is_overdispersed")


# Save results to CSV
write.csv(results_df, file = "Statistics/overdispersion/qbs_e_overdispersion.csv", row.names = FALSE)







#__________________________________________####
# INDEX COMPARISON ####

biodiv_index_dat <- all_dat_count[,1:4]
biodiv_index_dat$shannon_index <- shannon_data$Shannon_Index
biodiv_index_dat$qbs_ar_index <- qbs_dat$qbs_score
biodiv_index_dat$qbs_c_index <- qbs_c$qbs_c_score
# biodiv_index_dat$qbs_e_index <- qbs_e_dat$qbs_score

dir.create(path = "Data/EMI_indexes/")
write.csv(x = biodiv_index_dat, file = "Data/EMI_indexes/biodiv_index_dat.csv")


# Calculate correlation matrix for selected biodiversity indices
# We are assuming columns named "Shannon_Index", "qbs_score", and "qbs_c_score"

data <- biodiv_index_dat

# Check the column names
colnames(data)

# Calculate correlation matrix for selected biodiversity indices
cor_matrix <- cor(data[c("shannon_index", "qbs_ar_index", "qbs_c_index")], use = "complete.obs")

# Print the correlation matrix to confirm
print(cor_matrix)

# Convert correlation matrix to long format for ggplot2
cor_long <- melt(cor_matrix)
colnames(cor_long) <- c("Index1", "Index2", "Correlation")

# Plot correlation matrix
ggplot(cor_long, aes(x = Index1, y = Index2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  geom_text(aes(label = round(Correlation, 2)), color = "black", size = 5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Matrix of Biodiversity Indices",
       x = "Index",
       y = "Index")






data <- biodiv_index_dat

#rename column by position
colnames(qbs_e_dat)[4] <- 'Plot'
colnames(qbs_e_dat)[3] <- 'Block'
colnames(qbs_e_dat)[10] <- 'qbs_e_score'


qbs_e_dat$Block <- as.factor(qbs_e_dat$Block)
qbs_e_dat$Year <- as.factor(qbs_e_dat$Year)




# Merge the two data frames on a common column (e.g., "Sample")
# Use `left_join` to keep all rows from the main data, adding `NA` for missing samples in `emi_data`
merged_data <- left_join(data, qbs_e_dat, by = c("Block", "Year"), relationship = "many-to-many")




# Calculate correlation matrix for selected biodiversity indices
cor_matrix <- cor(merged_data[c("qbs_ar_index", "qbs_c_index", "qbs_e_score", "shannon_index")], use = "complete.obs")





# Print the correlation matrix to check the results
print(cor_matrix)

colnames(cor_matrix) <- c('QBS-ar', "QBS-c", "QBS-e", "Shannon")
rownames(cor_matrix) <- c('QBS-ar', "QBS-c", "QBS-e", "Shannon")


# Convert correlation matrix to long format for plotting
cor_long <- reshape2::melt(cor_matrix)
colnames(cor_long) <- c("Index1", "Index2", "Correlation")


# Assuming `cor_long` is the melted correlation data
ggplot(cor_long, aes(x = Index1, y = Index2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_viridis(option = "viridis", name = "Correlation") +  # Use viridis scale with "plasma" palette
  geom_text(aes(label = round(Correlation, 2)), 
            color = "white", 
            # size = 5, 
            fontface = 4) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
       x = "Index",
       y = "Index")


ggsave(filename = "Plots/figures/index_correlation_plot.png", width = 5, height = 5)






