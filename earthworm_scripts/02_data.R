### Earthworm data
### J Collins 
### 2024-06-01
###

#__________________________________________####
# Set wd ####

setwd(rstudioapi::getActiveProject())

getwd()






#__________________________________________####
# PACKAGES ####

source(file = "earthworm_scripts/01_packages.R")








#__________________________________________####
# DATA ####

dat_2023 <- read_xlsx(path = "sym_link_soil_biology/data/2023_worm_data.xlsx", 
                 sheet = 1, 
                 col_names = TRUE)


dat_2024 <- read_xlsx(path = "data/2024_worm_data.xlsx", 
                      sheet = 1, 
                      col_names = TRUE)


### combine all data sets ####

#all_dat <- rbind(dat_2023, dat_2024)

all_dat <- read.csv(file = "data/worm_data.csv")

# Replace NA with 0
all_dat[is.na(all_dat)] <- 0



### 02.2 FACTORS ####

all_dat$treatment <- factor(all_dat$treatment, 
                                levels = c("Conventional", "Conservation"))

all_dat$block <- factor(all_dat$block, 
                            levels = c("1", "2", "3", "4", "5"))

all_dat$plot <- factor(all_dat$plot, 
                           levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))

all_dat$year <- factor(all_dat$year, 
                       levels = c("2022", "2023", "2024"))

print(unique(all_dat$crop))

all_dat$crop <- factor(all_dat$crop, 
                       levels = c("Spring Beans" , "Winter Wheat" , "Oilseed Rape" , "Spring Barley"))





### 02.3 SUMMARY STATS ####

#### juveniles ####

# Calculates mean, sd, se and IC - block
juvinile_sum <- all_dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n = n(),
    mean = mean(juveniles_m2),
    sd = sd(juveniles_m2)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

juvinile_sum$treatment <- factor(juvinile_sum$treatment, 
                            levels = c("Conventional", "Conservation"))


#### epigeic ####

epigeic_sum <- all_dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n=n(),
    mean = mean(epigeic_m2),
    sd = sd(epigeic_m2)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1 - 0.05) / 2 + .5, n-1))

epigeic_sum$treatment <- factor(x = epigeic_sum$treatment, 
                      levels = c("Conventional", "Conservation"))


#### endogeic ####

endogeic_sum <- all_dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n=n(),
    mean = mean(endogeic_m2),
    sd = sd(endogeic_m2)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1 - 0.05) / 2 + .5, n-1))


endogeic_sum$treatment <- factor(x = endogeic_sum$treatment, 
                      levels = c("Conventional", "Conservation"))




#### anecic ####

anecic_sum <- all_dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n=n(),
    mean = mean(anecic_m2),
    sd = sd(anecic_m2)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1 - 0.05) / 2 + .5, n-1))


anecic_sum$treatment <- factor(x = anecic_sum$treatment, 
                                 levels = c("Conventional", "Conservation"))



#### total_worms_m2 ####

total_worms_m2_sum <- all_dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n=n(),
    mean = mean(total_worms_m2),
    sd = sd(total_worms_m2)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1 - 0.05) / 2 + .5, n-1))


total_worms_m2_sum$treatment <- factor(x = total_worms_m2_sum$treatment, 
                               levels = c("Conventional", "Conservation"))


#### total_mass_g_m2 ####

total_mass_g_m2_sum <- all_dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n=n(),
    mean = mean(mass_g_m2),
    sd = sd(mass_g_m2)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1 - 0.05) / 2 + .5, n-1))


total_mass_g_m2_sum$treatment <- factor(x = total_mass_g_m2_sum$treatment, 
                                       levels = c("Conventional", "Conservation"))

