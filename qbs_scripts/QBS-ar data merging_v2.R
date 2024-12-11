## ---------------------------
## Script name: QBS data join of multiple samples
##
## Purpose of script: Take a folder containing QBS template with data from
## multiple samples (one sheet per sample), join them and write output csv files. 
## This is applied to both the "sample" data (sheet 2 of the QBS template) and 
## to the "subsample" data (sheet 3 of the QBS template) (as well as to the MINOTAUR data sheet).
## The scripts create three different data frames that can be directly analyzed in
## R, without the need of writing out csv. In that case, the dataframe to be considered are "dfOutSample" and "dfOutSubsample".
## In addition, the user have to consider that "Acari_20" is the name of the first variable (i.e. column) with 
## actual community abundance data. Please also note that the columns before "Acari_20" contains meta-data, ancillary data, and other
## calculations, such as the QBS-ar, the QBS-ar_BF, and the spectral analysis

##
## Author: Francesco Vitali, Elena Tondini
## Email: francesco.vitali@crea.gov.it, elena.tondini@crea.gov.it
## Date Created: 28-03-2023
## Version: 2.0
##
## To ease the process of joining results from multiple samples, we prepared a script to automatically produce three data files (*.csv or *.xlsx, or both)
## extracting data from all the *.xlsx QBS template file (filled with observation data) that are collected in a specific folder.
## The script is written in R (https://cran.r-project.org/ - accessed on 01/09/2022). Users are suggested to install both R and Rstudio Desktop (https://www.rstudio.com/), 
## and are required to install the "readxl" and "writexl" package for handling the xlsx files. Brief instructions are as follows:
## 1) Install R software in your computer (5a-10a)
## 2) Optionally (but recommended), install RStudio in your computer (5b-9b)
## 3) In R, install the "readxl" and "writexl" package: run the code install.packages("readxl") or use the Tools -> Install Package menù in RStudio. repeat for the "writexl" package.
## 4) Place all the QBS templates template files, compiled with observation data, in a single folder

## IF USING BASE R
## 5a) Download the file "3.QBS-ar data merging_v2.R", place the script in any folder on your computer. To simplify running the code, the folder containing compiled QBS-ar sheet should be placed in the same folder as the script.
## 6a) Use a text editor (suggested in Windows, is notepad, do not use Word!) to provide the path to the folder containing QBS compiled templates. Optionally, the script can be placed in the same folder
## 7a) The user should take the full folder path ( In Windows, this can be done by right clicking on the folder, and then select "Properties", there you will find the path to the folder, which can be copied) 
##     and paste it in substitution of "/path/to/folder" in the "3.QBS-ar data merging.R"
## 8a) In Windows, the user should change the "\" character in "/"
## 9a) Open "R" and set the working directory to the path where the R script is placed. This can be done with the setwd("/path/to/folder") command, using the copied local folder part in substitution of "/path/to/folder" 
## 10a) Run the script by using the code source("3.QBS-ar data merging.R"). The excel files are not accessible by R if they are opened, so be sure to close all of the files that have to be merged

## IF USING RSTUDIO
## 5b) Download the file "3.QBS-ar data merging_v2.R" and open it in RStudio. The file should be automatically recognised and opened in Rstudio, and can be simply opened by double left click. If not it can be opened from Rstudio (File -> Open File) or by right click and "Open with" selecting RStudio
## To simplify running the code, the folder containing compiled QBS-ar sheet should be placed in the same folder as the script
## 6b) Upon opening, you will see some text (i.e., the code). The only requested action by the user, is to modify the path to the folder containing QBS compiled templates, as indicated at line 18 of the script, in substitution of "/path/to/folder", as done for step 7a
## 7b) In Windows, the full folder path can be obtained by right clicking on the folder, and then select "Properties", there you will find the path to the folder, which can be copied
## 8b) In windows, the user should change the "\" character in "/"
## 9b) Run the whole script by clicking ctrl+shift+enter, or run it line by line with ctrl+enter. The excel files are not accessible by R if they are opened, so be sure to close all of the files that have to be merged

##  ---------------------------

# Set User parameters

analysisDir <- "c:/path/to/folder"    
resFolder <- paste0(analysisDir, "/Results")
# Change it accordingly to the full path of the folder containing QBS files
# In Windows, the full folder path can be obtained by right clicking on the folder, 
# and then select "Properties", there you will find the path to the folder, 
# which can be copied. In windows, the user should change the "\" character in "/"

OutputType <- "csv" # default to csv
# Change it accordingly to the preferred output format. Possible values:
# 1: "csv" if just a csv file is desired. Field separator is TAB, decimal separator is "."
# 2: "xlsx" if just an xls file is desider.
# 3: "both" if both files are desired. 

# Create output folder
print("Creating results folder")
if (!dir.exists(resFolder)){
  dir.create(resFolder)
}else{
  print("Results folder already exist")
}

# Set default parameters and create default objects
defaultColSample <- c("Sample_code",
                      "Sampling_date",
                      "Lat_mean",
                      "Long_mean",
                      "Sampled_area_for_each_subsample",
                      "Soil_type_WRB",
                      "Texture",
                      "Land_use",
                      "Farming_system"	,
                      "Cropping_system",
                      "Type_main_crop",
                      "Dominant_plant_species",
                      "Fertilizer_type",
                      "Plant_protection_products",
                      "Last_tillage_event",
                      "Tillage_sytem",
                      "Tillage_depth_cm",
                      "Irrigation_type",
                      "Grazing_method"	,
                      "Last_fertilizer_application",
                      "Last_irrigation_event",
                      "Last_PPP_application",
                      "Weight_mean",
                      "QBS-ar",
                      "QBS-ar_BF",
                      "Spectral_analysis",
                      "Variability_replicates",
                      "Eudaphic_n_ar",
                      "Emiedaphic_n_ar",
                      "Epigeic_n_ar",
                      "Eudaphic_ab_ar",
                      "Emiedaphic_ab_ar",
                      "Epigeic_ab_ar",
                      "Eudaphic_n_BF",
                      "Emiedaphic_n_BF",
                      "Epigeic_n_BF",
                      "Eudaphic_ab_BF",
                      "Emiedaphic_ab_BF",
                      "Epigeic_ab_BF",
                      "Acari_20",
                      "Araneae_01",
                      "Araneae_05",
                      "Pseudoscorp._20",
                      "Opiliones_10",
                      "Palpigradi_20",
                      "Isopoda_10",
                      "Symphyla_20",
                      "Diplopoda_10",
                      "Diplopoda_20",
                      "Chilopoda_10",
                      "Chilopoda_20",
                      "Pauropoda_20",
                      "Collembola_01",
                      "Collembola_02",
                      "Collembola_04",
                      "Collembola_06",
                      "Collembola_08",
                      "Collembola_10",
                      "Collembola_20",
                      "Diplura_20",
                      "Protura_20",
                      "Coleoptera_01",
                      "Coleoptera_05",
                      "Coleoptera_10",
                      "Coleoptera_15",
                      "Coleoptera_20",
                      "Coleoptera-L_10",
                      "Diptera_01",
                      "Diptera-L_10",
                      "Hymenoptera_01",
                      "Hymenoptera_05",
                      "Hymenoptera-L_10",
                      "Hemiptera_01",
                      "Psocoptera_01",
                      "Thysanoptera_01",
                      "Embioptera_10",
                      "Dermaptera_01",
                      "Other")

defaultColSubsample <- c("ID_subsample",
                         "Sample_code",
                         "Sampling_date",
                         "Lat",
                         "Long",
                         "Weight",
                         "QBS-ar_BF_rep",
                         "Acari_20",
                         "Araneae_01",
                         "Araneae_05",
                         "Pseudoscorp._20",
                         "Opiliones_10",
                         "Palpigradi_20",
                         "Isopoda_10",
                         "Symphyla_20",
                         "Diplopoda_10",
                         "Diplopoda_20",
                         "Chilopoda_10",
                         "Chilopoda_20",
                         "Pauropoda_20",
                         "Collembola_01",
                         "Collembola_02",
                         "Collembola_04",
                         "Collembola_06",
                         "Collembola_08",
                         "Collembola_10",
                         "Collembola_20",
                         "Diplura_20",
                         "Protura_20",
                         "Coleoptera_01",
                         "Coleoptera_05",
                         "Coleoptera_10",
                         "Coleoptera_15",
                         "Coleoptera_20",
                         "Coleoptera-L_10",
                         "Diptera_01",
                         "Diptera-L_10",
                         "Hymenoptera_01",
                         "Hymenoptera_05",
                         "Hymenoptera-L_10",
                         "Hemiptera_01",
                         "Psocoptera_01",
                         "Thysanoptera_01",
                         "Embioptera_10",
                         "Dermaptera_01",
                         "Other")

defaultColMinotaur<-c("ID_Field",
                      "Observation_level",
                      "ID_sample",
                      "Longitude",
                      "Latitude",
                      "Date_collection",
                      "Sampled_area_for_each_subsample",
                      "Soil_type_WRB",
                      "Texture",
                      "Land_use",
                      "Farming_system"	,
                      "Cropping_system",
                      "Type_main_crop",
                      "Dominant_plant_species",
                      "Fertilizer_type",
                      "Plant_protection_products",
                      "Last_tillage_event",
                      "Tillage_sytem",
                      "Tillage_depth_cm",
                      "Irrigation_type",
                      "Grazing_method"	,
                      "Last_fertilizer_application",
                      "Last_irrigation_event",
                      "Last_PPP_application",
                      "Diversity_index_applied",
                      "Diversity_index_result",
                      "Varaibility_of_diversity_index",
                      "Second_Diversity_index_applied",
                      "Second_Diversity_index_result",
                      "Varaibility_of_second_diversity_index",
                      "Acari_20",
                      "Oribatida",
                      "Acari_Number_taxa",
                      "Collembola_01",
                      "Collembola_02"	,
                      "Collembola_04",
                      "Collembola_06",
                      "Collembola_08",
                      "Collembola_10",
                      "Collembola_20",
                      "Collembola_AB",
                      "Collembola_Number_taxa",
                      "Araneae_01",
                      "Araneae_05",
                      "AB_Araneae_AB",
                      "Araneae_number_taxa",
                      "Chilopoda_10",
                      "Chilopoda_20",
                      "Chilopoda_AB",
                      "Chilopoda_Number_taxa",	
                      "Coleoptera_01",
                      "Coleoptera_05"	,
                      "Coleoptera_10"	,
                      "Coleoptera_15",
                      "Coleoptera_20",
                      "Coleoptera-L_10"	,
                      "Coleoptera_AB",
                      "Coleoptera_Number_taxa",
                      "Dermaptera_01",
                      "Diplopoda_10",	
                      "Diplopoda_20",	
                      "Diplopoda_AB",	
                      "Diplopoda_Number_taxa",	
                      "Diplura_20",	
                      "Diptera_01",	
                      "Diptera-L_10",
                      "Embioptera_10",	
                      "Hemiptera_01",
                      "Hymenoptera_01",
                      "Hymenoptera_05",
                      "Hymenoptera-L_10",
                      "Hymenoptera_AB",
                      "Hymenoptera_Number_taxa",
                      "Isopoda_10",
                      "Opiliones_10",
                      "Palpigradi_20"	,
                      "Pauropoda_20",	
                      "Protura_20",	
                      "Pseudoscorp._20",	
                      "Psocoptera_01",	
                      "Symphyla_20",	
                      "Thysanoptera_01",
                      "Other BFs",	
                      "List of other BFs"	,
                      "EMI value of others BFs"	,
                      "Enchytreids_AB",
                      "Enchytreids_Number_taxa",
                      "Enchytreids_r_strategist_AB",	
                      "Enchytreids_k_strategist_AB",	
                      "Fridericia _AB",	
                      "Enchytraeus_AB")



  

dfOutSample <-  data.frame(matrix(nrow = 0, ncol = length(defaultColSample)))
colnames(dfOutSample) <- defaultColSample

dfOutSubsample <-  data.frame(matrix(nrow = 0, ncol = length(defaultColSubsample)))
colnames(dfOutSubsample) <- defaultColSubsample

dfOutMinotaur <-  data.frame(matrix(nrow = 0, ncol = length(defaultColMinotaur)))
colnames(dfOutMinotaur) <- defaultColMinotaur


# Load libraries
library(readxl)
library(writexl)

# Get the file in the folder
filesList <- list.files(path= analysisDir, pattern="*.xlsx", full.names=TRUE, recursive=FALSE, all.files = F)


#
# Main loop for reading file and creating a dataframe for sample sheet
#

for (i in 1:length(filesList)) {
  
  sample <- filesList[i] # get filenames
  print(paste("Working on file", sample))
  read_excel(sample, sheet = "Export_Sample") -> dfNewRow # read the Export_Sample sheet from the file
  
  # Some check on the input 
  # Should always be 1 row and 78 columns
  if (nrow(dfNewRow) != 1 | ncol(dfNewRow) != 78) {
    print("Error. Expecting 1 row and 60 columns as input")
    break
    # Columns should always be in the order of default_columns
  } else if (!all.equal(colnames(dfNewRow),defaultColSample)) {
    print("Error. Columns are not in the expected order")
    break
  } else {
    dfOutSample <- rbind(dfOutSample, dfNewRow) # grow final df by one row
  }
}


# Save results 

if (OutputType == "csv") { # write only csv file output
  write.table(x = dfOutSample, file = paste0(resFolder,"/Joined_sample_metadata.csv"), sep = "\t", dec = ".", row.names=FALSE)
} else if (OutputType == "xlsx") { # write only xls file output
  write_xlsx(x = dfOutSample, path = paste0(resFolder,"/Joined_sample_metadata.xlsx"))
} else if (OutputType == "both") { # write both
  write.table(x = dfOutSample, file = paste0(resFolder,"/Joined_sample_metadata.csv"), sep = "\t", dec = ".", row.names=FALSE)
  write_xlsx(x = dfOutSample, path = paste0(resFolder,"/Joined_sample_metadata.xlsx"))
}


#
# Main loop for reading file and creating a dataframe for subsample sheet 
#

for (i in 1:length(filesList)) {
  
  sample <- filesList[i] # get filenames
  print(paste("Working on file", sample))
  read_excel(sample, sheet = "Export_Subsamples") -> dfNewRow # read the Export_Subsample sheet from the file
  
  # Some check on the input 
  # Should always be 3 row and 46 columns
  if (nrow(dfNewRow) != 3 | ncol(dfNewRow) != 46) {
    print("Error. Expecting 3 rows and 46 columns as input")
    break
    # Columns should always be in the order of default_columns
  } else if (!all.equal(colnames(dfNewRow),defaultColSubsample)) {
    print("Error. Columns are not in the expected order")
    break
  } else {
    dfOutSubsample <- rbind(dfOutSubsample, dfNewRow) # grow final df by one row
  }
}

# Save results 

if (OutputType == "csv") { # write only csv file output
  write.table(x = dfOutSubsample, file = paste0(resFolder,"/Joined_subsample.csv"), sep = "\t", dec = ".", row.names=FALSE)
} else if (OutputType == "xlsx") { # write only xls file output
  write_xlsx(x = dfOutSubsample, path = paste0(resFolder,"/Joined_subsample.xlsx"))
} else if (OutputType == "both") { # write both
  write.table(x = dfOutSubsample, file = paste0(resFolder,"/Joined_subsample.csv"), sep = "\t", dec = ".", row.names=FALSE)
  write_xlsx(x = dfOutSubsample, path = paste0(resFolder,"/Joined_subsample.xlsx"))
}


#
# Main loop for reading file and creating a dataframe for Export_MINOTAUR
#

for (i in 1:length(filesList)) {
  
  sample <- filesList[i] # get filenames
  print(paste("Working on file", sample))
  read_excel(sample, sheet = "Export_MINOTAUR") -> dfNewRow # read the Export_MINOTAUR sheet from the file
  
  # Some check on the input 
  # Should always be 4 row and 91 columns
  if (nrow(dfNewRow) != 4 | ncol(dfNewRow) != 91) {
    print("Error. Expecting 4 row and 91 columns as input")
    break
    # Columns should always be in the order of default_columns
  } else if (!all.equal(colnames(dfNewRow),defaultColMinotaur)) {
    print("Error. Columns are not in the expected order")
    break
  } else {
    dfOutMinotaur<- rbind(dfOutMinotaur, dfNewRow) # grow final df by one row
  }
}

# Save results 

if (OutputType == "csv") { # write only csv file output
  write.table(x = dfOutMinotaur, file = paste0(resFolder,"/Joined_MINOTAUR.csv"), sep = "\t", dec = ".", row.names=FALSE)
} else if (OutputType == "xlsx") { # write only xls file output
  write_xlsx(x = dfOutMinotaur, path = paste0(resFolder,"/Joined_MINOTAUR.xlsx"))
} else if (OutputType == "both") { # write both
  write.table(x = dfOutMinotaur, file = paste0(resFolder,"/Joined_MINOTAUR.csv"), sep = "\t", dec = ".", row.names=FALSE)
  write_xlsx(x = dfOutMinotaur, path = paste0(resFolder,"/Joined_MINOTAUR.xlsx"))
}

