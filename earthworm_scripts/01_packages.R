### Earthworm data
### J Collins 
### 2024-06-01
###

## 01 PACKAGES ####

suppressPackageStartupMessages({
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(ggplot2)) install.packages("ggplot2")
  if (!require(ggpubr)) install.packages("ggpubr")
  if (!require(gridExtra)) install.packages("gridExtra")
  if (!require(readxl)) install.packages("readxl")
  if (!require(readr)) install.packages("ggrepel")
  if (!require(plotrix)) install.packages("plotrix")
  if (!require(plotrix)) install.packages("rstatix")
  if (!require(plotrix)) install.packages("agricolae")
  if (!require(plotrix)) install.packages("ggstats")
  if (!require(plotrix)) install.packages("nlme")
  
  library(dplyr) # for "glimpse" and data manipulation
  library(ggplot2) # general plotting
  library(ggpubr) # custom plotting
  library(gridExtra) # grid plotting
  library(readxl) # read .xlsx files
  library(ggrepel) # Automatically position non-overlapping text labels with 'ggplot2'
  library(plotrix) # standard error
  library("rstatix") # significance bars in plot 
  library("agricolae") # poster theme
  library("ggstats") # calculate proportions and add them to barplots
  library("nlme")
})
