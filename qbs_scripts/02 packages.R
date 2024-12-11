# 02 PACKAGES ####
## WHO: JOE COLLINS
## WHAT: QBS data PACKAGES
## LAST EDIT: 16/01/2024
####

## 01 Load packages ####

# Install and load necessary packages if not already installed

### Helper function to install and load packages
install_and_load <- function(package, description) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, repos = "http://cran.us.r-project.org")
    library(package, character.only = TRUE)
  }
  message(description)
}


### Data ####
install_and_load("dplyr", "dplyr: general data manipulation tools")
install_and_load("sjmisc", "sjmisc: data management tools for labeled data")
install_and_load("readxl", "readxl: tools to read Excel files")
install_and_load("reshape2", "reshape2: tools for reshaping data")
install_and_load("devtools", "devtools: development tools for R packages")

### plots ####
install_and_load("ggplot2", "ggplot2: data visualization using the Grammar of Graphics")
install_and_load("ggsci", "ggsci: scientific color palettes for ggplot2")
install_and_load("gridExtra", "gridExtra: tools for arranging grid-based plots")
install_and_load("viridis", "viridis: color scales for plots")
install_and_load("ggbiplot", "ggbiplot: PCA plotting using ggplot2")
install_and_load("ggpubr", "ggpubr: tools for arranging ggplots and adding statistics")

### spatial packages ####
install_and_load("sf", "sf: handling simple features (spatial vector data)")
install_and_load("raster", "raster: tools for raster data analysis")
install_and_load("rasterVis", "rasterVis: visualizations for raster data")
install_and_load("rgdal", "rgdal: geospatial data handling tools")

### statistics ####
install_and_load("sjPlot", "sjPlot: creates tables for regression and descriptive statistics")
install_and_load("sjlabelled", "sjlabelled: label management for data frames")
install_and_load("broom", "broom: converts statistical analysis objects into tidy tibbles")
install_and_load("factoextra", "factoextra: visualization tools for multivariate data analysis")
install_and_load("lme4", "lme4: tools for fitting mixed-effects models")
install_and_load("emmeans", "emmeans: pairwise comparisons for GLM's")
install_and_load("lmerTest", "p values for lme")
install_and_load("MASS", "for negative binomial models")
install_and_load("AER", "overdispersion tests")

### Biodiversity ####
install_and_load("vegan", "for diversity calculation")

