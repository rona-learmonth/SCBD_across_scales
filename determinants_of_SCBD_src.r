#determinants_of_species_contributions_to_beta_diversity.R
#

# --------------------------------------------------------------------------
# REQUIRES
# --------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(adespatial)
  library(ape)
  library(BIEN)
  library(betareg)
  library(car)
  library(data.table)
  library(dotwhisker)
  library(dplyr)
  library(ggplot2)
  library(ggpubr)
  library(labdsv)
  library(lubridate)
  library(MuMIn)
  library(openxlsx)
  library(pez)
  library(picante)
  library(phytools)
  library(purrr)
  library(readr)
  library(readxl)
  library(rgbif)
  library(stringr)
  library(tidyr)
  library(vegan)
  library(viridis)
})

# --------------------------------------------------------------------------
# PATHS
# --------------------------------------------------------------------------

data_path <- file.path(getwd(), "data")
reports_path <- file.path(getwd(), "reports")
figures_path <- file.path(getwd(), "reports", "plots")
models_path <- file.path(getwd(),"reports","models")

if (!dir.exists(data_path)) {
  dir.create(data_path, recursive = TRUE)
}

if (!dir.exists(figures_path)) {
  dir.create(figures_path, recursive = TRUE)
}

if (!dir.exists(reports_path)) {
  dir.create(reports_path, recursive = TRUE)
}


