# Install/Load the libraries
if(!require("pacman")) install.packages("pacman"); library (pacman)
pacman::p_load(rstudioapi, jsonlite, dplyr, ggplot2, zoo, ggrepel, openxlsx,
               lubridate, scales, knitr, gt, tidyverse, purrr, plotly)

# Load relevant data
tblListeningData <- readRDS(paste0("Data/", params$strName, "/Listening Data.rds"))
