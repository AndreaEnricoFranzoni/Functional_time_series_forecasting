rm(list=ls())
graphics.off()
cat("\014")

set.seed(23032000)


#put here the directory of the file install (in this way every load will be coherent with the folders architecture)
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"

#installing packages
source(paste0(dir_w,"/requirements.R"))

#installing PPCKO
devtools::install_github("AndreaEnricoFranzoni/PPCforAutoregressiveOperator", force = TRUE)
library(PPCKO)