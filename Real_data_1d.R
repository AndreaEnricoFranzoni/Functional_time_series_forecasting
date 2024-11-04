rm(list=ls())
graphics.off()
cat("\014")

set.seed(23032000)

#change here
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_prediction"
load(paste0(dir_w,"/utils/data/data_1d/data.Rdata"))
