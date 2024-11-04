dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_prediction"

#installing packages
source(paste0(dir_w,"/utils/requirements.R"))
#installing PPCKO
devtools::install_github("AndreaEnricoFranzoni/PPCforAutoregressiveOperator", force = TRUE)



#just for finishing creating package
setwd("/Users/andreafranzoni/Documents/Politecnico/Magistrale/PPCforAutoregressiveOperator_local")
devtools::install("PPCKO.local")
setwd("/Users/andreafranzoni/Documents/Politecnico/Magistrale/PPCforAutoregressiveOperator_local2")
devtools::install("PPCKO.local2")


dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_prediction"
setwd(dir_w)