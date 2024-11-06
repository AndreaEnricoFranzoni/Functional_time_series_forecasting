dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series"

#installing packages
source(paste0(dir_w,"/utils/requirements.R"))




#just for finishing creating package
setwd("/Users/andreafranzoni/Documents/Politecnico/Magistrale/PPCforAutoregressiveOperator_local")
devtools::install("PPCKO.local")
setwd("/Users/andreafranzoni/Documents/Politecnico/Magistrale/PPCforAutoregressiveOperator_local2")
devtools::install("PPCKO.local2")
setwd("/Users/andreafranzoni/Documents/Politecnico/Magistrale/KE_local")
devtools::install("KE.local")


dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_prediction"
setwd(dir_w)





#installing PPCKO
devtools::install_github("AndreaEnricoFranzoni/PPCforAutoregressiveOperator", force = TRUE)