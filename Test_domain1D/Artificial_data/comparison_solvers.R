rm(list=ls())
graphics.off()
cat("\014")
set.seed(23032000)


### Comparison between solvers


dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"
dir_res = paste0(dir_w,"/Test_domain1D/Artificial_data/results")


#in which folder the result of the prediction are
path_res_pred = paste0(dir_res,"/results_prediction")


prediction_method = c("PPC", "PPC_gen")

for (pred_met in prediction_method) {
  
  files <- list.files(path = paste0(path_res_pred,paste0("/",pred_met)), full.names = TRUE)
  for (file in files) {
    load(file)
  }
}


name_kernel = "gau"
#name_kernel = "id"
#name_kernel = "spt"
#name_kernel = "sps"

norm_kernel = "0_5"
#norm_kernel = "0_8"


ex_slv = get(paste0("res_PPC_",paste0(paste0(name_kernel,"_"),norm_kernel)))
gep_slv = get(paste0("res_PPC_gen_",paste0(paste0(name_kernel,"_"),norm_kernel)))

N = length(ex_slv$En)


en_ex  = ex_slv$En
en_gep = gep_slv$En
summary(en_ex)
summary(en_gep)
sd(en_ex)
sd(en_gep)

en_ex - en_gep


rn_ex  = ex_slv$Rn
rn_gep = gep_slv$Rn
summary(rn_ex)
summary(rn_gep)
sd(rn_ex)
sd(rn_gep)

rn_ex - rn_gep


#reg param
for (i in 1:N) {
  print(ex_slv$Prediction[[i]]$Alpha - gep_slv$Prediction[[i]]$Alpha)
}
#number of PPCs
for (i in 1:N) {
  print(ex_slv$Prediction[[i]]$N_PPCs - gep_slv$Prediction[[i]]$N_PPCs)
}