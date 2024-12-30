rm(list=ls())
graphics.off()
cat("\014")
set.seed(23032000)


### Comparison between solvers


dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"
dir_res = paste0(dir_w,"/Test_domain1D/RealWorld_data/results")


#in which folder the result of the prediction are
path_res_pred = paste0(dir_res,"/results_prediction")
path_res_err = paste0(dir_res,"/results_errors")


prediction_method = c("PPC", "PPC_gen")

for (pred_met in prediction_method) {
  
  files <- list.files(path = paste0(path_res_pred,paste0("/",pred_met)), full.names = TRUE)
  for (file in files) {
    load(file)
  }
}


load(paste0(path_res_err,"/PPC_err.Rdata"))
load(paste0(path_res_err,"/PPC_gen_err.Rdata"))



##-----offer-----
en_ex  = err_PPC$pred_offer$en
en_gep = err_PPC_gen$pred_offer$en
summary(en_ex)
summary(en_gep)
sd(en_ex)
sd(en_gep)

en_ex - en_gep


rn_ex  = err_PPC$pred_offer$rn
rn_gep = err_PPC_gen$pred_offer$rn
summary(rn_ex)
summary(rn_gep)
sd(rn_ex)
sd(rn_gep)

rn_ex - rn_gep

N = length(prediction_PPC_offer)
#reg param
for (i in 1:N) {
  print(prediction_PPC_offer[[i]]$Alpha - prediction_PPC_gen_offer[[i]]$Alpha)
}
#number of PPCs
for (i in 1:N) {
  print(prediction_PPC_offer[[i]]$N_PPCs - prediction_PPC_gen_offer[[i]]$N_PPCs)
}



##-----demand-----
en_ex  = err_PPC$pred_demand$en
en_gep = err_PPC_gen$pred_demand$en
summary(en_ex)
summary(en_gep)
sd(en_ex)
sd(en_gep)

en_ex - en_gep


rn_ex  = err_PPC$pred_demand$rn
rn_gep = err_PPC_gen$pred_demand$rn
summary(rn_ex)
summary(rn_gep)
sd(rn_ex)
sd(rn_gep)

rn_ex - rn_gep

N = length(prediction_PPC_offer)
#reg param
for (i in 1:N) {
  print(prediction_PPC_demand[[i]]$Alpha - prediction_PPC_gen_demand[[i]]$Alpha)
}
#number of PPCs
for (i in 1:N) {
  print(prediction_PPC_demand[[i]]$N_PPCs - prediction_PPC_gen_demand[[i]]$N_PPCs)
}
