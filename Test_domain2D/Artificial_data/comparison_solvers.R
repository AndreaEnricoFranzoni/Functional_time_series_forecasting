rm(list=ls())
graphics.off()
cat("\014")
set.seed(23032000)


################################################
#### Compare PPC ex_solver and gep_solver   ####
################################################

#put here the path of the local copy of the directory
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"


#in which folder the result of the prediction are
dir_res = paste0(dir_w,"/Test_domain2D/Artificial_data/results")
path_res_pred = paste0(dir_res,"/results_prediction")


#evaluate the results of the two solvers
prediction_method = c("PPC", "PPC_gen")

for (pred_met in prediction_method) {
  
  files <- list.files(path = paste0(path_res_pred,paste0("/",pred_met)), full.names = TRUE)
  for (file in files) {
    load(file)
  }
}



N = length(err_PPC_en)


summary(err_PPC_en)
summary(err_PPC_gen_en)
sd(err_PPC_en)
sd(err_PPC_gen_en)

err_PPC_en - err_PPC_gen_en



summary(err_PPC_rn)
summary(err_PPC_gen_rn)
sd(err_PPC_rn)
sd(err_PPC_gen_rn)

err_PPC_rn - err_PPC_gen_rn


#reg param
for (i in 1:N) {
  print(pred_PPC[[i]]$Alpha - pred_PPC_gen[[i]]$Alpha)
}
#number of PPCs
for (i in 1:N) {
  print(pred_PPC[[i]]$N_PPCs - pred_PPC_gen[[i]]$N_PPCs)
}
