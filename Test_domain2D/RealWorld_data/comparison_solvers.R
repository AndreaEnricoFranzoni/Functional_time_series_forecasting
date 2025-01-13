rm(list=ls())
graphics.off()
cat("\014")
set.seed(23032000)


################################################
#### Compare PPC ex_solver and gep_solver   ####
################################################

#put here the path of the local copy of the directory
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"



dir_res = paste0(dir_w,"/Test_domain2D/RealWorld_data/results")


#in which folder the result of the prediction are
path_res_pred = paste0(dir_res,"/results_prediction")

prediction_method = c("PPC", "PPC_gen")

for (pred_met in prediction_method) {
  
  files <- list.files(path = paste0(path_res_pred,paste0("/",pred_met)), full.names = TRUE)
  for (file in files) {
    load(file)
  }
}


#in which folder the errors are
path_res_err = paste0(dir_res,"/results_prediction_errors")

diff_strat = c("original","diff_1","diff_2")
zone = c("mouth","center")

for (strat_df in diff_strat){
  for (zone_pd in zone){
    files <- list.files(path = paste0(paste0(path_res_err,paste0("/",zone_pd)),paste0("/",strat_df)), full.names = TRUE)
    for (file in files) {
      load(file)
    }
  }
}



##----mouth----

##----original----
ex_err = err_PPC_mouth
gep_err = err_PPC_gen_mouth

ex_pred = prediction_PPC_mouth
gep_pred = prediction_PPC_gen_mouth
N = length(ex_pred)

summary(ex_err$en)
summary(gep_err$en)
sd(ex_err$en)
sd(gep_err$en)

summary(ex_err$rn)
summary(gep_err$rn)
sd(ex_err$rn)
sd(gep_err$rn)


#reg param
for (i in 1:N) {
  print(ex_pred[[i]]$Alpha - gep_pred[[i]]$Alpha)
}
#number of PPCs
for (i in 1:N) {
  print(ex_pred[[i]]$N_PPCs - gep_pred[[i]]$N_PPCs)
}


##----diff 1----
ex_err = err_PPC_mouth_diff_1
gep_err = err_PPC_gen_mouth_diff_1

ex_pred = prediction_PPC_mouth
gep_pred = prediction_PPC_gen_mouth
N = length(ex_pred)

summary(ex_err$en)
summary(gep_err$en)
sd(ex_err$en)
sd(gep_err$en)

summary(ex_err$rn)
summary(gep_err$rn)
sd(ex_err$rn)
sd(gep_err$rn)


#reg param
for (i in 1:N) {
  print(ex_pred[[i]]$Alpha - gep_pred[[i]]$Alpha)
}

#number of PPCs
for (i in 1:N) {
  print(ex_pred[[i]]$N_PPCs - gep_pred[[i]]$N_PPCs)
}




##----diff 2----
ex_err = err_PPC_mouth_diff_2
gep_err = err_PPC_gen_mouth_diff_2

ex_pred = prediction_PPC_mouth
gep_pred = prediction_PPC_gen_mouth
N = length(ex_pred)

summary(ex_err$en)
summary(gep_err$en)
sd(ex_err$en)
sd(gep_err$en)

summary(ex_err$rn)
summary(gep_err$rn)
sd(ex_err$rn)
sd(gep_err$rn)


#reg param
for (i in 1:N) {
  print(ex_pred[[i]]$Alpha - gep_pred[[i]]$Alpha)
}

#number of PPCs
for (i in 1:N) {
  print(ex_pred[[i]]$N_PPCs - gep_pred[[i]]$N_PPCs)
}






##----center----

##----original----
ex_err = err_PPC_center
gep_err = err_PPC_gen_center

ex_pred = prediction_PPC_mouth
gep_pred = prediction_PPC_gen_mouth
N = length(ex_pred)

summary(ex_err$en)
summary(gep_err$en)
sd(ex_err$en)
sd(gep_err$en)

summary(ex_err$rn)
summary(gep_err$rn)
sd(ex_err$rn)
sd(gep_err$rn)


#reg param
for (i in 1:N) {
  print(ex_pred[[i]]$Alpha - gep_pred[[i]]$Alpha)
}
#number of PPCs
for (i in 1:N) {
  print(ex_pred[[i]]$N_PPCs - gep_pred[[i]]$N_PPCs)
}


##----diff 1----
ex_err = err_PPC_center_diff_1
gep_err = err_PPC_gen_center_diff_1

ex_pred = prediction_PPC_mouth
gep_pred = prediction_PPC_gen_mouth
N = length(ex_pred)

summary(ex_err$en)
summary(gep_err$en)
sd(ex_err$en)
sd(gep_err$en)

summary(ex_err$rn)
summary(gep_err$rn)
sd(ex_err$rn)
sd(gep_err$rn)


#reg param
for (i in 1:N) {
  print(ex_pred[[i]]$Alpha - gep_pred[[i]]$Alpha)
}

#number of PPCs
for (i in 1:N) {
  print(ex_pred[[i]]$N_PPCs - gep_pred[[i]]$N_PPCs)
}




##----diff 2----
ex_err = err_PPC_center_diff_2
gep_err = err_PPC_gen_center_diff_2

ex_pred = prediction_PPC_mouth
gep_pred = prediction_PPC_gen_mouth
N = length(ex_pred)

summary(ex_err$en)
summary(gep_err$en)
sd(ex_err$en)
sd(gep_err$en)

summary(ex_err$rn)
summary(gep_err$rn)
sd(ex_err$rn)
sd(gep_err$rn)


#reg param
for (i in 1:N) {
  print(ex_pred[[i]]$Alpha - gep_pred[[i]]$Alpha)
}

#number of PPCs
for (i in 1:N) {
  print(ex_pred[[i]]$N_PPCs - gep_pred[[i]]$N_PPCs)
}












