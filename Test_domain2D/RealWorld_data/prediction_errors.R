rm(list=ls())
graphics.off()
cat("\014")

set.seed(23032000)


dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"
dir_res = paste0(dir_w,"/Test_domain2D/RealWorld_data/results")


#in which folder the result of the prediction are
path_res_pred = paste0(dir_res,"/results_prediction")
#if you want to save the result 
save_res = TRUE

#where to store the results
path_stor_res = paste0(paste0(dir_res,"/results_prediction_errors"))  

prediction_method = c("PPC", "PPC_exp_pow", "PPC_gen","MP", "NP")

for (pred_met in prediction_method) {
  
  files <- list.files(path = paste0(path_res_pred,paste0("/",pred_met)), full.names = TRUE)
  for (file in files) {
    load(file)
  }
  
}


#where the data are
dir_data = paste0(dir_w,"/Test_domain2D/RealWorld_data/data")
path_data_mouth = paste0(dir_data,"/mouth")
path_data_center = paste0(dir_data,"/center")



#load the dataset
load(paste0(dir_data,"/BS.Rdata"))
#load data for mouth zone
load(paste0(path_data_mouth,"/BS_mouth.Rdata"))
#load data for center zone
load(paste0(path_data_center,"/BS_center.Rdata"))


#paramter for the data
first_day_first_train = "20170101"
idx_first_day_first_train = which(dates==first_day_first_train)
window_train_set = 99
days_to_be_pred = 1000

N = days_to_be_pred


## ----- mouth -----
# errors on mouth predictions

mouth = data_2d_wrapper_from_list(Xt_mouth)

#store errors
{
  en_PPC_mouth = numeric(N)
  en_PPC_mouth_diff_1 = numeric(N)
  en_PPC_mouth_diff_2 = numeric(N)
  
  rn_PPC_mouth = numeric(N)
  rn_PPC_mouth_diff_1 = numeric(N)
  rn_PPC_mouth_diff_2 = numeric(N)
  
  
  en_PPC_exp_pow_mouth = numeric(N)
  en_PPC_exp_pow_mouth_diff_1 = numeric(N)
  en_PPC_exp_pow_mouth_diff_2 = numeric(N)
  
  rn_PPC_exp_pow_mouth = numeric(N)
  rn_PPC_exp_pow_mouth_diff_1 = numeric(N)
  rn_PPC_exp_pow_mouth_diff_2 = numeric(N)
  
  
  en_PPC_gen_mouth = numeric(N)
  en_PPC_gen_mouth_diff_1 = numeric(N)
  en_PPC_gen_mouth_diff_2 = numeric(N)
  
  rn_PPC_gen_mouth = numeric(N)
  rn_PPC_gen_mouth_diff_1 = numeric(N)
  rn_PPC_gen_mouth_diff_2 = numeric(N)
  
  
  en_MP_mouth = numeric(N)
  en_MP_mouth_diff_1 = numeric(N)
  en_MP_mouth_diff_2 = numeric(N)
  
  rn_MP_mouth = numeric(N)
  rn_MP_mouth_diff_1 = numeric(N)
  rn_MP_mouth_diff_2 = numeric(N)
  
  
  en_NP_mouth = numeric(N)
  en_NP_mouth_diff_1 = numeric(N)
  en_NP_mouth_diff_2 = numeric(N)
  
  rn_NP_mouth = numeric(N)
  rn_NP_mouth_diff_1 = numeric(N)
  rn_NP_mouth_diff_2 = numeric(N)
}


for(i in 1:days_to_be_pred){
  
  
  # real value to be predicted
  test_set = mouth[,i + idx_first_day_first_train + window_train_set]
  
  
  # errors on direct prediction
  pred_ppc = as.vector(prediction_PPC_mouth[[i]]$Prediction)
  pred_ppc_exp_pow = as.vector(prediction_PPC_exp_pow_mouth[[i]]$Prediction)
  pred_ppc_gen = as.vector(prediction_PPC_gen_mouth[[i]]$Prediction)
  pred_mp  = prediction_MP_mouth[[i]]$Prediction
  pred_np  = prediction_NP_mouth[[i]]$Prediction
  
  en_PPC_mouth[i] = sqrt(MLmetrics::MSE(pred_ppc[!is.na(test_set) & !is.na(pred_ppc)], test_set[!is.na(test_set) & !is.na(pred_ppc)]))
  rn_PPC_mouth[i] = MLmetrics::MAE(pred_ppc[!is.na(test_set) & !is.na(pred_ppc)], test_set[!is.na(test_set) & !is.na(pred_ppc)])
  
  en_PPC_exp_pow_mouth[i] = sqrt(MLmetrics::MSE(pred_ppc_exp_pow[!is.na(test_set) & !is.na(pred_ppc_exp_pow)], test_set[!is.na(test_set) & !is.na(pred_ppc_exp_pow)]))
  rn_PPC_exp_pow_mouth[i] = MLmetrics::MAE(pred_ppc_exp_pow[!is.na(test_set) & !is.na(pred_ppc_exp_pow)], test_set[!is.na(test_set) & !is.na(pred_ppc_exp_pow)])
  
  en_PPC_gen_mouth[i] = sqrt(MLmetrics::MSE(pred_ppc_gen[!is.na(test_set) & !is.na(pred_ppc_gen)], test_set[!is.na(test_set) & !is.na(pred_ppc_gen)]))
  rn_PPC_gen_mouth[i] = MLmetrics::MAE(pred_ppc_gen[!is.na(test_set) & !is.na(pred_ppc_gen)], test_set[!is.na(test_set) & !is.na(pred_ppc_gen)])
  
  en_MP_mouth[i]  = sqrt(MLmetrics::MSE(pred_mp[!is.na(test_set) & !is.na(pred_mp)], test_set[!is.na(test_set) & !is.na(pred_mp)]))
  rn_MP_mouth[i]  = MLmetrics::MAE(pred_mp[!is.na(test_set) & !is.na(pred_mp)], test_set[!is.na(test_set) & !is.na(pred_mp)])
  
  en_NP_mouth[i]  = sqrt(MLmetrics::MSE(pred_np[!is.na(test_set) & !is.na(pred_np)], test_set[!is.na(test_set) & !is.na(pred_np)]))
  rn_NP_mouth[i]  = MLmetrics::MAE(pred_np[!is.na(test_set) & !is.na(pred_np)], test_set[!is.na(test_set) & !is.na(pred_np)])
  
  
  # errors on diff1
  pred_ppc = as.vector(prediction_PPC_mouth_diff_1[[i]]$Prediction)
  pred_ppc_exp_pow = as.vector(prediction_PPC_exp_pow_mouth_diff_1[[i]]$Prediction)
  pred_ppc_gen = as.vector(prediction_PPC_gen_mouth_diff_1[[i]]$Prediction)
  pred_mp  = prediction_MP_mouth_diff_1[[i]]$Prediction
  pred_np  = prediction_NP_mouth_diff_1[[i]]$Prediction
  
  en_PPC_mouth_diff_1[i] = sqrt(MLmetrics::MSE(pred_ppc[!is.na(test_set) & !is.na(pred_ppc)], test_set[!is.na(test_set) & !is.na(pred_ppc)]))
  rn_PPC_mouth_diff_1[i] = MLmetrics::MAE(pred_ppc[!is.na(test_set) & !is.na(pred_ppc)], test_set[!is.na(test_set) & !is.na(pred_ppc)])
  
  en_PPC_exp_pow_mouth_diff_1[i] = sqrt(MLmetrics::MSE(pred_ppc_exp_pow[!is.na(test_set) & !is.na(pred_ppc_exp_pow)], test_set[!is.na(test_set) & !is.na(pred_ppc_exp_pow)]))
  rn_PPC_exp_pow_mouth_diff_1[i] = MLmetrics::MAE(pred_ppc_exp_pow[!is.na(test_set) & !is.na(pred_ppc_exp_pow)], test_set[!is.na(test_set) & !is.na(pred_ppc_exp_pow)])
  
  en_PPC_gen_mouth_diff_1[i] = sqrt(MLmetrics::MSE(pred_ppc_gen[!is.na(test_set) & !is.na(pred_ppc_gen)], test_set[!is.na(test_set) & !is.na(pred_ppc_gen)]))
  rn_PPC_gen_mouth_diff_1[i] = MLmetrics::MAE(pred_ppc_gen[!is.na(test_set) & !is.na(pred_ppc_gen)], test_set[!is.na(test_set) & !is.na(pred_ppc_gen)])
  
  en_MP_mouth_diff_1[i]  = sqrt(MLmetrics::MSE(pred_mp[!is.na(test_set) & !is.na(pred_mp)], test_set[!is.na(test_set) & !is.na(pred_mp)]))
  rn_MP_mouth_diff_1[i]  = MLmetrics::MAE(pred_mp[!is.na(test_set) & !is.na(pred_mp)], test_set[!is.na(test_set) & !is.na(pred_mp)])
  
  en_NP_mouth_diff_1[i]  = sqrt(MLmetrics::MSE(pred_np[!is.na(test_set) & !is.na(pred_np)], test_set[!is.na(test_set) & !is.na(pred_np)]))
  rn_NP_mouth_diff_1[i]  = MLmetrics::MAE(pred_np[!is.na(test_set) & !is.na(pred_np)], test_set[!is.na(test_set) & !is.na(pred_np)])
  
  
  # errors on diff2
  pred_ppc = as.vector(prediction_PPC_mouth_diff_2[[i]]$Prediction)
  pred_ppc_exp_pow = as.vector(prediction_PPC_exp_pow_mouth_diff_2[[i]]$Prediction)
  pred_ppc_gen = as.vector(prediction_PPC_gen_mouth_diff_2[[i]]$Prediction)
  pred_mp  = prediction_MP_mouth_diff_2[[i]]$Prediction
  pred_np  = prediction_NP_mouth_diff_2[[i]]$Prediction
  
  en_PPC_mouth_diff_2[i] = sqrt(MLmetrics::MSE(pred_ppc[!is.na(test_set) & !is.na(pred_ppc)], test_set[!is.na(test_set) & !is.na(pred_ppc)]))
  rn_PPC_mouth_diff_2[i] = MLmetrics::MAE(pred_ppc[!is.na(test_set) & !is.na(pred_ppc)], test_set[!is.na(test_set) & !is.na(pred_ppc)])
  
  en_PPC_exp_pow_mouth_diff_2[i] = sqrt(MLmetrics::MSE(pred_ppc_exp_pow[!is.na(test_set) & !is.na(pred_ppc_exp_pow)], test_set[!is.na(test_set) & !is.na(pred_ppc_exp_pow)]))
  rn_PPC_exp_pow_mouth_diff_2[i] = MLmetrics::MAE(pred_ppc_exp_pow[!is.na(test_set) & !is.na(pred_ppc_exp_pow)], test_set[!is.na(test_set) & !is.na(pred_ppc_exp_pow)])
  
  en_PPC_gen_mouth_diff_2[i] = sqrt(MLmetrics::MSE(pred_ppc_gen[!is.na(test_set) & !is.na(pred_ppc_gen)], test_set[!is.na(test_set) & !is.na(pred_ppc_gen)]))
  rn_PPC_gen_mouth_diff_2[i] = MLmetrics::MAE(pred_ppc_gen[!is.na(test_set) & !is.na(pred_ppc_gen)], test_set[!is.na(test_set) & !is.na(pred_ppc_gen)])
  
  en_MP_mouth_diff_2[i]  = sqrt(MLmetrics::MSE(pred_mp[!is.na(test_set) & !is.na(pred_mp)], test_set[!is.na(test_set) & !is.na(pred_mp)]))
  rn_MP_mouth_diff_2[i]  = MLmetrics::MAE(pred_mp[!is.na(test_set) & !is.na(pred_mp)], test_set[!is.na(test_set) & !is.na(pred_mp)])
  
  en_NP_mouth_diff_2[i]  = sqrt(MLmetrics::MSE(pred_np[!is.na(test_set) & !is.na(pred_np)], test_set[!is.na(test_set) & !is.na(pred_np)]))
  rn_NP_mouth_diff_2[i]  = MLmetrics::MAE(pred_np[!is.na(test_set) & !is.na(pred_np)], test_set[!is.na(test_set) & !is.na(pred_np)])
}


# saving results
err_PPC_mouth = list(en = en_PPC_mouth, rn = rn_PPC_mouth)
err_PPC_exp_pow_mouth = list(en = en_PPC_exp_pow_mouth, rn = rn_PPC_exp_pow_mouth)
err_PPC_gen_mouth = list(en = en_PPC_gen_mouth, rn = rn_PPC_gen_mouth)
err_MP_mouth  = list(en = en_MP_mouth,  rn = rn_MP_mouth)
err_NP_mouth  = list(en = en_NP_mouth,  rn = rn_NP_mouth)
subfolder = "/mouth/original"

if(save_res){
  save(err_PPC_mouth, file = paste0(paste0(path_stor_res,subfolder),"/err_PPC_mouth.Rdata") )
  save(err_PPC_exp_pow_mouth, file = paste0(paste0(path_stor_res,subfolder),"/err_PPC_exp_pow_mouth.Rdata") )
  save(err_PPC_gen_mouth, file = paste0(paste0(path_stor_res,subfolder),"/err_PPC_gen_mouth.Rdata") )
  save(err_MP_mouth, file = paste0(paste0(path_stor_res,subfolder),"/err_MP_mouth.Rdata") )
  save(err_NP_mouth, file = paste0(paste0(path_stor_res,subfolder),"/err_NP_mouth.Rdata") )
}


err_PPC_mouth_diff_1 = list(en = en_PPC_mouth_diff_1, rn = rn_PPC_mouth_diff_1)
err_PPC_exp_pow_mouth_diff_1 = list(en = en_PPC_exp_pow_mouth_diff_1, rn = rn_PPC_exp_pow_mouth_diff_1)
err_PPC_gen_mouth_diff_1 = list(en = en_PPC_gen_mouth_diff_1, rn = rn_PPC_gen_mouth_diff_1)
err_MP_mouth_diff_1  = list(en = en_MP_mouth_diff_1 , rn = rn_MP_mouth_diff_1)
err_NP_mouth_diff_1  = list(en = en_NP_mouth_diff_1 , rn = rn_NP_mouth_diff_1)
subfolder = "/mouth/diff_1"

if(save_res){
  save(err_PPC_mouth_diff_1, file = paste0(paste0(path_stor_res,subfolder),"/err_PPC_mouth_diff_1.Rdata") )
  save(err_PPC_exp_pow_mouth_diff_1, file = paste0(paste0(path_stor_res,subfolder),"/err_PPC_exp_pow_mouth_diff_1.Rdata") )
  save(err_PPC_gen_mouth_diff_1, file = paste0(paste0(path_stor_res,subfolder),"/err_PPC_gen_mouth_diff_1.Rdata") )
  save(err_MP_mouth_diff_1,  file = paste0(paste0(path_stor_res,subfolder),"/err_MP_mouth_diff_1.Rdata") )
  save(err_NP_mouth_diff_1,  file = paste0(paste0(path_stor_res,subfolder),"/err_NP_mouth_diff_1.Rdata") )
}


err_PPC_mouth_diff_2 = list(en = en_PPC_mouth_diff_2, rn = rn_PPC_mouth_diff_2)
err_PPC_exp_pow_mouth_diff_2 = list(en = en_PPC_exp_pow_mouth_diff_2, rn = rn_PPC_exp_pow_mouth_diff_2)
err_PPC_gen_mouth_diff_2 = list(en = en_PPC_gen_mouth_diff_2, rn = rn_PPC_gen_mouth_diff_2)
err_MP_mouth_diff_2  = list(en = en_MP_mouth_diff_2,  rn = rn_MP_mouth_diff_2)
err_NP_mouth_diff_2  = list(en = en_NP_mouth_diff_2,  rn = rn_NP_mouth_diff_2)
subfolder = "/mouth/diff_2"

if(save_res){
  save(err_PPC_mouth_diff_2, file = paste0(paste0(path_stor_res,subfolder),"/err_PPC_mouth_diff_2.Rdata") )
  save(err_PPC_exp_pow_mouth_diff_2, file = paste0(paste0(path_stor_res,subfolder),"/err_PPC_exp_pow_mouth_diff_2.Rdata") )
  save(err_PPC_gen_mouth_diff_2, file = paste0(paste0(path_stor_res,subfolder),"/err_PPC_gen_mouth_diff_2.Rdata") )
  save(err_MP_mouth_diff_2,  file = paste0(paste0(path_stor_res,subfolder),"/err_MP_mouth_diff_2.Rdata") )
  save(err_NP_mouth_diff_2,  file = paste0(paste0(path_stor_res,subfolder),"/err_NP_mouth_diff_2.Rdata") )
}





## ----- center -----

center = data_2d_wrapper_from_list(Xt_center)

{
  en_PPC_center = numeric(N)
  en_PPC_center_diff_1 = numeric(N)
  en_PPC_center_diff_2 = numeric(N)
  
  rn_PPC_center = numeric(N)
  rn_PPC_center_diff_1 = numeric(N)
  rn_PPC_center_diff_2 = numeric(N)
  
  
  en_PPC_exp_pow_center = numeric(N)
  en_PPC_exp_pow_center_diff_1 = numeric(N)
  en_PPC_exp_pow_center_diff_2 = numeric(N)
  
  rn_PPC_exp_pow_center = numeric(N)
  rn_PPC_exp_pow_center_diff_1 = numeric(N)
  rn_PPC_exp_pow_center_diff_2 = numeric(N)
  
  
  en_PPC_gen_center = numeric(N)
  en_PPC_gen_center_diff_1 = numeric(N)
  en_PPC_gen_center_diff_2 = numeric(N)
  
  rn_PPC_gen_center = numeric(N)
  rn_PPC_gen_center_diff_1 = numeric(N)
  rn_PPC_gen_center_diff_2 = numeric(N)
  
  
  en_MP_center = numeric(N)
  en_MP_center_diff_1 = numeric(N)
  en_MP_center_diff_2 = numeric(N)
  
  rn_MP_center = numeric(N)
  rn_MP_center_diff_1 = numeric(N)
  rn_MP_center_diff_2 = numeric(N)
  
  
  en_NP_center = numeric(N)
  en_NP_center_diff_1 = numeric(N)
  en_NP_center_diff_2 = numeric(N)
  
  rn_NP_center = numeric(N)
  rn_NP_center_diff_1 = numeric(N)
  rn_NP_center_diff_2 = numeric(N)
}




for(i in 1:days_to_be_pred){
  
  test_set = center[,i + idx_first_day_first_train + window_train_set]
  
  # errors on direct prediction
  pred_ppc = as.vector(prediction_PPC_center[[i]]$Prediction)
  pred_ppc_exp_pow = as.vector(prediction_PPC_exp_pow_center[[i]]$Prediction)
  pred_ppc_gen = as.vector(prediction_PPC_gen_center[[i]]$Prediction)
  pred_mp  = prediction_MP_center[[i]]$Prediction
  pred_np  = prediction_NP_center[[i]]$Prediction
  
  en_PPC_center[i] = sqrt(MLmetrics::MSE(pred_ppc[!is.na(test_set) & !is.na(pred_ppc)], test_set[!is.na(test_set) & !is.na(pred_ppc)]))
  rn_PPC_center[i] = MLmetrics::MAE(pred_ppc[!is.na(test_set) & !is.na(pred_ppc)], test_set[!is.na(test_set) & !is.na(pred_ppc)])
  
  en_PPC_exp_pow_center[i] = sqrt(MLmetrics::MSE(pred_ppc_exp_pow[!is.na(test_set) & !is.na(pred_ppc_exp_pow)], test_set[!is.na(test_set) & !is.na(pred_ppc_exp_pow)]))
  rn_PPC_exp_pow_center[i] = MLmetrics::MAE(pred_ppc_exp_pow[!is.na(test_set) & !is.na(pred_ppc_exp_pow)], test_set[!is.na(test_set) & !is.na(pred_ppc_exp_pow)])
  
  en_PPC_gen_center[i] = sqrt(MLmetrics::MSE(pred_ppc_gen[!is.na(test_set) & !is.na(pred_ppc_gen)], test_set[!is.na(test_set) & !is.na(pred_ppc_gen)]))
  rn_PPC_gen_center[i] = MLmetrics::MAE(pred_ppc_gen[!is.na(test_set) & !is.na(pred_ppc_gen)], test_set[!is.na(test_set) & !is.na(pred_ppc_gen)])
  
  en_MP_center[i]  = sqrt(MLmetrics::MSE(pred_mp[!is.na(test_set) & !is.na(pred_mp)], test_set[!is.na(test_set) & !is.na(pred_mp)]))
  rn_MP_center[i]  = MLmetrics::MAE(pred_mp[!is.na(test_set) & !is.na(pred_mp)], test_set[!is.na(test_set) & !is.na(pred_mp)])
  
  en_NP_center[i]  = sqrt(MLmetrics::MSE(pred_np[!is.na(test_set) & !is.na(pred_np)], test_set[!is.na(test_set) & !is.na(pred_np)]))
  rn_NP_center[i]  = MLmetrics::MAE(pred_np[!is.na(test_set) & !is.na(pred_np)], test_set[!is.na(test_set) & !is.na(pred_np)])
  
  
  # errors on diff1
  pred_ppc = as.vector(prediction_PPC_center_diff_1[[i]]$Prediction)
  pred_ppc_exp_pow = as.vector(prediction_PPC_exp_pow_center_diff_1[[i]]$Prediction)
  pred_ppc_gen = as.vector(prediction_PPC_gen_center_diff_1[[i]]$Prediction)
  pred_mp  = prediction_MP_center_diff_1[[i]]$Prediction
  pred_np  = prediction_NP_center_diff_1[[i]]$Prediction
  
  en_PPC_center_diff_1[i] = sqrt(MLmetrics::MSE(pred_ppc[!is.na(test_set) & !is.na(pred_ppc)], test_set[!is.na(test_set) & !is.na(pred_ppc)]))
  rn_PPC_center_diff_1[i] = MLmetrics::MAE(pred_ppc[!is.na(test_set) & !is.na(pred_ppc)], test_set[!is.na(test_set) & !is.na(pred_ppc)])
  
  en_PPC_exp_pow_center_diff_1[i] = sqrt(MLmetrics::MSE(pred_ppc_exp_pow[!is.na(test_set) & !is.na(pred_ppc_exp_pow)], test_set[!is.na(test_set) & !is.na(pred_ppc_exp_pow)]))
  rn_PPC_exp_pow_center_diff_1[i] = MLmetrics::MAE(pred_ppc_exp_pow[!is.na(test_set) & !is.na(pred_ppc_exp_pow)], test_set[!is.na(test_set) & !is.na(pred_ppc_exp_pow)])
  
  en_PPC_gen_center_diff_1[i] = sqrt(MLmetrics::MSE(pred_ppc_gen[!is.na(test_set) & !is.na(pred_ppc_gen)], test_set[!is.na(test_set) & !is.na(pred_ppc_gen)]))
  rn_PPC_gen_center_diff_1[i] = MLmetrics::MAE(pred_ppc_gen[!is.na(test_set) & !is.na(pred_ppc_gen)], test_set[!is.na(test_set) & !is.na(pred_ppc_gen)])
  
  en_MP_center_diff_1[i]  = sqrt(MLmetrics::MSE(pred_mp[!is.na(test_set) & !is.na(pred_mp)], test_set[!is.na(test_set) & !is.na(pred_mp)]))
  rn_MP_center_diff_1[i]  = MLmetrics::MAE(pred_mp[!is.na(test_set) & !is.na(pred_mp)], test_set[!is.na(test_set) & !is.na(pred_mp)])
  
  en_NP_center_diff_1[i]  = sqrt(MLmetrics::MSE(pred_np[!is.na(test_set) & !is.na(pred_np)], test_set[!is.na(test_set) & !is.na(pred_np)]))
  rn_NP_center_diff_1[i]  = MLmetrics::MAE(pred_np[!is.na(test_set) & !is.na(pred_np)], test_set[!is.na(test_set) & !is.na(pred_np)])
  
  
  # errors on diff2
  pred_ppc = as.vector(prediction_PPC_center_diff_2[[i]]$Prediction)
  pred_ppc_exp_pow = as.vector(prediction_PPC_exp_pow_center_diff_2[[i]]$Prediction)
  pred_ppc_gen = as.vector(prediction_PPC_gen_center_diff_2[[i]]$Prediction)
  pred_mp  = prediction_MP_center_diff_2[[i]]$Prediction
  pred_np  = prediction_NP_center_diff_2[[i]]$Prediction
  
  en_PPC_center_diff_2[i] = sqrt(MLmetrics::MSE(pred_ppc[!is.na(test_set) & !is.na(pred_ppc)], test_set[!is.na(test_set) & !is.na(pred_ppc)]))
  rn_PPC_center_diff_2[i] = MLmetrics::MAE(pred_ppc[!is.na(test_set) & !is.na(pred_ppc)], test_set[!is.na(test_set) & !is.na(pred_ppc)])
  
  en_PPC_exp_pow_center_diff_2[i] = sqrt(MLmetrics::MSE(pred_ppc_exp_pow[!is.na(test_set) & !is.na(pred_ppc_exp_pow)], test_set[!is.na(test_set) & !is.na(pred_ppc_exp_pow)]))
  rn_PPC_exp_pow_center_diff_2[i] = MLmetrics::MAE(pred_ppc_exp_pow[!is.na(test_set) & !is.na(pred_ppc_exp_pow)], test_set[!is.na(test_set) & !is.na(pred_ppc_exp_pow)])
  
  en_PPC_gen_center_diff_2[i] = sqrt(MLmetrics::MSE(pred_ppc_gen[!is.na(test_set) & !is.na(pred_ppc_gen)], test_set[!is.na(test_set) & !is.na(pred_ppc_gen)]))
  rn_PPC_gen_center_diff_2[i] = MLmetrics::MAE(pred_ppc_gen[!is.na(test_set) & !is.na(pred_ppc_gen)], test_set[!is.na(test_set) & !is.na(pred_ppc_gen)])
  
  en_MP_center_diff_2[i]  = sqrt(MLmetrics::MSE(pred_mp[!is.na(test_set) & !is.na(pred_mp)], test_set[!is.na(test_set) & !is.na(pred_mp)]))
  rn_MP_center_diff_2[i]  = MLmetrics::MAE(pred_mp[!is.na(test_set) & !is.na(pred_mp)], test_set[!is.na(test_set) & !is.na(pred_mp)])
  
  en_NP_center_diff_2[i]  = sqrt(MLmetrics::MSE(pred_np[!is.na(test_set) & !is.na(pred_np)], test_set[!is.na(test_set) & !is.na(pred_np)]))
  rn_NP_center_diff_2[i]  = MLmetrics::MAE(pred_np[!is.na(test_set) & !is.na(pred_np)], test_set[!is.na(test_set) & !is.na(pred_np)])
  
}

#save results
err_PPC_center = list(en = en_PPC_center, rn = rn_PPC_center)
err_PPC_exp_pow_center = list(en = en_PPC_exp_pow_center, rn = rn_PPC_exp_pow_center)
err_PPC_gen_center = list(en = en_PPC_gen_center, rn = rn_PPC_gen_center)
err_MP_center  = list(en = en_MP_center, rn = rn_MP_center)
err_NP_center  = list(en = en_NP_center, rn = rn_NP_center)
subfolder = "/center/original"

# save results
if(save_res){
  save(err_PPC_center, file = paste0(paste0(path_stor_res,subfolder),"/err_PPC_center.Rdata") )
  save(err_PPC_exp_pow_center, file = paste0(paste0(path_stor_res,subfolder),"/err_PPC_exp_pow_center.Rdata") )
  save(err_PPC_gen_center, file = paste0(paste0(path_stor_res,subfolder),"/err_PPC_gen_center.Rdata") )
  save(err_MP_center, file = paste0(paste0(path_stor_res,subfolder),"/err_MP_center.Rdata") )
  save(err_NP_center, file = paste0(paste0(path_stor_res,subfolder),"/err_NP_center.Rdata") )
}


err_PPC_center_diff_1 = list(en = en_PPC_center_diff_1, rn = rn_PPC_center_diff_1 )
err_PPC_exp_pow_center_diff_1 = list(en = en_PPC_exp_pow_center_diff_1, rn = rn_PPC_exp_pow_center_diff_1 )
err_PPC_gen_center_diff_1 = list(en = en_PPC_gen_center_diff_1, rn = rn_PPC_gen_center_diff_1 )
err_MP_center_diff_1  = list(en = en_MP_center_diff_1 , rn = rn_MP_center_diff_1 )
err_NP_center_diff_1  = list(en = en_NP_center_diff_1 , rn = rn_NP_center_diff_1 )
subfolder = "/center/diff_1"

# save results
if(save_res){
  save(err_PPC_center_diff_1, file = paste0(paste0(path_stor_res,subfolder),"/err_PPC_center_diff_1.Rdata") )
  save(err_PPC_exp_pow_center_diff_1, file = paste0(paste0(path_stor_res,subfolder),"/err_PPC_exp_pow_center_diff_1.Rdata") )
  save(err_PPC_gen_center_diff_1, file = paste0(paste0(path_stor_res,subfolder),"/err_PPC_gen_center_diff_1.Rdata") )
  save(err_MP_center_diff_1,  file = paste0(paste0(path_stor_res,subfolder),"/err_MP_center_diff_1.Rdata") )
  save(err_NP_center_diff_1,  file = paste0(paste0(path_stor_res,subfolder),"/err_NP_center_diff_1.Rdata") )
}


err_PPC_center_diff_2 = list(en = en_PPC_center_diff_2, rn = rn_PPC_center_diff_2 )
err_PPC_exp_pow_center_diff_2 = list(en = en_PPC_exp_pow_center_diff_2, rn = rn_PPC_exp_pow_center_diff_2 )
err_PPC_gen_center_diff_2 = list(en = en_PPC_gen_center_diff_2, rn = rn_PPC_gen_center_diff_2 )
err_MP_center_diff_2   = list(en = en_MP_center_diff_2, rn = rn_MP_center_diff_2 )
err_NP_center_diff_2   = list(en = en_NP_center_diff_2, rn = rn_NP_center_diff_2 )
subfolder = "/center/diff_2"

# save results
if(save_res){
  save(err_PPC_center_diff_2, file = paste0(paste0(path_stor_res,subfolder),"/err_PPC_center_diff_2.Rdata") )
  save(err_PPC_exp_pow_center_diff_2, file = paste0(paste0(path_stor_res,subfolder),"/err_PPC_exp_pow_center_diff_2.Rdata") )
  save(err_PPC_gen_center_diff_2, file = paste0(paste0(path_stor_res,subfolder),"/err_PPC_gen_center_diff_2.Rdata") )
  save(err_MP_center_diff_2,  file = paste0(paste0(path_stor_res,subfolder),"/err_MP_center_diff_2.Rdata") )
  save(err_NP_center_diff_2,  file = paste0(paste0(path_stor_res,subfolder),"/err_NP_center_diff_2.Rdata") )
}
