rm(list=ls())
graphics.off()
cat("\014")

set.seed(23032000)


#############################################################
#### Computing KE prediction as indicated in the readme  ####
#############################################################

#put here the path of the local copy of the directory
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"

#save res
save_res = TRUE
folder_res = "/KE"

#where the data are
dir_data = paste0(dir_w,"/Test_domain2D/RealWorld_data/data")
path_data_mouth = paste0(dir_data,"/mouth")
path_data_center = paste0(dir_data,"/center")

#where to store results
dir_res = paste0(dir_w,"/Test_domain2D/RealWorld_data/results/results_prediction")
dir_err = paste0(dir_w,"/Test_domain2D/RealWorld_data/results/results_prediction_errors")
path_store_res = paste0(dir_res,folder_res)


#load functions to use KE
source(paste0(dir_w,"/Test_domain2D/RealWorld_data/utils/EstimatedKernel_predictor_2D.R"))      
source(paste0(dir_w,"/Test_domain2D/RealWorld_data/utils/KE_cv_2D.R"))       



#load the dataset
load(paste0(dir_data,"/BS.Rdata"))

#load data for mouth zone
load(paste0(path_data_mouth,"/BS_mouth.Rdata"))
load(paste0(path_data_mouth,"/BS_mouth_diff_1.Rdata"))
load(paste0(path_data_mouth,"/BS_mouth_diff_2.Rdata"))

#load data for center zone
load(paste0(path_data_center,"/BS_center.Rdata"))
load(paste0(path_data_center,"/BS_center_diff_1.Rdata"))
load(paste0(path_data_center,"/BS_center_diff_2.Rdata"))


first_day_first_train = "20170101"
idx_first_day_first_train = which(dates==first_day_first_train)
window_train_set = 99
days_to_be_pred = 1000

#store results
prediction_KE_mouth          <- lapply(1:days_to_be_pred,function(x) NULL)
prediction_KE_mouth_diff_1   <- lapply(1:days_to_be_pred,function(x) NULL)
prediction_KE_mouth_diff_2   <- lapply(1:days_to_be_pred,function(x) NULL)
prediction_KE_center         <- lapply(1:days_to_be_pred,function(x) NULL)
prediction_KE_center_diff_1  <- lapply(1:days_to_be_pred,function(x) NULL)
prediction_KE_center_diff_2  <- lapply(1:days_to_be_pred,function(x) NULL)

{
  en_KE_mouth = numeric(days_to_be_pred)
  rn_KE_mouth = numeric(days_to_be_pred)
  en_KE_mouth_diff_1 = numeric(days_to_be_pred)
  rn_KE_mouth_diff_1 = numeric(days_to_be_pred)
  en_KE_mouth_diff_2 = numeric(days_to_be_pred)
  rn_KE_mouth_diff_2 = numeric(days_to_be_pred)
  
  en_KE_center = numeric(days_to_be_pred)
  rn_KE_center = numeric(days_to_be_pred)
  en_KE_center_diff_1 = numeric(days_to_be_pred)
  rn_KE_center_diff_1 = numeric(days_to_be_pred)
  en_KE_center_diff_2 = numeric(days_to_be_pred)
  rn_KE_center_diff_2 = numeric(days_to_be_pred)
}

# paramters for KE
improved_ke = FALSE
p_vec = c(2,3,4,5,6)


idx_not_nan_mouth = matrix(data = TRUE,nrow=length(lon_mouth),ncol=length(lat_mouth))
for (t in 1:length(Xt_mouth)) {
  tmp = !is.na(Xt_mouth[[t]])
  idx_not_nan_mouth = idx_not_nan_mouth & tmp
}

idx_not_nan_center = matrix(data = TRUE,nrow=length(lon_center),ncol=length(lat_center))
for (t in 1:length(Xt_center)) {
  tmp = !is.na(Xt_center[[t]])
  idx_not_nan_center = idx_not_nan_center & tmp
}



#preparing data diff 

Xt_mouth_diff1_list = lapply(1:(length(Xt_mouth)-1),function(x) NULL)
Xt_mouth_diff2_list = lapply(1:(length(Xt_mouth)-2),function(x) NULL)
Xt_center_diff1_list = lapply(1:(length(Xt_center)-1),function(x) NULL)
Xt_center_diff2_list = lapply(1:(length(Xt_mouth)-2),function(x) NULL)

for (t in 1:(length(Xt_mouth)-1)) {
  Xt_mouth_diff1_list[[t]]  = matrix(Xt_mouth_diff_1[,t], nrow = length(lon_mouth), ncol = length(lat_mouth), byrow = FALSE)
  Xt_center_diff1_list[[t]] = matrix(Xt_center_diff_1[,t], nrow = length(lon_center), ncol = length(lat_center), byrow = FALSE)
}

for (t in 1:(length(Xt_mouth)-2)) {
  Xt_mouth_diff2_list[[t]] = matrix(Xt_mouth_diff_2[,t], nrow = length(lon_mouth), ncol = length(lat_mouth), byrow = FALSE)
  Xt_center_diff2_list[[t]] = matrix(Xt_center_diff_2[,t], nrow = length(lon_center), ncol = length(lat_center), byrow = FALSE)
}


##-----mouth prediction-----

# time serie
mouth = data_2d_wrapper_from_list(Xt_mouth)

string_message = "
                  KE prediction of mouth zone "
for (i in 1:days_to_be_pred) {
  
  train_set = Xt_mouth[(i - 1 + idx_first_day_first_train):(i - 1 + idx_first_day_first_train + window_train_set)]
  pred_set  = as.vector(Xt_mouth[[i + idx_first_day_first_train + window_train_set]][idx_not_nan_mouth])
  
  predictor = cv_EK_2d( X = train_set,
                        grid_eval1 = lon_mouth,
                        grid_eval2 = lat_mouth,
                        p_vector = p_vec,
                        improved = improved_ke,
                        idx_not_nan = idx_not_nan_mouth)
  
  
  
  prediction_KE_mouth[[i]] = list(Prediction = predictor$prediction, N_PCs = predictor$N_PCs_ret )
  en_KE_mouth[i] = sqrt(MLmetrics::MSE(predictor$prediction,pred_set))
  rn_KE_mouth[i] = MLmetrics::MAE(predictor$prediction,pred_set)
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, days_to_be_pred)
  setTxtProgressBar(txtProgressBar(min = 1, max = days_to_be_pred, style = 3), i)
  cat("\r", message)
}

# save results
if(save_res){
  save(prediction_KE_mouth, file = paste0(path_store_res,"/prediction_KE_mouth.Rdata"))
}

err_KE_mouth = list(en=en_KE_mouth,rn=rn_KE_mouth)


# diff 1
string_message = "
                  KE prediction of mouth zone diff 1 "
for (i in 1:days_to_be_pred) {
  
  # parto 
  train_set = Xt_mouth_diff1_list[(i - 2 + idx_first_day_first_train):(i - 2 + idx_first_day_first_train + window_train_set)]
  pred_set  = as.vector(Xt_mouth[[i + idx_first_day_first_train + window_train_set]][idx_not_nan_mouth])
  
  predictor = cv_EK_2d( X = train_set,
                        grid_eval1 = lon_mouth,
                        grid_eval2 = lat_mouth,
                        p_vector = p_vec,
                        improved = improved_ke,
                        idx_not_nan = idx_not_nan_mouth)
  
  prediction = as.vector(predictor$prediction) + as.vector(Xt_mouth[[i - 1 + idx_first_day_first_train + window_train_set]][idx_not_nan_mouth])
  
  
  prediction_KE_mouth_diff_1[[i]] = list(Prediction = prediction, N_PCs = predictor$N_PCs_ret )
  en_KE_mouth_diff_1[i] = sqrt(MLmetrics::MSE(prediction,pred_set))
  rn_KE_mouth_diff_1[i] = MLmetrics::MAE(prediction,pred_set)
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, days_to_be_pred)
  setTxtProgressBar(txtProgressBar(min = 1, max = days_to_be_pred, style = 3), i)
  cat("\r", message)
}

# save results
if(save_res){
  save(prediction_KE_mouth_diff_1, file = paste0(path_store_res,"/prediction_KE_mouth_diff_1.Rdata"))
}

err_KE_mouth_diff_1 = list(en=en_KE_mouth_diff_1,rn=rn_KE_mouth_diff_1)


# diff 2
string_message = "
                  KE prediction of mouth zone diff 2 "
for (i in 1:days_to_be_pred) {
  
  # parto 
  train_set = Xt_mouth_diff2_list[(i - 3 + idx_first_day_first_train):(i - 3 + idx_first_day_first_train + window_train_set)]
  pred_set  = as.vector(Xt_mouth[[i + idx_first_day_first_train + window_train_set]][idx_not_nan_mouth])
  
  
  predictor = cv_EK_2d( X = train_set,
                        grid_eval1 = lon_mouth,
                        grid_eval2 = lat_mouth,
                        p_vector = p_vec,
                        improved = improved_ke,
                        idx_not_nan = idx_not_nan_mouth)
  
  
  prediction = as.vector(predictor$prediction) + 2*as.vector(Xt_mouth[[(i - 1 + idx_first_day_first_train + window_train_set)]][idx_not_nan_mouth]) - as.vector(Xt_mouth[[(i - 2 + idx_first_day_first_train + window_train_set)]][idx_not_nan_mouth])
  
  
  prediction_KE_mouth_diff_2[[i]] = list(Prediction = prediction, N_PCs = predictor$N_PCs_ret )
  en_KE_mouth_diff_2[i] = sqrt(MLmetrics::MSE(prediction,pred_set))
  rn_KE_mouth_diff_2[i] = MLmetrics::MAE(prediction,pred_set)
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, days_to_be_pred)
  setTxtProgressBar(txtProgressBar(min = 1, max = days_to_be_pred, style = 3), i)
  cat("\r", message)
}

# save results
if(save_res){
  save(prediction_KE_mouth_diff_2, file = paste0(path_store_res,"/prediction_KE_mouth_diff_2.Rdata"))
}

err_KE_mouth_diff_2 = list(en=en_KE_mouth_diff_2,rn=rn_KE_mouth_diff_2)




##-----center prediction-----

# time serie
center = data_2d_wrapper_from_list(Xt_center)

string_message = "
                  KE prediction of center zone "
for (i in 1:days_to_be_pred) {
  
  
  train_set = Xt_center[(i - 1 + idx_first_day_first_train):(i - 1 + idx_first_day_first_train + window_train_set)]
  pred_set  = as.vector(Xt_center[[i + idx_first_day_first_train + window_train_set]][idx_not_nan_center])
  
  
  predictor = cv_EK_2d( X = train_set,
                        grid_eval1 = lon_center,
                        grid_eval2 = lat_center,
                        p_vector = p_vec,
                        improved = improved_ke,
                        idx_not_nan = idx_not_nan_center)
  
  
  
  prediction_KE_center[[i]] = list(Prediction = predictor$prediction, N_PCs = predictor$N_PCs_ret )
  en_KE_center[i] = sqrt(MLmetrics::MSE(predictor$prediction,pred_set))
  rn_KE_center[i] = MLmetrics::MAE(predictor$prediction,pred_set)
  
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, days_to_be_pred)
  setTxtProgressBar(txtProgressBar(min = 1, max = days_to_be_pred, style = 3), i)
  cat("\r", message)
}

# save results
if(save_res){
  save(prediction_KE_center, file = paste0(path_store_res,"/prediction_KE_center.Rdata"))
}

err_KE_center = list(en=en_KE_center,rn=rn_KE_center)


# diff 1
string_message = "
                  KE prediction of center zone diff 1 "
for (i in 1:days_to_be_pred) {
  
  # parto 
  train_set = Xt_center_diff1_list[(i - 2 + idx_first_day_first_train):(i - 2 + idx_first_day_first_train + window_train_set)]
  pred_set  = as.vector(Xt_center[[i + idx_first_day_first_train + window_train_set]][idx_not_nan_center])
  
  predictor = cv_EK_2d( X = train_set,
                        grid_eval1 = lon_center,
                        grid_eval2 = lat_center,
                        p_vector = p_vec,
                        improved = improved_ke,
                        idx_not_nan = idx_not_nan_center)
  
  prediction = as.vector(predictor$prediction) + as.vector(Xt_center[[i - 1 + idx_first_day_first_train + window_train_set]][idx_not_nan_center])
  
  
  prediction_KE_center_diff_1[[i]] = list(Prediction = prediction, N_PCs = predictor$N_PCs_ret )
  en_KE_center_diff_1[i] = sqrt(MLmetrics::MSE(prediction,pred_set))
  rn_KE_center_diff_1[i] = MLmetrics::MAE(prediction,pred_set)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, days_to_be_pred)
  setTxtProgressBar(txtProgressBar(min = 1, max = days_to_be_pred, style = 3), i)
  cat("\r", message)
}

# save results
if(save_res){
  save(prediction_KE_center_diff_1, file = paste0(path_store_res,"/prediction_KE_center_diff_1.Rdata"))
}

err_KE_center_diff_1 = list(en=en_KE_center_diff_1,rn=rn_KE_center_diff_1)


# diff 2
string_message = "
                  KE prediction of center zone diff 2 "
for (i in 1:days_to_be_pred) {
  
  
  
  train_set = Xt_center_diff2_list[(i - 3 + idx_first_day_first_train):(i - 3 + idx_first_day_first_train + window_train_set)]
  pred_set  = as.vector(Xt_center[[i + idx_first_day_first_train + window_train_set]][idx_not_nan_center])
  
  
  predictor = cv_EK_2d( X = train_set,
                        grid_eval1 = lon_center,
                        grid_eval2 = lat_center,
                        p_vector = p_vec,
                        improved = improved_ke,
                        idx_not_nan = idx_not_nan_center)
  
  
  prediction = as.vector(predictor$prediction) + 2*as.vector(Xt_center[[(i - 1 + idx_first_day_first_train + window_train_set)]][idx_not_nan_center]) - as.vector(Xt_center[[(i - 2 + idx_first_day_first_train + window_train_set)]][idx_not_nan_center])
  
  
  prediction_KE_center_diff_2[[i]] = list(Prediction = prediction, N_PCs = predictor$N_PCs_ret )
  en_KE_center_diff_2[i] = sqrt(MLmetrics::MSE(prediction,pred_set))
  rn_KE_center_diff_2[i] = MLmetrics::MAE(prediction,pred_set)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, days_to_be_pred)
  setTxtProgressBar(txtProgressBar(min = 1, max = days_to_be_pred, style = 3), i)
  cat("\r", message)
}

# save results
if(save_res){
  save(prediction_KE_center_diff_2, file = paste0(path_store_res,"/prediction_KE_center_diff_2.Rdata"))
}

err_KE_center_diff_2 = list(en=en_KE_center_diff_2,rn=rn_KE_center_diff_2)


path_store_err = paste0(dir_w,"/Test_domain2D/RealWorld_data/results/results_prediction_errors")
if(save_res){
  save(err_KE_mouth, file = paste0(path_store_err,"/mouth/original/err_KE_mouth.Rdata") )
  save(err_KE_mouth_diff_1, file = paste0(path_store_err,"/mouth/diff_1/err_KE_mouth_diff_1.Rdata") )
  save(err_KE_mouth_diff_2, file = paste0(path_store_err,"/mouth/diff_2/err_KE_mouth_diff_2.Rdata") )
  save(err_KE_center, file = paste0(path_store_err,"/center/original/err_KE_center.Rdata") )
  save(err_KE_center_diff_1, file = paste0(path_store_err,"/center/diff_1/err_KE_center_diff_1.Rdata") )
  save(err_KE_center_diff_2, file = paste0(path_store_err,"/center/diff_2/err_KE_center_diff_2.Rdata") )
}
