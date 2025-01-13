rm(list=ls())
graphics.off()
cat("\014")

set.seed(23032000)

#############################################################
#### Computing CC prediction as indicated in the readme  ####
#############################################################

#put here the path of the local copy of the directory
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"

#save res
save_res = TRUE
folder_res = "/CC"

#where the data are
dir_data = paste0(dir_w,"/Test_domain2D/RealWorld_data/data")
path_data_mouth = paste0(dir_data,"/mouth")
path_data_center = paste0(dir_data,"/center")

#where to store results
dir_res = paste0(dir_w,"/Test_domain2D/RealWorld_data/results/results_prediction")
dir_err = paste0(dir_w,"/Test_domain2D/RealWorld_data/results/results_prediction_errors")
path_store_res = paste0(dir_res,folder_res)


#load functions to use CC
source(paste0(dir_w,"/Test_domain2D/RealWorld_data/utils/FARConcurrent_predictor.R"))



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
prediction_CC_mouth          <- lapply(1:days_to_be_pred,function(x) NULL)
prediction_CC_mouth_diff_1   <- lapply(1:days_to_be_pred,function(x) NULL)
prediction_CC_mouth_diff_2   <- lapply(1:days_to_be_pred,function(x) NULL)
prediction_CC_center         <- lapply(1:days_to_be_pred,function(x) NULL)
prediction_CC_center_diff_1  <- lapply(1:days_to_be_pred,function(x) NULL)
prediction_CC_center_diff_2  <- lapply(1:days_to_be_pred,function(x) NULL)

{
  en_CC_mouth = numeric(days_to_be_pred)
  rn_CC_mouth = numeric(days_to_be_pred)
  en_CC_mouth_diff_1 = numeric(days_to_be_pred)
  rn_CC_mouth_diff_1 = numeric(days_to_be_pred)
  en_CC_mouth_diff_2 = numeric(days_to_be_pred)
  rn_CC_mouth_diff_2 = numeric(days_to_be_pred)

  en_CC_center = numeric(days_to_be_pred)
  rn_CC_center = numeric(days_to_be_pred)
  en_CC_center_diff_1 = numeric(days_to_be_pred)
  rn_CC_center_diff_1 = numeric(days_to_be_pred)
  en_CC_center_diff_2 = numeric(days_to_be_pred)
  rn_CC_center_diff_2 = numeric(days_to_be_pred)
}




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

n_points_mouth = sum(idx_not_nan_mouth)
grid_mouth     = 1:n_points_mouth
n_points_center = sum(idx_not_nan_center)
grid_center     = 1:n_points_center


##-----mouth prediction-----

# time serie
mouth = data_2d_wrapper_from_list(Xt_mouth)

string_message = "
                  CC prediction of mouth zone "
for (i in 1:days_to_be_pred) {
  
  train_set = Xt_mouth[(i - 1 + idx_first_day_first_train):(i - 1 + idx_first_day_first_train + window_train_set)]
  pred_set  = as.vector(Xt_mouth[[i + idx_first_day_first_train + window_train_set]][idx_not_nan_mouth])
  
  
  n = length(train_set)
  Xt_list_of_vec = vector("list", length = n)
  
  # for each time
  for(tt in 1:(n)){
    Xt_list_of_vec[[tt]] = vector("list", length = 1)
    Xt_list_of_vec[[tt]][[1]] = as.vector(train_set[[tt]][idx_not_nan_mouth])
  }
  
  
  
  predictor = FAR_concurrent_predictor( data_y = Xt_list_of_vec,
                                        l = 3,
                                        b = 2,
                                        fitted_order=1, 
                                        seed_split=23032000,
                                        grid=grid_mouth,
                                        center=TRUE)
  
  pred = predictor$predicted_fun_n_plus_1[[1]][[1]]
  
  prediction_CC_mouth[[i]] = list(Prediction = pred )
  en_CC_mouth[i] = sqrt(MLmetrics::MSE(pred,pred_set))
  rn_CC_mouth[i] = MLmetrics::MAE(pred,pred_set)
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, days_to_be_pred)
  setTxtProgressBar(txtProgressBar(min = 1, max = days_to_be_pred, style = 3), i)
  cat("\r", message)
}

# save results
if(save_res){
  save(prediction_CC_mouth, file = paste0(path_store_res,"/prediction_CC_mouth.Rdata"))
}

err_CC_mouth = list(en=en_CC_mouth,rn=rn_CC_mouth)


# diff 1
string_message = "
                  CC prediction of mouth zone diff 1 "
for (i in 1:days_to_be_pred) {
  
  # parto 
  train_set = Xt_mouth_diff1_list[(i - 2 + idx_first_day_first_train):(i - 2 + idx_first_day_first_train + window_train_set)]
  pred_set  = as.vector(Xt_mouth[[i + idx_first_day_first_train + window_train_set]][idx_not_nan_mouth])
  
  n = length(train_set)
  Xt_list_of_vec = vector("list", length = n)
  
  # for each time
  for(tt in 1:(n)){
    Xt_list_of_vec[[tt]] = vector("list", length = 1)
    Xt_list_of_vec[[tt]][[1]] = as.vector(train_set[[tt]][idx_not_nan_mouth])
  }
  
  
  
  predictor = FAR_concurrent_predictor( data_y = Xt_list_of_vec,
                                        l = 3,
                                        b = 2,
                                        fitted_order=1, 
                                        seed_split=23032000,
                                        grid=grid_mouth,
                                        center=TRUE)
  
  pred = as.vector(predictor$predicted_fun_n_plus_1[[1]][[1]]) + as.vector(Xt_mouth[[i - 1 + idx_first_day_first_train + window_train_set]][idx_not_nan_mouth])
  
  
  prediction_CC_mouth_diff_1[[i]] = list(Prediction = pred )
  en_CC_mouth_diff_1[i] = sqrt(MLmetrics::MSE(pred,pred_set))
  rn_CC_mouth_diff_1[i] = MLmetrics::MAE(pred,pred_set)
  

  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, days_to_be_pred)
  setTxtProgressBar(txtProgressBar(min = 1, max = days_to_be_pred, style = 3), i)
  cat("\r", message)
}

# save results
if(save_res){
  save(prediction_CC_mouth_diff_1, file = paste0(path_store_res,"/prediction_CC_mouth_diff_1.Rdata"))
}

err_CC_mouth_diff_1 = list(en=en_CC_mouth_diff_1,rn=rn_CC_mouth_diff_1)


# diff 2
string_message = "
                  CC prediction of mouth zone diff 2 "
for (i in 1:days_to_be_pred) {
  
  # parto 
  train_set = Xt_mouth_diff2_list[(i - 3 + idx_first_day_first_train):(i - 3 + idx_first_day_first_train + window_train_set)]
  pred_set  = as.vector(Xt_mouth[[i + idx_first_day_first_train + window_train_set]][idx_not_nan_mouth])
  
  
  n = length(train_set)
  Xt_list_of_vec = vector("list", length = n)
  
  # for each time
  for(tt in 1:(n)){
    Xt_list_of_vec[[tt]] = vector("list", length = 1)
    Xt_list_of_vec[[tt]][[1]] = as.vector(train_set[[tt]][idx_not_nan_mouth])
  }
  
  
  
  predictor = FAR_concurrent_predictor( data_y = Xt_list_of_vec,
                                        l = 3,
                                        b = 2,
                                        fitted_order=1, 
                                        seed_split=23032000,
                                        grid=grid_mouth,
                                        center=TRUE)
  
  
  pred = as.vector(predictor$predicted_fun_n_plus_1[[1]][[1]]) + 2*as.vector(Xt_mouth[[(i - 1 + idx_first_day_first_train + window_train_set)]][idx_not_nan_mouth]) - as.vector(Xt_mouth[[(i - 2 + idx_first_day_first_train + window_train_set)]][idx_not_nan_mouth])
  
  
  prediction_CC_mouth_diff_2[[i]] = list(Prediction = pred )
  en_CC_mouth_diff_2[i] = sqrt(MLmetrics::MSE(pred,pred_set))
  rn_CC_mouth_diff_2[i] = MLmetrics::MAE(pred,pred_set)
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, days_to_be_pred)
  setTxtProgressBar(txtProgressBar(min = 1, max = days_to_be_pred, style = 3), i)
  cat("\r", message)
}

# save results
if(save_res){
  save(prediction_CC_mouth_diff_2, file = paste0(path_store_res,"/prediction_CC_mouth_diff_2.Rdata"))
}

err_CC_mouth_diff_2 = list(en=en_CC_mouth_diff_2,rn=rn_CC_mouth_diff_2)




##-----center prediction-----

# time serie
center = data_2d_wrapper_from_list(Xt_center)

string_message = "
                  CC prediction of center zone "
for (i in 1:days_to_be_pred) {
  
  
  train_set = Xt_center[(i - 1 + idx_first_day_first_train):(i - 1 + idx_first_day_first_train + window_train_set)]
  pred_set  = as.vector(Xt_center[[i + idx_first_day_first_train + window_train_set]][idx_not_nan_center])
  
  
  n = length(train_set)
  Xt_list_of_vec = vector("list", length = n)
  
  # for each time
  for(tt in 1:(n)){
    Xt_list_of_vec[[tt]] = vector("list", length = 1)
    Xt_list_of_vec[[tt]][[1]] = as.vector(train_set[[tt]][idx_not_nan_center])
  }
  
  
  
  predictor = FAR_concurrent_predictor( data_y = Xt_list_of_vec,
                                        l = 3,
                                        b = 2,
                                        fitted_order=1, 
                                        seed_split=23032000,
                                        grid=grid_center,
                                        center=TRUE)
  
  pred = predictor$predicted_fun_n_plus_1[[1]][[1]]
  
  prediction_CC_center[[i]] = list(Prediction = pred )
  en_CC_center[i] = sqrt(MLmetrics::MSE(pred,pred_set))
  rn_CC_center[i] = MLmetrics::MAE(pred,pred_set)
  
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, days_to_be_pred)
  setTxtProgressBar(txtProgressBar(min = 1, max = days_to_be_pred, style = 3), i)
  cat("\r", message)
}

# save results
if(save_res){
  save(prediction_CC_center, file = paste0(path_store_res,"/prediction_CC_center.Rdata"))
}

err_CC_center = list(en=en_CC_center,rn=rn_CC_center)


# diff 1
string_message = "
                  CC prediction of center zone diff 1 "
for (i in 1:days_to_be_pred) {
  
  # parto 
  train_set = Xt_center_diff1_list[(i - 2 + idx_first_day_first_train):(i - 2 + idx_first_day_first_train + window_train_set)]
  pred_set  = as.vector(Xt_center[[i + idx_first_day_first_train + window_train_set]][idx_not_nan_center])
  
  n = length(train_set)
  Xt_list_of_vec = vector("list", length = n)
  
  # for each time
  for(tt in 1:(n)){
    Xt_list_of_vec[[tt]] = vector("list", length = 1)
    Xt_list_of_vec[[tt]][[1]] = as.vector(train_set[[tt]][idx_not_nan_center])
  }
  
  
  
  predictor = FAR_concurrent_predictor( data_y = Xt_list_of_vec,
                                        l = 3,
                                        b = 2,
                                        fitted_order=1, 
                                        seed_split=23032000,
                                        grid=grid_center,
                                        center=TRUE)
  
  
  pred = as.vector(predictor$predicted_fun_n_plus_1[[1]][[1]]) + as.vector(Xt_center[[i - 1 + idx_first_day_first_train + window_train_set]][idx_not_nan_center])
  
  
  prediction_CC_center_diff_1[[i]] = list(Prediction = pred )
  en_CC_center_diff_1[i] = sqrt(MLmetrics::MSE(pred,pred_set))
  rn_CC_center_diff_1[i] = MLmetrics::MAE(pred,pred_set)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, days_to_be_pred)
  setTxtProgressBar(txtProgressBar(min = 1, max = days_to_be_pred, style = 3), i)
  cat("\r", message)
}

# save results
if(save_res){
  save(prediction_CC_center_diff_1, file = paste0(path_store_res,"/prediction_CC_center_diff_1.Rdata"))
}

err_CC_center_diff_1 = list(en=en_CC_center_diff_1,rn=rn_CC_center_diff_1)


# diff 2
string_message = "
                  CC prediction of center zone diff 2 "
for (i in 1:days_to_be_pred) {
  
  
  
  train_set = Xt_center_diff2_list[(i - 3 + idx_first_day_first_train):(i - 3 + idx_first_day_first_train + window_train_set)]
  pred_set  = as.vector(Xt_center[[i + idx_first_day_first_train + window_train_set]][idx_not_nan_center])
  
  
  n = length(train_set)
  Xt_list_of_vec = vector("list", length = n)
  
  # for each time
  for(tt in 1:(n)){
    Xt_list_of_vec[[tt]] = vector("list", length = 1)
    Xt_list_of_vec[[tt]][[1]] = as.vector(train_set[[tt]][idx_not_nan_center])
  }
  
  
  
  predictor = FAR_concurrent_predictor( data_y = Xt_list_of_vec,
                                        l = 3,
                                        b = 2,
                                        fitted_order=1, 
                                        seed_split=23032000,
                                        grid=grid_center,
                                        center=TRUE)
  
  
  pred = as.vector(predictor$predicted_fun_n_plus_1[[1]][[1]] ) + 2*as.vector(Xt_center[[(i - 1 + idx_first_day_first_train + window_train_set)]][idx_not_nan_center]) - as.vector(Xt_center[[(i - 2 + idx_first_day_first_train + window_train_set)]][idx_not_nan_center])
  
  
  prediction_CC_center_diff_2[[i]] = list(Prediction = pred)
  en_CC_center_diff_2[i] = sqrt(MLmetrics::MSE(pred,pred_set))
  rn_CC_center_diff_2[i] = MLmetrics::MAE(pred,pred_set)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, days_to_be_pred)
  setTxtProgressBar(txtProgressBar(min = 1, max = days_to_be_pred, style = 3), i)
  cat("\r", message)
}

# save results
if(save_res){
  save(prediction_CC_center_diff_2, file = paste0(path_store_res,"/prediction_CC_center_diff_2.Rdata"))
}

err_CC_center_diff_2 = list(en=en_CC_center_diff_2,rn=rn_CC_center_diff_2)


path_store_err = paste0(dir_w,"/Test_domain2D/RealWorld_data/results/results_prediction_errors")
if(save_res){
  save(err_CC_mouth, file = paste0(path_store_err,"/mouth/original/err_CC_mouth.Rdata") )
  save(err_CC_mouth_diff_1, file = paste0(path_store_err,"/mouth/diff_1/err_CC_mouth_diff_1.Rdata") )
  save(err_CC_mouth_diff_2, file = paste0(path_store_err,"/mouth/diff_2/err_CC_mouth_diff_2.Rdata") )
  save(err_CC_center, file = paste0(path_store_err,"/center/original/err_CC_center.Rdata") )
  save(err_CC_center_diff_1, file = paste0(path_store_err,"/center/diff_1/err_CC_center_diff_1.Rdata") )
  save(err_CC_center_diff_2, file = paste0(path_store_err,"/center/diff_2/err_CC_center_diff_2.Rdata") )
}
