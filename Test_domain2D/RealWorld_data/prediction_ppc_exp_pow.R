rm(list=ls())
graphics.off()
cat("\014")

set.seed(23032000)
library(PPCKO)

#change here
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"

#save res
save_res = TRUE
folder_res = "/PPC_exp_pow"

#where the data are
dir_data = paste0(dir_w,"/Test_domain2D/RealWorld_data/data")
path_data_mouth = paste0(dir_data,"/mouth")
path_data_center = paste0(dir_data,"/center")

#where to store results
dir_res = paste0(dir_w,"/Test_domain2D/RealWorld_data/results/results_prediction")
path_store_res = paste0(dir_res,folder_res)


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
prediction_PPC_exp_pow_mouth          <- lapply(1:days_to_be_pred,function(x) NULL)
prediction_PPC_exp_pow_mouth_diff_1   <- lapply(1:days_to_be_pred,function(x) NULL)
prediction_PPC_exp_pow_mouth_diff_2   <- lapply(1:days_to_be_pred,function(x) NULL)
prediction_PPC_exp_pow_center         <- lapply(1:days_to_be_pred,function(x) NULL)
prediction_PPC_exp_pow_center_diff_1  <- lapply(1:days_to_be_pred,function(x) NULL)
prediction_PPC_exp_pow_center_diff_2  <- lapply(1:days_to_be_pred,function(x) NULL)

# paramters for PPC KO
id_CV_ = "CV_alpha"
alpha_vec = c(0.0001,0.001,0.01,0.1,1,10)
threshold_ppc = 0.9


##-----mouth prediction-----

# time serie
mouth = data_2d_wrapper_from_list(Xt_mouth)

string_message = "
                  PPC exp pow prediction of mouth zone "
for (i in 1:days_to_be_pred) {
  
  train_set = mouth[,(i - 1 + idx_first_day_first_train):(i - 1 + idx_first_day_first_train + window_train_set)]
  
  predictor = PPC_KO_2d( X = train_set,
                         id_CV = id_CV_,
                         threshold_ppc = threshold_ppc,
                         alpha_vec = alpha_vec,
                         disc_ev_x1 = lon_mouth,
                         num_disc_ev_x1 = length(lon_mouth),
                         disc_ev_x2 = lat_mouth,
                         num_disc_ev_x2 = length(lat_mouth),
                         left_extreme_x1 = min(lon_mouth),
                         right_extreme_x1 = max(lon_mouth),
                         left_extreme_x2 = min(lat_mouth),
                         right_extreme_x2 = max(lat_mouth))
  
  
  prediction_PPC_exp_pow_mouth[[i]] = list(Prediction = predictor$`One-step ahead prediction`, Alpha = predictor$Alpha, N_PPCs = predictor$`Number of PPCs retained`, Exp_Pow = predictor$`Explanatory power PPCs` )
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, days_to_be_pred)
  setTxtProgressBar(txtProgressBar(min = 1, max = days_to_be_pred, style = 3), i)
  cat("\r", message)
}

# save results
if(save_res){
  save(prediction_PPC_exp_pow_mouth, file = paste0(path_store_res,"/prediction_PPC_exp_pow_mouth.Rdata"))
}


# diff 1
string_message = "
                  PPC exp pow prediction of mouth zone diff 1 "
for (i in 1:days_to_be_pred) {
  
  # parto 
  train_set = Xt_mouth_diff_1[,(i - 2 + idx_first_day_first_train):(i - 2 + idx_first_day_first_train + window_train_set)]
  
  predictor = PPC_KO_2d( X = train_set,
                         id_CV = id_CV_,
                         threshold_ppc = threshold_ppc,
                         alpha_vec = alpha_vec,
                         disc_ev_x1 = lon_mouth,
                         num_disc_ev_x1 = length(lon_mouth),
                         disc_ev_x2 = lat_mouth,
                         num_disc_ev_x2 = length(lat_mouth),
                         left_extreme_x1 = min(lon_mouth),
                         right_extreme_x1 = max(lon_mouth),
                         left_extreme_x2 = min(lat_mouth),
                         right_extreme_x2 = max(lat_mouth))
  
  prediction = as.vector(predictor$`One-step ahead prediction`) + mouth[,i - 1 + idx_first_day_first_train + window_train_set]
  
  
  prediction_PPC_exp_pow_mouth_diff_1[[i]] = list(Prediction = prediction, Alpha = predictor$Alpha, N_PPCs = predictor$`Number of PPCs retained`, Exp_Pow = predictor$`Explanatory power PPCs` )
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, days_to_be_pred)
  setTxtProgressBar(txtProgressBar(min = 1, max = days_to_be_pred, style = 3), i)
  cat("\r", message)
}

# save results
if(save_res){
  save(prediction_PPC_exp_pow_mouth_diff_1, file = paste0(path_store_res,"/prediction_PPC_exp_pow_mouth_diff_1.Rdata"))
}


# diff 2
string_message = "
                  PPC exp pow prediction of mouth zone diff 2 "
for (i in 1:days_to_be_pred) {
  
  # parto 
  train_set = Xt_mouth_diff_2[,(i - 3 + idx_first_day_first_train):(i - 3 + idx_first_day_first_train + window_train_set)]
  
  predictor = PPC_KO_2d( X = train_set,
                         id_CV = id_CV_,
                         threshold_ppc = threshold_ppc,
                         alpha_vec = alpha_vec,
                         disc_ev_x1 = lon_mouth,
                         num_disc_ev_x1 = length(lon_mouth),
                         disc_ev_x2 = lat_mouth,
                         num_disc_ev_x2 = length(lat_mouth),
                         left_extreme_x1 = min(lon_mouth),
                         right_extreme_x1 = max(lon_mouth),
                         left_extreme_x2 = min(lat_mouth),
                         right_extreme_x2 = max(lat_mouth))
  
  
  prediction = as.vector(predictor$`One-step ahead prediction`) + 2*mouth[,(i - 1 + idx_first_day_first_train + window_train_set)] - mouth[,(i - 2 + idx_first_day_first_train + window_train_set)]
  
  
  prediction_PPC_exp_pow_mouth_diff_2[[i]] = list(Prediction = prediction, Alpha = predictor$Alpha, N_PPCs = predictor$`Number of PPCs retained`, Exp_Pow = predictor$`Explanatory power PPCs` )
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, days_to_be_pred)
  setTxtProgressBar(txtProgressBar(min = 1, max = days_to_be_pred, style = 3), i)
  cat("\r", message)
}

# save results
if(save_res){
  save(prediction_PPC_exp_pow_mouth_diff_2, file = paste0(path_store_res,"/prediction_PPC_exp_pow_mouth_diff_2.Rdata"))
}





##-----center prediction-----

# time serie
center = data_2d_wrapper_from_list(Xt_center)

string_message = "
                  PPC exp pow prediction of center zone "
for (i in 1:days_to_be_pred) {
  
  train_set = center[,(i - 1 + idx_first_day_first_train):(i - 1 + idx_first_day_first_train + window_train_set)]
  
  predictor = PPC_KO_2d( X = train_set,
                         id_CV = id_CV_,
                         threshold_ppc = threshold_ppc,
                         alpha_vec = alpha_vec,
                         disc_ev_x1 = lon_center,
                         num_disc_ev_x1 = length(lon_center),
                         disc_ev_x2 = lat_center,
                         num_disc_ev_x2 = length(lat_center),
                         left_extreme_x1 = min(lon_center),
                         right_extreme_x1 = max(lon_center),
                         left_extreme_x2 = min(lat_center),
                         right_extreme_x2 = max(lat_center))
  
  
  prediction_PPC_exp_pow_center[[i]] = list(Prediction = predictor$`One-step ahead prediction`, Alpha = predictor$Alpha, N_PPCs = predictor$`Number of PPCs retained`, Exp_Pow = predictor$`Explanatory power PPCs` )
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, days_to_be_pred)
  setTxtProgressBar(txtProgressBar(min = 1, max = days_to_be_pred, style = 3), i)
  cat("\r", message)
}

# save results
if(save_res){
  save(prediction_PPC_exp_pow_center, file = paste0(path_store_res,"/prediction_PPC_exp_pow_center.Rdata"))
}


# diff 1
string_message = "
                  PPC exp pow prediction of center zone diff 1 "
for (i in 1:days_to_be_pred) {
  
  # parto 
  train_set = Xt_center_diff_1[,(i - 2 + idx_first_day_first_train):(i - 2 + idx_first_day_first_train + window_train_set)]
  
  predictor = PPC_KO_2d( X = train_set,
                         id_CV = id_CV_,
                         threshold_ppc = threshold_ppc,
                         alpha_vec = alpha_vec,
                         disc_ev_x1 = lon_center,
                         num_disc_ev_x1 = length(lon_center),
                         disc_ev_x2 = lat_center,
                         num_disc_ev_x2 = length(lat_center),
                         left_extreme_x1 = min(lon_center),
                         right_extreme_x1 = max(lon_center),
                         left_extreme_x2 = min(lat_center),
                         right_extreme_x2 = max(lat_center))
  
  
  prediction = as.vector(predictor$`One-step ahead prediction`) + center[,i - 1 + idx_first_day_first_train + window_train_set]
  
  
  prediction_PPC_exp_pow_center_diff_1[[i]] = list(Prediction = prediction, Alpha = predictor$Alpha, N_PPCs = predictor$`Number of PPCs retained`, Exp_Pow = predictor$`Explanatory power PPCs` )
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, days_to_be_pred)
  setTxtProgressBar(txtProgressBar(min = 1, max = days_to_be_pred, style = 3), i)
  cat("\r", message)
}

# save results
if(save_res){
  save(prediction_PPC_exp_pow_center_diff_1, file = paste0(path_store_res,"/prediction_PPC_exp_pow_center_diff_1.Rdata"))
}


# diff 2
string_message = "
                  PPC exp pow prediction of center zone diff 2 "
for (i in 1:days_to_be_pred) {
  
  # parto 
  train_set = Xt_center_diff_2[,(i - 3 + idx_first_day_first_train):(i - 3 + idx_first_day_first_train + window_train_set)]
  
  predictor = PPC_KO_2d( X = train_set,
                         id_CV = id_CV_,
                         threshold_ppc = threshold_ppc,
                         alpha_vec = alpha_vec,
                         disc_ev_x1 = lon_center,
                         num_disc_ev_x1 = length(lon_center),
                         disc_ev_x2 = lat_center,
                         num_disc_ev_x2 = length(lat_center),
                         left_extreme_x1 = min(lon_center),
                         right_extreme_x1 = max(lon_center),
                         left_extreme_x2 = min(lat_center),
                         right_extreme_x2 = max(lat_center))
  
  prediction = as.vector(predictor$`One-step ahead prediction`) + 2*center[,(i - 1 + idx_first_day_first_train + window_train_set)] - center[,(i - 2 + idx_first_day_first_train + window_train_set)]
  
  
  prediction_PPC_exp_pow_center_diff_2[[i]] = list(Prediction = prediction, Alpha = predictor$Alpha, N_PPCs = predictor$`Number of PPCs retained`, Exp_Pow = predictor$`Explanatory power PPCs` )
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, days_to_be_pred)
  setTxtProgressBar(txtProgressBar(min = 1, max = days_to_be_pred, style = 3), i)
  cat("\r", message)
}

# save results
if(save_res){
  save(prediction_PPC_exp_pow_center_diff_2, file = paste0(path_store_res,"/prediction_PPC_exp_pow_center_diff_2.Rdata"))
}