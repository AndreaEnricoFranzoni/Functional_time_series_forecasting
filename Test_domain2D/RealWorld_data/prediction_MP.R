rm(list=ls())
graphics.off()
cat("\014")

set.seed(23032000)

#############################################################
#### Computing MP prediction as indicated in the readme  ####
#############################################################


#put here the path of the local copy of the directory
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"

#save res
save_res = TRUE
folder_res = "/MP"

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
prediction_MP_mouth          <- lapply(1:days_to_be_pred,function(x) NULL)
prediction_MP_mouth_diff_1   <- lapply(1:days_to_be_pred,function(x) NULL)
prediction_MP_mouth_diff_2   <- lapply(1:days_to_be_pred,function(x) NULL)
prediction_MP_center         <- lapply(1:days_to_be_pred,function(x) NULL)
prediction_MP_center_diff_1  <- lapply(1:days_to_be_pred,function(x) NULL)
prediction_MP_center_diff_2  <- lapply(1:days_to_be_pred,function(x) NULL)




##-----mouth prediction-----

# time serie
mouth = data_2d_wrapper_from_list(Xt_mouth)

string_message = "
                  MP prediction of mouth zone "
for (i in 1:days_to_be_pred) {
  
  train_set = mouth[,(i - 1 + idx_first_day_first_train):(i - 1 + idx_first_day_first_train + window_train_set)]
  
  predictor = rowMeans(train_set)
  
  
  prediction_MP_mouth[[i]] = list(Prediction = predictor )
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, days_to_be_pred)
  setTxtProgressBar(txtProgressBar(min = 1, max = days_to_be_pred, style = 3), i)
  cat("\r", message)
}

# save results
if(save_res){
  save(prediction_MP_mouth, file = paste0(path_store_res,"/prediction_MP_mouth.Rdata"))
}


# diff 1
string_message = "
                  MP prediction of mouth zone diff 1 "
for (i in 1:days_to_be_pred) {
  
  # sto trainando su delta_X_t:
  # indice i: c'è delta_X_(i+1): voglio predire delta_X_t, quindi gli indici sono shiftati indietro di uno
  train_set = Xt_mouth_diff_1[,(i - 2 + idx_first_day_first_train):(i - 2 + idx_first_day_first_train + window_train_set)]
  
  # predici delta_X_t, e poi aggiungi X_(t-1)
  predictor = rowMeans(train_set) + mouth[,i - 1 + idx_first_day_first_train + window_train_set]
  
  
  
  prediction_MP_mouth_diff_1[[i]] = list(Prediction = predictor )
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, days_to_be_pred)
  setTxtProgressBar(txtProgressBar(min = 1, max = days_to_be_pred, style = 3), i)
  cat("\r", message)
}

# save results
if(save_res){
  save(prediction_MP_mouth_diff_1, file = paste0(path_store_res,"/prediction_MP_mouth_diff_1.Rdata"))
}


# diff 2
string_message = "
                  MP prediction of mouth zone diff 2 "
for (i in 1:days_to_be_pred) {
  
  # sto trainando su delta2_X_t:
  # indice i: c'è delta_X_(i+2): voglio predire delta2_X_t, quindi gli indici sono shiftati indietro di due
  train_set = Xt_mouth_diff_2[,(i - 3 + idx_first_day_first_train):(i - 3 + idx_first_day_first_train + window_train_set)]
  
  predictor = rowMeans(train_set) + 2*mouth[,(i - 1 + idx_first_day_first_train + window_train_set)] - mouth[,(i - 2 + idx_first_day_first_train + window_train_set)]
  
  
  prediction_MP_mouth_diff_2[[i]] = list(Prediction = predictor)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, days_to_be_pred)
  setTxtProgressBar(txtProgressBar(min = 1, max = days_to_be_pred, style = 3), i)
  cat("\r", message)
}

# save results
if(save_res){
  save(prediction_MP_mouth_diff_2, file = paste0(path_store_res,"/prediction_MP_mouth_diff_2.Rdata"))
}





##-----center prediction-----

# time serie
center = data_2d_wrapper_from_list(Xt_center)

string_message = "
                  MP prediction of center zone "
for (i in 1:days_to_be_pred) {
  
  
  train_set = center[,(i - 1 + idx_first_day_first_train):(i - 1 + idx_first_day_first_train + window_train_set)]
  
  predictor = rowMeans(train_set)
  
  
  prediction_MP_center[[i]] = list(Prediction = predictor )
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, days_to_be_pred)
  setTxtProgressBar(txtProgressBar(min = 1, max = days_to_be_pred, style = 3), i)
  cat("\r", message)
}

# save results
if(save_res){
  save(prediction_MP_center, file = paste0(path_store_res,"/prediction_MP_center.Rdata"))
}


# diff 1
string_message = "
                  MP prediction of center zone diff 1 "
for (i in 1:days_to_be_pred) {
  
  # parto 
  train_set = Xt_center_diff_1[,(i - 2 + idx_first_day_first_train):(i - 2 + idx_first_day_first_train + window_train_set)]
  
  predictor = rowMeans(train_set) + center[,i - 1 + idx_first_day_first_train + window_train_set]
  

  prediction_MP_center_diff_1[[i]] = list(Prediction = predictor )
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, days_to_be_pred)
  setTxtProgressBar(txtProgressBar(min = 1, max = days_to_be_pred, style = 3), i)
  cat("\r", message)
}

# save results
if(save_res){
  save(prediction_MP_center_diff_1, file = paste0(path_store_res,"/prediction_MP_center_diff_1.Rdata"))
}


# diff 2
string_message = "
                  MP prediction of center zone diff 2 "
for (i in 1:days_to_be_pred) {
  
  # parto 
  train_set = Xt_center_diff_2[,(i - 3 + idx_first_day_first_train):(i - 3 + idx_first_day_first_train + window_train_set)]
  
  predictor = rowMeans(train_set) + 2*center[,(i - 1 + idx_first_day_first_train + window_train_set)] - center[,(i - 2 + idx_first_day_first_train + window_train_set)]
  
  
  prediction_MP_center_diff_2[[i]] = list(Prediction = predictor )
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, days_to_be_pred)
  setTxtProgressBar(txtProgressBar(min = 1, max = days_to_be_pred, style = 3), i)
  cat("\r", message)
}

# save results
if(save_res){
  save(prediction_MP_center_diff_2, file = paste0(path_store_res,"/prediction_MP_center_diff_2.Rdata"))
}

