rm(list=ls())
graphics.off()
cat("\014")
set.seed(23032000)



#############################################################
#### Computing MP prediction as indicated in the readme  ####
#############################################################

#put here the path of the local copy of the directory
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"

#if you want to save the result 
save_res = FALSE



#where to store the results
dir_stor_res = "/Test_domain1D/RealWorld_data/results/results_prediction"
name_folder_res = "/MP"
path_stor_res = paste0(paste0(dir_w,dir_stor_res),name_folder_res)  

#load data
{
  load(paste0(dir_w,"/Test_domain1D/RealWorld_data/utils/data/MGS_cg_260419_310120_data.Rdata"))
}


# ----- data param -----
{
  x_grid_dim         <- 401
  x_grid             <- MGS_cg_260419_310120_data$x_axis[1:x_grid_dim]
  left_extreme       <- min(x_grid)
  right_extreme      <- max(x_grid)
  
  tot_time_instants  <- length(MGS_cg_260419_310120_data$y_axis)
  offers_dataset     <- matrix(data = NA, nrow = x_grid_dim, ncol = tot_time_instants)
  demands_dataset    <- matrix(data = NA, nrow = x_grid_dim, ncol = tot_time_instants)
  
  for (i in 1:tot_time_instants) {
    offers_dataset[,i]  <-  MGS_cg_260419_310120_data$y_axis[[i]][[2]][1:x_grid_dim]
    demands_dataset[,i] <-  MGS_cg_260419_310120_data$y_axis[[i]][[3]][1:x_grid_dim]
  }
  
  first_prediction <- 98
}


# ----- storing results -----
prediction_MP_offer  <- lapply((first_prediction):tot_time_instants-1,function(x) NULL)
prediction_MP_demand <- lapply((first_prediction):tot_time_instants-1,function(x) NULL)
total_predictions     <- length(prediction_MP_offer)







string_message = "
                  MP prediction of offers price "

for (i in 1:total_predictions) {
  
  train_set = offers_dataset[,1:(i-2+first_prediction)]
  
  MP_predictor = rowMeans(train_set)
  
  
  prediction_MP_offer[[i]] = list(Prediction = MP_predictor )
  
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, total_predictions)
  setTxtProgressBar(txtProgressBar(min = 1, max = total_predictions, style = 3), i)
  cat("\r", message)
}

#save results
if(save_res){
  file_pred_offers = paste0(path_stor_res,"/prediction_MP_offer.Rdata")
  save(prediction_MP_offer, file = file_pred_offers)
}






string_message = "
                  MP prediction of demands price "

for (i in 1:total_predictions) {
  
  train_set = demands_dataset[,1:(i-2+first_prediction)]
  
  MP_predictor = rowMeans(train_set)
  
  
  prediction_MP_demand[[i]] = list(Prediction = MP_predictor )
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, total_predictions)
  setTxtProgressBar(txtProgressBar(min = 1, max = total_predictions, style = 3), i)
  cat("\r", message)
}

# save results
if(save_res){
  file_pred_demands = paste0(path_stor_res,"/prediction_MP_demand.Rdata")
  save(prediction_MP_demand, file = file_pred_demands)
}
