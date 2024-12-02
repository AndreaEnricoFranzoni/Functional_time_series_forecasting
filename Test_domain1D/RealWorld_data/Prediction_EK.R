rm(list=ls())
graphics.off()
cat("\014")
set.seed(23032000)

library(KePredictor)


#change here
#change here
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"
dir_stor_res = "/Test_domain1D/RealWorld_data/results/results_prediction"
name_folder_res_1 = "/KE"
name_folder_res_2 = "/KEI"

#if you want to save the result 
save_res = FALSE
#where to store the results
path_stor_res_1 = paste0(paste0(dir_w,dir_stor_res),name_folder_res_1)  
path_stor_res_2 = paste0(paste0(dir_w,dir_stor_res),name_folder_res_2)  

load(paste0(dir_w,"/Test_domain1D/RealWorld_data/utils/data/MGS_cg_260419_310120_data.Rdata"))


# ----- data -----
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
prediction_KE_offer  <- lapply((first_prediction):tot_time_instants-1,function(x) NULL)
prediction_KE_demand <- lapply((first_prediction):tot_time_instants-1,function(x) NULL)
prediction_KEI_offer  <- lapply((first_prediction):tot_time_instants-1,function(x) NULL)
prediction_KEI_demand <- lapply((first_prediction):tot_time_instants-1,function(x) NULL)
total_predictions     <- length(prediction_KE_offer)


# ----- KE parameters -----
{
  k_vec = 1:10
}




string_message = "
                  KE and KEI prediction of offers price "

for (i in 1:total_predictions) {
  
  train_set = offers_dataset[,1:(i-2+first_prediction)]

  KE_predictor  = KE( X = train_set, k_vec = k_vec)
  KEI_predictor = KEI(X = train_set, k_vec = k_vec)
  
  
  prediction_KE_offer[[i]]  = list(Prediction = KE_predictor$`One-step ahead prediction`, N_comp = KE_predictor$`Number of Components retained`, Exp_Pow = KE_predictor$`Explained variance` )
  prediction_KEI_offer[[i]] = list(Prediction = KEI_predictor$`One-step ahead prediction`, N_comp = KEI_predictor$`Number of Components retained`, Exp_Pow = KEI_predictor$`Explained variance` ) 
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, total_predictions)
  setTxtProgressBar(txtProgressBar(min = 1, max = total_predictions, style = 3), i)
  cat("\r", message)
}

# save results
if(save_res){
  save(prediction_KE_offer,  file = paste0(path_stor_res_1,"/prediction_KE_offer.Rdata"))
  save(prediction_KEI_offer, file = paste0(path_stor_res_2,"/prediction_KEI_offer.Rdata"))
}





string_message = "
                  PPC prediction of demands price "

for (i in 1:total_predictions) {
  
  train_set = demands_dataset[,1:(i-2+first_prediction)]
  
  KE_predictor  = KE( X = train_set, k_vec = k_vec)
  KEI_predictor = KEI(X = train_set, k_vec = k_vec)
  
  
  prediction_KE_demand[[i]]  = list(Prediction = KE_predictor$`One-step ahead prediction`, N_comp = KE_predictor$`Number of Components retained`, Exp_Pow = KE_predictor$`Explained variance` )
  prediction_KEI_demand[[i]] = list(Prediction = KEI_predictor$`One-step ahead prediction`, N_comp = KEI_predictor$`Number of Components retained`, Exp_Pow = KEI_predictor$`Explained variance` ) 
  
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, total_predictions)
  setTxtProgressBar(txtProgressBar(min = 1, max = total_predictions, style = 3), i)
  cat("\r", message)
}

# save results
if(save_res){
  save(prediction_KE_demand,  file = paste0(path_stor_res_1,"/prediction_KE_demand.Rdata"))
  save(prediction_KEI_demand, file = paste0(path_stor_res_2,"/prediction_KEI_demand.Rdata"))
}
