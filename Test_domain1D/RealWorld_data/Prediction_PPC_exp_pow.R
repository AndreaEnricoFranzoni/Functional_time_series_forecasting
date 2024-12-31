rm(list=ls())
graphics.off()
cat("\014")
set.seed(23032000)

library(PPCKO)


#change here
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"
dir_stor_res = "/Test_domain1D/RealWorld_data/results/results_prediction"
name_folder_res = "/PPC_exp_pow"

#if you want to save the result 
save_res = TRUE
#where to store the results
path_stor_res = paste0(paste0(dir_w,dir_stor_res),name_folder_res)  


# ----- data -----
load(paste0(dir_w,"/Test_domain1D/RealWorld_data/utils/data/MGS_cg_260419_310120_data.Rdata"))

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
prediction_PPC_exp_pow_offer  <- lapply((first_prediction):tot_time_instants-1,function(x) NULL)
prediction_PPC_exp_pow_demand <- lapply((first_prediction):tot_time_instants-1,function(x) NULL)
total_predictions     <- length(prediction_PPC_exp_pow_offer)

# ----- PPC KO parameters -----
{
  id_CV_ = "CV_alpha"
  threshold_ppc = 0.9
}





## ----- offers prediction -----
string_message = "
                  PPC exp pow prediction of offers price "

for (i in 1:total_predictions) {
  
  train_set = offers_dataset[,1:(i-2+first_prediction)]
  
  PPC_predictor = PPC_KO( X = train_set,
                          id_CV = id_CV_,
                          threshold_ppc = threshold_ppc,
                          disc_ev = x_grid,
                          left_extreme = left_extreme,
                          right_extreme = right_extreme)
  
  prediction_PPC_exp_pow_offer[[i]] = list(Prediction = PPC_predictor$`One-step ahead prediction`, Alpha = PPC_predictor$Alpha, N_PPCs = PPC_predictor$`Number of PPCs retained`, Exp_Pow = PPC_predictor$`Explanatory power PPCs` )
  
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, total_predictions)
  setTxtProgressBar(txtProgressBar(min = 1, max = total_predictions, style = 3), i)
  cat("\r", message)
}

#save results
if(save_res){
  file_pred_offers = paste0(path_stor_res,"/prediction_PPC_exp_pow_offer.Rdata")
  save(prediction_PPC_exp_pow_offer, file = file_pred_offers)
}





## ----- demands prediction -----
string_message = "
                  PPC exp pow prediction of demands price "

for (i in 1:total_predictions) {
  
  train_set = demands_dataset[,1:(i-2+first_prediction)]
  
  PPC_predictor = PPC_KO( X = train_set,
                          id_CV = id_CV_,
                          threshold_ppc = threshold_ppc,
                          disc_ev = x_grid,
                          left_extreme = left_extreme,
                          right_extreme = right_extreme)
  
  prediction_PPC_exp_pow_demand[[i]] = list(Prediction = PPC_predictor$`One-step ahead prediction`, Alpha = PPC_predictor$Alpha, N_PPCs = PPC_predictor$`Number of PPCs retained`, Exp_Pow = PPC_predictor$`Explanatory power PPCs` )
  
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, total_predictions)
  setTxtProgressBar(txtProgressBar(min = 1, max = total_predictions, style = 3), i)
  cat("\r", message)
}

#save results
if(save_res){
  file_pred_demands = paste0(path_stor_res,"/prediction_PPC_exp_pow_demand.Rdata")
  save(prediction_PPC_exp_pow_demand, file = file_pred_demands)
}