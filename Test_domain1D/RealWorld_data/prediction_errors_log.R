rm(list=ls())
graphics.off()
cat("\014")
set.seed(23032000)

##############################################################################
### Computing the prediction errors on log-curves for the predictions made ###
##############################################################################



#put here the path of the local copy of the directory
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"


#if you want to save the result in a folder 
save_res = TRUE
format = ".jpg"
dir_res = paste0(dir_w,"/Test_domain1D/RealWorld_data/results")


#in which folder the result of the prediction are
path_res_pred = paste0(dir_res,"/results_prediction_log")

#where to store the results
path_stor_res = paste0(paste0(dir_res,"/results_errors_log"))  

#predictors used
prediction_method = c("PPC", "KE", "KEI", "MP", "NP", "CC")

for (pred_met in prediction_method) {
  
  files <- list.files(path = paste0(path_res_pred,paste0("/",pred_met)), full.names = TRUE)
  for (file in files) {
    load(file)
  }
  
}

tot_pred = length(prediction_PPC_offer_log)



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
    offers_dataset[,i]  <-  log(MGS_cg_260419_310120_data$y_axis[[i]][[2]][1:x_grid_dim])
    demands_dataset[,i] <-  log(MGS_cg_260419_310120_data$y_axis[[i]][[3]][1:x_grid_dim])
  }
  
  first_prediction <- 98  #first day that I predict (counting from the beginning)
}


{
  en_offer_PPC_log  = numeric(tot_pred)
  rn_offer_PPC_log  = numeric(tot_pred)
  en_demand_PPC_log = numeric(tot_pred)
  rn_demand_PPC_log = numeric(tot_pred)
  
  en_offer_KE_log  = numeric(tot_pred)
  rn_offer_KE_log  = numeric(tot_pred)
  en_demand_KE_log = numeric(tot_pred)
  rn_demand_KE_log = numeric(tot_pred)
  
  en_offer_KEI_log  = numeric(tot_pred)
  rn_offer_KEI_log  = numeric(tot_pred)
  en_demand_KEI_log = numeric(tot_pred)
  rn_demand_KEI_log = numeric(tot_pred)
  
  en_offer_MP_log  = numeric(tot_pred)
  rn_offer_MP_log  = numeric(tot_pred)
  en_demand_MP_log = numeric(tot_pred)
  rn_demand_MP_log = numeric(tot_pred)
  
  en_offer_NP_log  = numeric(tot_pred)
  rn_offer_NP_log  = numeric(tot_pred)
  en_demand_NP_log = numeric(tot_pred)
  rn_demand_NP_log = numeric(tot_pred)
  
  en_offer_CC_log  = numeric(tot_pred)
  rn_offer_CC_log  = numeric(tot_pred)
  en_demand_CC_log = numeric(tot_pred)
  rn_demand_CC_log = numeric(tot_pred)
}



for (i in 1:tot_pred) {
  
  real_offer_curve = offers_dataset[,i-1+first_prediction]
  real_demand_curve = offers_dataset[,i-1+first_prediction]
  
  # saving prediction errors for offers curves
  en_offer_PPC_log[i] = sqrt(MLmetrics::MSE(real_offer_curve,prediction_PPC_offer_log[[i]]$Prediction))
  rn_offer_PPC_log[i] = MLmetrics::MAE(real_offer_curve,prediction_PPC_offer_log[[i]]$Prediction)
  
  en_offer_KE_log[i] = sqrt(MLmetrics::MSE(real_offer_curve,prediction_KE_offer_log[[i]]$Prediction))
  rn_offer_KE_log[i] = MLmetrics::MAE(real_offer_curve,prediction_KE_offer_log[[i]]$Prediction)
  
  en_offer_KEI_log[i] = sqrt(MLmetrics::MSE(real_offer_curve,prediction_KEI_offer_log[[i]]$Prediction))
  rn_offer_KEI_log[i] = MLmetrics::MAE(real_offer_curve,prediction_KEI_offer_log[[i]]$Prediction)
  
  en_offer_MP_log[i] = sqrt(MLmetrics::MSE(real_offer_curve,prediction_MP_offer_log[[i]]$Prediction))
  rn_offer_MP_log[i] = MLmetrics::MAE(real_offer_curve,prediction_MP_offer_log[[i]]$Prediction)
  
  en_offer_NP_log[i] = sqrt(MLmetrics::MSE(real_offer_curve,prediction_NP_offer_log[[i]]$Prediction))
  rn_offer_NP_log[i] = MLmetrics::MAE(real_offer_curve,prediction_NP_offer_log[[i]]$Prediction)
  
  en_offer_CC_log[i] = sqrt(MLmetrics::MSE(real_offer_curve,prediction_CC_offer_log[[i]]))
  rn_offer_CC_log[i] = MLmetrics::MAE(real_offer_curve,prediction_CC_offer_log[[i]])
  
  # saving prediction errors for demands curves
  en_demand_PPC_log[i] = sqrt(MLmetrics::MSE(real_demand_curve,prediction_PPC_demand_log[[i]]$Prediction))
  rn_demand_PPC_log[i] = MLmetrics::MAE(real_demand_curve,prediction_PPC_demand_log[[i]]$Prediction)

  en_demand_KE_log[i] = sqrt(MLmetrics::MSE(real_demand_curve,prediction_KE_demand_log[[i]]$Prediction))
  rn_demand_KE_log[i] = MLmetrics::MAE(real_demand_curve,prediction_KE_demand_log[[i]]$Prediction)
  
  en_demand_KEI_log[i] = sqrt(MLmetrics::MSE(real_demand_curve,prediction_KEI_demand_log[[i]]$Prediction))
  rn_demand_KEI_log[i] = MLmetrics::MAE(real_demand_curve,prediction_KEI_demand_log[[i]]$Prediction)
  
  en_demand_MP_log[i] = sqrt(MLmetrics::MSE(real_demand_curve,prediction_MP_demand_log[[i]]$Prediction))
  rn_demand_MP_log[i] = MLmetrics::MAE(real_demand_curve,prediction_MP_demand_log[[i]]$Prediction)
  
  en_demand_NP_log[i] = sqrt(MLmetrics::MSE(real_demand_curve,prediction_NP_demand_log[[i]]$Prediction))
  rn_demand_NP_log[i] = MLmetrics::MAE(real_demand_curve,prediction_NP_demand_log[[i]]$Prediction)
  
  en_demand_CC_log[i] = sqrt(MLmetrics::MSE(real_offer_curve,prediction_CC_demand_log[[i]]))
  rn_demand_CC_log[i] = MLmetrics::MAE(real_offer_curve,prediction_CC_demand_log[[i]])
  
}


{
  err_PPC_log = list( pred_offer  = list(en = en_offer_PPC_log,  rn = rn_offer_PPC_log),
                      pred_demand = list(en = en_demand_PPC_log, rn = rn_demand_PPC_log))
  err_KE_log  = list( pred_offer  = list(en = en_offer_KE_log,  rn = rn_offer_KE_log),
                      pred_demand = list(en = en_demand_KE_log, rn = rn_demand_KE_log))
  err_KEI_log = list( pred_offer  = list(en = en_offer_KEI_log,  rn = rn_offer_KEI_log),
                      pred_demand = list(en = en_demand_KEI_log, rn = rn_demand_KEI_log))
  err_MP_log  = list( pred_offer  = list(en = en_offer_MP_log,  rn = rn_offer_MP_log),
                      pred_demand = list(en = en_demand_MP_log, rn = rn_demand_MP_log))
  err_NP_log  = list( pred_offer  = list(en = en_offer_NP_log,  rn = rn_offer_NP_log),
                      pred_demand = list(en = en_demand_NP_log, rn = rn_demand_NP_log))
  err_CC_log  = list( pred_offer  = list(en = en_offer_CC_log,  rn = rn_offer_CC_log),
                      pred_demand = list(en = en_demand_CC_log, rn = rn_demand_CC_log))
}


if(save_res){
  
  save(err_PPC_log, file = paste0(path_stor_res,"/PPC_log_err.Rdata"))
  save(err_KE_log,  file = paste0(path_stor_res,"/KE_log_err.Rdata")) 
  save(err_KEI_log, file = paste0(path_stor_res,"/KEI_log_err.Rdata"))
  save(err_MP_log,  file = paste0(path_stor_res,"/MP_log_err.Rdata"))
  save(err_NP_log,  file = paste0(path_stor_res,"/NP_log_err.Rdata"))
  save(err_CC_log,  file = paste0(path_stor_res,"/CC_log_err.Rdata"))
}
