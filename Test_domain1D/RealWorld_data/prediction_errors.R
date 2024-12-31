rm(list=ls())
graphics.off()
cat("\014")
set.seed(23032000)

### Computing the prediction errors for the predictions made


#if you want to save the result in a folder 
save_res = TRUE
format = ".jpg"

dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"
dir_res = paste0(dir_w,"/Test_domain1D/RealWorld_data/results")



#in which folder the result of the prediction are
path_res_pred = paste0(dir_res,"/results_prediction")

#where to store the results
path_stor_res = paste0(paste0(dir_res,"/results_errors"))  

prediction_method = c("PPC", "PPC_exp_pow", "PPC_gen", "KE", "KEI", "MP", "NP", "CC")

for (pred_met in prediction_method) {

  files <- list.files(path = paste0(path_res_pred,paste0("/",pred_met)), full.names = TRUE)
  for (file in files) {
    load(file)
  }
  
}

tot_pred = length(prediction_PPC_offer)



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
  
  first_prediction <- 98  #first day that I predict (counting from the beginning)
}


{
  en_offer_PPC  = numeric(tot_pred)
  rn_offer_PPC  = numeric(tot_pred)
  en_demand_PPC = numeric(tot_pred)
  rn_demand_PPC = numeric(tot_pred)
  
  en_offer_PPC_exp_pow  = numeric(tot_pred)
  rn_offer_PPC_exp_pow  = numeric(tot_pred)
  en_demand_PPC_exp_pow = numeric(tot_pred)
  rn_demand_PPC_exp_pow = numeric(tot_pred)
  
  en_offer_PPC_gen  = numeric(tot_pred)
  rn_offer_PPC_gen  = numeric(tot_pred)
  en_demand_PPC_gen = numeric(tot_pred)
  rn_demand_PPC_gen = numeric(tot_pred)
  
  en_offer_KE  = numeric(tot_pred)
  rn_offer_KE  = numeric(tot_pred)
  en_demand_KE = numeric(tot_pred)
  rn_demand_KE = numeric(tot_pred)
  
  en_offer_KEI  = numeric(tot_pred)
  rn_offer_KEI  = numeric(tot_pred)
  en_demand_KEI = numeric(tot_pred)
  rn_demand_KEI = numeric(tot_pred)
  
  en_offer_MP  = numeric(tot_pred)
  rn_offer_MP  = numeric(tot_pred)
  en_demand_MP = numeric(tot_pred)
  rn_demand_MP = numeric(tot_pred)
  
  en_offer_NP  = numeric(tot_pred)
  rn_offer_NP  = numeric(tot_pred)
  en_demand_NP = numeric(tot_pred)
  rn_demand_NP = numeric(tot_pred)
  
  en_offer_CC  = numeric(tot_pred)
  rn_offer_CC  = numeric(tot_pred)
  en_demand_CC = numeric(tot_pred)
  rn_demand_CC = numeric(tot_pred)
}



for (i in 1:tot_pred) {
  
  real_offer_curve = offers_dataset[,i-1+first_prediction]
  real_demand_curve = offers_dataset[,i-1+first_prediction]
  
  # saving prediction errors for offers curves
  en_offer_PPC[i] = sqrt(MLmetrics::MSE(real_offer_curve,prediction_PPC_offer[[i]]$Prediction))
  rn_offer_PPC[i] = MLmetrics::MAE(real_offer_curve,prediction_PPC_offer[[i]]$Prediction)
  
  en_offer_PPC_exp_pow[i] = sqrt(MLmetrics::MSE(real_offer_curve,prediction_PPC_exp_pow_offer[[i]]$Prediction))
  rn_offer_PPC_exp_pow[i] = MLmetrics::MAE(real_offer_curve,prediction_PPC_exp_pow_offer[[i]]$Prediction)
  
  en_offer_PPC_gen[i] = sqrt(MLmetrics::MSE(real_offer_curve,prediction_PPC_gen_offer[[i]]$Prediction))
  rn_offer_PPC_gen[i] = MLmetrics::MAE(real_offer_curve,prediction_PPC_gen_offer[[i]]$Prediction)
  
  en_offer_KE[i] = sqrt(MLmetrics::MSE(real_offer_curve,prediction_KE_offer[[i]]$Prediction))
  rn_offer_KE[i] = MLmetrics::MAE(real_offer_curve,prediction_KE_offer[[i]]$Prediction)
  
  en_offer_KEI[i] = sqrt(MLmetrics::MSE(real_offer_curve,prediction_KEI_offer[[i]]$Prediction))
  rn_offer_KEI[i] = MLmetrics::MAE(real_offer_curve,prediction_KEI_offer[[i]]$Prediction)
  
  en_offer_MP[i] = sqrt(MLmetrics::MSE(real_offer_curve,prediction_MP_offer[[i]]$Prediction))
  rn_offer_MP[i] = MLmetrics::MAE(real_offer_curve,prediction_MP_offer[[i]]$Prediction)
  
  en_offer_NP[i] = sqrt(MLmetrics::MSE(real_offer_curve,prediction_NP_offer[[i]]$Prediction))
  rn_offer_NP[i] = MLmetrics::MAE(real_offer_curve,prediction_NP_offer[[i]]$Prediction)
  
  en_offer_CC[i] = sqrt(MLmetrics::MSE(real_offer_curve,prediction_CC_offer[[i]]))
  rn_offer_CC[i] = MLmetrics::MAE(real_offer_curve,prediction_CC_offer[[i]])
  
  # saving prediction errors for demands curves
  en_demand_PPC[i] = sqrt(MLmetrics::MSE(real_demand_curve,prediction_PPC_demand[[i]]$Prediction))
  rn_demand_PPC[i] = MLmetrics::MAE(real_demand_curve,prediction_PPC_demand[[i]]$Prediction)
  
  en_demand_PPC_exp_pow[i] = sqrt(MLmetrics::MSE(real_demand_curve,prediction_PPC_exp_pow_demand[[i]]$Prediction))
  rn_demand_PPC_exp_pow[i] = MLmetrics::MAE(real_demand_curve,prediction_PPC_exp_pow_demand[[i]]$Prediction)
  
  en_demand_PPC_gen[i] = sqrt(MLmetrics::MSE(real_demand_curve,prediction_PPC_gen_demand[[i]]$Prediction))
  rn_demand_PPC_gen[i] = MLmetrics::MAE(real_demand_curve,prediction_PPC_gen_demand[[i]]$Prediction)
  
  en_demand_KE[i] = sqrt(MLmetrics::MSE(real_demand_curve,prediction_KE_demand[[i]]$Prediction))
  rn_demand_KE[i] = MLmetrics::MAE(real_demand_curve,prediction_KE_demand[[i]]$Prediction)
  
  en_demand_KEI[i] = sqrt(MLmetrics::MSE(real_demand_curve,prediction_KEI_demand[[i]]$Prediction))
  rn_demand_KEI[i] = MLmetrics::MAE(real_demand_curve,prediction_KEI_demand[[i]]$Prediction)
  
  en_demand_MP[i] = sqrt(MLmetrics::MSE(real_demand_curve,prediction_MP_demand[[i]]$Prediction))
  rn_demand_MP[i] = MLmetrics::MAE(real_demand_curve,prediction_MP_demand[[i]]$Prediction)
  
  en_demand_NP[i] = sqrt(MLmetrics::MSE(real_demand_curve,prediction_NP_demand[[i]]$Prediction))
  rn_demand_NP[i] = MLmetrics::MAE(real_demand_curve,prediction_NP_demand[[i]]$Prediction)
  
  en_demand_CC[i] = sqrt(MLmetrics::MSE(real_offer_curve,prediction_CC_demand[[i]]))
  rn_demand_CC[i] = MLmetrics::MAE(real_offer_curve,prediction_CC_demand[[i]])

}


{
err_PPC = list( pred_offer  = list(en = en_offer_PPC,  rn = rn_offer_PPC),
                pred_demand = list(en = en_demand_PPC, rn = rn_demand_PPC))
err_PPC_exp_pow = list( pred_offer  = list(en = en_offer_PPC_exp_pow,  rn = rn_offer_PPC_exp_pow),
                        pred_demand = list(en = en_demand_PPC_exp_pow, rn = rn_demand_PPC_exp_pow))
err_PPC_gen = list( pred_offer  = list(en = en_offer_PPC_gen,  rn = rn_offer_PPC_gen),
                    pred_demand = list(en = en_demand_PPC_gen, rn = rn_demand_PPC_gen))
err_KE  = list( pred_offer  = list(en = en_offer_KE,  rn = rn_offer_KE),
                pred_demand = list(en = en_demand_KE, rn = rn_demand_KE))
err_KEI = list( pred_offer  = list(en = en_offer_KEI,  rn = rn_offer_KEI),
                pred_demand = list(en = en_demand_KEI, rn = rn_demand_KEI))
err_MP  = list( pred_offer  = list(en = en_offer_MP,  rn = rn_offer_MP),
                pred_demand = list(en = en_demand_MP, rn = rn_demand_MP))
err_NP  = list( pred_offer  = list(en = en_offer_NP,  rn = rn_offer_NP),
                pred_demand = list(en = en_demand_NP, rn = rn_demand_NP))
err_CC  = list( pred_offer  = list(en = en_offer_CC,  rn = rn_offer_CC),
                pred_demand = list(en = en_demand_CC, rn = rn_demand_CC))
}


if(save_res){
  
  save(err_PPC, file = paste0(path_stor_res,"/PPC_err.Rdata"))
  save(err_PPC_exp_pow, file = paste0(path_stor_res,"/PPC_exp_pow_err.Rdata"))
  save(err_PPC_gen, file = paste0(path_stor_res,"/PPC_gen_err.Rdata"))
  save(err_KE,  file = paste0(path_stor_res,"/KE_err.Rdata")) 
  save(err_KEI, file = paste0(path_stor_res,"/KEI_err.Rdata"))
  save(err_MP,  file = paste0(path_stor_res,"/MP_err.Rdata"))
  save(err_NP,  file = paste0(path_stor_res,"/NP_err.Rdata"))
  save(err_CC,  file = paste0(path_stor_res,"/CC_err.Rdata"))
}
