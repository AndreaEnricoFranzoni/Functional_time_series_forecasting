rm(list=ls())
graphics.off()
cat("\014")
set.seed(23032000)

library(PPCKO)


#change here
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"

load(paste0(dir_w,"/Test_domain1D/RealWorld_data/utils/data/MGS_cg_260419_310120_data.Rdata"))

#if you want to store results
save_res = TRUE
dir_res = "/Test_domain1D/RealWorld_data/results"
name_folder_res = "/results_next_instant_forecast"
path_stor_res = paste0(paste0(dir_w,dir_res),name_folder_res)




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


## -----offers-----

# check hp
hp_offer = KO_check_hps(offers_dataset)
# forecasting
PPC_pred_offer = PPC_KO( X = offers_dataset,
                        id_CV = "CV",
                        disc_ev = x_grid,
                        left_extreme = left_extreme,
                        right_extreme = right_extreme)
# show results
KO_show_results(PPC_pred_offer,hp_offer,x_lab="Quantity [MWh]", y_lab = "Price offer [Euro/MWh]")

if(save_res){
  save(PPC_pred_offer, file = paste0(path_stor_res,"/prediction_PPC_offer.Rdata"))
  save(hp_offer, file = paste0(path_stor_res,"/hp_PPC_offer.Rdata"))
}




## -----demands-----

# check hp
hp_demand = KO_check_hps(demands_dataset)
# forecasting
PPC_pred_demand = PPC_KO( X = demands_dataset,
                          id_CV = "CV",
                          disc_ev = x_grid,
                          left_extreme = left_extreme,
                          right_extreme = right_extreme)
# show results
KO_show_results(PPC_pred_demand,hp_demand,x_lab="Quantity [MWh]", y_lab = "Price demand [Euro/MWh]")

if(save_res){
  save(PPC_pred_demand, file = paste0(path_stor_res,"/prediction_PPC_demand.Rdata"))
  save(hp_demand, file = paste0(path_stor_res,"/hp_PPC_demand.Rdata"))
}