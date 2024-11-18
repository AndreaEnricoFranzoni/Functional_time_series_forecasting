## in questo script, si sta facendo previsione dei prezzi di offerta e richiest
## del gas, per 184 giorni, utilizzando i precedenti 97 come train set


rm(list=ls())
graphics.off()
cat("\014")

set.seed(23032000)

#change here
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series"

load(paste0(dir_w,"/Test_domain1D/RealWorld_data/final_p_q_260419_310120_data.Rdata"))
load(paste0(dir_w,"/Test_domain1D/RealWorld_data/MGS_cg_260419_310120_data.Rdata"))
load(paste0(dir_w,"/Test_domain1D/RealWorld_data/result_260419_310120_GS_after_corr.Rdata"))

total_abscissa     <- 401
alpha_v = c(1e-4,1e-3,1e-2,1e-1,1,1e1,1e2)
quantities_dataset_ko = MGS_cg_260419_310120_data$x_axis[1:total_abscissa]
a = min(quantities_dataset_ko)
b = max(quantities_dataset_ko)

time_instants_tot  <- length(MGS_cg_260419_310120_data$y_axis)
offers_dataset_ko  <- matrix(data = NA, nrow = 401, ncol = time_instants_tot)
demands_dataset_ko <- matrix(data = NA, nrow = 401, ncol = time_instants_tot)

for (i in 1:time_instants_tot) {
  offers_dataset_ko[,i] =  MGS_cg_260419_310120_data$y_axis[[i]][[2]][1:401]
  demands_dataset_ko[,i] =  MGS_cg_260419_310120_data$y_axis[[i]][[3]][1:401]
}



start_dataset=1 #giorno di inizio del dataset..1 ? "2019-04-26"
value_seed=0
end_dataset=90 #giorno di fine del dataset..90 ? "2019-07-24"
delay = 7
l=39
last_prediction=281 #ultimo giorno che prevedo... ? 2020-01-31


#predictions of offer prices
result_KO_price_offer_mw  = lapply((end_dataset+8):last_prediction-1,function(x) NULL)
total_runs = length(result_KO_price_offer_mw)

{
pb <- progress::progress_bar$new(
  format = " KO test [:bar] :percent in :elapsed",
  total = total_runs, clear = FALSE, width= 60)
for (i in 1:total_runs) {
  
  offer_mw = offers_dataset_ko[,i:(i-1+end_dataset+delay)]
  ko_offer_mw = PPCKO.local2::PPC_KO(offer_mw,"CV",alpha_vec = alpha_v,disc_ev = quantities_dataset_ko,left_extreme = a,right_extreme = b)
  result_KO_price_offer_mw[[i]] = ko_offer_mw$`One-step ahead prediction`
  
  pb$tick()
}
}


predictions_offers_KO = result_KO_price_offer_mw
file_saving_offers = paste0(dir_w,"/Test_domain1D/RealWorld_data/predictions_offers_KO.RData")
save(predictions_offers_KO, file = file_saving_offers)
load(file_saving_offers)







#prediction of demand prices
result_KO_price_demand_mw = lapply((end_dataset+8):last_prediction-1,function(x) NULL)
total_runs = length(result_KO_price_demand_mw)

{
pb <- progress::progress_bar$new(format = " KO test [:bar] :percent in :elapsed",
                                 total = total_runs, clear = FALSE, width= 60)
for (i in 1:total_runs){
    
    demand_mw = demands_dataset_ko[,i:(i-1+end_dataset+delay)]
    ko_demand_mw = PPCKO.local2::PPC_KO(demand_mw,"CV",alpha_vec = alpha_v,disc_ev = quantities_dataset_ko,left_extreme = a,right_extreme = b)
    result_KO_price_demand_mw[[i]] = ko_demand_mw$`One-step ahead prediction`
  
    pb$tick()
  }
}


predictions_demands_KO = result_KO_price_demand_mw
file_saving_demands = paste0(dir_w,"/Test_domain1D/RealWorld_data/predictions_demands_KO.RData")
save(predictions_demands_KO, file = file_saving_demands)

load(file_saving_demands)