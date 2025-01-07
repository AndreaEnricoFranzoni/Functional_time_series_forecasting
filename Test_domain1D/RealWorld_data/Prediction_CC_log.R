rm(list=ls())
graphics.off()
cat("\014")
set.seed(23032000)



#change here
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"
dir_stor_res = "/Test_domain1D/RealWorld_data/results/results_prediction_log"
name_folder_res = "/CC"

#if you want to save the result 
save_res = TRUE
#where to store the results
path_stor_res = paste0(paste0(dir_w,dir_stor_res),name_folder_res)  

#load data
{
  load(paste0(dir_w,"/Test_domain1D/RealWorld_data/utils/data/final_p_q_260419_310120_data.Rdata"))
  load(paste0(dir_w,"/Test_domain1D/RealWorld_data/utils/data/MGS_cg_260419_310120_data.Rdata"))
  load(paste0(dir_w,"/Test_domain1D/RealWorld_data/utils/data/result_260419_310120_GS_after_corr.Rdata"))
  source(paste0(dir_w,"/Test_domain1D/RealWorld_data/utils/CC_code/fun_GS_mod.R"))
  source(paste0(dir_w,"/Test_domain1D/RealWorld_data/utils/CC_code/fun_corr_monot.R"))
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
    offers_dataset[,i]  <-  log(MGS_cg_260419_310120_data$y_axis[[i]][[2]][1:x_grid_dim])
    demands_dataset[,i] <-  log(MGS_cg_260419_310120_data$y_axis[[i]][[3]][1:x_grid_dim])
  }
  
  first_prediction <- 98
}


# ----- storing results (for curves' predictions) -----
prediction_CC_offer_log  <- lapply((first_prediction):tot_time_instants-1,function(x) NULL)
prediction_CC_demand_log <- lapply((first_prediction):tot_time_instants-1,function(x) NULL)
total_predictions        <- length(prediction_CC_offer_log)


############### The code to perform prediction with the concurrent predictor has been
############### gently given by Jacopo Diquigiovanni for 
############### "Distribution-Free Prediction Bands for Multivariate Functional Time Series:
###############  an Application to the Italian Gas Market" by
############### Diquigiovanni, J., Fontana, M. and Vantini, S. 



############
# 1 --- Parameters that identifies first training day
############

#a window of 90 days is used for training the predicor. Its size is fixed
start_dataset=1     #starting day of dataset    (first "2019-04-26")
value_seed=0
end_dataset=90      #ending day of the dataset  (first "2019-07-24")
l=39
last_prediction=281 #last day to be predicted   ("2020-01-31")
# storing results (for CP bands)
result_CONFORMAL_log=lapply((end_dataset+8):last_prediction,function(x) NULL)


############
# 2 --- Parameters for data and for performing CP for the bands
############

pos_first_q=1   # first abscissa: q=0
pos_last_q=401  # last abscissa:  q=200000

b=1             # Chernozukov's parameter
s_type="st-dev" # modulation function

AR_order_v=1
AR_order_a=1
inclusion_price=TRUE
price_order=1



############
# 2 --- Results
############

# paramters to store results, predicted curves and results for CP bands
{
number_of_predictions = length((end_dataset+8):last_prediction) #number of curves predicted: 184
vec_day_to_be_predicted=rep(NA,number_of_predictions)
class(vec_day_to_be_predicted) <- "Date"
vec_final_price_new_day=rep(NA,number_of_predictions)
vec_final_quantity_new_day=rep(NA,number_of_predictions)
training_index=matrix(NA, number_of_predictions, length(start_dataset:end_dataset)-l-(max(AR_order_v,AR_order_a)+7)) # il numero di colonne ? n-l-burn_in, ossia m
calibration_included_index=matrix(NA,number_of_predictions,(l+1)/b-1)

curves_are_included_alp25=rep(NA,number_of_predictions)
curves_are_included_alp25_after_corr=rep(NA,number_of_predictions)
final_pq_is_included_alp25=rep(NA,number_of_predictions)
av_width_alp25=rep(NA,number_of_predictions)
av_width_alp25_after_corr=rep(NA,number_of_predictions)  
ncs_alp25=matrix(NA,number_of_predictions,(l+1)/b-1)
widths_alp25=matrix(NA,number_of_predictions,2)
widths_alp25_after_corr=matrix(NA,number_of_predictions,2)
inflim_predset_v_alp25=matrix(NA,number_of_predictions,length(pos_last_q:pos_first_q))
inflim_predset_a_alp25=matrix(NA,number_of_predictions,length(pos_last_q:pos_first_q))
suplim_predset_v_alp25=matrix(NA,number_of_predictions,length(pos_last_q:pos_first_q))
suplim_predset_a_alp25=matrix(NA,number_of_predictions,length(pos_last_q:pos_first_q))


curves_are_included_alp50=rep(NA,number_of_predictions)
curves_are_included_alp50_after_corr=rep(NA,number_of_predictions)
final_pq_is_included_alp50=rep(NA,number_of_predictions)
av_width_alp50=rep(NA,number_of_predictions)
av_width_alp50_after_corr=rep(NA,number_of_predictions)  
ncs_alp50=matrix(NA,number_of_predictions,(l+1)/b-1)
widths_alp50=matrix(NA,number_of_predictions,2)
widths_alp50_after_corr=matrix(NA,number_of_predictions,2)
inflim_predset_v_alp50=matrix(NA,number_of_predictions,length(pos_last_q:pos_first_q))
inflim_predset_a_alp50=matrix(NA,number_of_predictions,length(pos_last_q:pos_first_q))
suplim_predset_v_alp50=matrix(NA,number_of_predictions,length(pos_last_q:pos_first_q))
suplim_predset_a_alp50=matrix(NA,number_of_predictions,length(pos_last_q:pos_first_q))
}



# list for storing results: ogni elemento è un giorno che viene predetto(consecutivi, in questo caso particolare)
# "Day predicted": which day is being predicted
# "Price day to be predicted": the price in which gas is traded in that day
# "Quantity day to be predicted": the quantity of gas traded that day
# "Prediction": point prediction for the two curves (offer and demand)
# "PredSet_0_<%>_<inf/sup>_lim": the curve for lower/upper bound for the prediction region at 25/50%

for (h in 1:number_of_predictions){
  #h = 1
  pos_first_day_dataset= (start_dataset - 1) + h
  pos_last_day_dataset= (end_dataset - 1) + h 
  
  #!!!!!!!!!#
  pos_day_to_be_predicted=pos_last_day_dataset + 8 #il giorno che prevedo ? 8 giorni dopo di quando finiscono i dati che ho
  result_CONFORMAL_log[[h]]["Position day to be predicted"] = pos_day_to_be_predicted
  
  
  #######
  ### 2 --- Acquisisco i dati
  #######
  
  #---Input: step functions for offers and demands
  #---Output:
  # 1- MGS_red_data: list long as number of days in training and calibration. Every element of this list is a list that contains two vectors: p_cg_v (sell curves) e p_cg_a (curve for vending)
  # 2- red_day: vectors with days of MGS_red_data
  # 3- red_common_grid: vectors with discrete grid (abscissa values)
  # 4- new_MGS_red_data: same as MGS_red_data
  # 5- new_red_day: vectors with days of new_MGS_red_data
  
  
  #Out 1  
  #dataset for constructing the interval
  load(paste0(dir_w,"/Test_domain1D/RealWorld_data/utils/data/MGS_cg_260419_310120_data.Rdata"))
  MGS_red_data=lapply(pos_first_day_dataset:pos_last_day_dataset,function(x) NULL)
  for (i in pos_first_day_dataset:pos_last_day_dataset){
    MGS_red_data[[i-pos_first_day_dataset+1]]=list(log(MGS_cg_260419_310120_data$y_axis[[i]]$p_cg_v[pos_first_q:pos_last_q]),log(MGS_cg_260419_310120_data$y_axis[[i]]$p_cg_a[pos_first_q:pos_last_q]))
    names(MGS_red_data[[i-pos_first_day_dataset+1]])=c("p_cg_v","p_cg_a")
  }
  
  #data in the training set
  #Out 2
  red_day=do.call("c",lapply((MGS_cg_260419_310120_data$y_axis),function(x) x$day))[pos_first_day_dataset:pos_last_day_dataset]#;range(red_day)
  
  #abscissa
  #Out 3
  red_common_grid=MGS_cg_260419_310120_data$x_axis[pos_first_q:pos_last_q]#;range(red_common_grid)
  result_CONFORMAL_log[[h]]["Volumes_grid"] = list(grid=red_common_grid)
  
  #real curve
  #Out 4  
  new_MGS_red_data=list(list(log(MGS_cg_260419_310120_data$y_axis[[pos_day_to_be_predicted]]$p_cg_v[pos_first_q:pos_last_q]),log(MGS_cg_260419_310120_data$y_axis[[pos_day_to_be_predicted]]$p_cg_a[pos_first_q:pos_last_q])))
  names(new_MGS_red_data[[1]])=c("p_cg_v","p_cg_a")
  
  result_CONFORMAL_log[[h]]["new_MGS_red_data"] = new_MGS_red_data
  
  #day to be predicted
  #Out 5
  new_red_day=(MGS_cg_260419_310120_data$y_axis[[pos_day_to_be_predicted]]$day)
  result_CONFORMAL_log[[h]]["Day predicted"] = as.Date(new_red_day)
  result_CONFORMAL_log[[h]]["Day predicted"] = (MGS_cg_260419_310120_data$y_axis[[pos_day_to_be_predicted]]$day)
  
  #real prices and quantities traded
  #Out 6
  final_price_new_day=log(final_p_q_260419_310120_data$Prezzo[pos_day_to_be_predicted])
  final_quantity_new_day=final_p_q_260419_310120_data$Volumi[pos_day_to_be_predicted]
  
  result_CONFORMAL_log[[h]]["Price to be predicted"] = final_price_new_day
  result_CONFORMAL_log[[h]]["Quantity to be predicted"] = final_quantity_new_day
  
  #--check
  if (final_p_q_260419_310120_data$GiornoGas[pos_day_to_be_predicted]!=new_red_day) stop("Errore! I giorni non corrispondono")
  #--
  
  data_prices=NULL
  new_data_prices=NULL
  
  
  if(price_order!=0){ #only for price_order >1
    
    for (i in 1:price_order){  
      if( (start_dataset+h-2-i)<1) data_prices=cbind(data_prices,
                                                     c(rep(NA,length(MGS_red_data)-length(1:(start_dataset+h-2-i + length(MGS_red_data) -1))), log(final_p_q_260419_310120_data$Prezzo[1:(start_dataset+h-2-i + length(MGS_red_data) -1)]))
      ) else{
        data_prices=cbind(data_prices,log(final_p_q_260419_310120_data$Prezzo[(start_dataset+h-2-i):(start_dataset+h-2-i + length(MGS_red_data) -1)]))  }
      new_data_prices=c(new_data_prices,log(final_p_q_260419_310120_data$Prezzo[end_dataset+6+h-i])) #se voglio prevedere il giorno t+1, l'ultimo prezzo che ho ? al giorno t-1 (perch? quello del giorno t ? reso pubblico dopo le ore 10 che ? il termine ultimo per mettere un'offerta in una asta). Quindi inserir? a seconda del valore di price_order il prezzo finale del giorno t-1,t-2,...,t-price_order
    }
  }  
  rm(MGS_cg_260419_310120_data,i) 
  
  
  if(value_seed==0) realdata_seed=h
  if(value_seed!=0) realdata_seed=value_seed
  
  
  #######
  ### 3 --- Algorithm
  #######  
  
  
  #--- Point prediction
  
  point_prediction=point_prediction_conformal_TS_fstep_ppnaive_withpr_GS(data_y=MGS_red_data,
                                                                         l=l,
                                                                         b=1,
                                                                         seed_split=realdata_seed,
                                                                         AR_order_v=AR_order_v, 
                                                                         AR_order_a=AR_order_a, 
                                                                         common_grid=red_common_grid,
                                                                         inclusion_price=inclusion_price,
                                                                         price_order=price_order,
                                                                         data_prices=data_prices,
                                                                         new_data_prices=new_data_prices)
  
  vec_day_to_be_predicted[h]=new_red_day
  vec_final_price_new_day[h]=final_price_new_day
  vec_final_quantity_new_day[h]=final_quantity_new_day
  
  training_index[h,]=point_prediction$training
  calibration_included_index[h,]=point_prediction$obs_calibration_included
  
  #storing point predictions
  result_CONFORMAL_log[[h]]["Prediction"] = point_prediction$predicted_fun_n_plus_8
  prediction_CC_offer_log[[h]]  = point_prediction$predicted_fun_n_plus_8[[1]]$predicted_p_cg_v
  prediction_CC_demand_log[[h]] = point_prediction$predicted_fun_n_plus_8[[1]]$predicted_p_cg_a
  
  
  # constructing bands
  ####ALPHA 0.25
  #--- Obtaining k,s ...
  
  conformal_TS=functional_conformal_prediction_TS_fstep_ppnaive_GS(data_y=point_prediction$data_y,
                                                                   hat_y=point_prediction$predicted_fun,
                                                                   t_values=NULL,
                                                                   m=point_prediction$m,
                                                                   alpha=0.25,
                                                                   s_type=s_type,
                                                                   randomized=FALSE,
                                                                   seed_tau=FALSE,
                                                                   training=point_prediction$training,
                                                                   obs_calibration_included=point_prediction$obs_calibration_included)
  
  
  av_width_alp25[h]=conformal_TS$average_width
  widths_alp25[h,]=conformal_TS$widths
  ncs_alp25[h,]=conformal_TS$rho
  
  #--- Obtaining prediction sets
  
  prediction_set=computation_conformal_prediction_set_fstep_ppnaive(hat_y=point_prediction$predicted_fun_n_plus_8,
                                                                    observed_y=new_MGS_red_data,
                                                                    k_s=conformal_TS$k_s,
                                                                    s=conformal_TS$s,
                                                                    alpha=conformal_TS$alpha,
                                                                    randomized=conformal_TS$randomized,
                                                                    extremes_are_included=conformal_TS$extremes_are_included)
  
  
  curves_are_included_alp25[h]=prediction_set$inclusion[[1]]
  
  corrected_v=correction_monotonic_function(inflim = prediction_set$inf_lim[[1]]$predicted_p_cg_v,
                                            suplim = prediction_set$sup_lim[[1]]$predicted_p_cg_v,
                                            type   = "v")
  
  inflim_vendita_alp25=corrected_v[[1]]
  suplim_vendita_alp25=corrected_v[[2]]
  
  corrected_a=correction_monotonic_function(inflim = prediction_set$inf_lim[[1]]$predicted_p_cg_a,
                                            suplim = prediction_set$sup_lim[[1]]$predicted_p_cg_a,
                                            type   = "a")
  
  inflim_acquisto_alp25=corrected_a[[1]]
  suplim_acquisto_alp25=corrected_a[[2]]
  
  
  result_CONFORMAL_log[[h]]["PredSet_0_25_inf_lim"] <- list(list(sale = inflim_vendita_alp25, purchase = inflim_acquisto_alp25)) 
  result_CONFORMAL_log[[h]]["PredSet_0_25_sup_lim"] <- list(list(sale=suplim_vendita_alp25,purchase=suplim_acquisto_alp25) )
  
  
  
  
  rm(corrected_v,corrected_a)
  
  #monotonicity correction for bands
  
  inflim_mod_alp25=list(list(inflim_vendita_alp25,inflim_acquisto_alp25))
  suplim_mod_alp25=list(list(suplim_vendita_alp25,suplim_acquisto_alp25))
  
  point_belonging_after_correction=Map(function(w,t,r) Map(function(x,y,z) ((x>=y) & (x<=z)),w,t,r),new_MGS_red_data,inflim_mod_alp25,suplim_mod_alp25)
  curves_are_included_alp25_after_corr[h]=lapply(lapply(point_belonging_after_correction,unlist),all)[[1]]
  
  #lenght bands
  widths_alp25_after_corr[h,]=c(mean(suplim_vendita_alp25-inflim_vendita_alp25), mean(suplim_acquisto_alp25-inflim_acquisto_alp25))
  av_width_alp25_after_corr[h]=mean(widths_alp25_after_corr[h,])  
  
  #pred set
  inflim_predset_v_alp25[h,]=inflim_vendita_alp25
  inflim_predset_a_alp25[h,]=inflim_acquisto_alp25
  suplim_predset_v_alp25[h,]=suplim_vendita_alp25
  suplim_predset_a_alp25[h,]=suplim_acquisto_alp25  
  
  #CP bands
  counter=0
  
  inflim_sovrap_alp25=rep(NA,length(inflim_vendita_alp25))
  suplim_sovrap_alp25=rep(NA,length(suplim_vendita_alp25))
  
  for(i in 1:length(inflim_vendita_alp25)){
    
    if(inflim_vendita_alp25[i]<=inflim_acquisto_alp25[i] & inflim_acquisto_alp25[i]<=suplim_vendita_alp25[i] & suplim_vendita_alp25[i]<=suplim_acquisto_alp25[i]) {inflim_sovrap_alp25[i]=inflim_acquisto_alp25[i]; suplim_sovrap_alp25[i]=suplim_vendita_alp25[i];counter=counter+1}
    if(inflim_vendita_alp25[i]<=inflim_acquisto_alp25[i] & inflim_acquisto_alp25[i]<=suplim_acquisto_alp25[i] & suplim_acquisto_alp25[i]<=suplim_vendita_alp25[i]){inflim_sovrap_alp25[i]=inflim_acquisto_alp25[i]; suplim_sovrap_alp25[i]=suplim_acquisto_alp25[i];counter=counter+1}
    if(inflim_acquisto_alp25[i]<=inflim_vendita_alp25[i] & inflim_vendita_alp25[i]<=suplim_vendita_alp25[i] & suplim_vendita_alp25[i]<=suplim_acquisto_alp25[i]){inflim_sovrap_alp25[i]=inflim_vendita_alp25[i]; suplim_sovrap_alp25[i]=suplim_vendita_alp25[i];counter=counter+1}
    if(inflim_acquisto_alp25[i]<=inflim_vendita_alp25[i] & inflim_vendita_alp25[i]<=suplim_acquisto_alp25[i] & suplim_acquisto_alp25[i]<=suplim_vendita_alp25[i]){inflim_sovrap_alp25[i]=inflim_vendita_alp25[i]; suplim_sovrap_alp25[i]=suplim_acquisto_alp25[i];counter=counter+1}  
  }
  
  
  if (final_quantity_new_day %in% red_common_grid){
    final_pq_is_included_alp25[h]= final_price_new_day >= inflim_sovrap_alp25[red_common_grid==final_quantity_new_day] & final_price_new_day <= suplim_sovrap_alp25[red_common_grid==final_quantity_new_day]
  } else {
    final_pq_is_included_alp25[h]= final_price_new_day >= inflim_sovrap_alp25[ which.min(abs(red_common_grid-final_quantity_new_day))] & final_price_new_day <= suplim_sovrap_alp25[ which.min(abs(red_common_grid-final_quantity_new_day))]
    
  }
  
  
  ####ALPHA 0.50
  #--- Obtaining k,s ...
  
  conformal_TS=functional_conformal_prediction_TS_fstep_ppnaive_GS(data_y=point_prediction$data_y,
                                                                   hat_y=point_prediction$predicted_fun,
                                                                   t_values=NULL,
                                                                   m=point_prediction$m,
                                                                   alpha=0.50,
                                                                   s_type=s_type,
                                                                   randomized=FALSE,
                                                                   seed_tau=FALSE,
                                                                   training=point_prediction$training,
                                                                   obs_calibration_included=point_prediction$obs_calibration_included)
  
  
  av_width_alp50[h]=conformal_TS$average_width
  widths_alp50[h,]=conformal_TS$widths
  ncs_alp50[h,]=conformal_TS$rho
  
  #--- Obtaining prediction sets
  
  prediction_set=computation_conformal_prediction_set_fstep_ppnaive(hat_y=point_prediction$predicted_fun_n_plus_8,
                                                                    observed_y=new_MGS_red_data,
                                                                    k_s=conformal_TS$k_s,
                                                                    s=conformal_TS$s,
                                                                    alpha=conformal_TS$alpha,
                                                                    randomized=conformal_TS$randomized,
                                                                    extremes_are_included=conformal_TS$extremes_are_included)
  
  
  
  curves_are_included_alp50[h]=prediction_set$inclusion[[1]]
  
  corrected_v=correction_monotonic_function(inflim = prediction_set$inf_lim[[1]]$predicted_p_cg_v,
                                            suplim = prediction_set$sup_lim[[1]]$predicted_p_cg_v,
                                            type   = "v")
  
  inflim_vendita_alp50=corrected_v[[1]]
  suplim_vendita_alp50=corrected_v[[2]]
  
  corrected_a=correction_monotonic_function(inflim = prediction_set$inf_lim[[1]]$predicted_p_cg_a,
                                            suplim = prediction_set$sup_lim[[1]]$predicted_p_cg_a,
                                            type   = "a")
  
  inflim_acquisto_alp50=corrected_a[[1]]
  suplim_acquisto_alp50=corrected_a[[2]]
  
  result_CONFORMAL_log[[h]]["PredSet_0_50_inf_lim"] <- list(list(sale = inflim_vendita_alp50, purchase = inflim_acquisto_alp50)) 
  result_CONFORMAL_log[[h]]["PredSet_0_50_sup_lim"] <- list(list(sale=suplim_vendita_alp50,purchase=suplim_acquisto_alp50) )
  
  
  
  rm(corrected_v,corrected_a)
  
  inflim_mod_alp50=list(list(inflim_vendita_alp50,inflim_acquisto_alp50))
  suplim_mod_alp50=list(list(suplim_vendita_alp50,suplim_acquisto_alp50))
  
  point_belonging_after_correction=Map(function(w,t,r) Map(function(x,y,z) ((x>=y) & (x<=z)),w,t,r),new_MGS_red_data,inflim_mod_alp50,suplim_mod_alp50)
  curves_are_included_alp50_after_corr[h]=lapply(lapply(point_belonging_after_correction,unlist),all)[[1]]
  
  widths_alp50_after_corr[h,]=c(mean(suplim_vendita_alp50-inflim_vendita_alp50), mean(suplim_acquisto_alp50-inflim_acquisto_alp50))
  av_width_alp50_after_corr[h]=mean(widths_alp50_after_corr[h,])  
  
  inflim_predset_v_alp50[h,]=inflim_vendita_alp50
  inflim_predset_a_alp50[h,]=inflim_acquisto_alp50
  suplim_predset_v_alp50[h,]=suplim_vendita_alp50
  suplim_predset_a_alp50[h,]=suplim_acquisto_alp50    
  
  counter=0
  
  inflim_sovrap_alp50=rep(NA,length(inflim_vendita_alp50))
  suplim_sovrap_alp50=rep(NA,length(suplim_vendita_alp50))
  
  for(i in 1:length(inflim_vendita_alp50)){
    
    if(inflim_vendita_alp50[i]<=inflim_acquisto_alp50[i] & inflim_acquisto_alp50[i]<=suplim_vendita_alp50[i] & suplim_vendita_alp50[i]<=suplim_acquisto_alp50[i]) {inflim_sovrap_alp50[i]=inflim_acquisto_alp50[i]; suplim_sovrap_alp50[i]=suplim_vendita_alp50[i];counter=counter+1}
    if(inflim_vendita_alp50[i]<=inflim_acquisto_alp50[i] & inflim_acquisto_alp50[i]<=suplim_acquisto_alp50[i] & suplim_acquisto_alp50[i]<=suplim_vendita_alp50[i]){inflim_sovrap_alp50[i]=inflim_acquisto_alp50[i]; suplim_sovrap_alp50[i]=suplim_acquisto_alp50[i];counter=counter+1}
    if(inflim_acquisto_alp50[i]<=inflim_vendita_alp50[i] & inflim_vendita_alp50[i]<=suplim_vendita_alp50[i] & suplim_vendita_alp50[i]<=suplim_acquisto_alp50[i]){inflim_sovrap_alp50[i]=inflim_vendita_alp50[i]; suplim_sovrap_alp50[i]=suplim_vendita_alp50[i];counter=counter+1}
    if(inflim_acquisto_alp50[i]<=inflim_vendita_alp50[i] & inflim_vendita_alp50[i]<=suplim_acquisto_alp50[i] & suplim_acquisto_alp50[i]<=suplim_vendita_alp50[i]){inflim_sovrap_alp50[i]=inflim_vendita_alp50[i]; suplim_sovrap_alp50[i]=suplim_acquisto_alp50[i];counter=counter+1}  
  }
  
  if (final_quantity_new_day %in% red_common_grid){
    final_pq_is_included_alp50[h]= final_price_new_day >= inflim_sovrap_alp50[red_common_grid==final_quantity_new_day] & final_price_new_day <= suplim_sovrap_alp50[red_common_grid==final_quantity_new_day]
  } else {
    final_pq_is_included_alp50[h]= final_price_new_day >= inflim_sovrap_alp50[ which.min(abs(red_common_grid-final_quantity_new_day))] & final_price_new_day <= suplim_sovrap_alp50[ which.min(abs(red_common_grid-final_quantity_new_day))]
    
  }
  
  result_CONFORMAL_log[[h]]["Sovrap_0_25"] <- list(list(inflim = inflim_sovrap_alp25, suplim = suplim_sovrap_alp25)) 
  result_CONFORMAL_log[[h]]["Sovrap_0_50"] <- list(list(inflim = inflim_sovrap_alp50, suplim = suplim_sovrap_alp50) )
  
}


#save results
if(save_res){
  file_pred_offers = paste0(path_stor_res,"/prediction_CC_offer_log.Rdata")
  save(prediction_CC_offer_log, file = file_pred_offers)
  
  file_pred_demands = paste0(path_stor_res,"/prediction_CC_demand_log.Rdata")
  save(prediction_CC_demand_log, file = file_pred_demands)
  
  file_conformal = paste0(path_stor_res,"/CP_CC_log.Rdata")
  save(result_CONFORMAL_log, file = file_conformal)
}
