rm(list=ls())
graphics.off()
cat("\014")

set.seed(23032000)

#change here
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series"

file_saving_offers = paste0(dir_w,"/Test_domain1D/RealWorld_data/predictions_offers_KO.RData")
load(file_saving_offers)
file_saving_demands = paste0(dir_w,"/Test_domain1D/RealWorld_data/predictions_demands_KO.RData")
load(predictions_demands_KO)

############
# 1 --- Parametri che identificano primo giorno del training, m, l.
############

start_dataset=1 #giorno di inizio del dataset..1 ? "2019-04-26"
value_seed=0
end_dataset=90 #giorno di fine del dataset..90 ? "2019-07-24"
l=39
last_prediction=281 #ultimo giorno che prevedo... ? 2020-01-31


############
# 2 --- Parametri che terrei fissi per il momento
############

pos_first_q=1 # inizio dell'asse delle x.. 1 ? q=0
pos_last_q=401 # fine dell'asse delle x..451 ? q=200mila

b=1 #? il parametro di Chernozukov
s_type="st-dev"

##PARAMETRI CHE FACCIO GIRARE
AR_order_v=1
AR_order_a=1
inclusion_price=TRUE
price_order=1


#########################################
## nella finestra q=[0,200.000] sempre le funzioni terminano dopo (insomma non devo inventare nessun valore) e nella
## finestra temporale considerata DI PREVISIONE ho che sempre l'incrocio tra le due curve avviene in [0,200.000] [al massimo 116.733,7]
#########################################

############
# 2 --- Risultati
############

#tempistiche: se number_of_predictions=68 circa 10-11 minuti

number_of_predictions=length((end_dataset+8):last_prediction)
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



source(paste0(dir_w,"/Test_domain1D/RealWorld_data/fun_GS_mod.R"))
source(paste0(dir_w,"/Test_domain1D/RealWorld_data/fun_corr_monot.R"))




#lista per storare i risultati: ogni elemento è un giorno che viene predetto(consecutivi, in questo caso particolare)
# "Day predicted": which day is being predicted
# "Price day to be predicted": the price in which gas is traded in that day
# "Quantity day to be predicted": the quantity of gas traded that day
# "Prediction": point prediction for the two curves (offer and demand)
# "PredSet_0_<%>_<inf/sup>_lim": the curve for lower/upper bound for the prediction region at 25/50%
result_CONFORMAL=lapply((end_dataset+8):last_prediction,function(x) NULL)

for (h in 1:number_of_predictions){
  #h = 1
  pos_first_day_dataset= (start_dataset - 1) + h
  pos_last_day_dataset= (end_dataset - 1) + h 
  
  #!!!!!!!!!#
  pos_day_to_be_predicted=pos_last_day_dataset + 8 #il giorno che prevedo ? 8 giorni dopo di quando finiscono i dati che ho
  result_CONFORMAL[[h]]["Position day to be predicted"] = pos_day_to_be_predicted
  
  
  #######
  ### 2 --- Acquisisco i dati
  #######
  
  #---Input: Le funzioni a step valutate sulla common grid (ossia il file "MGS_cg_260419_310120_data.Rdata")
  #---Output:
  # 1- MGS_red_data: lista lunga quanto il numero di giorni considerati di training e calibration. Ogni elemento di questa lista ? a sua volta una lista contenente 2 vettori: p_cg_v (contenente la curva di vendita di quel giorno e valutata sulla griglia scelta) e p_cg_a (stessa cosa ma per curva acquisto)
  # 2- red_day: vettore contenente i giorni dell'anno a cui si riferiscono i dati contenuti in MGS_red_data
  # 3- red_common_grid: vettore contenente la griglia comune su cui i dati contenuti in MGS_red_data sono valutati
  # 4- new_MGS_red_data: lista lunga quanto il numero di giorni che voglio prevedere (tengo solamente uno per il momento). Ogni elemento di questa lista ? a sua volta una lista contenente 2 vettori, come su MGS_red_data
  # 5- new_red_day: vettore contenente i giorni dell'anno a cui si riferiscono i dati contenuti in new_MGS_red_data
  
  
  load(paste0(dir_w,"/Test_domain1D/RealWorld_data/MGS_cg_260419_310120_data.Rdata"))  
  load(paste0(dir_w,"/Test_domain1D/RealWorld_data/final_p_q_260419_310120_data.Rdata"))
  
  #Out 1  
  #preparando il dataset che uso per costruire l'intervallo
  MGS_red_data=lapply(pos_first_day_dataset:pos_last_day_dataset,function(x) NULL)
  for (i in pos_first_day_dataset:pos_last_day_dataset){
    MGS_red_data[[i-pos_first_day_dataset+1]]=list(MGS_cg_260419_310120_data$y_axis[[i]]$p_cg_v[pos_first_q:pos_last_q],MGS_cg_260419_310120_data$y_axis[[i]]$p_cg_a[pos_first_q:pos_last_q])
    names(MGS_red_data[[i-pos_first_day_dataset+1]])=c("p_cg_v","p_cg_a")
  }
  
  #dati nel training set
  #Out 2
  red_day=do.call("c",lapply(MGS_cg_260419_310120_data$y_axis,function(x) x$day))[pos_first_day_dataset:pos_last_day_dataset]#;range(red_day)
  
  #ascisse dei dati funzionali
  #Out 3
  red_common_grid=MGS_cg_260419_310120_data$x_axis[pos_first_q:pos_last_q]#;range(red_common_grid)
  result_CONFORMAL[[h]]["Volumes_grid"] = list(grid=red_common_grid)
  
  #dato che voglio confrontare con la mia predizione (vero dato)
  #Out 4  
  new_MGS_red_data=list(list(MGS_cg_260419_310120_data$y_axis[[pos_day_to_be_predicted]]$p_cg_v[pos_first_q:pos_last_q],MGS_cg_260419_310120_data$y_axis[[pos_day_to_be_predicted]]$p_cg_a[pos_first_q:pos_last_q]))
  names(new_MGS_red_data[[1]])=c("p_cg_v","p_cg_a")
  
  result_CONFORMAL[[h]]["new_MGS_red_data"] = new_MGS_red_data
  
  #giorno che voglio predire
  #Out 5
  new_red_day=MGS_cg_260419_310120_data$y_axis[[pos_day_to_be_predicted]]$day
  result_CONFORMAL[[h]]["Day predicted"] = as.Date(new_red_day)
  result_CONFORMAL[[h]]["Day predicted"] = MGS_cg_260419_310120_data$y_axis[[pos_day_to_be_predicted]]$day
  
  #prezzi veri nel giorno che voglio predire
  #Out 6
  final_price_new_day=final_p_q_260419_310120_data$Prezzo[pos_day_to_be_predicted]
  final_quantity_new_day=final_p_q_260419_310120_data$Volumi[pos_day_to_be_predicted]
  
  result_CONFORMAL[[h]]["Price to be predicted"] = final_price_new_day
  result_CONFORMAL[[h]]["Quantity to be predicted"] = final_quantity_new_day
  
  #--check
  if (final_p_q_260419_310120_data$GiornoGas[pos_day_to_be_predicted]!=new_red_day) stop("Errore! I giorni non corrispondono")
  #--
  
  data_prices=NULL
  new_data_prices=NULL
  
  
  if(price_order!=0){ #NON SO SE VADA BENE CON price_order >1
    
    for (i in 1:price_order){  
      if( (start_dataset+h-2-i)<1) data_prices=cbind(data_prices,
                                                     c(rep(NA,length(MGS_red_data)-length(1:(start_dataset+h-2-i + length(MGS_red_data) -1))), final_p_q_260419_310120_data$Prezzo[1:(start_dataset+h-2-i + length(MGS_red_data) -1)])
      ) else{
        data_prices=cbind(data_prices,final_p_q_260419_310120_data$Prezzo[(start_dataset+h-2-i):(start_dataset+h-2-i + length(MGS_red_data) -1)])  }
      new_data_prices=c(new_data_prices,final_p_q_260419_310120_data$Prezzo[end_dataset+6+h-i]) #se voglio prevedere il giorno t+1, l'ultimo prezzo che ho ? al giorno t-1 (perch? quello del giorno t ? reso pubblico dopo le ore 10 che ? il termine ultimo per mettere un'offerta in una asta). Quindi inserir? a seconda del valore di price_order il prezzo finale del giorno t-1,t-2,...,t-price_order
    }
  }  
  rm(MGS_cg_260419_310120_data,i) 
  
  
  if(value_seed==0) realdata_seed=h
  if(value_seed!=0) realdata_seed=value_seed
  
  
  
  
  #######
  ### 3 --- Algoritmo
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
  
  result_CONFORMAL[[h]]["Prediction"] = point_prediction$predicted_fun_n_plus_8
  
  
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
  
  
  
  #result_CONFORMAL[[h]]["PredSet_0_25_inf_lim"] = prediction_set$inf_lim
  #result_CONFORMAL[[h]]["PredSet_0_25_sup_lim"] = prediction_set$sup_lim
  
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
  
  
  result_CONFORMAL[[h]]["PredSet_0_25_inf_lim"] <- list(list(sale = inflim_vendita_alp25, purchase = inflim_acquisto_alp25)) 
  result_CONFORMAL[[h]]["PredSet_0_25_sup_lim"] <- list(list(sale=suplim_vendita_alp25,purchase=suplim_acquisto_alp25) )
  
  
  
  
  rm(corrected_v,corrected_a)
  
  #Calcolo l'inclusione delle curve dopo la "correction_monotonic_function". DEVE VENIRE UGUALE A  curves_are_included_alp25 SECONDO ME
  
  inflim_mod_alp25=list(list(inflim_vendita_alp25,inflim_acquisto_alp25))
  suplim_mod_alp25=list(list(suplim_vendita_alp25,suplim_acquisto_alp25))
  
  point_belonging_after_correction=Map(function(w,t,r) Map(function(x,y,z) ((x>=y) & (x<=z)),w,t,r),new_MGS_red_data,inflim_mod_alp25,suplim_mod_alp25)
  curves_are_included_alp25_after_corr[h]=lapply(lapply(point_belonging_after_correction,unlist),all)[[1]]
  
  #Calcolo ampiezze pred. set
  
  
  widths_alp25_after_corr[h,]=c(mean(suplim_vendita_alp25-inflim_vendita_alp25), mean(suplim_acquisto_alp25-inflim_acquisto_alp25))
  av_width_alp25_after_corr[h]=mean(widths_alp25_after_corr[h,])  
  
  #Salvo i pred set
  
  inflim_predset_v_alp25[h,]=inflim_vendita_alp25
  inflim_predset_a_alp25[h,]=inflim_acquisto_alp25
  suplim_predset_v_alp25[h,]=suplim_vendita_alp25
  suplim_predset_a_alp25[h,]=suplim_acquisto_alp25  
  
  #Calcolo
  
  #  inflim_vendita_alp25=prediction_set$inf_lim[[1]]$predicted_p_cg_v
  #  inflim_acquisto_alp25=prediction_set$inf_lim[[1]]$predicted_p_cg_a
  
  #  suplim_vendita_alp25=prediction_set$sup_lim[[1]]$predicted_p_cg_v
  #  suplim_acquisto_alp25=prediction_set$sup_lim[[1]]$predicted_p_cg_a
  
  
  counter=0
  
  inflim_sovrap_alp25=rep(NA,length(inflim_vendita_alp25))
  suplim_sovrap_alp25=rep(NA,length(suplim_vendita_alp25))
  
  for(i in 1:length(inflim_vendita_alp25)){
    
    if(inflim_vendita_alp25[i]<=inflim_acquisto_alp25[i] & inflim_acquisto_alp25[i]<=suplim_vendita_alp25[i] & suplim_vendita_alp25[i]<=suplim_acquisto_alp25[i]) {inflim_sovrap_alp25[i]=inflim_acquisto_alp25[i]; suplim_sovrap_alp25[i]=suplim_vendita_alp25[i];counter=counter+1}
    if(inflim_vendita_alp25[i]<=inflim_acquisto_alp25[i] & inflim_acquisto_alp25[i]<=suplim_acquisto_alp25[i] & suplim_acquisto_alp25[i]<=suplim_vendita_alp25[i]){inflim_sovrap_alp25[i]=inflim_acquisto_alp25[i]; suplim_sovrap_alp25[i]=suplim_acquisto_alp25[i];counter=counter+1}
    if(inflim_acquisto_alp25[i]<=inflim_vendita_alp25[i] & inflim_vendita_alp25[i]<=suplim_vendita_alp25[i] & suplim_vendita_alp25[i]<=suplim_acquisto_alp25[i]){inflim_sovrap_alp25[i]=inflim_vendita_alp25[i]; suplim_sovrap_alp25[i]=suplim_vendita_alp25[i];counter=counter+1}
    if(inflim_acquisto_alp25[i]<=inflim_vendita_alp25[i] & inflim_vendita_alp25[i]<=suplim_acquisto_alp25[i] & suplim_acquisto_alp25[i]<=suplim_vendita_alp25[i]){inflim_sovrap_alp25[i]=inflim_vendita_alp25[i]; suplim_sovrap_alp25[i]=suplim_acquisto_alp25[i];counter=counter+1}  
  }
  
  
  #potevo prendere direttamente l'else perch? il primo if ? incluso nel secondo diciamo, ma tengo cosi intanto metti che voglio fare il codice piu generico
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
  
  result_CONFORMAL[[h]]["PredSet_0_50_inf_lim"] <- list(list(sale = inflim_vendita_alp50, purchase = inflim_acquisto_alp50)) 
  result_CONFORMAL[[h]]["PredSet_0_50_sup_lim"] <- list(list(sale=suplim_vendita_alp50,purchase=suplim_acquisto_alp50) )
  
  
  
  rm(corrected_v,corrected_a)
  
  #Calcolo l'inclusione delle curve dopo la "correction_monotonic_function". DEVE VENIRE UGUALE A  curves_are_included_alp25 SECONDO ME
  
  inflim_mod_alp50=list(list(inflim_vendita_alp50,inflim_acquisto_alp50))
  suplim_mod_alp50=list(list(suplim_vendita_alp50,suplim_acquisto_alp50))
  
  point_belonging_after_correction=Map(function(w,t,r) Map(function(x,y,z) ((x>=y) & (x<=z)),w,t,r),new_MGS_red_data,inflim_mod_alp50,suplim_mod_alp50)
  curves_are_included_alp50_after_corr[h]=lapply(lapply(point_belonging_after_correction,unlist),all)[[1]]
  
  #Calcolo ampiezze pred. set
  
  
  widths_alp50_after_corr[h,]=c(mean(suplim_vendita_alp50-inflim_vendita_alp50), mean(suplim_acquisto_alp50-inflim_acquisto_alp50))
  av_width_alp50_after_corr[h]=mean(widths_alp50_after_corr[h,])  
  
  #Salvo i pred set
  
  inflim_predset_v_alp50[h,]=inflim_vendita_alp50
  inflim_predset_a_alp50[h,]=inflim_acquisto_alp50
  suplim_predset_v_alp50[h,]=suplim_vendita_alp50
  suplim_predset_a_alp50[h,]=suplim_acquisto_alp50    
  
  #Calcolo
  
  #  inflim_vendita_alp50=prediction_set$inf_lim[[1]]$predicted_p_cg_v
  #  inflim_acquisto_alp50=prediction_set$inf_lim[[1]]$predicted_p_cg_a
  
  #  suplim_vendita_alp50=prediction_set$sup_lim[[1]]$predicted_p_cg_v
  #  suplim_acquisto_alp50=prediction_set$sup_lim[[1]]$predicted_p_cg_a
  
  
  counter=0
  
  inflim_sovrap_alp50=rep(NA,length(inflim_vendita_alp50))
  suplim_sovrap_alp50=rep(NA,length(suplim_vendita_alp50))
  
  for(i in 1:length(inflim_vendita_alp50)){
    
    if(inflim_vendita_alp50[i]<=inflim_acquisto_alp50[i] & inflim_acquisto_alp50[i]<=suplim_vendita_alp50[i] & suplim_vendita_alp50[i]<=suplim_acquisto_alp50[i]) {inflim_sovrap_alp50[i]=inflim_acquisto_alp50[i]; suplim_sovrap_alp50[i]=suplim_vendita_alp50[i];counter=counter+1}
    if(inflim_vendita_alp50[i]<=inflim_acquisto_alp50[i] & inflim_acquisto_alp50[i]<=suplim_acquisto_alp50[i] & suplim_acquisto_alp50[i]<=suplim_vendita_alp50[i]){inflim_sovrap_alp50[i]=inflim_acquisto_alp50[i]; suplim_sovrap_alp50[i]=suplim_acquisto_alp50[i];counter=counter+1}
    if(inflim_acquisto_alp50[i]<=inflim_vendita_alp50[i] & inflim_vendita_alp50[i]<=suplim_vendita_alp50[i] & suplim_vendita_alp50[i]<=suplim_acquisto_alp50[i]){inflim_sovrap_alp50[i]=inflim_vendita_alp50[i]; suplim_sovrap_alp50[i]=suplim_vendita_alp50[i];counter=counter+1}
    if(inflim_acquisto_alp50[i]<=inflim_vendita_alp50[i] & inflim_vendita_alp50[i]<=suplim_acquisto_alp50[i] & suplim_acquisto_alp50[i]<=suplim_vendita_alp50[i]){inflim_sovrap_alp50[i]=inflim_vendita_alp50[i]; suplim_sovrap_alp50[i]=suplim_acquisto_alp50[i];counter=counter+1}  
  }
  
  
  #potevo prendere direttamente l'else perch? il primo if ? incluso nel secondo diciamo, ma tengo cosi intanto metti che voglio fare il codice piu generico
  if (final_quantity_new_day %in% red_common_grid){
    final_pq_is_included_alp50[h]= final_price_new_day >= inflim_sovrap_alp50[red_common_grid==final_quantity_new_day] & final_price_new_day <= suplim_sovrap_alp50[red_common_grid==final_quantity_new_day]
  } else {
    final_pq_is_included_alp50[h]= final_price_new_day >= inflim_sovrap_alp50[ which.min(abs(red_common_grid-final_quantity_new_day))] & final_price_new_day <= suplim_sovrap_alp50[ which.min(abs(red_common_grid-final_quantity_new_day))]
    
  }
  
  result_CONFORMAL[[h]]["Sovrap_0_25"] <- list(list(inflim = inflim_sovrap_alp25, suplim = suplim_sovrap_alp25)) 
  result_CONFORMAL[[h]]["Sovrap_0_50"] <- list(list(inflim = inflim_sovrap_alp50, suplim = suplim_sovrap_alp50) )
  
  
  
  #library("data.table")
  #library(ggplot2)
  #library(gridExtra)
  
  #output1=rbind(point_prediction$predicted_fun_n_plus_8[[1]]$predicted_p_cg_v,inflim_vendita_alp25,suplim_vendita_alp25,inflim_vendita_alp50,suplim_vendita_alp50,new_MGS_red_data[[1]]$p_cg_v)
  #output2=rbind(point_prediction$predicted_fun_n_plus_8[[1]]$predicted_p_cg_a,inflim_acquisto_alp25,suplim_acquisto_alp25,inflim_acquisto_alp50,suplim_acquisto_alp50,new_MGS_red_data[[1]]$p_cg_a)
  #output3=rbind(inflim_sovrap_alp25,suplim_sovrap_alp25,inflim_sovrap_alp50,suplim_sovrap_alp50)
  
  #rownames(output1)=c('lvl','lwr_alp25','upr_alp25','lwr_alp50','upr_alp50','obs')
  #rownames(output2)=c('lvl','lwr_alp25','upr_alp25','lwr_alp50','upr_alp50','obs')
  #rownames(output3)=c('lwr_alp25','upr_alp25','lwr_alp50','upr_alp50')
  
  
  #output1=data.table(t(output1))
  #output1$x_grid=red_common_grid
  
  #output2=data.table(t(output2))
  #output2$x_grid=red_common_grid
  
  #output3=data.table(t(output3))
  #output3$x_grid=red_common_grid
  
  #alpha_plot=.2
  #plot1 = ggplot() +
    
    #    geom_ribbon(data = output1, aes(x = x_grid, ymin=lwr_alp10, ymax=upr_alp10),alpha=alpha_plot,fill="#C66454",col="#C66454") + 
    #    geom_ribbon(data = output1, aes(x = x_grid, ymin=lwr_alp25, ymax=upr_alp25),alpha=alpha_plot,fill="#763C32",col="#763C32") + 
    #    geom_ribbon(data = output1, aes(x = x_grid, ymin=lwr_alp50, ymax=upr_alp50),alpha=alpha_plot,fill="#271410",col="#271410") + 
  #  geom_line(data = output1, aes(x = x_grid, y=lwr_alp25),alpha=0.7,col="#e32c0e",size=1,linetype="longdash") +#fill="#11242E",col="#11242E") + 
   # geom_line(data = output1, aes(x = x_grid, y=upr_alp25),alpha=0.7,col="#e32c0e",size=1,linetype="longdash") +#fill="#11242E",col="#11242E") + 
    #geom_line(data = output1, aes(x = x_grid, y=lwr_alp50),alpha=alpha_plot,col="#a66a60",size=1,linetype="longdash") +#fill="#11242E",col="#11242E") + 
 #   geom_line(data = output1, aes(x = x_grid, y=upr_alp50),alpha=alpha_plot,col="#a66a60",size=1,linetype="longdash") +#fill="#11242E",col="#11242E") + 
  #  geom_line(data = output1, aes(x = x_grid, y = lvl),col="#C66454",size=0.7,linetype = "dashed") +
   # geom_line(data = output1, aes(x = x_grid, y = obs),col="#C66454",size=1) +
    #annotate("point", x = final_quantity_new_day, y = final_price_new_day, colour = "#030101",shape=4,size=6) +
#    theme_bw() +
 #   labs(x='q',y='p(q)') +
  #  ggtitle(paste("C. Vend - Day ", pos_day_to_be_predicted, " - ", new_red_day))
  
 # plot2 = ggplot() +
    
    #    geom_ribbon(data = output2, aes(x = x_grid, ymin=lwr_alp10, ymax=upr_alp10),alpha=alpha_plot,fill="#56B4E9",col="#56B4E9") + 
    #    geom_ribbon(data = output2, aes(x = x_grid, ymin=lwr_alp25, ymax=upr_alp25),alpha=alpha_plot,fill="#336C8B",col="#336C8B") + 
    #      geom_ribbon(data = output2, aes(x = x_grid, ymin=lwr_alp50, ymax=upr_alp50),alpha=alpha_plot,col="#11242E") +#fill="#11242E",col="#11242E") +
  #  geom_line(data = output2, aes(x = x_grid, y=lwr_alp25),alpha=0.7,col="#566269",size=1,linetype="longdash") +#fill="#11242E",col="#11242E") + 
   # geom_line(data = output2, aes(x = x_grid, y=upr_alp25),alpha=0.7,col="#566269",size=1,linetype="longdash") +#fill="#11242E",col="#11242E") + 
#    geom_line(data = output2, aes(x = x_grid, y=lwr_alp50),alpha=alpha_plot,col="#2d05f7",size=1,linetype="longdash") +#fill="#11242E",col="#11242E") + 
 #   geom_line(data = output2, aes(x = x_grid, y=upr_alp50),alpha=alpha_plot,col="#2d05f7",size=1,linetype="longdash") +#fill="#11242E",col="#11242E") + 
#    geom_line(data = output2, aes(x = x_grid, y = obs),col="#56B4E9",size=1) +
#    geom_line(data = output2, aes(x = x_grid, y = lvl),col="#56B4E9",size=0.7,linetype = "dashed") +
 #   annotate("point", x = final_quantity_new_day, y = final_price_new_day, colour = "#030101",shape=4,size=6) +
#    theme_bw() +
 #   labs(x='q',y='p(q)') +
  #  ggtitle(paste("C. Acq - Day ", pos_day_to_be_predicted, " - ", new_red_day))
  #      ylim(-80,80) +
  
 # plot3 = ggplot() +
    
  #  geom_ribbon(data = output3, aes(x = x_grid, ymin=lwr_alp25, ymax=upr_alp25),alpha=alpha_plot,fill="#336C8B",col="#336C8B") + 
   # geom_ribbon(data = output3, aes(x = x_grid, ymin=lwr_alp50, ymax=upr_alp50),alpha=alpha_plot,col="#11242E") +#fill="#11242E",col="#11242E") +
#    annotate("point", x = final_quantity_new_day, y = final_price_new_day, colour = "#030101",shape=4,size=6) +
 #   theme_bw() +
  #  labs(x='q',y='p(q)') +
  #  ggtitle(paste("Sovrap C. - Day" , pos_day_to_be_predicted, " - ", new_red_day))
  #      ylim(-80,80) +
  
  
  
 # png(paste("g_p1_",pos_day_to_be_predicted,"_uff_after_corr.png",sep=""))
#  print(grid.arrange(plot1, plot2, plot3, ncol=2))
 # dev.off()
  
  
  
}

file_saving_res_conf = paste0(dir_w,"/Test_domain1D/RealWorld_data/result_CONFORMAL.RData")
save(result_CONFORMAL, file = file_saving_res_conf)


#KO predictions
file_saving_offers = paste0(dir_w,"/Test_domain1D/RealWorld_data/predictions_offers_KO.RData")
load(file_saving_offers)
file_saving_demands = paste0(dir_w,"/Test_domain1D/RealWorld_data/predictions_demands_KO.RData")
load(file_saving_demands)


library("data.table")
library(ggplot2)
library(gridExtra)
library(patchwork)

#############################
##saving offers predictions##
#############################
for (h in 1:number_of_predictions){
  output1=rbind( result_CONFORMAL[[h]]$Prediction$predicted_p_cg_v, result_CONFORMAL[[h]]$PredSet_0_25_inf_lim$sale, result_CONFORMAL[[h]]$PredSet_0_25_sup_lim$sale, result_CONFORMAL[[h]]$PredSet_0_50_inf_lim$sale, result_CONFORMAL[[h]]$PredSet_0_50_sup_lim$sale, result_CONFORMAL[[h]]$new_MGS_red_data$p_cg_v)
  KO_offers = data.frame(predictions_offers_KO[[h]])
  
  rownames(output1)=c('lvl','lwr_alp25','upr_alp25','lwr_alp50','upr_alp50','obs')
  
  output1=data.table(t(output1))
  output1$x_grid=result_CONFORMAL[[h]]$Volumes_grid
  
  KO_offers$x_grid = result_CONFORMAL[[h]]$Volumes_grid
  colnames(KO_offers) = c("KO_pred_offer","x_grid")
  
  n_tot = length(result_CONFORMAL[[h]]$Volumes_grid)
  mse_pred_CP = sum((output1$lvl - output1$obs)^2)/n_tot
  mse_pred_KO = sum((KO_offers$KO_pred_offer - output1$obs)^2)/n_tot
  
  
  alpha_plot=.2
  plot1 = ggplot() +
    
    #    geom_ribbon(data = output1, aes(x = x_grid, ymin=lwr_alp10, ymax=upr_alp10),alpha=alpha_plot,fill="#C66454",col="#C66454") + 
    #    geom_ribbon(data = output1, aes(x = x_grid, ymin=lwr_alp25, ymax=upr_alp25),alpha=alpha_plot,fill="#763C32",col="#763C32") + 
    #    geom_ribbon(data = output1, aes(x = x_grid, ymin=lwr_alp50, ymax=upr_alp50),alpha=alpha_plot,fill="#271410",col="#271410") + 
    geom_line(data = output1, aes(x = x_grid, y=lwr_alp25),alpha=0.7,col="#e32c0e",size=1,linetype="longdash") +#fill="#11242E",col="#11242E") + 
    geom_line(data = output1, aes(x = x_grid, y=upr_alp25),alpha=0.7,col="#e32c0e",size=1,linetype="longdash") +#fill="#11242E",col="#11242E") + 
    geom_line(data = KO_offers, aes(x = x_grid, y = KO_pred_offer), col="black") +
    geom_line(data = output1, aes(x = x_grid, y=lwr_alp50),alpha=alpha_plot,col="#a66a60",size=1,linetype="longdash") +#fill="#11242E",col="#11242E") + 
    geom_line(data = output1, aes(x = x_grid, y=upr_alp50),alpha=alpha_plot,col="#a66a60",size=1,linetype="longdash") +#fill="#11242E",col="#11242E") + 
    geom_line(data = output1, aes(x = x_grid, y = lvl),col="#C66454",size=0.7,linetype = "dashed") +
    geom_line(data = output1, aes(x = x_grid, y = obs),col="#C66454",size=1) +
    annotate("point", x = result_CONFORMAL[[h]]$`Quantity day to be predicted`, y = result_CONFORMAL[[h]]$`Price day to be predicted`, colour = "#030101",shape=4,size=6) +
    theme_bw() +
    labs(x='q',y='p(q)') +
    ggtitle(paste("C. Vend - Day ", result_CONFORMAL[[h]]$`Position day to be predicted`, " - ", as.Date(result_CONFORMAL[[h]]$`Day predicted`)))
  
  
  mse_pred <- data.frame(
    algorithm = c("KO", "CP"),
    err_pred = c(mse_pred_KO, mse_pred_CP)
  )
  
  # Creare il bar chart
  mse_pred_barchart <- ggplot(mse_pred, aes(x = algorithm, y = err_pred, fill = algorithm)) +
    geom_bar(stat = "identity") +
    labs(title = "MSE prediction", x = "", y = NULL,fill = "") +
    scale_fill_manual(values = c("KO" = "black", "CP" = "#C66454")) + 
    theme(legend.position = "bottom")
  
  plot_final <- plot1 + mse_pred_barchart + plot_layout(widths = c(3, 1))
  
  
  title = paste("C. Vend - Day ", result_CONFORMAL[[h]]$`Position day to be predicted`, " - ", as.Date(result_CONFORMAL[[h]]$`Day predicted`))
  ggsave(filename = paste0(title,".pdf"),
         plot = plot_final,
         device = NULL,
         path = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series/Test_domain1D/RealWorld_data/comparison/offers",
         scale = 1,
         width = NA,
         height = NA,
         dpi = 300)
}





##############################
##saving demands predictions##
##############################
for (h in 1:number_of_predictions){
  output2=rbind( result_CONFORMAL[[h]]$Prediction$predicted_p_cg_a, result_CONFORMAL[[h]]$PredSet_0_25_inf_lim$purchase, result_CONFORMAL[[h]]$PredSet_0_25_sup_lim$purchase, result_CONFORMAL[[h]]$PredSet_0_50_inf_lim$purchase, result_CONFORMAL[[h]]$PredSet_0_50_sup_lim$purchase, result_CONFORMAL[[h]]$new_MGS_red_data$p_cg_a)
  KO_demands = data.frame(predictions_demands_KO[[h]])
  
  rownames(output2)=c('lvl','lwr_alp25','upr_alp25','lwr_alp50','upr_alp50','obs')
  
  output2=data.table(t(output2))
  output2$x_grid=result_CONFORMAL[[h]]$Volumes_grid
  
  KO_demands$x_grid = result_CONFORMAL[[h]]$Volumes_grid
  colnames(KO_demands) = c("KO_pred_demand","x_grid")
  
  n_tot = length(result_CONFORMAL[[h]]$Volumes_grid)
  mse_pred_CP = sum((output2$lvl - output2$obs)^2)/n_tot
  mse_pred_KO = sum((KO_demands$KO_pred_demand - output2$obs)^2)/n_tot
  
  
  alpha_plot=.2
  plot2 = ggplot() +
    
    #    geom_ribbon(data = output2, aes(x = x_grid, ymin=lwr_alp10, ymax=upr_alp10),alpha=alpha_plot,fill="#56B4E9",col="#56B4E9") + 
    #    geom_ribbon(data = output2, aes(x = x_grid, ymin=lwr_alp25, ymax=upr_alp25),alpha=alpha_plot,fill="#336C8B",col="#336C8B") + 
    #      geom_ribbon(data = output2, aes(x = x_grid, ymin=lwr_alp50, ymax=upr_alp50),alpha=alpha_plot,col="#11242E") +#fill="#11242E",col="#11242E") +
    geom_line(data = output2, aes(x = x_grid, y=lwr_alp25),alpha=0.7,col="#566269",size=1,linetype="longdash") +#fill="#11242E",col="#11242E") + 
    geom_line(data = output2, aes(x = x_grid, y=upr_alp25),alpha=0.7,col="#566269",size=1,linetype="longdash") +#fill="#11242E",col="#11242E") + 
    geom_line(data = KO_demands, aes(x = x_grid, y = KO_pred_demand), col="black") +
    geom_line(data = output2, aes(x = x_grid, y=lwr_alp50),alpha=alpha_plot,col="#2d05f7",size=1,linetype="longdash") +#fill="#11242E",col="#11242E") + 
    geom_line(data = output2, aes(x = x_grid, y=upr_alp50),alpha=alpha_plot,col="#2d05f7",size=1,linetype="longdash") +#fill="#11242E",col="#11242E") + 
    geom_line(data = output2, aes(x = x_grid, y = obs),col="#56B4E9",size=1) +
    geom_line(data = output2, aes(x = x_grid, y = lvl),col="#56B4E9",size=0.7,linetype = "dashed") +
    annotate("point", x = result_CONFORMAL[[h]]$`Quantity day to be predicted`, y = result_CONFORMAL[[h]]$`Price day to be predicted`, colour = "#030101",shape=4,size=6) +
    theme_bw() +
    labs(x='q',y='p(q)') +
    ggtitle(paste("C. Acq - Day ", result_CONFORMAL[[h]]$`Position day to be predicted`, " - ", as.Date(result_CONFORMAL[[h]]$`Day predicted`)))
  
  
  mse_pred <- data.frame(
    algorithm = c("KO", "CP"),
    err_pred = c(mse_pred_KO, mse_pred_CP)
  )
  
  # Creare il bar chart
  mse_pred_barchart <- ggplot(mse_pred, aes(x = algorithm, y = err_pred, fill = algorithm)) +
    geom_bar(stat = "identity") +
    labs(title = "MSE prediction", x = "", y = NULL,fill = "") +
    scale_fill_manual(values = c("KO" = "black", "CP" = "#56B4E9")) + 
    theme(legend.position = "bottom")
  
  plot_final <- plot2 + mse_pred_barchart + plot_layout(widths = c(3, 1))
  
  
  title = paste("C. Acq - Day ", result_CONFORMAL[[h]]$`Position day to be predicted`, " - ", as.Date(result_CONFORMAL[[h]]$`Day predicted`))
  ggsave(filename = paste0(title,".pdf"),
         plot = plot_final,
         device = NULL,
         path = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series/Test_domain1D/RealWorld_data/comparison/demands",
         scale = 1,
         width = NA,
         height = NA,
         dpi = 300)
}


















output1=rbind( result_CONFORMAL[[h]]$Prediction$predicted_p_cg_v, result_CONFORMAL[[h]]$PredSet_0_25_inf_lim$sale, result_CONFORMAL[[h]]$PredSet_0_25_sup_lim$sale, result_CONFORMAL[[h]]$PredSet_0_50_inf_lim$sale, result_CONFORMAL[[h]]$PredSet_0_50_sup_lim$sale, result_CONFORMAL[[h]]$new_MGS_red_data$p_cg_v)
#output1=rbind(point_prediction$predicted_fun_n_plus_8[[1]]$predicted_p_cg_v,inflim_vendita_alp25,suplim_vendita_alp25,inflim_vendita_alp50,suplim_vendita_alp50,new_MGS_red_data[[1]]$p_cg_v)
output2=rbind( result_CONFORMAL[[h]]$Prediction$predicted_p_cg_a, result_CONFORMAL[[h]]$PredSet_0_25_inf_lim$purchase, result_CONFORMAL[[h]]$PredSet_0_25_sup_lim$purchase, result_CONFORMAL[[h]]$PredSet_0_50_inf_lim$purchase, result_CONFORMAL[[h]]$PredSet_0_50_sup_lim$purchase, result_CONFORMAL[[h]]$new_MGS_red_data$p_cg_a)
#output2=rbind(point_prediction$predicted_fun_n_plus_8[[1]]$predicted_p_cg_a,inflim_acquisto_alp25,suplim_acquisto_alp25,inflim_acquisto_alp50,suplim_acquisto_alp50,new_MGS_red_data[[1]]$p_cg_a)
output3=rbind(result_CONFORMAL[[h]]$Sovrap_0_25$inflim,result_CONFORMAL[[h]]$Sovrap_0_25$suplim,result_CONFORMAL[[h]]$Sovrap_0_50$inflim,result_CONFORMAL[[h]]$Sovrap_0_50$suplim)
#output3=rbind(inflim_sovrap_alp25,suplim_sovrap_alp25,inflim_sovrap_alp50,suplim_sovrap_alp50)
KO_offers = data.frame(predictions_offers_KO[[h]])

rownames(output1)=c('lvl','lwr_alp25','upr_alp25','lwr_alp50','upr_alp50','obs')
rownames(output2)=c('lvl','lwr_alp25','upr_alp25','lwr_alp50','upr_alp50','obs')
rownames(output3)=c('lwr_alp25','upr_alp25','lwr_alp50','upr_alp50')


output1=data.table(t(output1))
output1$x_grid=result_CONFORMAL[[h]]$Volumes_grid

output2=data.table(t(output2))
output2$x_grid=result_CONFORMAL[[h]]$Volumes_grid

output3=data.table(t(output3))
output3$x_grid=result_CONFORMAL[[h]]$Volumes_grid

KO_offers$x_grid = result_CONFORMAL[[h]]$Volumes_grid
colnames(KO_offers) = c("KO_pred_offer","x_grid")

alpha_plot=.2
plot1 = ggplot() +
  
  #    geom_ribbon(data = output1, aes(x = x_grid, ymin=lwr_alp10, ymax=upr_alp10),alpha=alpha_plot,fill="#C66454",col="#C66454") + 
  #    geom_ribbon(data = output1, aes(x = x_grid, ymin=lwr_alp25, ymax=upr_alp25),alpha=alpha_plot,fill="#763C32",col="#763C32") + 
  #    geom_ribbon(data = output1, aes(x = x_grid, ymin=lwr_alp50, ymax=upr_alp50),alpha=alpha_plot,fill="#271410",col="#271410") + 
  geom_line(data = output1, aes(x = x_grid, y=lwr_alp25),alpha=0.7,col="#e32c0e",size=1,linetype="longdash") +#fill="#11242E",col="#11242E") + 
  geom_line(data = output1, aes(x = x_grid, y=upr_alp25),alpha=0.7,col="#e32c0e",size=1,linetype="longdash") +#fill="#11242E",col="#11242E") + 
  geom_line(data = KO_offers, aes(x = x_grid, y = KO_pred_offer), col="black") +
  geom_line(data = output1, aes(x = x_grid, y=lwr_alp50),alpha=alpha_plot,col="#a66a60",size=1,linetype="longdash") +#fill="#11242E",col="#11242E") + 
  geom_line(data = output1, aes(x = x_grid, y=upr_alp50),alpha=alpha_plot,col="#a66a60",size=1,linetype="longdash") +#fill="#11242E",col="#11242E") + 
  geom_line(data = output1, aes(x = x_grid, y = lvl),col="#C66454",size=0.7,linetype = "dashed") +
  geom_line(data = output1, aes(x = x_grid, y = obs),col="#C66454",size=1) +
  annotate("point", x = result_CONFORMAL[[h]]$`Quantity day to be predicted`, y = result_CONFORMAL[[h]]$`Price day to be predicted`, colour = "#030101",shape=4,size=6) +
  theme_bw() +
  labs(x='q',y='p(q)') +
  ggtitle(paste("C. Vend - Day ", result_CONFORMAL[[h]]$`Position day to be predicted`, " - ", as.Date(result_CONFORMAL[[h]]$`Day predicted`)))

plot2 = ggplot() +
  
  #    geom_ribbon(data = output2, aes(x = x_grid, ymin=lwr_alp10, ymax=upr_alp10),alpha=alpha_plot,fill="#56B4E9",col="#56B4E9") + 
  #    geom_ribbon(data = output2, aes(x = x_grid, ymin=lwr_alp25, ymax=upr_alp25),alpha=alpha_plot,fill="#336C8B",col="#336C8B") + 
  #      geom_ribbon(data = output2, aes(x = x_grid, ymin=lwr_alp50, ymax=upr_alp50),alpha=alpha_plot,col="#11242E") +#fill="#11242E",col="#11242E") +
  geom_line(data = output2, aes(x = x_grid, y=lwr_alp25),alpha=0.7,col="#566269",size=1,linetype="longdash") +#fill="#11242E",col="#11242E") + 
  geom_line(data = output2, aes(x = x_grid, y=upr_alp25),alpha=0.7,col="#566269",size=1,linetype="longdash") +#fill="#11242E",col="#11242E") + 
  geom_line(data = output2, aes(x = x_grid, y=lwr_alp50),alpha=alpha_plot,col="#2d05f7",size=1,linetype="longdash") +#fill="#11242E",col="#11242E") + 
  geom_line(data = output2, aes(x = x_grid, y=upr_alp50),alpha=alpha_plot,col="#2d05f7",size=1,linetype="longdash") +#fill="#11242E",col="#11242E") + 
  geom_line(data = output2, aes(x = x_grid, y = obs),col="#56B4E9",size=1) +
  geom_line(data = output2, aes(x = x_grid, y = lvl),col="#56B4E9",size=0.7,linetype = "dashed") +
  annotate("point", x = result_CONFORMAL[[h]]$`Quantity day to be predicted`, y = result_CONFORMAL[[h]]$`Price day to be predicted`, colour = "#030101",shape=4,size=6) +
  theme_bw() +
  labs(x='q',y='p(q)') +
  ggtitle(paste("C. Acq - Day ", result_CONFORMAL[[h]]$`Position day to be predicted`, " - ", as.Date(result_CONFORMAL[[h]]$`Day predicted`)))
#      ylim(-80,80) +

plot3 = ggplot() +
  
  geom_ribbon(data = output3, aes(x = x_grid, ymin=lwr_alp25, ymax=upr_alp25),alpha=alpha_plot,fill="#336C8B",col="#336C8B") + 
  geom_ribbon(data = output3, aes(x = x_grid, ymin=lwr_alp50, ymax=upr_alp50),alpha=alpha_plot,col="#11242E") +#fill="#11242E",col="#11242E") +
  annotate("point", x = final_quantity_new_day, y = final_price_new_day, colour = "#030101",shape=4,size=6) +
  theme_bw() +
  labs(x='q',y='p(q)') +
  ggtitle(paste("Sovrap C. - Day" , result_CONFORMAL[[h]]$`Position day to be predicted`, " - ", as.Date(result_CONFORMAL[[h]]$`Day predicted`)))
#      ylim(-80,80) +


plot_total = grid.arrange(plot1, plot2, plot3, ncol=2)
png(paste("g_p1_",pos_day_to_be_predicted,"_uff_after_corr.png",sep=""))
quartz()
print(plot_total)
dev.off()

quartz()
print(plot1)

title = paste("C. Vend - Day ", result_CONFORMAL[[h]]$`Position day to be predicted`, " - ", as.Date(result_CONFORMAL[[h]]$`Day predicted`))
ggsave(filename = paste0(title,".pdf"),
       plot = plot1,
       device = NULL,
       path = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series/Test_domain1D/RealWorld_data/comparison/offers",
       scale = 1,
       width = NA,
       height = NA,
       dpi = 300)













my_result=list(start_dataset,
               end_dataset,
               l,
               last_prediction,
               AR_order_v,
               AR_order_a,
               inclusion_price,
               price_order,
               value_seed,
               pos_first_q,
               pos_last_q,
               red_common_grid,
               b,
               s_type,
               number_of_predictions,
               vec_day_to_be_predicted,
               training_index,
               calibration_included_index, 
               vec_final_price_new_day,
               vec_final_quantity_new_day,
               curves_are_included_alp25,
               curves_are_included_alp25_after_corr,
               final_pq_is_included_alp25,
               av_width_alp25,
               av_width_alp25_after_corr,
               ncs_alp25,
               widths_alp25,
               widths_alp25_after_corr,
               curves_are_included_alp50,
               curves_are_included_alp50_after_corr,
               final_pq_is_included_alp50,
               av_width_alp50,
               av_width_alp50_after_corr,
               ncs_alp50,
               widths_alp50,
               widths_alp50_after_corr,
               inflim_predset_v_alp25,
               inflim_predset_a_alp25,
               inflim_predset_v_alp50,
               inflim_predset_a_alp50,
               suplim_predset_v_alp25,
               suplim_predset_a_alp25,
               suplim_predset_v_alp50,
               suplim_predset_a_alp50)


names(my_result)=c("start_dataset",
                   "end_dataset",
                   "l",
                   "last_prediction",
                   "AR_order_v",
                   "AR_order_a",
                   "inclusion_price",
                   "price_order",
                   "value_seed",
                   "pos_first_q",
                   "pos_last_q",
                   "red_common_grid",
                   "b",
                   "s_type",
                   "number_of_predictions",
                   "vec_day_to_be_predicted",
                   "training_index",
                   "calibration_included_index",  
                   "vec_final_price_new_day",
                   "vec_final_quantity_new_day",
                   "curves_are_included_alp25",
                   "curves_are_included_alp25_after_corr",
                   "final_pq_is_included_alp25",
                   "av_width_alp25",
                   "av_width_alp25_after_corr",
                   "ncs_alp25",
                   "widths_alp25",
                   "widths_alp25_after_corr",
                   "curves_are_included_alp50",
                   "curves_are_included_alp50_after_corr",
                   "final_pq_is_included_alp50",
                   "av_width_alp50",
                   "av_width_alp50_after_corr",
                   "ncs_alp50",
                   "widths_alp50",
                   "widths_alp50_after_corr",
                   "inflim_predset_v_alp25",
                   "inflim_predset_a_alp25",
                   "inflim_predset_v_alp50",
                   "inflim_predset_a_alp50",
                   "suplim_predset_v_alp25",
                   "suplim_predset_a_alp25",
                   "suplim_predset_v_alp50",
                   "suplim_predset_a_alp50")

assign("result_260419_310120_GS_after_corr",my_result)

save(result_260419_310120_GS_after_corr,file="result_260419_310120_GS_after_corr.Rdata")
