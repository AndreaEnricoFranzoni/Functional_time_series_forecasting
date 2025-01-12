rm(list=ls())
graphics.off()
cat("\014")
set.seed(23032000)

#############################################################################################################################
### Evaluating where the intersection between PPC predicted curves happens, visualizing its distance wrt to real traded   ###
### quantity and price with bagplots                                                                                      ###
#############################################################################################################################


#put here the path of the local copy of the directory
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"


#if you want to save the result in a folder 
save_res = TRUE
dir_res = paste0(dir_w,"/Test_domain1D/RealWorld_data/results")


#in which folder the result of the PPC predictions are
path_res_pred = paste0(dir_res,"/results_prediction/PPC")
load(paste0(path_res_pred,"/prediction_PPC_demand.Rdata"))
load(paste0(path_res_pred,"/prediction_PPC_offer.Rdata"))
load(paste0(dir_res,"/results_prediction/CC/CP_CC.Rdata"))
load(paste0(dir_w,"/Test_domain1D/RealWorld_data/utils/data/MGS_cg_260419_310120_data.Rdata"))


#where to store the results, in case
path_stor_res = paste0(paste0(dir_res,"/results_final_q_p_predictions"))  
format_file = ".png"  #and, in case, the format


#saving when the intersection happen

x_grid_dim         <- 401
x_grid             <- MGS_cg_260419_310120_data$x_axis[1:x_grid_dim]
left_extreme       <- min(x_grid)
right_extreme      <- max(x_grid)
x_grid_inter       <- seq(left_extreme,right_extreme,length.out=1000)

N_pred = length(prediction_PPC_offer)

final_q_ppc = numeric(N_pred)
final_p_ppc = numeric(N_pred)
final_q_cc  = numeric(N_pred)
final_p_cc  = numeric(N_pred)
real_q_obs  = numeric(N_pred)
real_p_obs  = numeric(N_pred)


# when intersection happens for PPC predictions
for (i in 1:N_pred) {
  real_p = result_CONFORMAL[[i]]$`Price to be predicted`
  real_q = result_CONFORMAL[[i]]$`Quantity to be predicted`
  
  real_p_obs[i] = real_p
  real_q_obs[i] = real_q
  
  ppc_off = approxfun(x_grid,prediction_PPC_offer[[i]]$Prediction)
  ppc_dem = approxfun(x_grid,prediction_PPC_demand[[i]]$Prediction)
  
  for (j in 1:length(x_grid_inter)) {
    q_int = x_grid_inter[j]
    if(abs(ppc_off(q_int) - ppc_dem(q_int)) < 1e-2 ){
      final_q_ppc[i] = q_int
      final_p_ppc[i] = ppc_off(q_int)
      
      break
    }
  }
  
  cc_off = approxfun(x_grid,result_CONFORMAL[[i]]$Prediction$predicted_p_cg_v)
  cc_dem = approxfun(x_grid,result_CONFORMAL[[i]]$Prediction$predicted_p_cg_a)
  
  for (j in 1:length(x_grid_inter)) {
    q_int = x_grid_inter[j]
    if(abs(cc_off(q_int) - cc_dem(q_int)) < 1e-2 ){
      final_q_cc[i] = q_int
      final_p_cc[i] = cc_off(q_int)
      break
    }
  }
  
  
  
  curves_names = c("PPC offer","PPC demand","CC offer","CC demand")
  curves <- data.frame( x = result_CONFORMAL[[i]]$Volumes_grid,
                        y = c(prediction_PPC_offer[[i]]$Prediction,
                              prediction_PPC_demand[[i]]$Prediction,
                              result_CONFORMAL[[i]]$Prediction$predicted_p_cg_v,
                              result_CONFORMAL[[i]]$Prediction$predicted_p_cg_a),
                        curvesNames = factor(rep(curves_names, each = length(result_CONFORMAL[[i]]$Volumes_grid))))
  
  points_final_q_p <- data.frame(
    x = c(real_q, final_q_ppc[i], final_q_cc[i]),
    y = c(real_p, final_p_ppc[i], final_p_cc[i]),
    point_label = c("OBS", "PPC", "CC")
  )
  
  title = paste("Day ", result_CONFORMAL[[i]]$`Position day to be predicted`, " - ", as.Date(result_CONFORMAL[[i]]$`Day predicted`))
  
  
  plot_curves = ggplot(curves, aes(x = x, y = y, color = curvesNames)) +
                geom_point(data = points_final_q_p, aes(x = x, y = y, shape = point_label, color = "black"), size = 4) +
                geom_line(size = 1) +  
                scale_color_manual(
      values = c("PPC offer" = "purple", "PPC demand" = "blue", 
                 "CC offer" = "red", "CC demand" = "orange" ),
      name = "Prediction curve"
    ) +
    scale_shape_manual(
      values = c("OBS" = 15, "PPC" = 16, 
                 "CC" = 17),
      name = "Quantity-price trading"
    ) +
    theme_bw() +
    theme(panel.grid = element_blank())+
    labs(x='q [MWh]',y='p(q) [Euro/MWh]') +
    ggtitle(title)+
    theme_minimal() +  
    theme(legend.title = element_text(size = 12), 
          legend.text = element_text(size = 10)) 
  
  if(save_res){
    ggsave(filename = paste0(paste(title,"_final_q_p"),format_file),
           plot = plot_curves,
           device = NULL,
           path = paste0(path_stor_res,"/final_q_p_plot"),
           scale = 1,
           width = NA,
           height = NA,
           dpi = 300)}
  
  rm(points_final_q_p)
}


final_q_p_ppc = list(final_q = final_q_ppc, final_p = final_p_ppc)
final_q_p_cc = list(final_q = final_q_cc, final_p = final_p_cc)

if(save_res){
  save(final_q_p_ppc,file=paste0(path_stor_res,"/final_q_p_ppc.Rdata"))
  save(final_q_p_cc,file=paste0(path_stor_res,"/final_q_p_cc.Rdata"))
}


# bagplot for PPC errors
dif_q_ppc = final_q_ppc - real_q_obs
dif_p_ppc = final_p_ppc - real_p_obs
dif_ppc <- data.frame(q = dif_q_ppc, p = dif_p_ppc)

quartz()
bgplt_ppc = bagplot(dif_ppc, factor = 3, show.whiskers = T, xlab = "Error in final quantity", ylab = "Error in final price", main="Comparison with PPC")



# bagplot for CC errors
dif_q_cc = final_q_cc - real_q_obs
dif_p_cc = final_p_cc - real_p_obs
dif_cc <- data.frame(q = dif_q_cc, p = dif_p_cc)

quartz()
bgplt_pcc = bagplot(dif_cc, factor = 3, show.whiskers = T, xlab = "Error in final quantity", ylab = "Error in final price", main="Comparison with CC")
