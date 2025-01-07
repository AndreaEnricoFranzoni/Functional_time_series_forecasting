rm(list=ls())
graphics.off()
cat("\014")
set.seed(23032000)

### Predicted curves plot
###

#if you want to save the result in a folder 
save_res = TRUE

dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"
dir_res = paste0(dir_w,"/Test_domain1D/RealWorld_data/results")



#in which folder the result of the prediction are
path_res_pred = paste0(dir_res,"/results_prediction_log")
#where to store the results, in case (the images)
path_stor_res = paste0(paste0(dir_res,"/results_plot_predictions_log"))  #saving boxplots
format = ".png"

prediction_method = c("PPC", "KE", "KEI", "MP", "NP", "CC")

#load the files with the predictions
for (pred_met in prediction_method) {
  files <- list.files(path = paste0(path_res_pred,paste0("/",pred_met)), full.names = TRUE)
  for (file in files) {
    load(file)
  }
}


library("data.table")
library(ggplot2)
library(gridExtra)
library(patchwork)

#############################
##saving offers predictions##
#############################
number_of_predictions = 184

for (h in 1:number_of_predictions){
  
  offers <- data.frame( x = result_CONFORMAL_log[[h]]$Volumes_grid,
                        y = c(prediction_PPC_offer_log[[h]]$Prediction,
                              prediction_KE_offer_log[[h]]$Prediction,
                              prediction_KEI_offer_log[[h]]$Prediction,
                              prediction_MP_offer_log[[h]]$Prediction,
                              prediction_NP_offer_log[[h]]$Prediction,
                              result_CONFORMAL_log[[h]]$Prediction$predicted_p_cg_v,
                              result_CONFORMAL_log[[h]]$new_MGS_red_data$p_cg_v),
                        predictor = factor(rep(c(prediction_method,"Observed curve"), each = length(result_CONFORMAL_log[[h]]$Volumes_grid))))
  
  plot_offer = ggplot(offers, aes(x = x, y = y, color = predictor)) +
    geom_line(size = 1) +  
    labs(color = "Predictor") + 
    scale_color_manual(values = c("darkolivegreen1", "lightblue", "lightgreen", "orange", "red", "black","blue")) +  
    annotate("point", x = result_CONFORMAL_log[[h]]$`Quantity to be predicted`, y = result_CONFORMAL_log[[h]]$`Price to be predicted`, colour = "#030101",shape=4,size=5) +
    theme_bw() +
    labs(x='q [MWh]',y='log(p(q)) [log(Euro/MWh)]') +
    ggtitle("Offer log-curve")+
    theme_minimal() +  
    theme(legend.title = element_text(size = 12), 
          legend.text = element_text(size = 10)) 
  
  
  demands <- data.frame( x = result_CONFORMAL_log[[h]]$Volumes_grid,
                         y = c(prediction_PPC_demand_log[[h]]$Prediction,
                               prediction_KE_demand_log[[h]]$Prediction,
                               prediction_KEI_demand_log[[h]]$Prediction,
                               prediction_MP_demand_log[[h]]$Prediction,
                               prediction_NP_demand_log[[h]]$Prediction,
                               result_CONFORMAL_log[[h]]$Prediction$predicted_p_cg_a,
                               result_CONFORMAL_log[[h]]$new_MGS_red_data$p_cg_a),
                         predictor = factor(rep(c(prediction_method,"Observed curve"), each = length(result_CONFORMAL_log[[h]]$Volumes_grid))))
  
  plot_demand = ggplot(demands, aes(x = x, y = y, color = predictor)) +
    geom_line(size = 1) +  
    labs(color = "Predictor") + 
    scale_color_manual(values = c("darkolivegreen1", "lightblue", "lightgreen", "orange", "red", "black","blue")) +  
    annotate("point", x = result_CONFORMAL_log[[h]]$`Quantity to be predicted`, y = result_CONFORMAL_log[[h]]$`Price to be predicted`, colour = "#030101",shape=4,size=5) +
    theme_bw() +
    labs(x='q [MWh]',y='log(p(q)) [log(Euro/MWh)]') +
    ggtitle("Demand log-curve") +
    theme_minimal() +  
    theme(legend.title = element_text(size = 12), 
          legend.text = element_text(size = 10))
  
  PPC_plt <- data.frame( x = result_CONFORMAL_log[[h]]$Volumes_grid,
                         y = c(prediction_PPC_offer_log[[h]]$Prediction,
                               prediction_PPC_demand_log[[h]]$Prediction,
                               result_CONFORMAL_log[[h]]$new_MGS_red_data$p_cg_v,
                               result_CONFORMAL_log[[h]]$new_MGS_red_data$p_cg_a),
                         curve = factor(rep(c("Offer pred","Demand pred","Offer obs","Demand obs"), each = length(result_CONFORMAL_log[[h]]$Volumes_grid))))
  
  plot_PPC_ov = ggplot(PPC_plt, aes(x = x, y = y, color = curve)) +
    geom_line(size = 1) +  
    labs(color = " ") + 
    scale_color_manual(values = c("lightblue","blue","lightgreen","purple")) +  
    annotate("point", x = result_CONFORMAL_log[[h]]$`Quantity to be predicted`, y = result_CONFORMAL_log[[h]]$`Price to be predicted`, colour = "#030101",shape=4,size=5) +
    theme_bw() +
    labs(x='q [MWh]',y='log(p(q)) [log(Euro/MWh)]') +
    ggtitle("PPC forecast, log-curves") +
    theme_minimal() +  
    theme(legend.title = element_text(size = 12), 
          legend.text = element_text(size = 10))
  
  title = paste("Day ", result_CONFORMAL_log[[h]]$`Position day to be predicted`, " - ", as.Date(result_CONFORMAL_log[[h]]$`Day predicted`))
  
  plot_final = plot_offer + plot_demand  + plot_layout(nrow = 2, ncol = 1, widths = c(3, 1)) + plot_annotation(title = title)
  
  if(save_res){
    ggsave(filename = paste0(paste(title,"_curves_log"),format),
           plot = plot_final,
           device = NULL,
           path = paste0(path_stor_res,"/curves_log"),
           scale = 1,
           width = NA,
           height = NA,
           dpi = 300)
    
    ggsave(filename = paste0(paste(title,"_PPC_pred_log"),format),
           plot = plot_PPC_ov,
           device = NULL,
           path = paste0(path_stor_res,"/PPC_forecast_log"),
           scale = 1,
           width = NA,
           height = NA,
           dpi = 300)
  }
}
