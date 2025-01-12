rm(list=ls())
graphics.off()
cat("\014")
set.seed(23032000)


#########################################################################################################
### Overlapping predicted curves plot, for each one of the predictors, for each of the days predicted ###
#########################################################################################################


#put here the path of the local copy of the directory
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"




#if you want to save the results
save_res = TRUE
format = ".png"
dir_res = paste0(dir_w,"/Test_domain1D/RealWorld_data/results")



#in which folder the result of the prediction are
path_res_pred = paste0(dir_res,"/results_prediction")
#where to store the results, in case (the images)
path_stor_res = paste0(paste0(dir_res,"/results_plot_predictions")) 


#predictors compared
prediction_method = c("PPC", "KE", "KEI", "MP", "NP", "CC")

#load the files with the predictions
for (pred_met in prediction_method) {
  files <- list.files(path = paste0(path_res_pred,paste0("/",pred_met)), full.names = TRUE)
  for (file in files) {
    load(file)
  }
}

#other useful packages for plot
library("data.table")
library(ggplot2)
library(gridExtra)
library(patchwork)


number_of_predictions = 184

for (h in 1:number_of_predictions){

  offers <- data.frame( x = result_CONFORMAL[[h]]$Volumes_grid,
                        y = c(prediction_PPC_offer[[h]]$Prediction,
                              prediction_KE_offer[[h]]$Prediction,
                              prediction_KEI_offer[[h]]$Prediction,
                              prediction_MP_offer[[h]]$Prediction,
                              prediction_NP_offer[[h]]$Prediction,
                              result_CONFORMAL[[h]]$Prediction$predicted_p_cg_v,
                              result_CONFORMAL[[h]]$new_MGS_red_data$p_cg_v),
                        predictor = factor(rep(c(prediction_method,"Observed curve"), each = length(result_CONFORMAL[[h]]$Volumes_grid))))
  
  plot_offer = ggplot(offers, aes(x = x, y = y, color = predictor)) +
               geom_line(size = 1) +  
               labs(color = "Predictor") + 
               scale_color_manual(values = c("darkolivegreen1", "lightblue", "lightgreen", "orange", "red", "black","blue")) +  
               annotate("point", x = result_CONFORMAL[[h]]$`Quantity to be predicted`, y = result_CONFORMAL[[h]]$`Price to be predicted`, colour = "#030101",shape=4,size=5) +
               theme_bw() +
               labs(x='q [MWh]',y='p(q) [Euro/MWh]') +
               ggtitle("Offer curve")+
               theme_minimal() +  
               theme(legend.title = element_text(size = 12), 
               legend.text = element_text(size = 10)) 
  
  
  demands <- data.frame( x = result_CONFORMAL[[h]]$Volumes_grid,
                         y = c(prediction_PPC_demand[[h]]$Prediction,
                               prediction_KE_demand[[h]]$Prediction,
                               prediction_KEI_demand[[h]]$Prediction,
                               prediction_MP_demand[[h]]$Prediction,
                               prediction_NP_demand[[h]]$Prediction,
                               result_CONFORMAL[[h]]$Prediction$predicted_p_cg_a,
                              result_CONFORMAL[[h]]$new_MGS_red_data$p_cg_a),
                          predictor = factor(rep(c(prediction_method,"Observed curve"), each = length(result_CONFORMAL[[h]]$Volumes_grid))))
  
  plot_demand = ggplot(demands, aes(x = x, y = y, color = predictor)) +
                geom_line(size = 1) +  
                labs(color = "Predictor") + 
                scale_color_manual(values = c("darkolivegreen1", "lightblue", "lightgreen", "orange", "red", "black","blue")) +  
                annotate("point", x = result_CONFORMAL[[h]]$`Quantity to be predicted`, y = result_CONFORMAL[[h]]$`Price to be predicted`, colour = "#030101",shape=4,size=5) +
                theme_bw() +
                labs(x='q [MWh]',y='p(q) [Euro/MWh]') +
                ggtitle("Demand curve") +
                theme_minimal() +  
                theme(legend.title = element_text(size = 12), 
                      legend.text = element_text(size = 10))
  
  PPC_plt <- data.frame( x = result_CONFORMAL[[h]]$Volumes_grid,
                         y = c(prediction_PPC_offer[[h]]$Prediction,
                               prediction_PPC_demand[[h]]$Prediction,
                               result_CONFORMAL[[h]]$new_MGS_red_data$p_cg_v,
                               result_CONFORMAL[[h]]$new_MGS_red_data$p_cg_a),
                         curve = factor(rep(c("Offer pred","Demand pred","Offer obs","Demand obs"), each = length(result_CONFORMAL[[h]]$Volumes_grid))))
  
  plot_PPC_ov = ggplot(PPC_plt, aes(x = x, y = y, color = curve)) +
                geom_line(size = 1) +  
                labs(color = " ") + 
                scale_color_manual(values = c("lightblue","blue","lightgreen","purple")) +  
                annotate("point", x = result_CONFORMAL[[h]]$`Quantity to be predicted`, y = result_CONFORMAL[[h]]$`Price to be predicted`, colour = "#030101",shape=4,size=5) +
                theme_bw() +
                labs(x='q [MWh]',y='p(q) [Euro/MWh]') +
                ggtitle("PPC forecast") +
                theme_minimal() +  
                theme(legend.title = element_text(size = 12), 
                      legend.text = element_text(size = 10))
  
  title = paste("Day ", result_CONFORMAL[[h]]$`Position day to be predicted`, " - ", as.Date(result_CONFORMAL[[h]]$`Day predicted`))
  
  plot_final = plot_offer + plot_demand  + plot_layout(nrow = 2, ncol = 1, widths = c(3, 1)) + plot_annotation(title = title)
  
  if(save_res){
    ggsave(filename = paste0(paste(title,"_curves"),format),
           plot = plot_final,
           device = NULL,
           path = paste0(path_stor_res,"/curves"),
           scale = 1,
           width = NA,
           height = NA,
           dpi = 300)
    
    ggsave(filename = paste0(paste(title,"_PPC_pred"),format),
           plot = plot_PPC_ov,
           device = NULL,
           path = paste0(path_stor_res,"/PPC_forecast"),
           scale = 1,
           width = NA,
           height = NA,
           dpi = 300)
  }
}
