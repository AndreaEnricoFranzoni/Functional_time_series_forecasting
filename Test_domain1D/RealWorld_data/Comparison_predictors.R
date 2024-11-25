rm(list=ls())
graphics.off()
cat("\014")
set.seed(23032000)

### Evaluation of prediction on simulations thorugh boxplots of En and Rn, and summary statistics of them
###

#if you want to save the result in a folder 
save_res = TRUE

dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"
dir_res = paste0(dir_w,"/Test_domain1D/RealWorld_data/results")
source(paste0(dir_w,"/Test_domain1D/RealWorld_data/utils/open_window.R"))


#in which folder the result of the prediction are
path_res_pred = paste0(dir_res,"/results_prediction")
#where to store the results, in case
path_stor_res = paste0(paste0(dir_res,"/results_plot_errors"))  #saving boxplots
path_stor_res_data = paste0(dir_res,"/results_errors")           #saving errors statistics

prediction_method = c("PPC", "KE", "KEI", "MP", "NP", "CC")

#load the files with the predictions
for (pred_met in prediction_method) {
  files <- list.files(path = paste0(path_res_pred,paste0("/",pred_met)), full.names = TRUE)
  for (file in files) {
    load(file)
  }
}

#dataset
{
  load(paste0(dir_w,"/Test_domain1D/RealWorld_data/utils/data/MGS_cg_260419_310120_data.Rdata"))
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


#evaluate the estimate of the errors
N = length(prediction_PPC_offer)  #total number of predictions
{
  err_PPC_offer_en  = numeric(N)
  err_PPC_offer_rn  = numeric(N)
  err_PPC_demand_en = numeric(N)
  err_PPC_demand_rn = numeric(N)
  
  err_KE_offer_en  = numeric(N)
  err_KE_offer_rn  = numeric(N)
  err_KE_demand_en = numeric(N)
  err_KE_demand_rn = numeric(N)
  
  err_KEI_offer_en  = numeric(N)
  err_KEI_offer_rn  = numeric(N)
  err_KEI_demand_en = numeric(N)
  err_KEI_demand_rn = numeric(N)
  
  err_MP_offer_en  = numeric(N)
  err_MP_offer_rn  = numeric(N)
  err_MP_demand_en = numeric(N)
  err_MP_demand_rn = numeric(N)
  
  err_NP_offer_en  = numeric(N)
  err_NP_offer_rn  = numeric(N)
  err_NP_demand_en = numeric(N)
  err_NP_demand_rn = numeric(N)
  
  err_CC_offer_en  = numeric(N)
  err_CC_offer_rn  = numeric(N)
  err_CC_demand_en = numeric(N)
  err_CC_demand_rn = numeric(N)
}


#errors estimate
for (i in 1:N) {

  actual_offer  = offers_dataset[,(i-1+first_prediction)]
  actual_demand = demands_dataset[,(i-1+first_prediction)]
  
  err_PPC_offer_en[i]  = sqrt(MLmetrics::MSE(prediction_PPC_offer[[i]]$Prediction,actual_offer))
  err_PPC_offer_rn[i]  = MLmetrics::MAE(prediction_PPC_offer[[i]]$Prediction,actual_offer)
  err_PPC_demand_en[i] = sqrt(MLmetrics::MSE(prediction_PPC_demand[[i]]$Prediction,actual_demand))
  err_PPC_demand_rn[i] = MLmetrics::MAE(prediction_PPC_demand[[i]]$Prediction,actual_demand)
  
  err_KE_offer_en[i]  = sqrt(MLmetrics::MSE(prediction_KE_offer[[i]]$Prediction,actual_offer))
  err_KE_offer_rn[i]  = MLmetrics::MAE(prediction_KE_offer[[i]]$Prediction,actual_offer)
  err_KE_demand_en[i] = sqrt(MLmetrics::MSE(prediction_KE_demand[[i]]$Prediction,actual_demand))
  err_KE_demand_rn[i] = MLmetrics::MAE(prediction_KE_demand[[i]]$Prediction,actual_demand)
  
  err_KEI_offer_en[i]  = sqrt(MLmetrics::MSE(prediction_KEI_offer[[i]]$Prediction,actual_offer))
  err_KEI_offer_rn[i]  = MLmetrics::MAE(prediction_KEI_offer[[i]]$Prediction,actual_offer)
  err_KEI_demand_en[i] = sqrt(MLmetrics::MSE(prediction_KEI_demand[[i]]$Prediction,actual_demand))
  err_KEI_demand_rn[i] = MLmetrics::MAE(prediction_KEI_demand[[i]]$Prediction,actual_demand)
  
  err_MP_offer_en[i]  = sqrt(MLmetrics::MSE(prediction_MP_offer[[i]]$Prediction,actual_offer))
  err_MP_offer_rn[i]  = MLmetrics::MAE(prediction_MP_offer[[i]]$Prediction,actual_offer)
  err_MP_demand_en[i] = sqrt(MLmetrics::MSE(prediction_MP_demand[[i]]$Prediction,actual_demand))
  err_MP_demand_rn[i] = MLmetrics::MAE(prediction_MP_demand[[i]]$Prediction,actual_demand)
  
  err_NP_offer_en[i]  = sqrt(MLmetrics::MSE(prediction_NP_offer[[i]]$Prediction,actual_offer))
  err_NP_offer_rn[i]  = MLmetrics::MAE(prediction_NP_offer[[i]]$Prediction,actual_offer)
  err_NP_demand_en[i] = sqrt(MLmetrics::MSE(prediction_NP_demand[[i]]$Prediction,actual_demand))
  err_NP_demand_rn[i] = MLmetrics::MAE(prediction_NP_demand[[i]]$Prediction,actual_demand)
  
  err_CC_offer_en[i]  = sqrt(MLmetrics::MSE(prediction_CC_offer[[i]],actual_offer))
  err_CC_offer_rn[i]  = MLmetrics::MAE(prediction_CC_offer[[i]],actual_offer)
  err_CC_demand_en[i] = sqrt(MLmetrics::MSE(prediction_CC_demand[[i]],actual_demand))
  err_CC_demand_rn[i] = MLmetrics::MAE(prediction_CC_demand[[i]],actual_demand)
  
}


err_offer = list( PPC = list(En = err_PPC_offer_en, Rn = err_PPC_offer_rn),
                  KE  = list(En = err_KE_offer_en, Rn = err_KE_offer_rn),
                  KEI = list(En = err_KEI_offer_en, Rn = err_KEI_offer_rn),
                  MP  = list(En = err_MP_offer_en, Rn = err_MP_offer_rn),
                  NP  = list(En = err_NP_offer_en, Rn = err_NP_offer_rn),
                  CC  = list(En = err_CC_offer_en, Rn = err_CC_offer_rn))

err_demand = list( PPC = list(En = err_PPC_demand_en, Rn = err_PPC_demand_rn),
                   KE  = list(En = err_KE_demand_en, Rn = err_KE_demand_rn),
                   KEI = list(En = err_KEI_demand_en, Rn = err_KEI_demand_rn),
                   MP  = list(En = err_MP_demand_en, Rn = err_MP_demand_rn),
                   NP  = list(En = err_NP_demand_en, Rn = err_NP_demand_rn),
                   CC  = list(En = err_CC_demand_en, Rn = err_CC_demand_rn))


# summary statistics of errors
mean_errors = matrix(nrow=4,ncol=length(prediction_method))
sd_errors   = matrix(nrow=4,ncol=length(prediction_method))


for (i in 1:length(prediction_method)) {
  mean_errors[1,i] = mean(err_offer[[i]]$En)
  mean_errors[2,i] = mean(err_demand[[i]]$En)
  mean_errors[3,i] = mean(err_offer[[i]]$Rn)
  mean_errors[4,i] = mean(err_demand[[i]]$Rn)
  
  sd_errors[1,i] = sd(err_offer[[i]]$En)
  sd_errors[2,i] = sd(err_demand[[i]]$En)
  sd_errors[3,i] = sd(err_offer[[i]]$Rn)
  sd_errors[4,i] = sd(err_demand[[i]]$Rn)
}

mean_errors = as.data.frame(mean_errors)
row.names(mean_errors) = c("En_offer","En_demand","Rn_offer","Rn_demand")
colnames(mean_errors) = prediction_method

sd_errors = as.data.frame(sd_errors)
row.names(sd_errors) = c("En_offer","En_demand","Rn_offer","Rn_demand")
colnames(sd_errors) = prediction_method

if(save_res){
  file_sav1 = paste0(path_stor_res_data,"/means_err.Rdata")
  file_sav2 = paste0(path_stor_res_data,"/sd_err.Rdata")
  
  save(mean_errors, file = file_sav1)
  save(sd_errors, file = file_sav2)
}



##########################
## BoxPlot of En offers ##
##########################
prediction_method = c("PPC", "KE", "KEI", "MP", "NP", "CC")
err_en <- c(err_PPC_offer_en, err_KE_offer_en, err_KEI_offer_en, err_MP_offer_en, err_NP_offer_en, err_CC_offer_en)
method <- rep(c("PPC","KE", "KEI", "MP", "NP", "CC"), each=N)
En <- data.frame(method, err_en)
method_order<- c("PPC","KE", "KEI", "MP", "NP", "CC")
En.box <- En %>% mutate(method=factor(x=method, levels=method_order))

##grouped boxplot
open_window()
pgplot <- ggplot(En.box, aes(x=method, y=err_en, fill=method)) + 
  geom_boxplot() + ggtitle("En offers predictions")
pgplot <- pgplot +
  #scale_y_continuous(limits=c(0,0.1)) +
  theme_bw() + 
  labs(x="", y="En", fill = "Prediction method") +
  #labs(x="", y=TeX(r'($\frac{1}{N} \; \sum_{j=1}^N (f_{t+1, j}^b - \hat{f}_{t+1,j}^b)^2$)'), fill="Prediction method") +
  theme(plot.title = element_text(face="bold", hjust=0.5, size=22),
        axis.text.x = element_text(size=22),
        axis.title.x = element_text(size=22),
        axis.text.y = element_text(size=22),
        axis.title.y = element_text(size=22),
        legend.title = element_text(size=22),
        legend.text = element_text(size=22),
        legend.position="bottom",
        legend.direction = "horizontal") +
  guides(fill=guide_legend(nrow=1, byrow=TRUE))
pgplot + 
  theme(legend.position="none")
print(pgplot)

if(save_res){
  title = "En_offers"
  ggsave(filename = paste0(title,".pdf"),
         plot = pgplot,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}




###########################
## BoxPlot of En demands ##
###########################
prediction_method = c("PPC", "KE", "KEI", "MP", "NP", "CC")
err_en <- c(err_PPC_demand_en, err_KE_demand_en, err_KEI_demand_en, err_MP_demand_en, err_NP_demand_en, err_CC_demand_en)
method <- rep(c("PPC","KE", "KEI", "MP", "NP", "CC"), each=N)
En <- data.frame(method, err_en)
method_order<- c("PPC","KE", "KEI", "MP", "NP", "CC")
En.box <- En %>% mutate(method=factor(x=method, levels=method_order))

##grouped boxplot
open_window()
pgplot <- ggplot(En.box, aes(x=method, y=err_en, fill=method)) + 
  geom_boxplot() + ggtitle("En demands predictions")
pgplot <- pgplot +
  #scale_y_continuous(limits=c(0,0.1)) +
  theme_bw() + 
  labs(x="", y="En", fill = "Prediction method") +
  #labs(x="", y=TeX(r'($\frac{1}{N} \; \sum_{j=1}^N (f_{t+1, j}^b - \hat{f}_{t+1,j}^b)^2$)'), fill="Prediction method") +
  theme(plot.title = element_text(face="bold", hjust=0.5, size=22),
        axis.text.x = element_text(size=22),
        axis.title.x = element_text(size=22),
        axis.text.y = element_text(size=22),
        axis.title.y = element_text(size=22),
        legend.title = element_text(size=22),
        legend.text = element_text(size=22),
        legend.position="bottom",
        legend.direction = "horizontal") +
  guides(fill=guide_legend(nrow=1, byrow=TRUE))
pgplot + 
  theme(legend.position="none")
print(pgplot)

if(save_res){
  title = "En_demands"
  ggsave(filename = paste0(title,".pdf"),
         plot = pgplot,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}




##########################
## BoxPlot of Rn offers ##
##########################
err_rn <- c(err_PPC_offer_rn, err_KE_offer_rn, err_KEI_offer_rn, err_MP_offer_rn, err_NP_offer_rn, err_CC_offer_rn)
method <- rep(c("PPC","KE", "KEI", "MP", "NP", "CC"), each=N)
Rn <- data.frame(method, err_rn)
method_order<- c("PPC","KE", "KEI", "MP", "NP", "CC")
Rn.box <- Rn %>% mutate(method=factor(x=method, levels=method_order))

##grouped boxplot
open_window()
pgplot <- ggplot(Rn.box, aes(x=method, y=err_rn, fill=method)) + 
  geom_boxplot()  + ggtitle("Rn offers predictions")
pgplot <- pgplot +
  #scale_y_continuous(limits=c(0,0.1)) +
  theme_bw() + 
  labs(x="", y="Rn", fill = "Prediction method") +
  #labs(x="", y=TeX(r'($\frac{1}{N} \; \sum_{j=1}^N (f_{t+1, j}^b - \hat{f}_{t+1,j}^b)^2$)'), fill="Prediction method") +
  theme(plot.title = element_text(face="bold", hjust=0.5, size=22),
        axis.text.x = element_text(size=22),
        axis.title.x = element_text(size=22),
        axis.text.y = element_text(size=22),
        axis.title.y = element_text(size=22),
        legend.title = element_text(size=22),
        legend.text = element_text(size=22),
        legend.position="bottom",
        legend.direction = "horizontal") +
  guides(fill=guide_legend(nrow=1, byrow=TRUE))
pgplot + 
  theme(legend.position="none")

if(save_res){
  title = "Rn_offers"
  ggsave(filename = paste0(title,".pdf"),
         plot = pgplot,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}




###########################
## BoxPlot of Rn demands ##
###########################
err_rn <- c(err_PPC_demand_rn, err_KE_demand_rn, err_KEI_demand_rn, err_MP_demand_rn, err_NP_demand_rn, err_CC_demand_rn)
method <- rep(c("PPC","KE", "KEI", "MP", "NP", "CC"), each=N)
Rn <- data.frame(method, err_rn)
method_order<- c("PPC","KE", "KEI", "MP", "NP", "CC")
Rn.box <- Rn %>% mutate(method=factor(x=method, levels=method_order))

##grouped boxplot
open_window()
pgplot <- ggplot(Rn.box, aes(x=method, y=err_rn, fill=method)) + 
  geom_boxplot()  + ggtitle("Rn demands predictions")
pgplot <- pgplot +
  #scale_y_continuous(limits=c(0,0.1)) +
  theme_bw() + 
  labs(x="", y="Rn", fill = "Prediction method") +
  #labs(x="", y=TeX(r'($\frac{1}{N} \; \sum_{j=1}^N (f_{t+1, j}^b - \hat{f}_{t+1,j}^b)^2$)'), fill="Prediction method") +
  theme(plot.title = element_text(face="bold", hjust=0.5, size=22),
        axis.text.x = element_text(size=22),
        axis.title.x = element_text(size=22),
        axis.text.y = element_text(size=22),
        axis.title.y = element_text(size=22),
        legend.title = element_text(size=22),
        legend.text = element_text(size=22),
        legend.position="bottom",
        legend.direction = "horizontal") +
  guides(fill=guide_legend(nrow=1, byrow=TRUE))
pgplot + 
  theme(legend.position="none")

if(save_res){
  title = "Rn_demands"
  ggsave(filename = paste0(title,".pdf"),
         plot = pgplot,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}

