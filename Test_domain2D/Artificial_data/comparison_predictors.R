rm(list=ls())
graphics.off()
cat("\014")

set.seed(23032000)


############################################################
#### Prediction evaluation as indicated in the readme   ####
############################################################


#put here the path of the local copy of the directory
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"

# save results
save_res = TRUE
format = ".jpg"
path_res_pred = paste0(dir_w,"/Test_domain2D/Artificial_data/results/results_prediction")
path_save_res = paste0(dir_w,"/Test_domain2D/Artificial_data/results/results_plots_err")

# predictors used
prediction_method = c("PPC", "EK", "EKI", "MP", "NP", "EX")

#load the files with the predictions
for (pred_met in prediction_method) {
  files <- list.files(path = paste0(path_res_pred,paste0("/",pred_met)), full.names = TRUE)
  for (file in files) {
    load(file)
  }
}

#evaluating errors
ENs = c(err_PPC_en,err_EK_en,err_EKI_en,err_MP_en,err_NP_en,err_EX_en)

N_en = length(err_PPC_en)

RNs = c(err_PPC_rn,
        err_EK_rn,
        err_EKI_rn,
        err_MP_rn,
        err_NP_rn,
        err_EX_rn)

N_rn = length(err_PPC_rn)


mean(err_PPC_en)
mean(err_EK_en)
mean(err_EKI_en)
mean(err_MP_en)
mean(err_NP_en)
mean(err_EX_en)
sd(err_PPC_en)
sd(err_EK_en)
sd(err_EKI_en)
sd(err_MP_en)
sd(err_NP_en)
sd(err_EX_en)


mean(err_PPC_rn)
mean(err_EK_rn)
mean(err_EKI_rn)
mean(err_MP_rn)
mean(err_NP_rn)
mean(err_EX_rn)
sd(err_PPC_rn)
sd(err_EK_rn)
sd(err_EKI_rn)
sd(err_MP_rn)
sd(err_NP_rn)
sd(err_EX_rn)




###################
## BoxPlot of En ##
###################
err_en <- c(err_PPC_en,err_EK_en,err_EKI_en,err_MP_en,err_NP_en,err_EX_en)
method <- rep(prediction_method, each=N_en)
En <- data.frame(method, err_en)
method_order<- prediction_method
En.box <- En %>% mutate(method=factor(x=method, levels=method_order))

##grouped boxplot
pgplot <- ggplot(En.box, aes(x=method, y=err_en, fill=method)) + 
  geom_boxplot() + ggtitle("Prediction error, 2D domain synthetic data")
pgplot <- pgplot +
  #scale_y_continuous(limits=c(0,0.1)) +
  theme_bw() + 
  labs(x="", y="En", fill = "") +
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
  title = "En_2d_syn"
  ggsave(filename = paste0(title,format),
         plot = pgplot,
         device = NULL,
         path = path_save_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}


###################
## BoxPlot of Rn ##
###################
err_rn <- RNs
method <- rep(prediction_method, each=N_rn)
Rn <- data.frame(method, err_rn)
method_order<- prediction_method
Rn.box <- Rn %>% mutate(method=factor(x=method, levels=method_order))

##grouped boxplot
pgplot <- ggplot(Rn.box, aes(x=method, y=err_rn, fill=method)) + 
  geom_boxplot()  + ggtitle("Prediction error, 2D domain synthetic data")
pgplot <- pgplot +
  #scale_y_continuous(limits=c(0,0.1)) +
  theme_bw() + 
  labs(x="", y="Rn", fill = "") +
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
  title = "Rn_2d_syn"
  ggsave(filename = paste0(title,format),
         plot = pgplot,
         device = NULL,
         path = path_save_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}
