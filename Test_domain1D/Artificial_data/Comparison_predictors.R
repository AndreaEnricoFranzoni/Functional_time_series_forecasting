rm(list=ls())
graphics.off()
cat("\014")
set.seed(23032000)

### Evaluation of prediction on simulations thorugh boxplots of En and Rn
###

dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"
dir_res = paste0(dir_w,"/Test_domain1D/Artificial_data/results")


#in which folder the result of the prediction are
path_res_pred = paste0(dir_res,"/results_prediction")
#if you want to save the result 
save_res = TRUE
#where to store the results
path_stor_res = paste0(paste0(dir_res,"/results_plot_errors"))  

prediction_method = c("PPC", "KE", "KEI", "MP", "NP", "EX")

for (pred_met in prediction_method) {

  files <- list.files(path = paste0(path_res_pred,paste0("/",pred_met)), full.names = TRUE)
  for (file in files) {
    load(file)
  }
  
}


###################
## BoxPlot of En ##
###################
err_en <- c(err.naive_en, err.perf_en, err.mean_en, err.dEK_en, err.dEKI_en, err.dPPC.KO_en, err.dPPC.KO_en2)
method <- rep(c("naive","perfect", "mean", "EK", "EKI", "PPC-KO", "PPC-KO2"), each=N)
En <- data.frame(method, err_en)
method_order<- c("naive", "perfect", "mean", "EK", "EKI", "PPC-KO", "PPC-KO2")
En.box <- En %>% mutate(method=factor(x=method, levels=method_order))

##grouped boxplot
quartz()
pgplot <- ggplot(En.box, aes(x=method, y=err_en, fill=method)) + 
  geom_boxplot() + ggtitle(name_kernel)
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



###################
## BoxPlot of Rn ##
###################
err_rn <- c(err.naive_rn, err.perf_rn, err.mean_rn, err.dEK_rn, err.dEKI_rn, err.dPPC.KO_rn, err.dPPC.KO_rn2)
method <- rep(c("naive","perfect", "mean", "EK", "EKI", "PPC-KO", "PPC-KO2"), each=N)
Rn <- data.frame(method, err_rn)
method_order<- c("naive", "perfect", "mean", "EK", "EKI", "PPC-KO", "PPC-KO2")
Rn.box <- Rn %>% mutate(method=factor(x=method, levels=method_order))

##grouped boxplot
quartz()
pgplot <- ggplot(Rn.box, aes(x=method, y=err_rn, fill=method)) + 
  geom_boxplot()  + ggtitle(name_kernel)
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