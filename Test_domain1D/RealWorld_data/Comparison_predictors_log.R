rm(list=ls())
graphics.off()
cat("\014")
set.seed(23032000)

##################################################################################################################
### Evaluation of log-predictions on simulations thorugh boxplots of En and Rn, and summary statistics of them ###
##################################################################################################################


#put here the path of the local copy of the directory
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"

#if you want to save the result in a folder 
save_res = TRUE
format = ".jpg"


dir_res = paste0(dir_w,"/Test_domain1D/RealWorld_data/results")
#in which folder the result of the prediction are
path_res_pred = paste0(dir_res,"/results_errors_log")
#where to store the results
path_stor_res = paste0(paste0(dir_res,"/results_plot_errors_log")) 


#predictors used
prediction_method = c("PPC", "KE", "KEI", "MP", "NP", "CC")

#load the files with the predictions
for (pred_met in prediction_method) {
  load(paste0(path_res_pred,paste0("/",paste0(pred_met,"_log_err.Rdata"))))
}

#imposing them after visual inspection
y_min = 0.0
y_max = 0.35


## ---- offers -----

#En
mean(err_PPC_log$pred_offer$en)
sd(err_PPC_log$pred_offer$en)

mean(err_KE_log$pred_offer$en)
sd(err_KE_log$pred_offer$en)

mean(err_KEI_log$pred_offer$en)
sd(err_KEI_log$pred_offer$en)

mean(err_MP_log$pred_offer$en)
sd(err_MP_log$pred_offer$en)

mean(err_NP_log$pred_offer$en)
sd(err_NP_log$pred_offer$en)

mean(err_CC_log$pred_offer$en)
sd(err_CC_log$pred_offer$en)

## BoxPlot of En
en_PPC = err_PPC_log$pred_offer$en
en_EK  = err_KE_log$pred_offer$en
en_EKI = err_KEI_log$pred_offer$en
en_MP  = err_MP_log$pred_offer$en
en_NP  = err_NP_log$pred_offer$en
en_CC  = err_CC_log$pred_offer$en


title_err = "Prediction offer log-curve"
N = length(en_PPC)


err_en <- c(en_PPC, en_EK, en_EKI, en_MP, en_NP, en_CC)
method <- rep(prediction_method, each=N)
En <- data.frame(method, err_en)
method_order<- prediction_method
En.box <- En %>% mutate(method=factor(x=method, levels=method_order))

pgplot <- ggplot(En.box, aes(x=method, y=err_en, fill=method)) + 
  geom_boxplot() + ggtitle(title_err)
pgplot <- pgplot +
  theme_bw() + 
  labs(x="", y="En", fill = "") +
  ylim(y_min,y_max) +
  theme(plot.title = element_text(face="bold", hjust=0.5, size=22),
        axis.text.x = element_text(size=22),
        axis.title.x = element_text(size=22),
        axis.text.y = element_text(size=22),
        axis.title.y = element_text(size=22),
        legend.title = element_text(size=22),
        legend.text = element_text(size=22),
        legend.position="none",
        legend.direction = "horizontal") +
  guides(fill=guide_legend(nrow=1, byrow=TRUE))
pgplot + theme(legend.position="none")

if(save_res){
  ggsave(filename = paste0("en_offer_log",format),
         plot = pgplot,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = 10,
         height = 5,
         dpi = 300)}

#Rn
mean(err_PPC_log$pred_offer$rn)
sd(err_PPC_log$pred_offer$rn)

mean(err_KE_log$pred_offer$rn)
sd(err_KE_log$pred_offer$rn)

mean(err_KEI_log$pred_offer$rn)
sd(err_KEI_log$pred_offer$rn)

mean(err_MP_log$pred_offer$rn)
sd(err_MP_log$pred_offer$rn)

mean(err_NP_log$pred_offer$rn)
sd(err_NP_log$pred_offer$rn)

mean(err_CC_log$pred_offer$rn)
sd(err_CC_log$pred_offer$rn)

## BoxPlot of Rn
rn_PPC = err_PPC_log$pred_offer$rn
rn_EK  = err_KE_log$pred_offer$rn
rn_EKI = err_KEI_log$pred_offer$rn
rn_MP  = err_MP_log$pred_offer$rn
rn_NP  = err_NP_log$pred_offer$rn
rn_CC  = err_CC_log$pred_offer$rn


title_err = "Prediction offer log-curve"
N = length(rn_PPC)


err_rn <- c(rn_PPC, rn_EK, rn_EKI, rn_MP, rn_NP, rn_CC)
method <- rep(prediction_method, each=N)
Rn <- data.frame(method, err_rn)
method_order<- prediction_method
Rn.box <- Rn %>% mutate(method=factor(x=method, levels=method_order))

pgplot <- ggplot(Rn.box, aes(x=method, y=err_rn, fill=method)) + 
  geom_boxplot() + ggtitle(title_err)
pgplot <- pgplot +
  theme_bw() + 
  labs(x="", y="Rn", fill = "") +
  ylim(y_min,y_max) +
  theme(plot.title = element_text(face="bold", hjust=0.5, size=22),
        axis.text.x = element_text(size=22),
        axis.title.x = element_text(size=22),
        axis.text.y = element_text(size=22),
        axis.title.y = element_text(size=22),
        legend.title = element_text(size=22),
        legend.text = element_text(size=22),
        legend.position="none",
        legend.direction = "horizontal") +
  guides(fill=guide_legend(nrow=1, byrow=TRUE))
pgplot + theme(legend.position="none")

if(save_res){
  ggsave(filename = paste0("rn_offer_log",format),
         plot = pgplot,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = 10,
         height = 5,
         dpi = 300)}


## ---- demands -----

#En
mean(err_PPC_log$pred_demand$en)
sd(err_PPC_log$pred_demand$en)

mean(err_KE_log$pred_demand$en)
sd(err_KE_log$pred_demand$en)

mean(err_KEI_log$pred_demand$en)
sd(err_KEI_log$pred_demand$en)

mean(err_MP_log$pred_demand$en)
sd(err_MP_log$pred_demand$en)

mean(err_NP_log$pred_demand$en)
sd(err_NP_log$pred_demand$en)

mean(err_CC_log$pred_demand$en)
sd(err_CC_log$pred_demand$en)

## BoxPlot of En
en_PPC = err_PPC_log$pred_demand$en
en_EK  = err_KE_log$pred_demand$en
en_EKI = err_KEI_log$pred_demand$en
en_MP  = err_MP_log$pred_demand$en
en_NP  = err_NP_log$pred_demand$en
en_CC  = err_CC_log$pred_demand$en


title_err = "Prediction demand log-curve"
N = length(en_PPC)


err_en <- c(en_PPC, en_EK, en_EKI, en_MP, en_NP, en_CC)
method <- rep(prediction_method, each=N)
En <- data.frame(method, err_en)
method_order<- prediction_method
En.box <- En %>% mutate(method=factor(x=method, levels=method_order))

pgplot <- ggplot(En.box, aes(x=method, y=err_en, fill=method)) + 
  geom_boxplot() + ggtitle(title_err)
pgplot <- pgplot +
  theme_bw() + 
  labs(x="", y="En", fill = "") +
  ylim(y_min,y_max) +
  theme(plot.title = element_text(face="bold", hjust=0.5, size=22),
        axis.text.x = element_text(size=22),
        axis.title.x = element_text(size=22),
        axis.text.y = element_text(size=22),
        axis.title.y = element_text(size=22),
        legend.title = element_text(size=22),
        legend.text = element_text(size=22),
        legend.position="none",
        legend.direction = "horizontal") +
  guides(fill=guide_legend(nrow=1, byrow=TRUE))
pgplot + theme(legend.position="none")

if(save_res){
  ggsave(filename = paste0("en_demand_log",format),
         plot = pgplot,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = 10,
         height = 5,
         dpi = 300)}

#Rn
mean(err_PPC_log$pred_demand$rn)
sd(err_PPC_log$pred_demand$rn)

mean(err_KE_log$pred_demand$rn)
sd(err_KE_log$pred_demand$rn)

mean(err_KEI_log$pred_demand$rn)
sd(err_KEI_log$pred_demand$rn)

mean(err_MP_log$pred_demand$rn)
sd(err_MP_log$pred_demand$rn)

mean(err_NP_log$pred_demand$rn)
sd(err_NP_log$pred_demand$rn)

mean(err_CC_log$pred_demand$rn)
sd(err_CC_log$pred_demand$rn)

## BoxPlot of Rn
rn_PPC = err_PPC_log$pred_demand$rn
rn_EK  = err_KE_log$pred_demand$rn
rn_EKI = err_KEI_log$pred_demand$rn
rn_MP  = err_MP_log$pred_demand$rn
rn_NP  = err_NP_log$pred_demand$rn
rn_CC  = err_CC_log$pred_demand$rn


title_err = "Prediction demand log-curve"
N = length(rn_PPC)


err_rn <- c(rn_PPC, rn_EK, rn_EKI, rn_MP, rn_NP, rn_CC)
method <- rep(prediction_method, each=N)
Rn <- data.frame(method, err_rn)
method_order<- prediction_method
Rn.box <- Rn %>% mutate(method=factor(x=method, levels=method_order))

pgplot <- ggplot(Rn.box, aes(x=method, y=err_rn, fill=method)) + 
  geom_boxplot() + ggtitle(title_err)
pgplot <- pgplot +
  theme_bw() + 
  labs(x="", y="Rn", fill = "") +
  ylim(y_min,y_max) +
  theme(plot.title = element_text(face="bold", hjust=0.5, size=22),
        axis.text.x = element_text(size=22),
        axis.title.x = element_text(size=22),
        axis.text.y = element_text(size=22),
        axis.title.y = element_text(size=22),
        legend.title = element_text(size=22),
        legend.text = element_text(size=22),
        legend.position="none",
        legend.direction = "horizontal") +
  guides(fill=guide_legend(nrow=1, byrow=TRUE))
pgplot + theme(legend.position="none")

if(save_res){
  ggsave(filename = paste0("rn_demand_log",format),
         plot = pgplot,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = 10,
         height = 5,
         dpi = 300)}
