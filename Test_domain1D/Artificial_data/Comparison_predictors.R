rm(list=ls())
graphics.off()
cat("\014")
set.seed(23032000)


##############################################################
#### Evaluating predictors as indicated in the readme     ####
##############################################################

#put here the path of the local copy of the directory
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"
#if you want to save the result 
save_res = TRUE
format = ".jpg"


#in which folder the result of the prediction are
dir_res = paste0(dir_w,"/Test_domain1D/Artificial_data/results")
path_res_pred = paste0(dir_res,"/results_prediction")


#where to store the results
path_stor_res = paste0(paste0(dir_res,"/results_plot_errors"))  


#predictors used
prediction_method = c("PPC", "KE", "KEI", "MP", "NP", "EX")
#loading predictions
for (pred_met in prediction_method) {

  files <- list.files(path = paste0(path_res_pred,paste0("/",pred_met)), full.names = TRUE)
  for (file in files) {
    load(file)
  }
  
}

y_min = 0.0
y_max = 1.3


##----- Gaussian Kernel, norm 0.5-----
kernel_name = "gau_0_5"
title_err = "Prediction error, gaussian kernel, norm = 0.5"


## BoxPlot of En
en_PPC = res_PPC_gau_0_5$En
en_EK  = res_KE_gau_0_5$En
en_EKI = res_KEI_gau_0_5$En
en_MP  = res_MP_gau_0_5$En
en_NP  = res_gau_0_5$En
en_EX  = res_EX_gau_0_5$En

N = length(en_PPC)


err_en <- c(en_PPC, en_EK, en_EKI, en_MP, en_NP, en_EX)
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
  ggsave(filename = paste0(paste0("en_",kernel_name),format),
         plot = pgplot,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = NA,
         height = NA,
         dpi = 300)}


## BoxPlot of Rn
rn_PPC = res_PPC_gau_0_5$Rn
rn_EK  = res_KE_gau_0_5$Rn
rn_EKI = res_KEI_gau_0_5$Rn
rn_MP  = res_MP_gau_0_5$Rn
rn_NP  = res_gau_0_5$Rn
rn_EX  = res_EX_gau_0_5$Rn

N = length(rn_PPC)


err_rn <- c(rn_PPC, rn_EK, rn_EKI, rn_MP, rn_NP, rn_EX)
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
  ggsave(filename = paste0(paste0("rn_",kernel_name),format),
         plot = pgplot,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = NA,
         height = NA,
         dpi = 300)}





##----- Gaussian Kernel, norm 0.8-----
kernel_name = "gau_0_8"
title_err = "Prediction error, gaussian kernel, norm = 0.8"


## BoxPlot of En
en_PPC = res_PPC_gau_0_8$En
en_EK  = res_KE_gau_0_8$En
en_EKI = res_KEI_gau_0_8$En
en_MP  = res_MP_gau_0_8$En
en_NP  = res_gau_0_8$En
en_EX  = res_EX_gau_0_8$En

N = length(en_PPC)


err_en <- c(en_PPC, en_EK, en_EKI, en_MP, en_NP, en_EX)
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
  ggsave(filename = paste0(paste0("en_",kernel_name),format),
         plot = pgplot,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = NA,
         height = NA,
         dpi = 300)}


## BoxPlot of Rn
rn_PPC = res_PPC_gau_0_8$Rn
rn_EK  = res_KE_gau_0_8$Rn
rn_EKI = res_KEI_gau_0_8$Rn
rn_MP  = res_MP_gau_0_8$Rn
rn_NP  = res_gau_0_8$Rn
rn_EX  = res_EX_gau_0_8$Rn

N = length(rn_PPC)


err_rn <- c(rn_PPC, rn_EK, rn_EKI, rn_MP, rn_NP, rn_EX)
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
  ggsave(filename = paste0(paste0("rn_",kernel_name),format),
         plot = pgplot,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = NA,
         height = NA,
         dpi = 300)}


##----- Identity Kernel, norm 0.5-----
kernel_name = "id_0_5"
title_err = "Prediction error, identity kernel, norm = 0.5"


## BoxPlot of En
en_PPC = res_PPC_id_0_5$En
en_EK  = res_KE_id_0_5$En
en_EKI = res_KEI_id_0_5$En
en_MP  = res_MP_id_0_5$En
en_NP  = res_id_0_5$En
en_EX  = res_EX_id_0_5$En

N = length(en_PPC)


err_en <- c(en_PPC, en_EK, en_EKI, en_MP, en_NP, en_EX)
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
  ggsave(filename = paste0(paste0("en_",kernel_name),format),
         plot = pgplot,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = NA,
         height = NA,
         dpi = 300)}


## BoxPlot of Rn
rn_PPC = res_PPC_id_0_5$Rn
rn_EK  = res_KE_id_0_5$Rn
rn_EKI = res_KEI_id_0_5$Rn
rn_MP  = res_MP_id_0_5$Rn
rn_NP  = res_id_0_5$Rn
rn_EX  = res_EX_id_0_5$Rn

N = length(rn_PPC)


err_rn <- c(rn_PPC, rn_EK, rn_EKI, rn_MP, rn_NP, rn_EX)
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
  ggsave(filename = paste0(paste0("rn_",kernel_name),format),
         plot = pgplot,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = NA,
         height = NA,
         dpi = 300)}





##----- Identity Kernel, norm 0.8-----
kernel_name = "id_0_8"
title_err = "Prediction error, identity kernel, norm = 0.8"


## BoxPlot of En
en_PPC = res_PPC_id_0_8$En
en_EK  = res_KE_id_0_8$En
en_EKI = res_KEI_id_0_8$En
en_MP  = res_MP_id_0_8$En
en_NP  = res_id_0_8$En
en_EX  = res_EX_id_0_8$En

N = length(en_PPC)


err_en <- c(en_PPC, en_EK, en_EKI, en_MP, en_NP, en_EX)
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
  ggsave(filename = paste0(paste0("en_",kernel_name),format),
         plot = pgplot,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = NA,
         height = NA,
         dpi = 300)}


## BoxPlot of Rn
rn_PPC = res_PPC_id_0_8$Rn
rn_EK  = res_KE_id_0_8$Rn
rn_EKI = res_KEI_id_0_8$Rn
rn_MP  = res_MP_id_0_8$Rn
rn_NP  = res_id_0_8$Rn
rn_EX  = res_EX_id_0_8$Rn

N = length(rn_PPC)


err_rn <- c(rn_PPC, rn_EK, rn_EKI, rn_MP, rn_NP, rn_EX)
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
  ggsave(filename = paste0(paste0("rn_",kernel_name),format),
         plot = pgplot,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = NA,
         height = NA,
         dpi = 300)}





##----- Slopint plane t Kernel, norm 0.5-----
kernel_name = "spt_0_5"
title_err = "Prediction error, sloping plane t kernel, norm = 0.5"


## BoxPlot of En
en_PPC = res_PPC_spt_0_5$En
en_EK  = res_KE_spt_0_5$En
en_EKI = res_KEI_spt_0_5$En
en_MP  = res_MP_spt_0_5$En
en_NP  = res_spt_0_5$En
en_EX  = res_EX_spt_0_5$En

N = length(en_PPC)


err_en <- c(en_PPC, en_EK, en_EKI, en_MP, en_NP, en_EX)
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
  ggsave(filename = paste0(paste0("en_",kernel_name),format),
         plot = pgplot,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = NA,
         height = NA,
         dpi = 300)}


## BoxPlot of Rn
rn_PPC = res_PPC_spt_0_5$Rn
rn_EK  = res_KE_spt_0_5$Rn
rn_EKI = res_KEI_spt_0_5$Rn
rn_MP  = res_MP_spt_0_5$Rn
rn_NP  = res_spt_0_5$Rn
rn_EX  = res_EX_spt_0_5$Rn

N = length(rn_PPC)


err_rn <- c(rn_PPC, rn_EK, rn_EKI, rn_MP, rn_NP, rn_EX)
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
  ggsave(filename = paste0(paste0("rn_",kernel_name),format),
         plot = pgplot,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = NA,
         height = NA,
         dpi = 300)}





##----- Slopint plane t Kernel, norm 0.8-----
kernel_name = "spt_0_8"
title_err = "Prediction error, sloping plane t kernel, norm = 0.8"


## BoxPlot of En
en_PPC = res_PPC_spt_0_8$En
en_EK  = res_KE_spt_0_8$En
en_EKI = res_KEI_spt_0_8$En
en_MP  = res_MP_spt_0_8$En
en_NP  = res_spt_0_8$En
en_EX  = res_EX_spt_0_8$En

N = length(en_PPC)


err_en <- c(en_PPC, en_EK, en_EKI, en_MP, en_NP, en_EX)
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
  ggsave(filename = paste0(paste0("en_",kernel_name),format),
         plot = pgplot,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = NA,
         height = NA,
         dpi = 300)}


## BoxPlot of Rn
rn_PPC = res_PPC_spt_0_8$Rn
rn_EK  = res_KE_spt_0_8$Rn
rn_EKI = res_KEI_spt_0_8$Rn
rn_MP  = res_MP_spt_0_8$Rn
rn_NP  = res_spt_0_8$Rn
rn_EX  = res_EX_spt_0_8$Rn

N = length(rn_PPC)


err_rn <- c(rn_PPC, rn_EK, rn_EKI, rn_MP, rn_NP, rn_EX)
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
  ggsave(filename = paste0(paste0("rn_",kernel_name),format),
         plot = pgplot,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = NA,
         height = NA,
         dpi = 300)}





##----- Sloping plane s Kernel, norm 0.5-----
kernel_name = "sps_0_5"
title_err = "Prediction error, sloping plane s kernel, norm = 0.5"


## BoxPlot of En
en_PPC = res_PPC_sps_0_5$En
en_EK  = res_KE_sps_0_5$En
en_EKI = res_KEI_sps_0_5$En
en_MP  = res_MP_sps_0_5$En
en_NP  = res_sps_0_5$En
en_EX  = res_EX_sps_0_5$En

N = length(en_PPC)


err_en <- c(en_PPC, en_EK, en_EKI, en_MP, en_NP, en_EX)
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
  ggsave(filename = paste0(paste0("en_",kernel_name),format),
         plot = pgplot,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = NA,
         height = NA,
         dpi = 300)}


## BoxPlot of Rn
rn_PPC = res_PPC_sps_0_5$Rn
rn_EK  = res_KE_sps_0_5$Rn
rn_EKI = res_KEI_sps_0_5$Rn
rn_MP  = res_MP_sps_0_5$Rn
rn_NP  = res_sps_0_5$Rn
rn_EX  = res_EX_sps_0_5$Rn

N = length(rn_PPC)


err_rn <- c(rn_PPC, rn_EK, rn_EKI, rn_MP, rn_NP, rn_EX)
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
  ggsave(filename = paste0(paste0("rn_",kernel_name),format),
         plot = pgplot,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = NA,
         height = NA,
         dpi = 300)}





##----- Sloping plane s Kernel, norm 0.8-----
kernel_name = "sps_0_8"
title_err = "Prediction error, sloping plane s kernel, norm = 0.8"


## BoxPlot of En
en_PPC = res_PPC_sps_0_8$En
en_EK  = res_KE_sps_0_5$En
en_EKI = res_KEI_sps_0_8$En
en_MP  = res_MP_sps_0_8$En
en_NP  = res_sps_0_8$En
en_EX  = res_EX_sps_0_8$En

N = length(en_PPC)


err_en <- c(en_PPC, en_EK, en_EKI, en_MP, en_NP, en_EX)
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
  ggsave(filename = paste0(paste0("en_",kernel_name),format),
         plot = pgplot,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = NA,
         height = NA,
         dpi = 300)}


## BoxPlot of Rn
rn_PPC = res_PPC_sps_0_8$Rn
rn_EK  = res_KE_sps_0_8$Rn
rn_EKI = res_KEI_sps_0_8$Rn
rn_MP  = res_MP_sps_0_8$Rn
rn_NP  = res_sps_0_8$Rn
rn_EX  = res_EX_sps_0_8$Rn

N = length(rn_PPC)


err_rn <- c(rn_PPC, rn_EK, rn_EKI, rn_MP, rn_NP, rn_EX)
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
  ggsave(filename = paste0(paste0("rn_",kernel_name),format),
         plot = pgplot,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = NA,
         height = NA,
         dpi = 300)}
