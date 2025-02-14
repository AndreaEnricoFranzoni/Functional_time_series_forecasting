rm(list=ls())
graphics.off()
cat("\014")
set.seed(23032000)


################################################
#### Compare PPC ex_solver and gep_solver   ####
################################################

#put here the path of the local copy of the directory
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"

# if saving results
save_res = TRUE
format = ".jpg"

dir_res = paste0(dir_w,"/Test_domain2D/RealWorld_data/results")



#in which folder the result of the prediction are
path_res_pred = paste0(dir_res,"/results_prediction")
path_save_res = paste0(dir_w,"/Test_domain2D/RealWorld_data/results/result_plot_comp_PPC_algo")

prediction_method = c("PPC", "PPC_exp_pow")

for (pred_met in prediction_method) {
  
  files <- list.files(path = paste0(path_res_pred,paste0("/",pred_met)), full.names = TRUE)
  for (file in files) {
    load(file)
  }
}


#in which folder the errors are
path_res_err = paste0(dir_res,"/results_prediction_errors")

diff_strat = c("original","diff_1","diff_2")
zone = c("mouth","center")

for (strat_df in diff_strat){
  for (zone_pd in zone){
    files <- list.files(path = paste0(paste0(path_res_err,paste0("/",zone_pd)),paste0("/",strat_df)), full.names = TRUE)
    for (file in files) {
      load(file)
    }
  }
}



## ---- mouth, original ----

zone = "mouth_zone"
agg_dt = ""
title = paste0("Prediction error, ",paste0(zone,paste0(" ",agg_dt)))
title_file_en = paste0(paste0("en_",paste0(zone,agg_dt)),"_ppc_algo")
title_file_rn = paste0(paste0("rn_",paste0(zone,agg_dt)),"_ppc_algo")


en_ppc = err_PPC_mouth$en
rn_ppc = err_PPC_mouth$rn

en_ppc_ep = err_PPC_exp_pow_mouth$en
rn_ppc_ep = err_PPC_exp_pow_mouth$rn

y_max = max(max(en_ppc,en_ppc_ep),max(rn_ppc,rn_ppc_ep))

N = length(en_ppc)

{
  mean(en_ppc)
  sd(en_ppc)
  mean(en_ppc_ep)
  sd(en_ppc_ep)
}

{
  mean(rn_ppc)
  sd(rn_ppc)
  mean(rn_ppc_ep)
  sd(rn_ppc_ep)
}



err_en <- c(en_ppc,en_ppc_ep)
method <- rep(prediction_method, each=N)
En <- data.frame(method, err_en)
method_order<- prediction_method
En.box <- En %>% mutate(method=factor(x=method, levels=method_order))

##grouped boxplot
pgplot <- ggplot(En.box, aes(x=method, y=err_en, fill=method)) + 
  geom_boxplot()  + ggtitle("Prediction error, mouth zone")
pgplot <- pgplot +
  #scale_y_continuous(limits=c(0,0.1)) +
  theme_bw() + 
  labs(x="", y="En", fill = "") +
  ylim(0,y_max) +
  #labs(x="", y=TeX(r'($\frac{1}{N} \; \sum_{j=1}^N (f_{t+1, j}^b - \hat{f}_{t+1,j}^b)^2$)'), fill="Prediction method") +
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
pgplot + 
  theme(legend.position="none")


if(save_res){
  ggsave(filename = paste0(title_file_en,format),
         plot = pgplot,
         device = NULL,
         path = path_save_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}



err_rn <- c(rn_ppc,rn_ppc_ep)
method <- rep(prediction_method, each=N)
Rn <- data.frame(method, err_rn)
method_order<- prediction_method
Rn.box <- Rn %>% mutate(method=factor(x=method, levels=method_order))

##grouped boxplot
pgplot <- ggplot(Rn.box, aes(x=method, y=err_rn, fill=method)) + 
  geom_boxplot()  + ggtitle("Prediction error, mouth zone")
pgplot <- pgplot +
  #scale_y_continuous(limits=c(0,0.1)) +
  theme_bw() + 
  labs(x="", y="Rn", fill = "") +
  ylim(0,y_max) +
  #labs(x="", y=TeX(r'($\frac{1}{N} \; \sum_{j=1}^N (f_{t+1, j}^b - \hat{f}_{t+1,j}^b)^2$)'), fill="Prediction method") +
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
pgplot + 
  theme(legend.position="none")


if(save_res){
  ggsave(filename = paste0(title_file_rn,format),
         plot = pgplot,
         device = NULL,
         path = path_save_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}




## ---- mouth, diff1 ----

title = paste0("Prediction error, mouth zone, one differentiation")
zone = "mouth"
agg_dt = "_diff_1"

title = paste0("Prediction error, ",paste0(zone,paste0(" ",agg_dt)))
title_file_en = paste0(paste0("en_",paste0(zone,agg_dt)),"_ppc_algo")
title_file_rn = paste0(paste0("rn_",paste0(zone,agg_dt)),"_ppc_algo")


en_ppc = err_PPC_mouth_diff_1$en
rn_ppc = err_PPC_mouth_diff_1$rn

en_ppc_ep = err_PPC_exp_pow_mouth_diff_1$en
rn_ppc_ep = err_PPC_exp_pow_mouth_diff_1$rn


y_max = max(max(en_ppc,en_ppc_ep),max(rn_ppc,rn_ppc_ep))

N = length(en_ppc)

{
  mean(en_ppc)
  sd(en_ppc)
  mean(en_ppc_ep)
  sd(en_ppc_ep)
}

{
  mean(rn_ppc)
  sd(rn_ppc)
  mean(rn_ppc_ep)
  sd(rn_ppc_ep)
}



err_en <- c(en_ppc,en_ppc_ep)
method <- rep(prediction_method, each=N)
En <- data.frame(method, err_en)
method_order<- prediction_method
En.box <- En %>% mutate(method=factor(x=method, levels=method_order))

##grouped boxplot
pgplot <- ggplot(En.box, aes(x=method, y=err_en, fill=method)) + 
  geom_boxplot()  + ggtitle("Prediction error, mouth zone, one differentiation")
pgplot <- pgplot +
  #scale_y_continuous(limits=c(0,0.1)) +
  theme_bw() + 
  labs(x="", y="En", fill = "") +
  ylim(0,y_max) +
  #labs(x="", y=TeX(r'($\frac{1}{N} \; \sum_{j=1}^N (f_{t+1, j}^b - \hat{f}_{t+1,j}^b)^2$)'), fill="Prediction method") +
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
pgplot + 
  theme(legend.position="none")


if(save_res){
  ggsave(filename = paste0(title_file_en,format),
         plot = pgplot,
         device = NULL,
         path = path_save_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}



err_rn <- c(rn_ppc,rn_ppc_ep)
method <- rep(prediction_method, each=N)
Rn <- data.frame(method, err_rn)
method_order<- prediction_method
Rn.box <- Rn %>% mutate(method=factor(x=method, levels=method_order))

##grouped boxplot
pgplot <- ggplot(Rn.box, aes(x=method, y=err_rn, fill=method)) + 
  geom_boxplot()  + ggtitle("Prediction error, mouth zone, one differentiation")
pgplot <- pgplot +
  #scale_y_continuous(limits=c(0,0.1)) +
  theme_bw() + 
  labs(x="", y="Rn", fill = "") +
  ylim(0,y_max) +
  #labs(x="", y=TeX(r'($\frac{1}{N} \; \sum_{j=1}^N (f_{t+1, j}^b - \hat{f}_{t+1,j}^b)^2$)'), fill="Prediction method") +
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
pgplot + 
  theme(legend.position="none")


if(save_res){
  ggsave(filename = paste0(title_file_rn,format),
         plot = pgplot,
         device = NULL,
         path = path_save_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}



## ---- mouth, diff2 ----

title = paste0("Prediction error, mouth zone, two differentiations")
zone = "mouth"
agg_dt = "_diff_2"

title = paste0("Prediction error, ",paste0(zone,paste0(" ",agg_dt)))
title_file_en = paste0(paste0("en_",paste0(zone,agg_dt)),"_ppc_algo")
title_file_rn = paste0(paste0("rn_",paste0(zone,agg_dt)),"_ppc_algo")


en_ppc = err_PPC_mouth_diff_2$en
rn_ppc = err_PPC_mouth_diff_2$rn

en_ppc_ep = err_PPC_exp_pow_mouth_diff_2$en
rn_ppc_ep = err_PPC_exp_pow_mouth_diff_2$rn

y_max = max(max(en_ppc,en_ppc_ep),max(rn_ppc,rn_ppc_ep))


N = length(en_ppc)

{
  mean(en_ppc)
  sd(en_ppc)
  mean(en_ppc_ep)
  sd(en_ppc_ep)
}

{
  mean(rn_ppc)
  sd(rn_ppc)
  mean(rn_ppc_ep)
  sd(rn_ppc_ep)
}



err_en <- c(en_ppc,en_ppc_ep)
method <- rep(prediction_method, each=N)
En <- data.frame(method, err_en)
method_order<- prediction_method
En.box <- En %>% mutate(method=factor(x=method, levels=method_order))

##grouped boxplot
pgplot <- ggplot(En.box, aes(x=method, y=err_en, fill=method)) + 
  geom_boxplot()  + ggtitle("Prediction error, mouth zone, two differentiations")
pgplot <- pgplot +
  #scale_y_continuous(limits=c(0,0.1)) +
  theme_bw() + 
  labs(x="", y="En", fill = "") +
  ylim(0,y_max) +
  #labs(x="", y=TeX(r'($\frac{1}{N} \; \sum_{j=1}^N (f_{t+1, j}^b - \hat{f}_{t+1,j}^b)^2$)'), fill="Prediction method") +
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
pgplot + 
  theme(legend.position="none")


if(save_res){
  ggsave(filename = paste0(title_file_en,format),
         plot = pgplot,
         device = NULL,
         path = path_save_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}



err_rn <- c(rn_ppc,rn_ppc_ep)
method <- rep(prediction_method, each=N)
Rn <- data.frame(method, err_rn)
method_order<- prediction_method
Rn.box <- Rn %>% mutate(method=factor(x=method, levels=method_order))

##grouped boxplot
pgplot <- ggplot(Rn.box, aes(x=method, y=err_rn, fill=method)) + 
  geom_boxplot()  + ggtitle("Prediction error, mouth zone, two differentiations")
pgplot <- pgplot +
  #scale_y_continuous(limits=c(0,0.1)) +
  theme_bw() + 
  labs(x="", y="Rn", fill = "") +
  ylim(0,y_max) +
  #labs(x="", y=TeX(r'($\frac{1}{N} \; \sum_{j=1}^N (f_{t+1, j}^b - \hat{f}_{t+1,j}^b)^2$)'), fill="Prediction method") +
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
pgplot + 
  theme(legend.position="none")


if(save_res){
  ggsave(filename = paste0(title_file_rn,format),
         plot = pgplot,
         device = NULL,
         path = path_save_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}



## ---- center, original ----

zone = "center_zone"
agg_dt = ""
title = paste0("Prediction error, ",paste0(zone,paste0(" ",agg_dt)))
title_file_en = paste0(paste0("en_",paste0(zone,agg_dt)),"_ppc_algo")
title_file_rn = paste0(paste0("rn_",paste0(zone,agg_dt)),"_ppc_algo")


en_ppc = err_PPC_center$en
rn_ppc = err_PPC_center$rn

en_ppc_ep = err_PPC_exp_pow_center$en
rn_ppc_ep = err_PPC_exp_pow_center$rn

y_max = max(max(en_ppc,en_ppc_ep),max(rn_ppc,rn_ppc_ep))



N = length(en_ppc)

{
  mean(en_ppc)
  sd(en_ppc)
  mean(en_ppc_ep)
  sd(en_ppc_ep)
}

{
  mean(rn_ppc)
  sd(rn_ppc)
  mean(rn_ppc_ep)
  sd(rn_ppc_ep)
}



err_en <- c(en_ppc,en_ppc_ep)
method <- rep(prediction_method, each=N)
En <- data.frame(method, err_en)
method_order<- prediction_method
En.box <- En %>% mutate(method=factor(x=method, levels=method_order))

##grouped boxplot
pgplot <- ggplot(En.box, aes(x=method, y=err_en, fill=method)) + 
  geom_boxplot()  + ggtitle("Prediction error, center zone")
pgplot <- pgplot +
  #scale_y_continuous(limits=c(0,0.1)) +
  theme_bw() + 
  labs(x="", y="En", fill = "") +
  ylim(0,y_max) +
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
  ggsave(filename = paste0(title_file_en,format),
         plot = pgplot,
         device = NULL,
         path = path_save_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}



err_rn <- c(rn_ppc,rn_ppc_ep)
method <- rep(prediction_method, each=N)
Rn <- data.frame(method, err_rn)
method_order<- prediction_method
Rn.box <- Rn %>% mutate(method=factor(x=method, levels=method_order))

##grouped boxplot
pgplot <- ggplot(Rn.box, aes(x=method, y=err_rn, fill=method)) + 
  geom_boxplot()  + ggtitle("Prediction error, center zone")
pgplot <- pgplot +
  #scale_y_continuous(limits=c(0,0.1)) +
  theme_bw() + 
  labs(x="", y="Rn", fill = "") +
  ylim(0,y_max) +
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
  ggsave(filename = paste0(title_file_rn,format),
         plot = pgplot,
         device = NULL,
         path = path_save_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}




## ---- mouth, diff1 ----

title = paste0("Prediction error, center zone, one differentiation")
zone = "center"
agg_dt = "_diff_1"

title = paste0("Prediction error, ",paste0(zone,paste0(" ",agg_dt)))
title_file_en = paste0(paste0("en_",paste0(zone,agg_dt)),"_ppc_algo")
title_file_rn = paste0(paste0("rn_",paste0(zone,agg_dt)),"_ppc_algo")


en_ppc = err_PPC_center_diff_1$en
rn_ppc = err_PPC_center_diff_1$rn

en_ppc_ep = err_PPC_exp_pow_center_diff_1$en
rn_ppc_ep = err_PPC_exp_pow_center_diff_1$rn

y_max = max(max(en_ppc,en_ppc_ep),max(rn_ppc,rn_ppc_ep))


N = length(en_ppc)

{
  mean(en_ppc)
  sd(en_ppc)
  mean(en_ppc_ep)
  sd(en_ppc_ep)
}

{
  mean(rn_ppc)
  sd(rn_ppc)
  mean(rn_ppc_ep)
  sd(rn_ppc_ep)
}



err_en <- c(en_ppc,en_ppc_ep)
method <- rep(prediction_method, each=N)
En <- data.frame(method, err_en)
method_order<- prediction_method
En.box <- En %>% mutate(method=factor(x=method, levels=method_order))

##grouped boxplot
pgplot <- ggplot(En.box, aes(x=method, y=err_en, fill=method)) + 
  geom_boxplot()  + ggtitle("Prediction error, center zone, one differentiation")
pgplot <- pgplot +
  #scale_y_continuous(limits=c(0,0.1)) +
  theme_bw() + 
  labs(x="", y="En", fill = "") +
  ylim(0,y_max) +
  #labs(x="", y=TeX(r'($\frac{1}{N} \; \sum_{j=1}^N (f_{t+1, j}^b - \hat{f}_{t+1,j}^b)^2$)'), fill="Prediction method") +
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
pgplot + 
  theme(legend.position="none")


if(save_res){
  ggsave(filename = paste0(title_file_en,format),
         plot = pgplot,
         device = NULL,
         path = path_save_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}



err_rn <- c(rn_ppc,rn_ppc_ep)
method <- rep(prediction_method, each=N)
Rn <- data.frame(method, err_rn)
method_order<- prediction_method
Rn.box <- Rn %>% mutate(method=factor(x=method, levels=method_order))

##grouped boxplot
pgplot <- ggplot(Rn.box, aes(x=method, y=err_rn, fill=method)) + 
  geom_boxplot()  + ggtitle("Prediction error, center zone, one differentiation")
pgplot <- pgplot +
  #scale_y_continuous(limits=c(0,0.1)) +
  theme_bw() + 
  labs(x="", y="Rn", fill = "") +
  ylim(0,y_max) +
  #labs(x="", y=TeX(r'($\frac{1}{N} \; \sum_{j=1}^N (f_{t+1, j}^b - \hat{f}_{t+1,j}^b)^2$)'), fill="Prediction method") +
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
pgplot + 
  theme(legend.position="none")


if(save_res){
  ggsave(filename = paste0(title_file_rn,format),
         plot = pgplot,
         device = NULL,
         path = path_save_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}



## ---- mouth, diff2 ----

title = paste0("Prediction error, center zone, two differentiations")
zone = "center"
agg_dt = "_diff_2"

title = paste0("Prediction error, ",paste0(zone,paste0(" ",agg_dt)))
title_file_en = paste0(paste0("en_",paste0(zone,agg_dt)),"_ppc_algo")
title_file_rn = paste0(paste0("rn_",paste0(zone,agg_dt)),"_ppc_algo")


en_ppc = err_PPC_center_diff_2$en
rn_ppc = err_PPC_center_diff_2$rn

en_ppc_ep = err_PPC_exp_pow_center_diff_2$en
rn_ppc_ep = err_PPC_exp_pow_center_diff_2$rn

y_max = max(max(en_ppc,en_ppc_ep),max(rn_ppc,rn_ppc_ep))


N = length(en_ppc)

{
  mean(en_ppc)
  sd(en_ppc)
  mean(en_ppc_ep)
  sd(en_ppc_ep)
}

{
  mean(rn_ppc)
  sd(rn_ppc)
  mean(rn_ppc_ep)
  sd(rn_ppc_ep)
}



err_en <- c(en_ppc,en_ppc_ep)
method <- rep(prediction_method, each=N)
En <- data.frame(method, err_en)
method_order<- prediction_method
En.box <- En %>% mutate(method=factor(x=method, levels=method_order))

##grouped boxplot
pgplot <- ggplot(En.box, aes(x=method, y=err_en, fill=method)) + 
  geom_boxplot()  + ggtitle("Prediction error, center zone, two differentiations")
pgplot <- pgplot +
  #scale_y_continuous(limits=c(0,0.1)) +
  theme_bw() + 
  labs(x="", y="En", fill = "") +
  ylim(0,y_max) +
  #labs(x="", y=TeX(r'($\frac{1}{N} \; \sum_{j=1}^N (f_{t+1, j}^b - \hat{f}_{t+1,j}^b)^2$)'), fill="Prediction method") +
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
pgplot + 
  theme(legend.position="none")


if(save_res){
  ggsave(filename = paste0(title_file_en,format),
         plot = pgplot,
         device = NULL,
         path = path_save_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}



err_rn <- c(rn_ppc,rn_ppc_ep)
method <- rep(prediction_method, each=N)
Rn <- data.frame(method, err_rn)
method_order<- prediction_method
Rn.box <- Rn %>% mutate(method=factor(x=method, levels=method_order))

##grouped boxplot
pgplot <- ggplot(Rn.box, aes(x=method, y=err_rn, fill=method)) + 
  geom_boxplot()  + ggtitle("Prediction error, center zone, two differentiations")
pgplot <- pgplot +
  #scale_y_continuous(limits=c(0,0.1)) +
  theme_bw() + 
  labs(x="", y="Rn", fill = "") +
  ylim(0,y_max) +
  #labs(x="", y=TeX(r'($\frac{1}{N} \; \sum_{j=1}^N (f_{t+1, j}^b - \hat{f}_{t+1,j}^b)^2$)'), fill="Prediction method") +
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
pgplot + 
  theme(legend.position="none")


if(save_res){
  ggsave(filename = paste0(title_file_rn,format),
         plot = pgplot,
         device = NULL,
         path = path_save_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}
