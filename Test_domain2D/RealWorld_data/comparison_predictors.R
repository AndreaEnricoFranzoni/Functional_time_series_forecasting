rm(list=ls())
graphics.off()
cat("\014")

set.seed(23032000)


dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"
dir_res = paste0(dir_w,"/Test_domain2D/RealWorld_data/results")


#in which folder the result of the prediction are
path_res_err = paste0(dir_res,"/results_prediction_errors")
#if you want to save the result 
save_res = TRUE
format = ".jpg"
#where to store the results
path_save_res = paste0(paste0(dir_res,"/results_plot_errors"))  

prediction_method = c("PPC", "MP", "NP")

# load the prediction errors
for (zone in c("center","mouth")) {
  for (data_agg in c("original","diff_1","diff_2")) {
    folder_path <- paste0(path_res_err,paste0(paste0("/",zone),paste0("/",data_agg)))
    files <- list.files(path = folder_path, full.names = TRUE) 
    for (file in files) {
      load(file) 
    }
  }
}


## ---- mouth, original ----

zone = "mouth"
agg_dt = ""
title = paste0("Prediction error, ",paste0(zone,paste0(" ",agg_dt)))
title_file_en = paste0("en_",paste0(zone,agg_dt))
title_file_rn = paste0("rn_",paste0(zone,agg_dt))


en_ppc = err_PPC_mouth$en
rn_ppc = err_PPC_mouth$rn

en_mp = err_MP_mouth$en
rn_mp = err_MP_mouth$rn

en_np = err_NP_mouth$en
rn_np = err_NP_mouth$rn

N = length(en_ppc)

{
  mean(en_ppc)
  sd(en_ppc)
  mean(en_mp)
  sd(en_mp)
  mean(en_np)
  sd(en_np)
}

{
  mean(rn_ppc)
  sd(rn_ppc)
  mean(rn_mp)
  sd(rn_mp)
  mean(rn_np)
  sd(rn_np)
}



err_en <- c(en_ppc,en_mp,en_np)
method <- rep(prediction_method, each=N)
En <- data.frame(method, err_en)
method_order<- prediction_method
En.box <- En %>% mutate(method=factor(x=method, levels=method_order))

##grouped boxplot
pgplot <- ggplot(En.box, aes(x=method, y=err_en, fill=method)) + 
  geom_boxplot()  + ggtitle(title)
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
  ggsave(filename = paste0(title_file_en,format),
         plot = pgplot,
         device = NULL,
         path = path_save_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}



err_rn <- c(rn_ppc,rn_mp,rn_np)
method <- rep(prediction_method, each=N)
Rn <- data.frame(method, err_rn)
method_order<- prediction_method
Rn.box <- Rn %>% mutate(method=factor(x=method, levels=method_order))

##grouped boxplot
pgplot <- ggplot(Rn.box, aes(x=method, y=err_en, fill=method)) + 
  geom_boxplot()  + ggtitle(title)
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
  ggsave(filename = paste0(title_file_rn,format),
         plot = pgplot,
         device = NULL,
         path = path_save_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}




## ---- mouth, diff 1 ----

title = paste0("Prediction error, mouth, diff 1")
zone = "mouth"
agg_dt = "_diff_1"

title_file_en = paste0("en_",paste0(zone,agg_dt))
title_file_rn = paste0("rn_",paste0(zone,agg_dt))


en_ppc = err_PPC_mouth_diff_1$en
rn_ppc = err_PPC_mouth_diff_1$rn

en_mp = err_MP_mouth_diff_1$en
rn_mp = err_MP_mouth_diff_1$rn

en_np = err_NP_mouth_diff_1$en
rn_np = err_NP_mouth_diff_1$rn

N = length(en_ppc)

{
  mean(en_ppc)
  sd(en_ppc)
  mean(en_mp)
  sd(en_mp)
  mean(en_np)
  sd(en_np)
}

{
  mean(rn_ppc)
  sd(rn_ppc)
  mean(rn_mp)
  sd(rn_mp)
  mean(rn_np)
  sd(rn_np)
}



err_en <- c(en_ppc,en_mp,en_np)
method <- rep(prediction_method, each=N)
En <- data.frame(method, err_en)
method_order<- prediction_method
En.box <- En %>% mutate(method=factor(x=method, levels=method_order))

##grouped boxplot
pgplot <- ggplot(En.box, aes(x=method, y=err_en, fill=method)) + 
  geom_boxplot()  + ggtitle(title)
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
  ggsave(filename = paste0(title_file_en,format),
         plot = pgplot,
         device = NULL,
         path = path_save_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}



err_rn <- c(rn_ppc,rn_mp,rn_np)
method <- rep(prediction_method, each=N)
Rn <- data.frame(method, err_rn)
method_order<- prediction_method
Rn.box <- Rn %>% mutate(method=factor(x=method, levels=method_order))

##grouped boxplot
pgplot <- ggplot(Rn.box, aes(x=method, y=err_en, fill=method)) + 
  geom_boxplot()  + ggtitle(title)
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
  ggsave(filename = paste0(title_file_rn,format),
         plot = pgplot,
         device = NULL,
         path = path_save_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}


## ---- mouth, diff 2 ----

title = paste0("Prediction error, mouth, diff 2")
zone = "mouth"
agg_dt = "_diff_2"

title_file_en = paste0("en_",paste0(zone,agg_dt))
title_file_rn = paste0("rn_",paste0(zone,agg_dt))


en_ppc = err_PPC_mouth_diff_2$en
rn_ppc = err_PPC_mouth_diff_2$rn

en_mp = err_MP_mouth_diff_2$en
rn_mp = err_MP_mouth_diff_2$rn

en_np = err_NP_mouth_diff_2$en
rn_np = err_NP_mouth_diff_2$rn

N = length(en_ppc)

{
  mean(en_ppc)
  sd(en_ppc)
  mean(en_mp)
  sd(en_mp)
  mean(en_np)
  sd(en_np)
}

{
  mean(rn_ppc)
  sd(rn_ppc)
  mean(rn_mp)
  sd(rn_mp)
  mean(rn_np)
  sd(rn_np)
}



err_en <- c(en_ppc,en_mp,en_np)
method <- rep(prediction_method, each=N)
En <- data.frame(method, err_en)
method_order<- prediction_method
En.box <- En %>% mutate(method=factor(x=method, levels=method_order))

##grouped boxplot
pgplot <- ggplot(En.box, aes(x=method, y=err_en, fill=method)) + 
  geom_boxplot()  + ggtitle(title)
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
  ggsave(filename = paste0(title_file_en,format),
         plot = pgplot,
         device = NULL,
         path = path_save_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}



err_rn <- c(rn_ppc,rn_mp,rn_np)
method <- rep(prediction_method, each=N)
Rn <- data.frame(method, err_rn)
method_order<- prediction_method
Rn.box <- Rn %>% mutate(method=factor(x=method, levels=method_order))

##grouped boxplot
pgplot <- ggplot(Rn.box, aes(x=method, y=err_en, fill=method)) + 
  geom_boxplot()  + ggtitle(title)
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
  ggsave(filename = paste0(title_file_rn,format),
         plot = pgplot,
         device = NULL,
         path = path_save_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}