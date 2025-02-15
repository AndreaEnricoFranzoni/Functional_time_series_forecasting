rm(list=ls())
graphics.off()
cat("\014")
set.seed(23032000)

######################################################################
#### PPC robustness if non FAR(1) process is generating the data  ####
######################################################################



#put here the path of the local copy of the directory
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"

#if you want to save the result 
save_res = TRUE
format_file = ".jpg"


#where to store the results
dir_stor_res = "/Test_PPC_robustness/no_far1/results/plot_errors"
path_stor_res = paste0(dir_w,dir_stor_res)


#where results are
dir_res = paste0(dir_w,"/Test_PPC_robustness/no_far1/results")
path_res_pred = paste0(dir_res,"/predictions")

# upload reults
files <- list.files(path = path_res_pred, full.names = TRUE)
for (file in files) {
  load(file)
}

prediction_method = c("PPC","EX")



#evaluate the distribution of the errors, with mean, sd and boxplots

#En

en_PPC = res_PPC_no_far1$En
en_EX  = res_EX_no_far1$En

N = length(en_PPC)

summary(en_PPC)
mean(en_PPC)
sd(en_PPC)


summary(en_EX)
mean(en_EX)
sd(en_EX)

rn_PPC = res_PPC_no_far1$Rn
rn_EX  = res_EX_no_far1$Rn


summary(rn_PPC)
mean(rn_PPC)
sd(rn_PPC)


summary(rn_EX)
mean(rn_EX)
sd(rn_EX)

N = length(rn_PPC)


y_max = max(max(en_PPC,rn_PPC),max(en_EX,rn_EX))


plot_name = "no_far1_proc"
## BoxPlot of En
title_err = "Prediction error, VAR(2) process"



err_en <- c(en_PPC, en_EX)
method <- rep(prediction_method, each=N)
En <- data.frame(method, err_en)
method_order<- prediction_method
En.box <- En %>% mutate(method=factor(x=method, levels=method_order))

pgplot <- ggplot(En.box, aes(x=method, y=err_en, fill=method)) + 
  geom_boxplot() 
#+ ggtitle(title_err)
pgplot <- pgplot +
  theme_bw() + 
  labs(x="", y="En", fill = "") +
  ylim(0,y_max) +
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
  ggsave(filename = paste0(paste0("en_",plot_name),format_file),
         plot = pgplot,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = NA,
         height = NA,
         dpi = 300)}



## BoxPlot of Rn
title_err = "Prediction error, VAR(2) process"



err_rn <- c(rn_PPC, rn_EX)
method <- rep(prediction_method, each=N)
Rn <- data.frame(method, err_rn)
method_order<- prediction_method
Rn.box <- Rn %>% mutate(method=factor(x=method, levels=method_order))

pgplot <- ggplot(Rn.box, aes(x=method, y=err_rn, fill=method)) + 
  geom_boxplot()
#+ ggtitle(title_err)
pgplot <- pgplot +
  theme_bw() + 
  labs(x="", y="Rn", fill = "") +
  ylim(0,y_max) +
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
  ggsave(filename = paste0(paste0("rn_",plot_name),format_file),
         plot = pgplot,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = NA,
         height = NA,
         dpi = 300)}
