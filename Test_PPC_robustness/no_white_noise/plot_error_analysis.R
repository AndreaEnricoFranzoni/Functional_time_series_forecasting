rm(list=ls())
graphics.off()
cat("\014")
set.seed(23032000)

#################################################################################
#### PPC robustness if no white noise errors process is generating the data  ####
#################################################################################



#put here the path of the local copy of the directory
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"

#if you want to save the result 
save_res = TRUE
format_file = ".jpg"


#where to store the results
dir_stor_res = "/Test_PPC_robustness/no_white_noise/results/plot_errors"
path_stor_res_all_story_bxplt_err     = paste0(dir_w,paste0(dir_stor_res,"/train_all_story"))
path_stor_res_moving_window_bxplt_err = paste0(dir_w,paste0(dir_stor_res,"/train_moving_window"))
dir_stor_res2 = "/Test_PPC_robustness/no_white_noise/results/error_dynamic_plot"
path_stor_res_all_story_dyn_err     = paste0(dir_w,paste0(dir_stor_res2,"/train_all_story"))
path_stor_res_moving_window_dyn_err = paste0(dir_w,paste0(dir_stor_res2,"/train_moving_window"))


##----train with all the story----
#where errors are
dir_res = paste0(dir_w,"/Test_PPC_robustness/no_white_noise/results")
path_res_pred = paste0(dir_res,"/predictions/train_all_story")

# upload reults
files <- list.files(path = path_res_pred, full.names = TRUE)
for (file in files) {
  load(file)
}

prediction_method = c("PPC","EX")

plot_name = "no_white_noise"


height_bx_plt = 3
width_bx_plt = 7

height_dyn_plt = 3
width_dyn_plt = 7


## -----Gaussian kernel-----
title_err = "Prediction error, no white noise, gaussian kernel"

#evaluate the distribution of the errors, with mean, sd and boxplots


#En
en_PPC = res_PPC_gau_no_wn$En
en_EX  = res_EX_gau_no_wn$En
N = length(en_PPC)

summary(en_PPC)
mean(en_PPC)
sd(en_PPC)

summary(en_EX)
mean(en_EX)
sd(en_EX)


#Rn
rn_PPC = res_PPC_gau_no_wn$Rn
rn_EX  = res_EX_gau_no_wn$Rn
N = length(rn_PPC)

summary(rn_PPC)
mean(rn_PPC)
sd(rn_PPC)

summary(rn_EX)
mean(rn_EX)
sd(rn_EX)

y_max = max(c(rn_PPC,en_PPC,rn_EX,en_EX))


## BoxPlot of En
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
  ylim(0,y_max)+
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
  ggsave(filename = paste0(paste0("en_gau_",plot_name),format_file),
         plot = pgplot,
         device = NULL,
         path = path_stor_res_all_story_bxplt_err,
         scale = 1,
         width = width_bx_plt,
         height = height_bx_plt,
         dpi = 300)}


#En dynamics
data <- data.frame(
  x = 51:300,
  y1 = en_PPC,
  y2 = en_EX
)

plt_err_dy = ggplot(data) +
  geom_line(aes(x = x, y = y1, color = "PPC"), size = 0.3) +
  geom_point(aes(x = x, y = y1, color = "PPC"), size = 0.5) +
  geom_line(aes(x = x, y = y2, color = "EX"), size = 0.3) +
  geom_point(aes(x = x, y = y2, color = "EX"), size = 0.5) +
  scale_color_manual(
    name = "",
    values = c("PPC" = "blue", "EX" = "red")
  ) +
  labs(
    title = "",
    x = "Predicted instant",
    y = "En"
  ) +
  theme_minimal()

print(plt_err_dy)

if(save_res){
  ggsave(filename = paste0(paste0("en_gau_dyn_",plot_name),format_file),
         plot = plt_err_dy,
         device = NULL,
         path = path_stor_res_all_story_dyn_err,
         scale = 1,
         width = width_dyn_plt,
         height = height_dyn_plt,
         dpi = 300)}


## BoxPlot of Rn
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
  ylim(0,y_max)+
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
  ggsave(filename = paste0(paste0("rn_gau_",plot_name),format_file),
         plot = pgplot,
         device = NULL,
         path = path_stor_res_all_story_bxplt_err,
         scale = 1,
         width = width_bx_plt,
         height = height_bx_plt,
         dpi = 300)}


#Rn dynamics
data <- data.frame(
  x = 51:300,
  y1 = rn_PPC,
  y2 = rn_EX
)

plt_err_dy = ggplot(data) +
  geom_line(aes(x = x, y = y1, color = "PPC"), size = 0.3) +
  geom_point(aes(x = x, y = y1, color = "PPC"), size = 0.5) +
  geom_line(aes(x = x, y = y2, color = "EX"), size = 0.3) +
  geom_point(aes(x = x, y = y2, color = "EX"), size = 0.5) +
  scale_color_manual(
    name = "",
    values = c("PPC" = "blue", "EX" = "red")
  ) +
  ylim(0,y_max)+
  labs(
    title = "",
    x = "Predicted instant",
    y = "Rn"
  ) +
  theme_minimal()

print(plt_err_dy)

if(save_res){
  ggsave(filename = paste0(paste0("rn_gau_dyn_",plot_name),format_file),
         plot = plt_err_dy,
         device = NULL,
         path = path_stor_res_all_story_dyn_err,
         scale = 1,
         width = width_dyn_plt,
         height = height_dyn_plt,
         dpi = 300)}




## -----Idenity kernel-----
title_err = "Prediction error, no white noise, identity kernel"

#evaluate the distribution of the errors, with mean, sd and boxplots


#En
en_PPC = res_PPC_id_no_wn$En
en_EX  = res_EX_id_no_wn$En
N = length(en_PPC)

summary(en_PPC)
mean(en_PPC)
sd(en_PPC)

summary(en_EX)
mean(en_EX)
sd(en_EX)


#Rn
rn_PPC = res_PPC_id_no_wn$Rn
rn_EX  = res_EX_id_no_wn$Rn
N = length(rn_PPC)

summary(rn_PPC)
mean(rn_PPC)
sd(rn_PPC)

summary(rn_EX)
mean(rn_EX)
sd(rn_EX)


y_max = max(c(rn_PPC,en_PPC,rn_EX,en_EX))


## BoxPlot of En
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
  ggsave(filename = paste0(paste0("en_id_",plot_name),format_file),
         plot = pgplot,
         device = NULL,
         path = path_stor_res_all_story_bxplt_err,
         scale = 1,
         width = width_bx_plt,
         height = height_bx_plt,
         dpi = 300)}


#En dynamics
data <- data.frame(
  x = 51:300,
  y1 = en_PPC,
  y2 = en_EX
)

plt_err_dy = ggplot(data) +
  geom_line(aes(x = x, y = y1, color = "PPC"), size = 0.3) +
  geom_point(aes(x = x, y = y1, color = "PPC"), size = 0.5) +
  geom_line(aes(x = x, y = y2, color = "EX"), size = 0.3) +
  geom_point(aes(x = x, y = y2, color = "EX"), size = 0.5) +
  scale_color_manual(
    name = "",
    values = c("PPC" = "blue", "EX" = "red")
  ) +
  ylim(0,y_max) +
  labs(
    title = "",
    x = "Predicted instant",
    y = "En"
  ) +
  theme_minimal()

print(plt_err_dy)

if(save_res){
  ggsave(filename = paste0(paste0("en_id_dyn_",plot_name),format_file),
         plot = plt_err_dy,
         device = NULL,
         path = path_stor_res_all_story_dyn_err,
         scale = 1,
         width = width_dyn_plt,
         height = height_dyn_plt,
         dpi = 300)}


## BoxPlot of Rn
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
  ylim(0,y_max) +
  labs(x="", y="Rn", fill = "") +
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
  ggsave(filename = paste0(paste0("rn_id_",plot_name),format_file),
         plot = pgplot,
         device = NULL,
         path = path_stor_res_all_story_bxplt_err,
         scale = 1,
         width = width_bx_plt,
         height = height_bx_plt,
         dpi = 300)}


#Rn dynamics
data <- data.frame(
  x = 51:300,
  y1 = rn_PPC,
  y2 = rn_EX
)

plt_err_dy = ggplot(data) +
  geom_line(aes(x = x, y = y1, color = "PPC"), size = 0.3) +
  geom_point(aes(x = x, y = y1, color = "PPC"), size = 0.5) +
  geom_line(aes(x = x, y = y2, color = "EX"), size = 0.3) +
  geom_point(aes(x = x, y = y2, color = "EX"), size = 0.5) +
  scale_color_manual(
    name = "",
    values = c("PPC" = "blue", "EX" = "red")
  ) +
  ylim(0,y_max) +
  labs(
    title = "",
    x = "Predicted instant",
    y = "Rn"
  ) +
  theme_minimal()

plt_err_dy

if(save_res){
  ggsave(filename = paste0(paste0("rn_id_dyn_",plot_name),format_file),
         plot = plt_err_dy,
         device = NULL,
         path = path_stor_res_all_story_dyn_err,
         scale = 1,
         width = width_dyn_plt,
         height = height_dyn_plt,
         dpi = 300)}




## -----Sloping plane t kernel-----
title_err = "Prediction error, no white noise, slopint plane t kernel"

#evaluate the distribution of the errors, with mean, sd and boxplots


#En
en_PPC = res_PPC_spt_no_wn$En
en_EX  = res_EX_spt_no_wn$En
N = length(en_PPC)

summary(en_PPC)
mean(en_PPC)
sd(en_PPC)

summary(en_EX)
mean(en_EX)
sd(en_EX)


#Rn
rn_PPC = res_PPC_spt_no_wn$Rn
rn_EX  = res_EX_spt_no_wn$Rn
N = length(rn_PPC)

summary(rn_PPC)
mean(rn_PPC)
sd(rn_PPC)

summary(rn_EX)
mean(rn_EX)
sd(rn_EX)

y_max = max(c(rn_PPC,en_PPC,rn_EX,en_EX))


## BoxPlot of En
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
  ylim(0,y_max) +
  labs(x="", y="En", fill = "") +
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
  ggsave(filename = paste0(paste0("en_spt_",plot_name),format_file),
         plot = pgplot,
         device = NULL,
         path = path_stor_res_all_story_bxplt_err,
         scale = 1,
         width = width_bx_plt,
         height = height_bx_plt,
         dpi = 300)}


#En dynamics
data <- data.frame(
  x = 51:300,
  y1 = en_PPC,
  y2 = en_EX
)

plt_err_dy = ggplot(data) +
  geom_line(aes(x = x, y = y1, color = "PPC"), size = 0.3) +
  geom_point(aes(x = x, y = y1, color = "PPC"), size = 0.5) +
  geom_line(aes(x = x, y = y2, color = "EX"), size = 0.3) +
  geom_point(aes(x = x, y = y2, color = "EX"), size = 0.5) +
  scale_color_manual(
    name = "",
    values = c("PPC" = "blue", "EX" = "red")
  ) +
  ylim(0,y_max) +
  labs(
    title = "",
    x = "Predicted instant",
    y = "En"
  ) +
  theme_minimal()

if(save_res){
  ggsave(filename = paste0(paste0("en_spt_dyn_",plot_name),format_file),
         plot = plt_err_dy,
         device = NULL,
         path = path_stor_res_all_story_dyn_err,
         scale = 1,
         width = width_dyn_plt,
         height = height_dyn_plt,
         dpi = 300)}


## BoxPlot of Rn
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
  ggsave(filename = paste0(paste0("rn_spt_",plot_name),format_file),
         plot = pgplot,
         device = NULL,
         path = path_stor_res_all_story_bxplt_err,
         scale = 1,
         width = width_bx_plt,
         height = height_bx_plt,
         dpi = 300)}


#Rn dynamics
data <- data.frame(
  x = 51:300,
  y1 = rn_PPC,
  y2 = rn_EX
)

plt_err_dy = ggplot(data) +
  geom_line(aes(x = x, y = y1, color = "PPC"), size = 0.3) +
  geom_point(aes(x = x, y = y1, color = "PPC"), size = 0.5) +
  geom_line(aes(x = x, y = y2, color = "EX"), size = 0.3) +
  geom_point(aes(x = x, y = y2, color = "EX"), size = 0.5) +
  scale_color_manual(
    name = "",
    values = c("PPC" = "blue", "EX" = "red")
  ) +
  ylim(0,y_max) +
  labs(
    title = "",
    x = "Predicted instant",
    y = "Rn"
  ) +
  theme_minimal()

if(save_res){
  ggsave(filename = paste0(paste0("rn_spt_dyn_",plot_name),format_file),
         plot = plt_err_dy,
         device = NULL,
         path = path_stor_res_all_story_dyn_err,
         scale = 1,
         width = width_dyn_plt,
         height = height_dyn_plt,
         dpi = 300)}




## -----Sloping plane s kernel-----
title_err = "Prediction error, no white noise, slopint plane s kernel"

#evaluate the distribution of the errors, with mean, sd and boxplots


#En
en_PPC = res_PPC_sps_no_wn$En
en_EX  = res_EX_sps_no_wn$En
N = length(en_PPC)

summary(en_PPC)
mean(en_PPC)
sd(en_PPC)

summary(en_EX)
mean(en_EX)
sd(en_EX)


#Rn
rn_PPC = res_PPC_sps_no_wn$Rn
rn_EX  = res_EX_sps_no_wn$Rn
N = length(rn_PPC)

summary(rn_PPC)
mean(rn_PPC)
sd(rn_PPC)

summary(rn_EX)
mean(rn_EX)
sd(rn_EX)


y_max = max(c(rn_PPC,en_PPC,rn_EX,en_EX))

## BoxPlot of En
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
  ylim(0,y_max) +
  labs(x="", y="En", fill = "") +
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
  ggsave(filename = paste0(paste0("en_sps_",plot_name),format_file),
         plot = pgplot,
         device = NULL,
         path = path_stor_res_all_story_bxplt_err,
         scale = 1,
         width = width_bx_plt,
         height = height_bx_plt,
         dpi = 300)}


#En dynamics
data <- data.frame(
  x = 51:300,
  y1 = en_PPC,
  y2 = en_EX
)

plt_err_dy = ggplot(data) +
  geom_line(aes(x = x, y = y1, color = "PPC"), size = 0.3) +
  geom_point(aes(x = x, y = y1, color = "PPC"), size = 0.5) +
  geom_line(aes(x = x, y = y2, color = "EX"), size = 0.3) +
  geom_point(aes(x = x, y = y2, color = "EX"), size = 0.5) +
  scale_color_manual(
    name = "",
    values = c("PPC" = "blue", "EX" = "red")
  ) +
  ylim(0,y_max) +
  labs(
    title = "",
    x = "Predicted instant",
    y = "En"
  ) +
  theme_minimal()

plt_err_dy

if(save_res){
  ggsave(filename = paste0(paste0("en_sps_dyn_",plot_name),format_file),
         plot = plt_err_dy,
         device = NULL,
         path = path_stor_res_all_story_dyn_err,
         scale = 1,
         width = width_dyn_plt,
         height = height_dyn_plt,
         dpi = 300)}


## BoxPlot of Rn
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
  ylim(0,y_max) +
  labs(x="", y="Rn", fill = "") +
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
  ggsave(filename = paste0(paste0("rn_sps_",plot_name),format_file),
         plot = pgplot,
         device = NULL,
         path = path_stor_res_all_story_bxplt_err,
         scale = 1,
         width = width_bx_plt,
         height = height_bx_plt,
         dpi = 300)}


#Rn dynamics
data <- data.frame(
  x = 51:300,
  y1 = rn_PPC,
  y2 = rn_EX
)

plt_err_dy = ggplot(data) +
  geom_line(aes(x = x, y = y1, color = "PPC"), size = 0.3) +
  geom_point(aes(x = x, y = y1, color = "PPC"), size = 0.5) +
  geom_line(aes(x = x, y = y2, color = "EX"), size = 0.3) +
  geom_point(aes(x = x, y = y2, color = "EX"), size = 0.5) +
  scale_color_manual(
    name = "",
    values = c("PPC" = "blue", "EX" = "red")
  ) +
  ylim(0,y_max) +
  labs(
    title = "",
    x = "Predicted instant",
    y = "Rn"
  ) +
  theme_minimal()

plt_err_dy

if(save_res){
  ggsave(filename = paste0(paste0("rn_sps_dyn_",plot_name),format_file),
         plot = plt_err_dy,
         device = NULL,
         path = path_stor_res_all_story_dyn_err,
         scale = 1,
         width = width_dyn_plt,
         height = height_dyn_plt,
         dpi = 300)}
