rm(list=ls())
graphics.off()
cat("\014")

set.seed(23032000)

#change here
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series"
load(paste0(dir_w,"/Test_domain1D/RealWorld_data/results/predictions_demands_KO.Rdata"))
load(paste0(dir_w,"/Test_domain1D/RealWorld_data/results/predictions_offers_KO.Rdata"))
load(paste0(dir_w,"/Test_domain1D/RealWorld_data/results/result_CONFORMAL.Rdata"))
load(paste0(dir_w,"/Test_domain1D/RealWorld_data/MGS_cg_260419_310120_data.Rdata"))

N = length(predictions_demands_KO)
last_idx = which(MGS_cg_260419_310120_data[[1]]==200000)

en_ko_o = numeric(N)
rn_ko_o = numeric(N)
en_cp_o = numeric(N)
rn_cp_o = numeric(N)

en_ko_d = numeric(N)
rn_ko_d = numeric(N)
en_cp_d = numeric(N)
rn_cp_d = numeric(N)

for (i in 1:N) {
  real_price = MGS_cg_260419_310120_data[[2]][[i+97]]$p_cg_v[1:last_idx]
  prev_ko = as.vector(predictions_offers_KO[i])[[1]]
  prev_cp = result_CONFORMAL[[i]]$Prediction$predicted_p_cg_v
  
  en_ko_o[i] = MLmetrics::MSE(prev_ko,real_price)
  rn_ko_o[i] = MLmetrics::MAE(prev_ko,real_price)
  en_cp_o[i] = MLmetrics::MSE(prev_cp,real_price)
  rn_cp_o[i] = MLmetrics::MAE(prev_cp,real_price)
  
  
  real_demands = MGS_cg_260419_310120_data[[2]][[i+97]]$p_cg_a[1:last_idx]
  prev_ko = as.vector(predictions_demands_KO[i])[[1]]
  prev_cp = result_CONFORMAL[[i]]$Prediction$predicted_p_cg_a
  
  en_ko_d[i] = MLmetrics::MSE(prev_ko,real_price)
  rn_ko_d[i] = MLmetrics::MAE(prev_ko,real_price)
  en_cp_d[i] = MLmetrics::MSE(prev_cp,real_price)
  rn_cp_d[i] = MLmetrics::MAE(prev_cp,real_price)
  
}


summary(en_ko_o)
mean(en_ko_o)
sd(en_ko_o)

summary(en_cp_o)
mean(en_cp_o)
sd(en_cp_o)

summary(rn_ko_o)
mean(rn_ko_o)
sd(rn_ko_o)

summary(rn_cp_o)
mean(rn_cp_o)
sd(rn_cp_o)



summary(en_ko_d)
mean(en_ko_d)
sd(en_ko_d)

summary(en_cp_d)
mean(en_cp_d)
sd(en_cp_d)

summary(rn_ko_d)
mean(rn_ko_d)
sd(rn_ko_d)

summary(rn_cp_d)
mean(rn_cp_d)
sd(rn_cp_d)


# En on offer prices
err_en <- c(en_ko_o, en_cp_o)
method <- rep(c("KO","CP"), each=N)
En <- data.frame(method, err_en)
method_order<- c("KO","CP")
En.box <- En %>% mutate(method=factor(x=method, levels=method_order))

##grouped boxplot

pgplot <- ggplot(En.box, aes(x=method, y=err_en, fill=method)) + 
  geom_boxplot() + ggtitle("Prediction errors on offer prices")
pgplot <- pgplot +
  theme_bw() + 
  labs(x="", y="En") +
  theme(plot.title = element_text(face="bold", hjust=0.5, size=22),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=22),
        axis.title.y = element_text(size=22))
pgplot + theme(legend.position="none")

title = "En_offers"
ggsave(filename = paste0(title,".pdf"),
       plot = pgplot,
       device = NULL,
       path = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series/Test_domain1D/RealWorld_data/results",
       scale = 1,
       width = 14,
       height = 10,
       dpi = 300)


# Rn on offer prices
err_rn <- c(rn_ko_o, rn_cp_o)
method <- rep(c("KO","CP"), each=N)
Rn <- data.frame(method, err_en)
method_order<- c("KO","CP")
Rn.box <- Rn %>% mutate(method=factor(x=method, levels=method_order))

##grouped boxplot

pgplot <- ggplot(Rn.box, aes(x=method, y=err_en, fill=method)) + 
  geom_boxplot() + ggtitle("Prediction errors on offer prices")
pgplot <- pgplot +
  theme_bw() + 
  labs(x="", y="Rn") +
  theme(plot.title = element_text(face="bold", hjust=0.5, size=22),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=22),
        axis.title.y = element_text(size=22))
pgplot + theme(legend.position="none")

title = "Rn_offers"
ggsave(filename = paste0(title,".pdf"),
       plot = pgplot,
       device = NULL,
       path = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series/Test_domain1D/RealWorld_data/results",
       scale = 1,
       width = 14,
       height = 10,
       dpi = 300)







# En on demand prices
err_en <- c(en_ko_d, en_cp_d)
method <- rep(c("KO","CP"), each=N)
En <- data.frame(method, err_en)
method_order<- c("KO","CP")
En.box <- En %>% mutate(method=factor(x=method, levels=method_order))

##grouped boxplot

pgplot <- ggplot(En.box, aes(x=method, y=err_en, fill=method)) + 
  geom_boxplot() + ggtitle("Prediction errors on demand prices")
pgplot <- pgplot +
  theme_bw() + 
  labs(x="", y="En") +
  theme(plot.title = element_text(face="bold", hjust=0.5, size=22),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=22),
        axis.title.y = element_text(size=22))
pgplot + theme(legend.position="none")

title = "En_demnds"
ggsave(filename = paste0(title,".pdf"),
       plot = pgplot,
       device = NULL,
       path = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series/Test_domain1D/RealWorld_data/results",
       scale = 1,
       width = 14,
       height = 10,
       dpi = 300)


# Rn on offer prices
err_rn <- c(rn_ko_d, rn_cp_d)
method <- rep(c("KO","CP"), each=N)
Rn <- data.frame(method, err_en)
method_order<- c("KO","CP")
Rn.box <- Rn %>% mutate(method=factor(x=method, levels=method_order))

##grouped boxplot

pgplot <- ggplot(Rn.box, aes(x=method, y=err_en, fill=method)) + 
  geom_boxplot() + ggtitle("Prediction errors on demand prices")
pgplot <- pgplot +
  theme_bw() + 
  labs(x="", y="Rn") +
  theme(plot.title = element_text(face="bold", hjust=0.5, size=22),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=22),
        axis.title.y = element_text(size=22))
pgplot + theme(legend.position="none")

title = "Rn_demands"
ggsave(filename = paste0(title,".pdf"),
       plot = pgplot,
       device = NULL,
       path = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series/Test_domain1D/RealWorld_data/results",
       scale = 1,
       width = 14,
       height = 10,
       dpi = 300)