rm(list=ls())
graphics.off()
cat("\014")
set.seed(23032000)

############################################################################################################################################
### Analysis of results of PPC log-forecasting: looking for regularization parameter, number of PPCs and explanatory power behavior  #######
############################################################################################################################################

#put here the path of the local copy of the directory
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"



#if you want to save the result in a folder 
save_res = TRUE
dir_res = paste0(dir_w,"/Test_domain1D/RealWorld_data/results")


#in which folder the result of the PPC predictions are
path_res_pred = paste0(dir_res,"/results_prediction_log/PPC")
load(paste0(path_res_pred,"/prediction_PPC_demand_log.Rdata"))
load(paste0(path_res_pred,"/prediction_PPC_offer_log.Rdata"))

#where to store the results, in case
path_stor_res = paste0(paste0(dir_res,"/results_analysis_PPC_log"))  #saving boxplots
format_file = ".png"  #and, in case, the format

#reg param that have been tested during the predictions
alpha_tested = c(10^(-10),10^(-9),10^(-8),10^(-7),10^(-6),10^(-5),10^(-4),10^(-3),10^(-2),10^(-1),
                 1,10,10^2,10^3,10^4,10^5,10^6,10^7,10^8,10^9,10^10)
#nPPCs tested during the predictions
k_test = 1:length(prediction_PPC_offer_log[[1]]$Prediction)

alphas_used_offer = numeric(length(prediction_PPC_offer_log))
k_used_offer = integer(length(prediction_PPC_offer_log))
exp_pow_offer = numeric(length(prediction_PPC_offer_log))
alphas_used_demand = numeric(length(prediction_PPC_offer_log))
k_used_demand = integer(length(prediction_PPC_offer_log))
exp_pow_demand = numeric(length(prediction_PPC_offer_log))
alphas_used = numeric(2*length(prediction_PPC_offer_log))
k_used = integer(2*length(prediction_PPC_offer_log))
exp_pow = numeric(2*length(prediction_PPC_offer_log))

for (i in 1:length(prediction_PPC_offer_log)) {
  alphas_used_offer[i] = prediction_PPC_offer_log[[i]]$Alpha
  alphas_used_demand[i] = prediction_PPC_demand_log[[i]]$Alpha
  
  k_used_offer[i] = prediction_PPC_offer_log[[i]]$N_PPCs
  k_used_demand[i] = prediction_PPC_demand_log[[i]]$N_PPCs
  
  if(length(prediction_PPC_offer_log[[i]]$Exp_Pow)>1){
    exp_pow_offer[i] = prediction_PPC_offer_log[[i]]$Exp_Pow[-1]
  }else{exp_pow_offer[i] = prediction_PPC_offer_log[[i]]$Exp_Pow}
  
  if(length(prediction_PPC_demand_log[[i]]$Exp_Pow)>1){
    exp_pow_demand[i] = prediction_PPC_demand_log[[i]]$Exp_Pow[-1]
  }else{exp_pow_demand[i] = prediction_PPC_demand_log[[i]]$Exp_Pow}
  
} 


## ----- barchart for alpha, count, offers predictions ------
count_alpha = integer(length(alpha_tested))

for (i in 1:length(alpha_tested)) {
  counter = 0
  alpha_fix = alpha_tested[i]
  for (j in 1:length(alphas_used_offer)) {
    if(alphas_used_offer[j]==alpha_fix){counter = counter+1}
  }
  count_alpha[i] = counter
}

alpha_count <- data.frame(
  Alpha = as.character(alpha_tested),
  Count = count_alpha
)

alpha_count$Alpha_num = as.numeric(alpha_count$Alpha)

barchart_alpha = ggplot(alpha_count, aes(x = reorder(Alpha,Alpha_num), y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "PPC Prediction log-offer", x = "Regularization parameter", y = "Count") +
  theme_minimal()
print(barchart_alpha)

if(save_res){
  title = "count_alpha_PPC_offer_log"
  ggsave(filename = paste0(title,format_file),
         plot = barchart_alpha,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}





## ----- barchart for alpha, count, demands predictions ------
count_alpha = integer(length(alpha_tested))

for (i in 1:length(alpha_tested)) {
  counter = 0
  alpha_fix = alpha_tested[i]
  for (j in 1:length(alphas_used_demand)) {
    if(alphas_used_demand[j]==alpha_fix){counter = counter+1}
  }
  count_alpha[i] = counter
}

alpha_count <- data.frame(
  Alpha = as.character(alpha_tested),
  Count = count_alpha
)

alpha_count$Alpha_num = as.numeric(alpha_count$Alpha)


barchart_alpha = ggplot(alpha_count, aes(x = reorder(Alpha,Alpha_num), y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "PPC Prediction log-demand", x = "Regularization parameter", y = "Count") +
  theme_minimal()
print(barchart_alpha)

if(save_res){
  title = "count_alpha_PPC_demand_log"
  ggsave(filename = paste0(title,format_file),
         plot = barchart_alpha,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}





## ----- barchart for nPPCs, count, offer predictions ------
count_k = integer(length(k_test))

for (i in 1:length(k_test)) {
  counter = 0
  k_fix = k_test[i]
  for (j in 1:length(k_used_offer)) {
    if(k_used_offer[j]==k_fix){counter = counter+1}
  }
  count_k[i] = counter
}

max_ind = max(which(count_k!=0)) + 1
k_count <- data.frame(
  Number_PPCs = as.character(k_test[1:max_ind]),
  Count = count_k[1:max_ind]
)


barchart_k = ggplot(k_count, aes(x = Number_PPCs, y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "PPC Prediction log-offer", x = "Number PPCs retained", y = "Count") +
  theme_minimal()
print(barchart_k)

if(save_res){
  title = "count_k_PPC_offer_log"
  ggsave(filename = paste0(title,format_file),
         plot = barchart_k,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}





## ----- barchart for nPPCs, count, demand predictions ------
count_k = integer(length(k_test))

for (i in 1:length(k_test)) {
  counter = 0
  k_fix = k_test[i]
  for (j in 1:length(k_used_demand)) {
    if(k_used_demand[j]==k_fix){counter = counter+1}
  }
  count_k[i] = counter
}

max_ind = max(which(count_k!=0)) + 1
k_count <- data.frame(
  Number_PPCs = as.character(k_test[1:max_ind]),
  Count = count_k[1:max_ind]
)


barchart_k = ggplot(k_count, aes(x = Number_PPCs, y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "PPC Prediction log-demand", x = "Number PPCs retained", y = "Count") +
  theme_minimal()
print(barchart_k)

if(save_res){
  title = "count_k_PPC_demand_log"
  ggsave(filename = paste0(title,format_file),
         plot = barchart_k,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}





## ----- boxplot expained power, offer predictions -----
dati <- data.frame(exp_pow = exp_pow_offer)
bp_exp_pow = ggplot(dati, aes(y = exp_pow)) +
  geom_boxplot(fill = "lightblue") +
  scale_y_continuous(limits = c(0.999, 1)) +
  labs(title = "PPC log-offer predictions", y = "Explanatory power") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
print(bp_exp_pow)

if(save_res){
  title = "exp_pow_PPC_offer_log"
  ggsave(filename = paste0(title,format_file),
         plot = bp_exp_pow,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}






## ----- boxplot expained power, demand predictions -----
dati <- data.frame(exp_pow = exp_pow_demand)
bp_exp_pow = ggplot(dati, aes(y = exp_pow)) +
  geom_boxplot(fill = "lightblue") +
  scale_y_continuous(limits = c(0.992, 1)) +
  labs(title = "PPC log-demand predictions", y = "Explanatory power") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
print(bp_exp_pow)

if(save_res){
  title = "exp_pow_PPC_demand_log"
  ggsave(filename = paste0(title,format_file),
         plot = bp_exp_pow,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}
