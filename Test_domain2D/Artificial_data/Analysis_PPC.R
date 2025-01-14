rm(list=ls())
graphics.off()
cat("\014")
set.seed(23032000)

########################################################################################################################################
### Analysis of results of PPC forecasting: looking for regularization parameter, number of PPCs and explanatory power behavior  #######
########################################################################################################################################

#put here the path of the local copy of the directory
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"

#if you want to save the results 
save_res = TRUE
format_file = ".jpg"
dir_res = paste0(dir_w,"/Test_domain2D/Artificial_data/results")


#in which folder the result of the PPC predictions are
path_res_pred = paste0(dir_res,"/results_prediction/PPC")
#where to store the results, in case
path_stor_res = paste0(paste0(dir_res,"/results_analysis_PPC"))  #saving boxplots


# upload reults
files <- list.files(path = path_res_pred, full.names = TRUE)
for (file in files) {
  load(file)
}


tot_run = length(pred_PPC)


#reg param that have been tested during the predictions
alpha_tested = c(10^(-10),10^(-9),10^(-8),10^(-7),10^(-6),10^(-5),10^(-4),10^(-3),10^(-2),10^(-1),
                 1,10,10^2,10^3,10^4,10^5,10^6,10^7,10^8,10^9,10^10)
#nPPCs tested during the predictions
k_test = 1:length(pred_PPC[[1]]$Prediction)

alphas_used  = numeric(tot_run)
k_used       = integer(tot_run)
exp_pow_used = numeric(tot_run)


for (i in 1:tot_run) {
  
  alphas_used[i]  = pred_PPC[[i]]$Alpha
  k_used[i]       = pred_PPC[[i]]$N_PPCs 
  exp_pow_used[i] = pred_PPC[[i]]$Exp_Pow[k_used[i]] 
}



## ----- barchart for alpha, count, offers predictions ------
count_alpha = integer(length(alpha_tested))

for (i in 1:length(alpha_tested)) {
  counter = 0
  alpha_fix = alpha_tested[i]
  for (j in 1:length(alphas_used)) {
    if(alphas_used[j]==alpha_fix){counter = counter+1}
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
  labs(title = "PPC Prediction", x = "Regularization parameter", y = "Times alpha being used") +
  theme_minimal()
print(barchart_alpha)

if(save_res){
  title = "count_alpha_2d_s"
  ggsave(filename = paste0(title,format_file),
         plot = barchart_alpha,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}








## ----- barchart for nPPCs, count ------
count_k = integer(length(k_test))

for (i in 1:length(k_test)) {
  counter = 0
  k_fix = k_test[i]
  for (j in 1:length(k_used)) {
    if(k_used[j]==k_fix){counter = counter+1}
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
  labs(title = "PPC predictions", x = "Number PPCs retained", y = "Time k beign retained") +
  theme_minimal()
print(barchart_k)

if(save_res){
  title = "count_k_2d_s"
  ggsave(filename = paste0(title,format_file),
         plot = barchart_k,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}






## ----- boxplot expained power, demand predictions -----
dati <- data.frame(exp_pow = exp_pow_used)
bp_exp_pow = ggplot(dati, aes(y = exp_pow)) +
  geom_boxplot(fill = "lightblue") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "PPC predictions", y = "Explanatory power retained") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
print(bp_exp_pow)

if(save_res){
  title = "exp_pow_2d_s"
  ggsave(filename = paste0(title,format_file),
         plot = bp_exp_pow,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}
