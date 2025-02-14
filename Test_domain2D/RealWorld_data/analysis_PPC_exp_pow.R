rm(list=ls())
graphics.off()
cat("\014")
set.seed(23032000)

#####################################################################################################################################################
### Analysis of results of PPC with exp pow forecasting: looking for regularization parameter, number of PPCs and explanatory power behavior  #######
#####################################################################################################################################################



#put here the path of the local copy of the directory
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"

#if you want to save the result in a folder 
save_res = TRUE
format_file = ".jpg"


dir_res = paste0(dir_w,"/Test_domain2D/RealWorld_data/results")


#in which folder the result of the PPC predictions are
path_res_pred = paste0(dir_res,"/results_prediction/PPC_exp_pow")

# upload reults
files <- list.files(path = path_res_pred, full.names = TRUE)
for (file in files) {
  load(file)
}


tot_run = 1000



#where to store the results, in case
path_stor_res = paste0(paste0(dir_res,"/results_analysis_PPC_exp_pow"))  #saving barplots


#reg param that have been tested during the predictions
alpha_tested = c(0.0001,0.001,0.01,0.1,1,10)
#nPPCs tested during the predictions
k_test_mout = 1:length(prediction_PPC_exp_pow_mouth[[1]]$Prediction )
k_test_cent = 1:length(prediction_PPC_exp_pow_center[[1]]$Prediction )

alphas_used_mou  = numeric(tot_run)
k_used_mou       = integer(tot_run)
exp_pow_used_mou = numeric(tot_run)
alphas_used_mou_d1  = numeric(tot_run)
k_used_mou_d1       = integer(tot_run)
exp_pow_used_mou_d1 = numeric(tot_run)
alphas_used_mou_d2  = numeric(tot_run)
k_used_mou_d2       = integer(tot_run)
exp_pow_used_mou_d2 = numeric(tot_run)

alphas_used_cen  = numeric(tot_run)
k_used_cen       = integer(tot_run)
exp_pow_used_cen = numeric(tot_run)
alphas_used_cen_d1  = numeric(tot_run)
k_used_cen_d1       = integer(tot_run)
exp_pow_used_cen_d1 = numeric(tot_run)
alphas_used_cen_d2  = numeric(tot_run)
k_used_cen_d2       = integer(tot_run)
exp_pow_used_cen_d2 = numeric(tot_run)


for (i in 1:tot_run) {
  
  alphas_used_mou[i]  = prediction_PPC_exp_pow_mouth[[i]]$Alpha
  k_used_mou[i]       = prediction_PPC_exp_pow_mouth[[i]]$N_PPCs 
  exp_pow_used_mou[i] = prediction_PPC_exp_pow_mouth[[i]]$Exp_Pow[k_used_mou[i]] 
  
  alphas_used_mou_d1[i]  = prediction_PPC_exp_pow_mouth_diff_1[[i]]$Alpha
  k_used_mou_d1[i]       = prediction_PPC_exp_pow_mouth_diff_1[[i]]$N_PPCs 
  exp_pow_used_mou_d1[i] = prediction_PPC_exp_pow_mouth_diff_1[[i]]$Exp_Pow[k_used_mou_d1[i]] 
  
  alphas_used_mou_d2[i]  = prediction_PPC_exp_pow_mouth_diff_2[[i]]$Alpha
  k_used_mou_d2[i]       = prediction_PPC_exp_pow_mouth_diff_2[[i]]$N_PPCs 
  exp_pow_used_mou_d2[i] = prediction_PPC_exp_pow_mouth_diff_2[[i]]$Exp_Pow[k_used_mou_d2[i]] 
  
  
  alphas_used_cen[i]  = prediction_PPC_exp_pow_center[[i]]$Alpha
  k_used_cen[i]       = prediction_PPC_exp_pow_center[[i]]$N_PPCs 
  exp_pow_used_cen[i] = prediction_PPC_exp_pow_center[[i]]$Exp_Pow[k_used_cen[i]] 
  
  alphas_used_cen_d1[i]  = prediction_PPC_exp_pow_center_diff_1[[i]]$Alpha
  k_used_cen_d1[i]       = prediction_PPC_exp_pow_center_diff_1[[i]]$N_PPCs 
  exp_pow_used_cen_d1[i] = prediction_PPC_exp_pow_center_diff_1[[i]]$Exp_Pow[k_used_cen_d1[i]] 
  
  alphas_used_cen_d2[i]  = prediction_PPC_exp_pow_center_diff_2[[i]]$Alpha
  k_used_cen_d2[i]       = prediction_PPC_exp_pow_center_diff_2[[i]]$N_PPCs 
  exp_pow_used_cen_d2[i] = prediction_PPC_exp_pow_center_diff_2[[i]]$Exp_Pow[k_used_cen_d2[i]] 
}



## ----- barchart for alpha, count, offers predictions ------

##mouth, normal data
count_alpha = integer(length(alpha_tested))

for (i in 1:length(alpha_tested)) {
  counter = 0
  alpha_fix = alpha_tested[i]
  for (j in 1:length(alphas_used_mou)) {
    if(alphas_used_mou[j]==alpha_fix){counter = counter+1}
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
  labs(title = "", x = "Regularization parameter", y = "Times alpha being selected") +
  theme_minimal()
print(barchart_alpha)

if(save_res){
  title = "count_alpha_mou_ppc_algo"
  ggsave(filename = paste0(title,format_file),
         plot = barchart_alpha,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}




##mouth, diff1
count_alpha = integer(length(alpha_tested))

for (i in 1:length(alpha_tested)) {
  counter = 0
  alpha_fix = alpha_tested[i]
  for (j in 1:length(alphas_used_mou_d1)) {
    if(alphas_used_mou_d1[j]==alpha_fix){counter = counter+1}
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
  labs(title = "", x = "Regularization parameter", y = "Times alpha being selected") +
  theme_minimal()
print(barchart_alpha)

if(save_res){
  title = "count_alpha_mou_d1_ppc_algo"
  ggsave(filename = paste0(title,format_file),
         plot = barchart_alpha,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}



##mouth, diff2
count_alpha = integer(length(alpha_tested))

for (i in 1:length(alpha_tested)) {
  counter = 0
  alpha_fix = alpha_tested[i]
  for (j in 1:length(alphas_used_mou_d2)) {
    if(alphas_used_mou_d2[j]==alpha_fix){counter = counter+1}
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
  labs(title = "", x = "Regularization parameter", y = "Times alpha being selected") +
  theme_minimal()
print(barchart_alpha)

if(save_res){
  title = "count_alpha_mou_d2_ppc_algo"
  ggsave(filename = paste0(title,format_file),
         plot = barchart_alpha,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}



##center, normal data
count_alpha = integer(length(alpha_tested))

for (i in 1:length(alpha_tested)) {
  counter = 0
  alpha_fix = alpha_tested[i]
  for (j in 1:length(alphas_used_cen)) {
    if(alphas_used_cen[j]==alpha_fix){counter = counter+1}
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
  labs(title = "", x = "Regularization parameter", y = "Times alpha being selected") +
  theme_minimal()
print(barchart_alpha)

if(save_res){
  title = "count_alpha_cen_ppc_algo"
  ggsave(filename = paste0(title,format_file),
         plot = barchart_alpha,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}



##center, diff 1
count_alpha = integer(length(alpha_tested))

for (i in 1:length(alpha_tested)) {
  counter = 0
  alpha_fix = alpha_tested[i]
  for (j in 1:length(alphas_used_cen_d1)) {
    if(alphas_used_cen_d1[j]==alpha_fix){counter = counter+1}
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
  labs(title = "", x = "Regularization parameter", y = "Times alpha being selected") +
  theme_minimal()
print(barchart_alpha)

if(save_res){
  title = "count_alpha_cen_d1_ppc_algo"
  ggsave(filename = paste0(title,format_file),
         plot = barchart_alpha,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}


##center, diff 2
count_alpha = integer(length(alpha_tested))

for (i in 1:length(alpha_tested)) {
  counter = 0
  alpha_fix = alpha_tested[i]
  for (j in 1:length(alphas_used_cen_d2)) {
    if(alphas_used_cen_d2[j]==alpha_fix){counter = counter+1}
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
  labs(title = "", x = "Regularization parameter", y = "Times alpha being selected") +
  theme_minimal()
print(barchart_alpha)

if(save_res){
  title = "count_alpha_cen_d2_ppc_algo"
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
## mouth, normal
count_k = integer(length(k_test_mout))

for (i in 1:length(k_test_mout)) {
  counter = 0
  k_fix = k_test_mout[i]
  for (j in 1:length(k_used_mou)) {
    if(k_used_mou[j]==k_fix){counter = counter+1}
  }
  count_k[i] = counter
}

max_ind = max(which(count_k!=0)) + 1
k_count <- data.frame(
  Number_PPCs = as.character(k_test_mout[1:max_ind]),
  Count = count_k[1:max_ind]
)


barchart_k = ggplot(k_count, aes(x = Number_PPCs, y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "", x = "Number PPCs retained", y = "Times k being retained") +
  theme_minimal()
print(barchart_k)

if(save_res){
  title = "count_k_mouth_ppc_algo"
  ggsave(filename = paste0(title,format_file),
         plot = barchart_k,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}


# mouth, diff1
count_k = integer(length(k_test_mout))

for (i in 1:length(k_test_mout)) {
  counter = 0
  k_fix = k_test_mout[i]
  for (j in 1:length(k_used_mou_d1)) {
    if(k_used_mou_d1[j]==k_fix){counter = counter+1}
  }
  count_k[i] = counter
}

max_ind = max(which(count_k!=0)) + 1
k_count <- data.frame(
  Number_PPCs = as.character(k_test_mout[1:max_ind]),
  Count = count_k[1:max_ind]
)


barchart_k = ggplot(k_count, aes(x = Number_PPCs, y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "", x = "Number PPCs retained", y = "Times k being retained") +
  theme_minimal()
print(barchart_k)

if(save_res){
  title = "count_k_mouth_d1_ppc_algo"
  ggsave(filename = paste0(title,format_file),
         plot = barchart_k,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}



# mouth, diff2
count_k = integer(length(k_test_mout))

for (i in 1:length(k_test_mout)) {
  counter = 0
  k_fix = k_test_mout[i]
  for (j in 1:length(k_used_mou_d2)) {
    if(k_used_mou_d2[j]==k_fix){counter = counter+1}
  }
  count_k[i] = counter
}

max_ind = max(which(count_k!=0)) + 1
k_count <- data.frame(
  Number_PPCs = as.character(k_test_mout[1:max_ind]),
  Count = count_k[1:max_ind]
)


k_count$Number_PPCs <- factor(k_count$Number_PPCs, levels = sort(as.numeric(unique(k_count$Number_PPCs))))
barchart_k = ggplot(k_count, aes(x = Number_PPCs, y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "", x = "Number PPCs retained", y = "Times k being retained") +
  theme_minimal()
print(barchart_k)

if(save_res){
  title = "count_k_mouth_d2_ppc_algo"
  ggsave(filename = paste0(title,format_file),
         plot = barchart_k,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}



## center, normal
count_k = integer(length(k_test_cent))

for (i in 1:length(k_test_cent)) {
  counter = 0
  k_fix = k_test_cent[i]
  for (j in 1:length(k_used_cen)) {
    if(k_used_cen[j]==k_fix){counter = counter+1}
  }
  count_k[i] = counter
}

max_ind = max(which(count_k!=0)) + 1
k_count <- data.frame(
  Number_PPCs = as.character(k_test_cent[1:max_ind]),
  Count = count_k[1:max_ind]
)


barchart_k = ggplot(k_count, aes(x = Number_PPCs, y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "", x = "Number PPCs retained", y = "Times k being retained") +
  theme_minimal()
print(barchart_k)

if(save_res){
  title = "count_k_center_ppc_algo"
  ggsave(filename = paste0(title,format_file),
         plot = barchart_k,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}


## center, d1
count_k = integer(length(k_test_cent))

for (i in 1:length(k_test_cent)) {
  counter = 0
  k_fix = k_test_cent[i]
  for (j in 1:length(k_used_cen_d1)) {
    if(k_used_cen_d1[j]==k_fix){counter = counter+1}
  }
  count_k[i] = counter
}

max_ind = max(which(count_k!=0)) + 1
k_count <- data.frame(
  Number_PPCs = as.character(k_test_cent[1:max_ind]),
  Count = count_k[1:max_ind]
)


barchart_k = ggplot(k_count, aes(x = Number_PPCs, y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "", x = "Number PPCs retained", y = "Times k being retained") +
  theme_minimal()
print(barchart_k)

if(save_res){
  title = "count_k_center_d1_ppc_algo"
  ggsave(filename = paste0(title,format_file),
         plot = barchart_k,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}


# center, d2
count_k = integer(length(k_test_cent))

for (i in 1:length(k_test_cent)) {
  counter = 0
  k_fix = k_test_cent[i]
  for (j in 1:length(k_used_cen_d2)) {
    if(k_used_cen_d2[j]==k_fix){counter = counter+1}
  }
  count_k[i] = counter
}

max_ind = max(which(count_k!=0)) + 1
k_count <- data.frame(
  Number_PPCs = as.character(k_test_cent[1:max_ind]),
  Count = count_k[1:max_ind]
)

k_count$Number_PPCs <- factor(k_count$Number_PPCs, levels = sort(as.numeric(unique(k_count$Number_PPCs))))

barchart_k = ggplot(k_count, aes(x = Number_PPCs, y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "", x = "Number PPCs retained", y = "Times k being retained") +
  theme_minimal()
print(barchart_k)

if(save_res){
  title = "count_k_center_d2_ppc_algo"
  ggsave(filename = paste0(title,format_file),
         plot = barchart_k,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}




