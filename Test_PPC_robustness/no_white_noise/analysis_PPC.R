rm(list=ls())
graphics.off()
cat("\014")
set.seed(23032000)

########################################################################################################################################
### Analysis of results of PPC forecasting: looking for regularization parameter, number of PPCs and explanatory power behavior  #######
########################################################################################################################################

#put here the path of the local copy of the directory
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"


#if you want to save the result 
save_res = TRUE
format_file = ".jpg"

dir_res = paste0(dir_w,"/Test_PPC_robustness/no_white_noise/results")
#in which folder the result of the PPC predictions are
path_res_pred = paste0(dir_res,"/predictions/train_all_story")

# upload reults
files <- list.files(path = path_res_pred, full.names = TRUE)
for (file in files) {
  load(file)
}

# eight different simulations: put here the data
{
  vec_data = c(res_PPC_gau_no_wn$Prediction,
               res_PPC_id_no_wn$Prediction,
               res_PPC_sps_no_wn$Prediction,
               res_PPC_spt_no_wn$Prediction)
}

tot_run          = length(vec_data)
number_pred_sing = length(vec_data[49][[1]]$`One-step ahead prediction`)


#where to store the results, in case
path_stor_res = paste0(paste0(dir_res,"/analysis_PPC"))  

#reg param that have been tested during the predictions
alpha_tested = c(10^(-10),10^(-9),10^(-8),10^(-7),10^(-6),10^(-5),10^(-4),10^(-3),10^(-2),10^(-1),
                 1,10,10^2,10^3,10^4,10^5,10^6,10^7,10^8,10^9,10^10)
#nPPCs tested during the predictions
k_test = 1:number_pred_sing

alphas_used  = numeric(length = tot_run)
k_used       = integer(length = tot_run)
exp_pow_used = numeric(length = tot_run)


for (i in 1:tot_run) {
  
  if(is.null(vec_data[[i]])){
    alphas_used[i]  = NaN
    k_used[i]       = NaN
    exp_pow_used[i] = NaN
  }
  else{
    alphas_used[i]  = vec_data[[i]]$Alpha
    k_used[i]       = vec_data[[i]]$`Number of PPCs retained`
    exp_pow_used[i] = vec_data[[i]]$`Explanatory power PPCs`[k_used[i]] 
  }
}


alphas_used <- as.vector(na.omit(alphas_used))
k_used <- as.vector(na.omit(k_used))
exp_pow_used <- as.vector(na.omit(exp_pow_used))



## ----- barchart for alpha, count------
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
  labs(title = "", x = "Regularization parameter", y = "Times alpha being selected") +
  theme_minimal()
print(barchart_alpha)

if(save_res){
  title = "count_alpha_PPC_no_wn"
  ggsave(filename = paste0(title,format_file),
         plot = barchart_alpha,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}


#alpha dynamic
#GA
plot_name="gau"
alpha_proc = numeric(299-49+1)
for (i in 49:299) {
  alpha_proc[i-48] = res_PPC_gau_no_wn$Prediction[[i]]$Alpha
}

df <- data.frame(
  x = 51:300,
  y = alpha_proc
)

all_y_values <- alpha_tested # Valori totali desiderati

# Creare una mappa per posizionare i valori sull'asse y equispaziato
y_transformed <- seq_along(all_y_values) # Indici equidistanti
y_map <- setNames(y_transformed, all_y_values) # Mappa: valore originale -> indice

# Trasformare i valori di y nei dati originali
df$y_transformed <- y_map[as.character(df$y)] # Mappa i valori esistenti

# Grafico
plt_alpha_dyn=ggplot(df, aes(x = x, y = y_transformed)) +
  geom_point(size = 1) +
  scale_y_continuous(
    breaks = y_transformed, 
    labels = all_y_values     
  ) +
  labs(
    title = "Regularization parameter dynamic, no white noise, GA kernel",
    x = "Time instant",
    y = "Regularization parameter"
  ) +
  theme_minimal()

print(plt_alpha_dyn)

if(save_res){
  ggsave(filename = paste0(paste0("alpha_dyn_",plot_name),format_file),
         plot = plt_alpha_dyn,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = NA,
         height = NA,
         dpi = 300)}


#ID
plot_name="id"
alpha_proc = numeric(299-49+1)
for (i in 49:299) {
  alpha_proc[i-48] = res_PPC_id_no_wn$Prediction[[i]]$Alpha
}

df <- data.frame(
  x = 49:299,
  y = alpha_proc
)

all_y_values <- alpha_tested # Valori totali desiderati

# Creare una mappa per posizionare i valori sull'asse y equispaziato
y_transformed <- seq_along(all_y_values) # Indici equidistanti
y_map <- setNames(y_transformed, all_y_values) # Mappa: valore originale -> indice

# Trasformare i valori di y nei dati originali
df$y_transformed <- y_map[as.character(df$y)] # Mappa i valori esistenti

# Grafico
plt_alpha_dyn=ggplot(df, aes(x = x, y = y_transformed)) +
  geom_point(size = 1) +
  scale_y_continuous(
    breaks = y_transformed, 
    labels = all_y_values     
  ) +
  labs(
    title = "Regularization parameter dynamic, no white noise, ID kernel",
    x = "Time instant",
    y = "Regularization parameter"
  ) +
  theme_minimal()

print(plt_alpha_dyn)

if(save_res){
  ggsave(filename = paste0(paste0("alpha_dyn_",plot_name),format_file),
         plot = plt_alpha_dyn,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = NA,
         height = NA,
         dpi = 300)}


#SPT
plot_name="spt"
alpha_proc = numeric(299-49+1)
for (i in 49:299) {
  alpha_proc[i-48] = res_PPC_spt_no_wn$Prediction[[i]]$Alpha
}

df <- data.frame(
  x = 49:299,
  y = alpha_proc
)

all_y_values <- alpha_tested # Valori totali desiderati

# Creare una mappa per posizionare i valori sull'asse y equispaziato
y_transformed <- seq_along(all_y_values) # Indici equidistanti
y_map <- setNames(y_transformed, all_y_values) # Mappa: valore originale -> indice

# Trasformare i valori di y nei dati originali
df$y_transformed <- y_map[as.character(df$y)] # Mappa i valori esistenti

# Grafico
plt_alpha_dyn=ggplot(df, aes(x = x, y = y_transformed)) +
  geom_point(size = 1) +
  scale_y_continuous(
    breaks = y_transformed, 
    labels = all_y_values     
  ) +
  labs(
    title = "Regularization parameter dynamic, no white noise, SPT kernel",
    x = "Time instant",
    y = "Regularization parameter"
  ) +
  theme_minimal()

print(plt_alpha_dyn)

if(save_res){
  ggsave(filename = paste0(paste0("alpha_dyn_",plot_name),format_file),
         plot = plt_alpha_dyn,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = NA,
         height = NA,
         dpi = 300)}


#SPT
plot_name="sps"
alpha_proc = numeric(299-49+1)
for (i in 49:299) {
  alpha_proc[i-48] = res_PPC_sps_no_wn$Prediction[[i]]$Alpha
}

df <- data.frame(
  x = 49:299,
  y = alpha_proc
)

all_y_values <- alpha_tested # Valori totali desiderati

# Creare una mappa per posizionare i valori sull'asse y equispaziato
y_transformed <- seq_along(all_y_values) # Indici equidistanti
y_map <- setNames(y_transformed, all_y_values) # Mappa: valore originale -> indice

# Trasformare i valori di y nei dati originali
df$y_transformed <- y_map[as.character(df$y)] # Mappa i valori esistenti

# Grafico
plt_alpha_dyn=ggplot(df, aes(x = x, y = y_transformed)) +
  geom_point(size = 1) +
  scale_y_continuous(
    breaks = y_transformed, 
    labels = all_y_values     
  ) +
  labs(
    title = "Regularization parameter dynamic, no white noise, SPS kernel",
    x = "Time instant",
    y = "Regularization parameter"
  ) +
  theme_minimal()

print(plt_alpha_dyn)

if(save_res){
  ggsave(filename = paste0(paste0("alpha_dyn_",plot_name),format_file),
         plot = plt_alpha_dyn,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = NA,
         height = NA,
         dpi = 300)}



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
  labs(title = "", x = "Number PPCs retained", y = "Time k being retained") +
  theme_minimal()
print(barchart_k)

if(save_res){
  title = "count_k_PPC_no_wn"
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
  labs(title = "", y = "Explanatory power retained") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
print(bp_exp_pow)

if(save_res){
  title = "exp_pow_PPC_no_wn"
  ggsave(filename = paste0(title,format_file),
         plot = bp_exp_pow,
         device = NULL,
         path = path_stor_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}
