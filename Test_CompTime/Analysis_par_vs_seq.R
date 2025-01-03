rm(list=ls())
graphics.off()
cat("\014")

library(tidyr)
library(ggplot2)

#change here 
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"


save_res = TRUE
format_fl = ".jpg"

load(paste0(dir_w,"/Test_CompTime/results/times_CV.Rdata"))
load(paste0(dir_w,"/Test_CompTime/results/times_CV_gen.Rdata"))

path_store_res = paste0(dir_w,"/Test_CompTime//results/par_vs_seq")

n_threads = length(times_CV)
threads = 1:n_threads
# in this script: everything is specific with respect tests in "TestCompTimeCV.R"
# T: number of time instants; N: number of parameters alpha
time_instants <- c(19,49,99,499)
sizes_alphas = c(5,10,25,50)



categories <- as.character(threads)

## ----  T = 19, N = 5 -----
i_t = 1
i_a = 1
instant   = as.character(time_instants[i_t])
alphas_sz = as.character(sizes_alphas[i_a])
title = paste0("CV performance with ",paste0(alphas_sz,paste0(" reg params and ",paste0(instant," time instants"))))

times = numeric(n_threads)
times_gen = numeric(n_threads)

for (i in threads) {
  times[i] = times_CV[[i]][i_a,i_t]
  times_gen[i] = times_CV_gen[[i]][i_a,i_t]
}



data <- data.frame(
  Category = categories,
  ex_solver = times,
  gep_solver = times_gen
)

# Trasforma in formato lungo
data_long <- data %>%
  pivot_longer(cols = c(ex_solver, gep_solver), names_to = "Group", values_to = "Value")

time_plot=ggplot(data_long, aes(x = Category, y = Value, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = title,
    x = "Threads",
    y = "Time [s]",
    fill = "Solver"
  ) +
  scale_fill_manual(values = c(ex_solver = "purple", gep_solver = "orange")) + 
  theme_minimal()





if(save_res){
  ggsave(filename = paste0(title,format_fl),
         plot = time_plot,
         device = NULL,
         path = path_store_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}





## ----  T = 19, N = 10 -----
i_t = 1
i_a = 2

instant   = as.character(time_instants[i_t])
alphas_sz = as.character(sizes_alphas[i_a])
title = paste0("CV performance with ",paste0(alphas_sz,paste0(" reg params and ",paste0(instant," time instants"))))

times = numeric(n_threads)
times_gen = numeric(n_threads)

for (i in threads) {
  times[i] = times_CV[[i]][i_a,i_t]
  times_gen[i] = times_CV_gen[[i]][i_a,i_t]
}



data <- data.frame(
  Category = categories,
  ex_solver = times,
  gep_solver = times_gen
)

# Trasforma in formato lungo
data_long <- data %>%
  pivot_longer(cols = c(ex_solver, gep_solver), names_to = "Group", values_to = "Value")

time_plot=ggplot(data_long, aes(x = Category, y = Value, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = title,
    x = "Threads",
    y = "Time [s]",
    fill = "Solver"
  ) +
  scale_fill_manual(values = c(ex_solver = "purple", gep_solver = "orange")) + 
  theme_minimal()




if(save_res){
  ggsave(filename = paste0(title,format_fl),
         plot = time_plot,
         device = NULL,
         path = path_store_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}





## ----  T = 19, N = 25 -----
i_t = 1
i_a = 3

instant   = as.character(time_instants[i_t])
alphas_sz = as.character(sizes_alphas[i_a])
title = paste0("CV performance with ",paste0(alphas_sz,paste0(" reg params and ",paste0(instant," time instants"))))

times = numeric(n_threads)
times_gen = numeric(n_threads)

for (i in threads) {
  times[i] = times_CV[[i]][i_a,i_t]
  times_gen[i] = times_CV_gen[[i]][i_a,i_t]
}



data <- data.frame(
  Category = categories,
  ex_solver = times,
  gep_solver = times_gen
)

# Trasforma in formato lungo
data_long <- data %>%
  pivot_longer(cols = c(ex_solver, gep_solver), names_to = "Group", values_to = "Value")

time_plot=ggplot(data_long, aes(x = Category, y = Value, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = title,
    x = "Threads",
    y = "Time [s]",
    fill = "Solver"
  ) +
  scale_fill_manual(values = c(ex_solver = "purple", gep_solver = "orange")) + 
  theme_minimal()




if(save_res){
  ggsave(filename = paste0(title,format_fl),
         plot = time_plot,
         device = NULL,
         path = path_store_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}





## ----  T = 19, N = 50 -----
i_t = 1
i_a = 4

instant   = as.character(time_instants[i_t])
alphas_sz = as.character(sizes_alphas[i_a])
title = paste0("CV performance with ",paste0(alphas_sz,paste0(" reg params and ",paste0(instant," time instants"))))

times = numeric(n_threads)
times_gen = numeric(n_threads)

for (i in threads) {
  times[i] = times_CV[[i]][i_a,i_t]
  times_gen[i] = times_CV_gen[[i]][i_a,i_t]
}



data <- data.frame(
  Category = categories,
  ex_solver = times,
  gep_solver = times_gen
)

# Trasforma in formato lungo
data_long <- data %>%
  pivot_longer(cols = c(ex_solver, gep_solver), names_to = "Group", values_to = "Value")

time_plot=ggplot(data_long, aes(x = Category, y = Value, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = title,
    x = "Threads",
    y = "Time [s]",
    fill = "Solver"
  ) +
  scale_fill_manual(values = c(ex_solver = "purple", gep_solver = "orange")) + 
  theme_minimal()




if(save_res){
  ggsave(filename = paste0(title,format_fl),
         plot = time_plot,
         device = NULL,
         path = path_store_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}





## ----  T = 49, N = 5 -----
i_t = 2
i_a = 1

instant   = as.character(time_instants[i_t])
alphas_sz = as.character(sizes_alphas[i_a])
title = paste0("CV performance with ",paste0(alphas_sz,paste0(" reg params and ",paste0(instant," time instants"))))

times = numeric(n_threads)
times_gen = numeric(n_threads)

for (i in threads) {
  times[i] = times_CV[[i]][i_a,i_t]
  times_gen[i] = times_CV_gen[[i]][i_a,i_t]
}



data <- data.frame(
  Category = categories,
  ex_solver = times,
  gep_solver = times_gen
)

# Trasforma in formato lungo
data_long <- data %>%
  pivot_longer(cols = c(ex_solver, gep_solver), names_to = "Group", values_to = "Value")

time_plot=ggplot(data_long, aes(x = Category, y = Value, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = title,
    x = "Threads",
    y = "Time [s]",
    fill = "Solver"
  ) +
  scale_fill_manual(values = c(ex_solver = "purple", gep_solver = "orange")) + 
  theme_minimal()




if(save_res){
  ggsave(filename = paste0(title,format_fl),
         plot = time_plot,
         device = NULL,
         path = path_store_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}


## ----  T = 49, N = 10 -----
i_t = 2
i_a = 2

instant   = as.character(time_instants[i_t])
alphas_sz = as.character(sizes_alphas[i_a])
title = paste0("CV performance with ",paste0(alphas_sz,paste0(" reg params and ",paste0(instant," time instants"))))

times = numeric(n_threads)
times_gen = numeric(n_threads)

for (i in threads) {
  times[i] = times_CV[[i]][i_a,i_t]
  times_gen[i] = times_CV_gen[[i]][i_a,i_t]
}



data <- data.frame(
  Category = categories,
  ex_solver = times,
  gep_solver = times_gen
)

# Trasforma in formato lungo
data_long <- data %>%
  pivot_longer(cols = c(ex_solver, gep_solver), names_to = "Group", values_to = "Value")

time_plot=ggplot(data_long, aes(x = Category, y = Value, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = title,
    x = "Threads",
    y = "Time [s]",
    fill = "Solver"
  ) +
  scale_fill_manual(values = c(ex_solver = "purple", gep_solver = "orange")) + 
  theme_minimal()




if(save_res){
  ggsave(filename = paste0(title,format_fl),
         plot = time_plot,
         device = NULL,
         path = path_store_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}





## ----  T = 49, N = 25 -----
i_t = 2
i_a = 3

instant   = as.character(time_instants[i_t])
alphas_sz = as.character(sizes_alphas[i_a])
title = paste0("CV performance with ",paste0(alphas_sz,paste0(" reg params and ",paste0(instant," time instants"))))

times = numeric(n_threads)
times_gen = numeric(n_threads)

for (i in threads) {
  times[i] = times_CV[[i]][i_a,i_t]
  times_gen[i] = times_CV_gen[[i]][i_a,i_t]
}



data <- data.frame(
  Category = categories,
  ex_solver = times,
  gep_solver = times_gen
)

# Trasforma in formato lungo
data_long <- data %>%
  pivot_longer(cols = c(ex_solver, gep_solver), names_to = "Group", values_to = "Value")

time_plot=ggplot(data_long, aes(x = Category, y = Value, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = title,
    x = "Threads",
    y = "Time [s]",
    fill = "Solver"
  ) +
  scale_fill_manual(values = c(ex_solver = "purple", gep_solver = "orange")) + 
  theme_minimal()




if(save_res){
  ggsave(filename = paste0(title,format_fl),
         plot = time_plot,
         device = NULL,
         path = path_store_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}





## ----  T = 49, N = 50 -----
i_t = 2
i_a = 4

instant   = as.character(time_instants[i_t])
alphas_sz = as.character(sizes_alphas[i_a])
title = paste0("CV performance with ",paste0(alphas_sz,paste0(" reg params and ",paste0(instant," time instants"))))

times = numeric(n_threads)
times_gen = numeric(n_threads)

for (i in threads) {
  times[i] = times_CV[[i]][i_a,i_t]
  times_gen[i] = times_CV_gen[[i]][i_a,i_t]
}



data <- data.frame(
  Category = categories,
  ex_solver = times,
  gep_solver = times_gen
)

# Trasforma in formato lungo
data_long <- data %>%
  pivot_longer(cols = c(ex_solver, gep_solver), names_to = "Group", values_to = "Value")

time_plot=ggplot(data_long, aes(x = Category, y = Value, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = title,
    x = "Threads",
    y = "Time [s]",
    fill = "Solver"
  ) +
  scale_fill_manual(values = c(ex_solver = "purple", gep_solver = "orange")) + 
  theme_minimal()




if(save_res){
  ggsave(filename = paste0(title,format_fl),
         plot = time_plot,
         device = NULL,
         path = path_store_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}





## ----  T = 99, N = 5 -----
i_t = 3
i_a = 1

instant   = as.character(time_instants[i_t])
alphas_sz = as.character(sizes_alphas[i_a])
title = paste0("CV performance with ",paste0(alphas_sz,paste0(" reg params and ",paste0(instant," time instants"))))

times = numeric(n_threads)
times_gen = numeric(n_threads)

for (i in threads) {
  times[i] = times_CV[[i]][i_a,i_t]
  times_gen[i] = times_CV_gen[[i]][i_a,i_t]
}


data <- data.frame(
  Category = categories,
  ex_solver = times,
  gep_solver = times_gen
)

# Trasforma in formato lungo
data_long <- data %>%
  pivot_longer(cols = c(ex_solver, gep_solver), names_to = "Group", values_to = "Value")

time_plot=ggplot(data_long, aes(x = Category, y = Value, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = title,
    x = "Threads",
    y = "Time [s]",
    fill = "Solver"
  ) +
  scale_fill_manual(values = c(ex_solver = "purple", gep_solver = "orange")) + 
  theme_minimal()




if(save_res){
  ggsave(filename = paste0(title,format_fl),
         plot = time_plot,
         device = NULL,
         path = path_store_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}





## ----  T = 99, N = 10 -----
i_t = 3
i_a = 2

instant   = as.character(time_instants[i_t])
alphas_sz = as.character(sizes_alphas[i_a])
title = paste0("CV performance with ",paste0(alphas_sz,paste0(" reg params and ",paste0(instant," time instants"))))

times = numeric(n_threads)
times_gen = numeric(n_threads)

for (i in threads) {
  times[i] = times_CV[[i]][i_a,i_t]
  times_gen[i] = times_CV_gen[[i]][i_a,i_t]
}



data <- data.frame(
  Category = categories,
  ex_solver = times,
  gep_solver = times_gen
)

# Trasforma in formato lungo
data_long <- data %>%
  pivot_longer(cols = c(ex_solver, gep_solver), names_to = "Group", values_to = "Value")

time_plot=ggplot(data_long, aes(x = Category, y = Value, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = title,
    x = "Threads",
    y = "Time [s]",
    fill = "Solver"
  ) +
  scale_fill_manual(values = c(ex_solver = "purple", gep_solver = "orange")) + 
  theme_minimal()




if(save_res){
  ggsave(filename = paste0(title,format_fl),
         plot = time_plot,
         device = NULL,
         path = path_store_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}





## ----  T = 99, N = 25 -----
i_t = 3
i_a = 3

instant   = as.character(time_instants[i_t])
alphas_sz = as.character(sizes_alphas[i_a])
title = paste0("CV performance with ",paste0(alphas_sz,paste0(" reg params and ",paste0(instant," time instants"))))

times = numeric(n_threads)
times_gen = numeric(n_threads)

for (i in threads) {
  times[i] = times_CV[[i]][i_a,i_t]
  times_gen[i] = times_CV_gen[[i]][i_a,i_t]
}



data <- data.frame(
  Category = categories,
  ex_solver = times,
  gep_solver = times_gen
)

# Trasforma in formato lungo
data_long <- data %>%
  pivot_longer(cols = c(ex_solver, gep_solver), names_to = "Group", values_to = "Value")

time_plot=ggplot(data_long, aes(x = Category, y = Value, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = title,
    x = "Threads",
    y = "Time [s]",
    fill = "Solver"
  ) +
  scale_fill_manual(values = c(ex_solver = "purple", gep_solver = "orange")) + 
  theme_minimal()




if(save_res){
  ggsave(filename = paste0(title,format_fl),
         plot = time_plot,
         device = NULL,
         path = path_store_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}





## ----  T = 99, N = 50 -----
i_t = 3
i_a = 4

instant   = as.character(time_instants[i_t])
alphas_sz = as.character(sizes_alphas[i_a])
title = paste0("CV performance with ",paste0(alphas_sz,paste0(" reg params and ",paste0(instant," time instants"))))

times = numeric(n_threads)
times_gen = numeric(n_threads)

for (i in threads) {
  times[i] = times_CV[[i]][i_a,i_t]
  times_gen[i] = times_CV_gen[[i]][i_a,i_t]
}



data <- data.frame(
  Category = categories,
  ex_solver = times,
  gep_solver = times_gen
)

# Trasforma in formato lungo
data_long <- data %>%
  pivot_longer(cols = c(ex_solver, gep_solver), names_to = "Group", values_to = "Value")

time_plot=ggplot(data_long, aes(x = Category, y = Value, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = title,
    x = "Threads",
    y = "Time [s]",
    fill = "Solver"
  ) +
  scale_fill_manual(values = c(ex_solver = "purple", gep_solver = "orange")) + 
  theme_minimal()




if(save_res){
  ggsave(filename = paste0(title,format_fl),
         plot = time_plot,
         device = NULL,
         path = path_store_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}





## ----  T = 499, N = 5 -----
i_t = 4
i_a = 1

instant   = as.character(time_instants[i_t])
alphas_sz = as.character(sizes_alphas[i_a])
title = paste0("CV performance with ",paste0(alphas_sz,paste0(" reg params and ",paste0(instant," time instants"))))

times = numeric(n_threads)
times_gen = numeric(n_threads)

for (i in threads) {
  times[i] = times_CV[[i]][i_a,i_t]
  times_gen[i] = times_CV_gen[[i]][i_a,i_t]
}



data <- data.frame(
  Category = categories,
  ex_solver = times,
  gep_solver = times_gen
)

# Trasforma in formato lungo
data_long <- data %>%
  pivot_longer(cols = c(ex_solver, gep_solver), names_to = "Group", values_to = "Value")

time_plot=ggplot(data_long, aes(x = Category, y = Value, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = title,
    x = "Threads",
    y = "Time [s]",
    fill = "Solver"
  ) +
  scale_fill_manual(values = c(ex_solver = "purple", gep_solver = "orange")) + 
  theme_minimal()




if(save_res){
  ggsave(filename = paste0(title,format_fl),
         plot = time_plot,
         device = NULL,
         path = path_store_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}






## ----  T = 499, N = 10 -----
i_t = 4
i_a = 2

instant   = as.character(time_instants[i_t])
alphas_sz = as.character(sizes_alphas[i_a])
title = paste0("CV performance with ",paste0(alphas_sz,paste0(" reg params and ",paste0(instant," time instants"))))

times = numeric(n_threads)
times_gen = numeric(n_threads)

for (i in threads) {
  times[i] = times_CV[[i]][i_a,i_t]
  times_gen[i] = times_CV_gen[[i]][i_a,i_t]
}



data <- data.frame(
  Category = categories,
  ex_solver = times,
  gep_solver = times_gen
)

# Trasforma in formato lungo
data_long <- data %>%
  pivot_longer(cols = c(ex_solver, gep_solver), names_to = "Group", values_to = "Value")

time_plot=ggplot(data_long, aes(x = Category, y = Value, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = title,
    x = "Threads",
    y = "Time [s]",
    fill = "Solver"
  ) +
  scale_fill_manual(values = c(ex_solver = "purple", gep_solver = "orange")) + 
  theme_minimal()




if(save_res){
  ggsave(filename = paste0(title,format_fl),
         plot = time_plot,
         device = NULL,
         path = path_store_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}





## ----  T = 499, N = 25 -----
i_t = 4
i_a = 3

instant   = as.character(time_instants[i_t])
alphas_sz = as.character(sizes_alphas[i_a])
title = paste0("CV performance with ",paste0(alphas_sz,paste0(" reg params and ",paste0(instant," time instants"))))

times = numeric(n_threads)
times_gen = numeric(n_threads)

for (i in threads) {
  times[i] = times_CV[[i]][i_a,i_t]
  times_gen[i] = times_CV_gen[[i]][i_a,i_t]
}



data <- data.frame(
  Category = categories,
  ex_solver = times,
  gep_solver = times_gen
)

# Trasforma in formato lungo
data_long <- data %>%
  pivot_longer(cols = c(ex_solver, gep_solver), names_to = "Group", values_to = "Value")

time_plot=ggplot(data_long, aes(x = Category, y = Value, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = title,
    x = "Threads",
    y = "Time [s]",
    fill = "Solver"
  ) +
  scale_fill_manual(values = c(ex_solver = "purple", gep_solver = "orange")) + 
  theme_minimal()




if(save_res){
  ggsave(filename = paste0(title,format_fl),
         plot = time_plot,
         device = NULL,
         path = path_store_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}





## ----  T = 499, N = 50 -----
i_t = 4
i_a = 4

instant   = as.character(time_instants[i_t])
alphas_sz = as.character(sizes_alphas[i_a])
title = paste0("CV performance with ",paste0(alphas_sz,paste0(" reg params and ",paste0(instant," time instants"))))

times = numeric(n_threads)
times_gen = numeric(n_threads)

for (i in threads) {
  times[i] = times_CV[[i]][i_a,i_t]
  times_gen[i] = times_CV_gen[[i]][i_a,i_t]
}



data <- data.frame(
  Category = categories,
  ex_solver = times,
  gep_solver = times_gen
)

# Trasforma in formato lungo
data_long <- data %>%
  pivot_longer(cols = c(ex_solver, gep_solver), names_to = "Group", values_to = "Value")

time_plot=ggplot(data_long, aes(x = Category, y = Value, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = title,
    x = "Threads",
    y = "Time [s]",
    fill = "Solver"
  ) +
  scale_fill_manual(values = c(ex_solver = "purple", gep_solver = "orange")) + 
  theme_minimal()




if(save_res){
  ggsave(filename = paste0(title,format_fl),
         plot = time_plot,
         device = NULL,
         path = path_store_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}





## ----- Plot seq vs par, time vs number of param for every instant, and viceversa-----

## ----- ex_solver-----
#prepare data
col_names = character(4)
row_names = character(4)

for (i in 1:4) {
  col_names[i] = paste0("TI ",as.character(time_instants[i]))
}

time_nt_1 = times_CV[[1]]
colnames(time_nt_1) = col_names

time_nt_5 = times_CV[[5]]
colnames(time_nt_5) = col_names

time_nt_1 <- time_nt_1 %>% mutate(Category = "Sequential", Row = rownames(.))
time_nt_5 <- time_nt_5 %>% mutate(Category = "5 threads parallel", Row = rownames(.))

custom_row_order <- c("5", "10", "25", "50") 
custom_column_order <- c("TI 19", "TI 49", "TI 99", "TI 499") 
custom_column_labels <- c("TI 19" = "19", "TI 49" = "49", "TI 99" = "99", "TI 499" = "499") 


# prepare plot
df_t_long <- bind_rows(time_nt_1, time_nt_5) %>%
  pivot_longer(cols = starts_with("TI "), names_to = "Column", values_to = "Value") %>%
  mutate(
    Row = factor(Row, levels = custom_row_order),         
    Column = factor(Column, levels = custom_column_order) 
  )


# Time vs size of cv params, size of time instants fixed
title1 = "Computational time vs size of alpha input space, number of time instants fixed, ex_solver"

plt1=ggplot(df_t_long, aes(x = Row, y = Value, fill = Category)) +
           geom_bar(stat = "identity", position = position_dodge()) +
           facet_wrap(~Column, nrow = 1,labeller = labeller(Column = custom_column_labels)) + 
           labs(title = title1,
                x = "Size of alpha input space",
                y = "Time [s]",
                fill = " ") +
          theme_minimal()
print(plt1)



# Time vs size of cv params, size of time instants fixed
title2 = "Computational time vs number of time instants, size of alpha input space fixed, ex_solver"

plt2=ggplot(df_t_long, aes(x = Column, y = Value, fill = Category)) +
            geom_bar(stat = "identity", position = position_dodge()) +
            facet_wrap(~Row, nrow = 1) + 
            scale_x_discrete(labels = custom_column_labels) +
            labs(title = title2,
                 x = "Number of time instants",
                 y = "Time [s]",
                 fill = " ") +
             theme_minimal()
print(plt2)


if(save_res){
  
  
  ggsave(filename = paste0(title1,format_fl),
         plot = plt1,
         device = NULL,
         path = path_store_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
  
  ggsave(filename = paste0(title2,format_fl),
         plot = plt2,
         device = NULL,
         path = path_store_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}


## ----- gep_solver-----
#prepare data
col_names = character(4)
row_names = character(4)

for (i in 1:4) {
  col_names[i] = paste0("TI ",as.character(time_instants[i]))
}

time_nt_1 = times_CV_gen[[1]]
colnames(time_nt_1) = col_names

time_nt_5 = times_CV_gen[[5]]
colnames(time_nt_5) = col_names

time_nt_1 <- time_nt_1 %>% mutate(Category = "Sequential", Row = rownames(.))
time_nt_5 <- time_nt_5 %>% mutate(Category = "5 threads parallel", Row = rownames(.))

custom_row_order <- c("5", "10", "25", "50") 
custom_column_order <- c("TI 19", "TI 49", "TI 99", "TI 499") 
custom_column_labels <- c("TI 19" = "19", "TI 49" = "49", "TI 99" = "99", "TI 499" = "499") 


# prepare plot
df_t_long <- bind_rows(time_nt_1, time_nt_5) %>%
  pivot_longer(cols = starts_with("TI "), names_to = "Column", values_to = "Value") %>%
  mutate(
    Row = factor(Row, levels = custom_row_order),         
    Column = factor(Column, levels = custom_column_order) 
  )


# Time vs size of cv params, size of time instants fixed
title1 = "Computational time vs size of alpha input space, number of time instants fixed, gep_solver"

plt1=ggplot(df_t_long, aes(x = Row, y = Value, fill = Category)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~Column, nrow = 1,labeller = labeller(Column = custom_column_labels)) + 
  labs(title = title1,
       x = "Size of alpha input space",
       y = "Time [s]",
       fill = " ") +
  theme_minimal()
print(plt1)



# Time vs size of cv params, size of time instants fixed
title2 = "Computational time vs number of time instants, size of alpha input space fixed, gep_solver"

plt2=ggplot(df_t_long, aes(x = Column, y = Value, fill = Category)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~Row, nrow = 1) + 
  scale_x_discrete(labels = custom_column_labels) +
  labs(title = title2,
       x = "Number of time instants",
       y = "Time [s]",
       fill = " ") +
  theme_minimal()
print(plt2)


if(save_res){
  
  
  ggsave(filename = paste0(title1,format_fl),
         plot = plt1,
         device = NULL,
         path = path_store_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
  
  ggsave(filename = paste0(title2,format_fl),
         plot = plt2,
         device = NULL,
         path = path_store_res,
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
}
