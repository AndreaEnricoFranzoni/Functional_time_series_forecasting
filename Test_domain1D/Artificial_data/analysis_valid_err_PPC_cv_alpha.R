rm(list=ls())
graphics.off()
cat("\014")
set.seed(23032000)

library(PPCKO)

#change here
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"
dir_stor_res = "/Test_domain1D/Artificial_data/results/results_valid_err_alpha"

format = ".jpg"


#if you want to save the result 
save_res = TRUE
#where to store the results
path_stor_res = paste0(dir_w,dir_stor_res)  
path_save_res = paste0(path_stor_res,"/plot_valid_err")

alpha_input_space = c(10^(-10),10^(-9),10^(-8),10^(-7),10^(-6),10^(-5),10^(-4),10^(-3),10^(-2),10^(-1),
                 1,10,10^2,10^3,10^4,10^5,10^6,10^7,10^8,10^9,10^10)



#load files with validation errors
{
  files <- list.files(paste0(path_stor_res,"/cv_alpha"), full.names = TRUE)
  for (file in files) {
    load(file)
  }
  
  files <- list.files(paste0(path_stor_res,"/cv_both"), full.names = TRUE)
  for (file in files) {
    load(file)
  }
}


##----validation errors on alpha doing cv on both parameters----


data_generated = c(c("_gau_0_5","Validation error on regularization parameter, gaussian kernel, norm 0.5"),
                   c("_gau_0_8","Validation error on regularization parameter, gaussian kernel, norm 0.8"),
                   c("_id_0_5","Validation error on regularization parameter, identity kernel, norm 0.5"),
                   c("_id_0_8","Validation error on regularization parameter, identity kernel, norm 0.8"),
                   c("_spt_0_5","Validation error on regularization parameter, sloping plane t kernel, norm 0.5"),
                   c("_spt_0_8","Validation error on regularization parameter, sloping plane t kernel, norm 0.8"),
                   c("_sps_0_5","Validation error on regularization parameter, sloping plane s kernel, norm 0.5"),
                   c("_sps_0_8","Validation error on regularization parameter, sloping plane s kernel, norm 0.8")
                   )
for (i in c(1,3,5,7,9,11,13,15)) {
  valid_errors = get(paste0("valid_err_cv_PPC",data_generated[i]))
  title = data_generated[i+1]
  
  valid_errors_plot = rowMeans(valid_errors)
  
  plot_cv_alpha <- ggplot( data = data.frame(x = 1:length(alpha_input_space), y = valid_errors_plot), aes(x = x, y = y)) +
                   geom_point( color = "blue", size = 1) +  
                   geom_line( color = "blue") +  
                   labs(title = title, x = "alpha", y = "Validation error on alpha (k opt via cv)") +
                   theme_minimal() +
                   scale_x_continuous( breaks = seq(1,length(valid_errors_plot), by = 1), labels = as.character(alpha_input_space)) + 
                   theme(plot.title = element_text( family = "Arial", face = "bold", color = "black", size = 16, hjust = 0.5),
                         panel.grid.major.x = element_line( color = "gray", linewidth = 0.5))
  
  
  ggsave(filename = paste0(paste0("plot_",paste0("valid_err_cv_PPC",data_generated[i])),format),
         plot = plot_cv_alpha,
         device = NULL,
         path = paste0(path_save_res,"/cv_both"),
         scale = 1,
         width = NA,
         height = NA,
         dpi = 300)

}




##---- validation errors on alpha doing cv only on alpha

data_generated = c(c("_gau_0_5","Validation error on regularization parameter, gaussian kernel, norm 0.5"),
                   c("_gau_0_8","Validation error on regularization parameter, gaussian kernel, norm 0.8"),
                   c("_id_0_5","Validation error on regularization parameter, identity kernel, norm 0.5"),
                   c("_id_0_8","Validation error on regularization parameter, identity kernel, norm 0.8"),
                   c("_spt_0_5","Validation error on regularization parameter, sloping plane t kernel, norm 0.5"),
                   c("_spt_0_8","Validation error on regularization parameter, sloping plane t kernel, norm 0.8"),
                   c("_sps_0_5","Validation error on regularization parameter, sloping plane s kernel, norm 0.5"),
                   c("_sps_0_8","Validation error on regularization parameter, sloping plane s kernel, norm 0.8"))

for (i in c(1,3,5,7,9,11,13,15)) {
  valid_errors = get(paste0("valid_err_cv_alpha_PPC",data_generated[i]))
  title = data_generated[i+1]
  
  valid_errors_plot = rowMeans(valid_errors)
  
  plot_cv_alpha <- ggplot( data = data.frame(x = 1:length(alpha_input_space), y = valid_errors_plot), aes(x = x, y = y)) +
    geom_point( color = "blue", size = 1) +  
    geom_line( color = "blue") +  
    labs(title = title, x = "alpha", y = "Validation error on alpha (k via exp pow)") +
    theme_minimal() +
    scale_x_continuous( breaks = seq(1,length(valid_errors_plot), by = 1), labels = as.character(alpha_input_space)) + 
    theme(plot.title = element_text( family = "Arial", face = "bold", color = "black", size = 16, hjust = 0.5),
          panel.grid.major.x = element_line( color = "gray", linewidth = 0.5))
  
  
  ggsave(filename = paste0(paste0("plot_",paste0("valid_err_cv_alpha_PPC",data_generated[i])),format),
         plot = plot_cv_alpha,
         device = NULL,
         path = paste0(path_save_res,"/cv_alpha"),
         scale = 1,
         width = NA,
         height = NA,
         dpi = 300)
}
