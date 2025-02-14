rm(list=ls())
graphics.off()
cat("\014")


#######################################################
#### ADF test for original data, zones and diff ts ####
#######################################################


library(PPCKO)

#put here the path of the local copy of the directory
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"
format_plt = ".png"
#where the predictions made with PPC are
dir_data = paste0(dir_w,"/Test_domain2D/RealWorld_data/results/results_next_instant_prediction")
#where to store the plots
dir_res = paste0(dir_w,"/Test_domain2D/RealWorld_data/results/results_plot_ppcs_next_instant_prediction")


folder_path <- dir_data
files <- list.files(path = folder_path, full.names = TRUE) 
for (file in files) {
  load(file) 
}

x1_lab = "Longitude [°]"
x2_lab = "Latitude [°]"
z_lab  = "SLA [m]"

##-----mouth, next instant----
results_ko = prediction_PPC_mouth_next_instant
title_plt = "ppc_pred_mouth_zone_"
subfolder_res = "/mouth"

results_ko$`Explanatory power PPCs`

results_ko$`Explanatory power PPCs`

z_min_wei = 0
z_max_wei = 0
z_min_dir = 0
z_max_dir = 0

for (i in 1:results_ko$`Number of PPCs retained`) { 
  z_min_wei = min(c(z_min_wei,results_ko$`Weights of PPCs`[[i]]),na.rm=TRUE)
  z_max_wei = max(c(z_max_wei,results_ko$`Weights of PPCs`[[i]]),na.rm=TRUE)
  z_min_dir = min(c(z_min_dir,results_ko$`Directions of PPCs`[[i]]),na.rm=TRUE)
  z_max_dir = max(c(z_max_dir,results_ko$`Directions of PPCs`[[i]]),na.rm=TRUE)
}

z_min_wei_pert = 0
z_max_wei_pert = 0
z_min_dir_pert = 0
z_max_dir_pert = 0

for (i in 1:results_ko$`Number of PPCs retained`) { 
  z_min_wei_pert = min(c(z_min_wei_pert, min( c(results_ko$`Mean function` - results_ko$`Weights of PPCs`[[i]]*results_ko$`Sd scores weights`[i],      results_ko$`Mean function` + results_ko$`Weights of PPCs`[[i]]*results_ko$`Sd scores weights`[i]), na.rm = TRUE )        ),na.rm=TRUE)
  z_max_wei_pert = max(c(z_max_wei_pert, max( c(results_ko$`Mean function` - results_ko$`Weights of PPCs`[[i]]*results_ko$`Sd scores weights`[i],      results_ko$`Mean function` + results_ko$`Weights of PPCs`[[i]]*results_ko$`Sd scores weights`[i]), na.rm = TRUE )        ),na.rm=TRUE)
  z_min_dir_pert = min(c(z_min_dir_pert, min( c(results_ko$`Mean function` - results_ko$`Directions of PPCs`[[i]]*results_ko$`Sd scores directions`[i],results_ko$`Mean function` + results_ko$`Directions of PPCs`[[i]]*results_ko$`Sd scores directions`[i]) ,na.rm = TRUE)   ),na.rm=TRUE)
  z_max_dir_pert = max(c(z_max_dir_pert, max( c(results_ko$`Mean function` - results_ko$`Directions of PPCs`[[i]]*results_ko$`Sd scores directions`[i],results_ko$`Mean function` + results_ko$`Directions of PPCs`[[i]]*results_ko$`Sd scores directions`[i]) ,na.rm = TRUE)   ),na.rm=TRUE)
}

const_map = matrix(1, nrow = dim(results_ko$`Mean function`)[1], ncol = dim(results_ko$`Mean function`)[2])
z_min_const_map_effect = 0
z_max_const_map_effect = 0
for (i in 1:results_ko$`Number of PPCs retained`){
  
  predictive_factor = sum(results_ko$`Weights of PPCs`[[i]] * const_map, na.rm = TRUE)
  ppc_mapped_into_loading = predictive_factor*results_ko$`Directions of PPCs`[[i]]
  z_min_const_map_effect = min(c(z_min_const_map_effect,min(ppc_mapped_into_loading,na.rm = TRUE)))
  z_max_const_map_effect = max(c(z_max_const_map_effect,max(ppc_mapped_into_loading,na.rm = TRUE)))
}

for (i in 1:results_ko$`Number of PPCs retained`){
  
  dir <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  wei <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  
  dir$z <- (results_ko$`Directions of PPCs`[[i]])[cbind(
    match(dir$x, results_ko$`Function discrete evaluations points dim1`),  
    match(dir$y, results_ko$`Function discrete evaluations points dim2`))]
  
  wei$z <- (results_ko$`Weights of PPCs`[[i]])[cbind(
    match(wei$x, results_ko$`Function discrete evaluations points dim1`),  
    match(wei$y, results_ko$`Function discrete evaluations points dim2`))]
  
  
  
  dir_plot <- ggplot(dir, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c( option = "viridis", na.value = "white",limits = c(z_min_dir, z_max_dir) ) +
    labs( x = x1_lab, y = x2_lab, fill = z_lab ) +
    labs( title = paste0("Direction a",i) ) + 
    guides(fill = guide_colorbar(barwidth = 1, barheight = 8)) + 
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5 ),
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank() ) +
    geom_hline( yintercept = seq(min(dir$y), max(dir$y), by = (quantile(dir$y)[2]-quantile(dir$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(dir$x), max(dir$x), by = (quantile(dir$x)[2]-quantile(dir$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  weight_plot <- ggplot(wei, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c(option = "viridis", na.value = "white",limits = c(z_min_wei, z_max_wei)) +
    labs( x = x1_lab, y = x2_lab, fill = z_lab ) +
    guides(fill = guide_colorbar(barwidth = 1, barheight = 8)) + 
    labs(title = paste0("Weight b",i)) + 
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank() ) +
    geom_hline( yintercept = seq(min(wei$y), max(wei$y), by = (quantile(wei$y)[2]-quantile(wei$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(wei$x), max(wei$x), by = (quantile(wei$x)[2]-quantile(wei$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  plot_dir_we <-  weight_plot + dir_plot + plot_layout(ncol = 2) + plot_annotation( title = paste0("PPC",i), theme = theme(plot.title = element_text(face = "bold",hjust = 0.5)) )
  
  
  ggsave(filename = paste0(paste0(title_plt,paste0("PPC",i)),format_plt),
         plot = plot_dir_we,
         device = NULL,
         path = paste0(dir_res,subfolder_res),
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
  
  dir$Category <- factor(ifelse(is.na(dir$z) | dir$z == 0, "Zero/NaN",
                                ifelse(dir$z > 0, "Positivo", "Negativo")))
  
  wei$Category <- factor(ifelse(is.na(wei$z) | wei$z == 0, "Zero/NaN",
                                ifelse(wei$z > 0, "Positivo", "Negativo")))
  
  dir_plot_cat = ggplot(dir, aes(x = x, y = y, fill = Category)) +
    geom_tile() +
    #annotation_map_tile() +
    scale_fill_manual(
      values = c("Negativo" = "blue", "Positivo" = "red", "Zero/NaN" = "white"),
      name = "Direction",
      labels = c("Negativo" = " Negative", 
                 "Positivo" = "Positive", 
                 "Zero/NaN" = "")) +
    theme_minimal() +
    labs(title = paste0("Direction a",i)) +
    labs( x = x1_lab, y = x2_lab) +
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank() ) +
    geom_hline( yintercept = seq(min(wei$y), max(wei$y), by = (quantile(wei$y)[2]-quantile(wei$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(wei$x), max(wei$x), by = (quantile(wei$x)[2]-quantile(wei$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  
  weight_plot_cat = ggplot(wei, aes(x = x, y = y, fill = Category)) +
    geom_tile() +
    scale_fill_manual(
      values = c("Negativo" = "blue", "Positivo" = "red", "Zero/NaN" = "white"),
      name = "Weight",
      labels = c("Negativo" = "Negative", 
                 "Positivo" = "Positive", 
                 "Zero/NaN" = "") ) +
    theme_minimal() +
    labs(title = paste0("Weight b",i)) +
    labs( x = x1_lab, y = x2_lab) +
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank() ) +
    geom_hline( yintercept = seq(min(wei$y), max(wei$y), by = (quantile(wei$y)[2]-quantile(wei$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(wei$x), max(wei$x), by = (quantile(wei$x)[2]-quantile(wei$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  plot_dir_we_cat <-  weight_plot_cat + dir_plot_cat + plot_layout(ncol = 2) + plot_annotation( title = paste0("PPC",i), theme = theme(plot.title = element_text(face = "bold",hjust = 0.5)) )
  
  ggsave(filename = paste0(paste0(paste0(title_plt,paste0("PPC",i),"_cat")),format_plt),
         plot = plot_dir_we_cat,
         device = NULL,
         path = paste0(dir_res,subfolder_res),
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
  
  
  ppc = expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  predictive_factor = sum(results_ko$`Weights of PPCs`[[i]] * const_map, na.rm = TRUE)
  ppc_mapped_into_loading = predictive_factor*results_ko$`Directions of PPCs`[[i]]
  ppc$z <- (ppc_mapped_into_loading)[cbind(
    match(ppc$x, results_ko$`Function discrete evaluations points dim1`),  
    match(ppc$y, results_ko$`Function discrete evaluations points dim2`))]
  
  ppc_plot <- ggplot(ppc, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c( option = "plasma", na.value = "white",limits = c(z_min_const_map_effect, z_max_const_map_effect) ) +
    labs( x = x1_lab, y = x2_lab, fill = z_lab ) +
    labs( title = paste0("Effect of PPC",i) ) + 
    guides(fill = guide_colorbar(barwidth = 1, barheight = 8)) + 
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5 ),
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank() ) +
    geom_hline( yintercept = seq(min(ppc$y), max(ppc$y), by = (quantile(ppc$y)[2]-quantile(ppc$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(ppc$x), max(ppc$x), by = (quantile(ppc$x)[2]-quantile(ppc$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  
  ggsave(filename = paste0(paste0(paste0(title_plt,paste0("PPC",i),"_const_map_effect")),format_plt),
         plot = ppc_plot,
         device = NULL,
         path = paste0(dir_res,subfolder_res),
         scale = 1,
         width = 7,
         height = 10,
         dpi = 300)
}

#plot the mean function  
data_mean <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)

data_mean$z <- (results_ko$`Mean function`)[cbind(
  match(data_mean$x, results_ko$`Function discrete evaluations points dim1`),  
  match(data_mean$y, results_ko$`Function discrete evaluations points dim2`))]

plot_mean <- ggplot(data_mean, aes(x = x, y = y, fill = z)) +
  geom_tile() +
  scale_fill_viridis_c(option = "viridis", na.value = "white",limits = c(z_min_dir_pert, z_max_dir_pert)) +
  labs( title = "Mean function", x = x1_lab, y = x2_lab, fill = z_lab ) +
  guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) + 
  theme_minimal() +
  theme( plot.title = element_text(face = "bold",hjust = 0.5),
         legend.position = "bottom",
         panel.grid.major = element_blank(),  
         panel.grid.minor = element_blank()) +
  geom_hline( yintercept = seq(min(data_mean$y), max(data_mean$y), by = (quantile(data_mean$y)[2]-quantile(data_mean$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
  geom_vline( xintercept = seq(min(data_mean$x), max(data_mean$x), by = (quantile(data_mean$x)[2]-quantile(data_mean$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  

ggsave(filename = paste0(paste0(title_plt,"_mean_function"),format_plt),
       plot = plot_mean,
       device = NULL,
       path = paste0(dir_res,subfolder_res),
       scale = 1,
       width = 7,
       height = 10,
       dpi = 300)  




#PPCs as perturbation of the mean
for (i in 1:results_ko$`Number of PPCs retained`){
  #perturbation of the mean from the direction
  data_up_dir <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  
  data_lw_dir <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  
  data_lw_dir$z <- ( results_ko$`Mean function` - results_ko$`Directions of PPCs`[[i]]*results_ko$`Sd scores directions`[i])[cbind(
    match(data_lw_dir$x, results_ko$`Function discrete evaluations points dim1`),  
    match(data_lw_dir$y, results_ko$`Function discrete evaluations points dim2`))]
  data_up_dir$z <- ( results_ko$`Mean function` + results_ko$`Directions of PPCs`[[i]]*results_ko$`Sd scores directions`[i])[cbind(
    match(data_up_dir$x, results_ko$`Function discrete evaluations points dim1`),  
    match(data_up_dir$y, results_ko$`Function discrete evaluations points dim2`))]
  
  
  plot_up_dir <- ggplot(data_up_dir, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c(option = "viridis", na.value = "white",limits = c(z_min_dir_pert, z_max_dir_pert)) +
    labs( title = paste0(paste0("Direction a",i),", upper mean perturbation"), x = x1_lab, y = x2_lab, fill = z_lab ) +
    guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) +
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           legend.position = "bottom",
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank()) +
    geom_hline( yintercept = seq(min(data_up_dir$y), max(data_up_dir$y), by = (quantile(data_up_dir$y)[2]-quantile(data_up_dir$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(data_up_dir$x), max(data_up_dir$x), by = (quantile(data_up_dir$x)[2]-quantile(data_up_dir$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  plot_bw_dir <- ggplot(data_lw_dir, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c(option = "viridis", na.value = "white",limits = c(z_min_dir_pert, z_max_dir_pert)) +
    labs( title = paste0(paste0("Direction a",i),", lower mean perturbation"), x = x1_lab, y = x2_lab, fill = z_lab ) +
    guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) +
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           legend.position = "bottom",
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank()) +
    geom_hline( yintercept = seq(min(data_lw_dir$y), max(data_lw_dir$y), by = (quantile(data_lw_dir$y)[2]-quantile(data_lw_dir$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(data_lw_dir$x), max(data_lw_dir$x), by = (quantile(data_lw_dir$x)[2]-quantile(data_lw_dir$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  plot_dir_pert <-  plot_bw_dir + plot_up_dir + plot_layout(ncol = 2) + plot_annotation( title = paste0(paste0("PPC",i)," as mean perturbation"), theme = theme(plot.title = element_text(face = "bold",hjust = 0.5)) )
  
  ggsave(filename = paste0(paste0(title_plt,paste0(paste0("PPC",i),"_mean_pert_dir")),format_plt),
         plot = plot_dir_pert,
         device = NULL,
         path = paste0(dir_res,subfolder_res),
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
  
  
  
  
  
  data_up_wei <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  
  data_lw_wei <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  
  data_lw_wei$z <- ( results_ko$`Mean function` - results_ko$`Weights of PPCs`[[i]]*results_ko$`Sd scores weights`[i])[cbind(
    match(data_lw_wei$x, results_ko$`Function discrete evaluations points dim1`),  
    match(data_lw_wei$y, results_ko$`Function discrete evaluations points dim2`))]
  data_up_wei$z <- ( results_ko$`Mean function` + results_ko$`Weights of PPCs`[[i]]*results_ko$`Sd scores weights`[i])[cbind(
    match(data_up_wei$x, results_ko$`Function discrete evaluations points dim1`),  
    match(data_up_wei$y, results_ko$`Function discrete evaluations points dim2`))]
  
  
  plot_up_wei <- ggplot(data_up_wei, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c(option = "viridis", na.value = "white",limits = c(z_min_wei_pert, z_max_wei_pert)) + 
    labs( title = paste0(paste0("Weight b",i),", upper mean perturbation"), x = x1_lab, y = x2_lab, fill = z_lab ) +
    guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) +
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           legend.position = "bottom",
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank()) +
    geom_hline( yintercept = seq(min(data_up_wei$y), max(data_up_wei$y), by = (quantile(data_up_wei$y)[2]-quantile(data_up_wei$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(data_up_wei$x), max(data_up_wei$x), by = (quantile(data_up_wei$x)[2]-quantile(data_up_wei$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  plot_bw_wei<- ggplot(data_lw_wei, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c(option = "viridis", na.value = "white",limits = c(z_min_wei_pert, z_max_wei_pert)) +  
    labs( title = paste0(paste0("Weight b",i),", lower mean perturbation"), x = x1_lab, y = x2_lab, fill = z_lab ) +
    guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) +
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           legend.position = "bottom",
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank()) +
    geom_hline( yintercept = seq(min(data_lw_wei$y), max(data_lw_wei$y), by = (quantile(data_lw_wei$y)[2]-quantile(data_lw_wei$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(data_lw_wei$x), max(data_lw_wei$x), by = (quantile(data_lw_wei$x)[2]-quantile(data_lw_wei$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  
  plot_wei_pert <-  plot_bw_wei + plot_up_wei + plot_layout(ncol = 2) + plot_annotation( title = paste0(paste0("PPC",i)," as mean perturbation"), theme = theme(plot.title = element_text(face = "bold",hjust = 0.5)) )
  
  ggsave(filename = paste0(paste0(title_plt,paste0(paste0("PPC",i),"_mean_pert_wei")),format_plt),
         plot = plot_wei_pert,
         device = NULL,
         path = paste0(dir_res,subfolder_res),
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
  
}  
    

##-----mouth, diff 1, next instant----
results_ko = prediction_PPC_mouth_diff_1_next_instant
title_plt = "ppc_pred_mouth_zone_diff_1_"
subfolder_res = "/mouth_1diff"

results_ko$`Explanatory power PPCs`

z_min_wei = 0
z_max_wei = 0
z_min_dir = 0
z_max_dir = 0

for (i in 1:results_ko$`Number of PPCs retained`) { 
  z_min_wei = min(c(z_min_wei,results_ko$`Weights of PPCs`[[i]]),na.rm=TRUE)
  z_max_wei = max(c(z_max_wei,results_ko$`Weights of PPCs`[[i]]),na.rm=TRUE)
  z_min_dir = min(c(z_min_dir,results_ko$`Directions of PPCs`[[i]]),na.rm=TRUE)
  z_max_dir = max(c(z_max_dir,results_ko$`Directions of PPCs`[[i]]),na.rm=TRUE)
}

z_min_wei_pert = 0
z_max_wei_pert = 0
z_min_dir_pert = 0
z_max_dir_pert = 0

for (i in 1:results_ko$`Number of PPCs retained`) { 
  z_min_wei_pert = min(c(z_min_wei_pert, min( c(results_ko$`Mean function` - results_ko$`Weights of PPCs`[[i]]*results_ko$`Sd scores weights`[i],      results_ko$`Mean function` + results_ko$`Weights of PPCs`[[i]]*results_ko$`Sd scores weights`[i]), na.rm = TRUE )        ),na.rm=TRUE)
  z_max_wei_pert = max(c(z_max_wei_pert, max( c(results_ko$`Mean function` - results_ko$`Weights of PPCs`[[i]]*results_ko$`Sd scores weights`[i],      results_ko$`Mean function` + results_ko$`Weights of PPCs`[[i]]*results_ko$`Sd scores weights`[i]), na.rm = TRUE )        ),na.rm=TRUE)
  z_min_dir_pert = min(c(z_min_dir_pert, min( c(results_ko$`Mean function` - results_ko$`Directions of PPCs`[[i]]*results_ko$`Sd scores directions`[i],results_ko$`Mean function` + results_ko$`Directions of PPCs`[[i]]*results_ko$`Sd scores directions`[i]) ,na.rm = TRUE)   ),na.rm=TRUE)
  z_max_dir_pert = max(c(z_max_dir_pert, max( c(results_ko$`Mean function` - results_ko$`Directions of PPCs`[[i]]*results_ko$`Sd scores directions`[i],results_ko$`Mean function` + results_ko$`Directions of PPCs`[[i]]*results_ko$`Sd scores directions`[i]) ,na.rm = TRUE)   ),na.rm=TRUE)
}

const_map = matrix(1, nrow = dim(results_ko$`Mean function`)[1], ncol = dim(results_ko$`Mean function`)[2])
z_min_const_map_effect = 0
z_max_const_map_effect = 0
for (i in 1:results_ko$`Number of PPCs retained`){
  
  predictive_factor = sum(results_ko$`Weights of PPCs`[[i]] * const_map, na.rm = TRUE)
  ppc_mapped_into_loading = predictive_factor*results_ko$`Directions of PPCs`[[i]]
  z_min_const_map_effect = min(c(z_min_const_map_effect,min(ppc_mapped_into_loading,na.rm = TRUE)))
  z_max_const_map_effect = max(c(z_max_const_map_effect,max(ppc_mapped_into_loading,na.rm = TRUE)))
}

for (i in 1:results_ko$`Number of PPCs retained`){
  
  dir <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  wei <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  
  dir$z <- (results_ko$`Directions of PPCs`[[i]])[cbind(
    match(dir$x, results_ko$`Function discrete evaluations points dim1`),  
    match(dir$y, results_ko$`Function discrete evaluations points dim2`))]
  
  wei$z <- (results_ko$`Weights of PPCs`[[i]])[cbind(
    match(wei$x, results_ko$`Function discrete evaluations points dim1`),  
    match(wei$y, results_ko$`Function discrete evaluations points dim2`))]
  
  
  
  dir_plot <- ggplot(dir, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c( option = "viridis", na.value = "white",limits = c(z_min_dir, z_max_dir) ) +
    labs( x = x1_lab, y = x2_lab, fill = z_lab ) +
    labs( title = paste0("Direction a",i) ) + 
    guides(fill = guide_colorbar(barwidth = 1, barheight = 8)) + 
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5 ),
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank() ) +
    geom_hline( yintercept = seq(min(dir$y), max(dir$y), by = (quantile(dir$y)[2]-quantile(dir$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(dir$x), max(dir$x), by = (quantile(dir$x)[2]-quantile(dir$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  weight_plot <- ggplot(wei, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c(option = "viridis", na.value = "white",limits = c(z_min_wei, z_max_wei)) +
    labs( x = x1_lab, y = x2_lab, fill = z_lab ) +
    guides(fill = guide_colorbar(barwidth = 1, barheight = 8)) + 
    labs(title = paste0("Weight b",i)) + 
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank() ) +
    geom_hline( yintercept = seq(min(wei$y), max(wei$y), by = (quantile(wei$y)[2]-quantile(wei$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(wei$x), max(wei$x), by = (quantile(wei$x)[2]-quantile(wei$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  plot_dir_we <-  weight_plot + dir_plot + plot_layout(ncol = 2) + plot_annotation( title = paste0("PPC",i), theme = theme(plot.title = element_text(face = "bold",hjust = 0.5)) )
  
  
  ggsave(filename = paste0(paste0(title_plt,paste0("PPC",i)),format_plt),
         plot = plot_dir_we,
         device = NULL,
         path = paste0(dir_res,subfolder_res),
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
  
  
  dir$Category <- factor(ifelse(is.na(dir$z) | dir$z == 0, "Zero/NaN",
                                ifelse(dir$z > 0, "Positivo", "Negativo")))
  
  wei$Category <- factor(ifelse(is.na(wei$z) | wei$z == 0, "Zero/NaN",
                                ifelse(wei$z > 0, "Positivo", "Negativo")))
  
  dir_plot_cat = ggplot(dir, aes(x = x, y = y, fill = Category)) +
    geom_tile() +
    #annotation_map_tile() +
    scale_fill_manual(
      values = c("Negativo" = "blue", "Positivo" = "red", "Zero/NaN" = "white"),
      name = "Direction",
      labels = c("Negativo" = " Negative", 
                 "Positivo" = "Positive", 
                 "Zero/NaN" = "")) +
    theme_minimal() +
    labs(title = paste0("Direction a",i)) +
    labs( x = x1_lab, y = x2_lab) +
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank() ) +
    geom_hline( yintercept = seq(min(wei$y), max(wei$y), by = (quantile(wei$y)[2]-quantile(wei$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(wei$x), max(wei$x), by = (quantile(wei$x)[2]-quantile(wei$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  
  weight_plot_cat = ggplot(wei, aes(x = x, y = y, fill = Category)) +
    geom_tile() +
    scale_fill_manual(
      values = c("Negativo" = "blue", "Positivo" = "red", "Zero/NaN" = "white"),
      name = "Weight",
      labels = c("Negativo" = "Negative", 
                 "Positivo" = "Positive", 
                 "Zero/NaN" = "") ) +
    theme_minimal() +
    labs(title = paste0("Weight b",i)) +
    labs( x = x1_lab, y = x2_lab) +
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank() ) +
    geom_hline( yintercept = seq(min(wei$y), max(wei$y), by = (quantile(wei$y)[2]-quantile(wei$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(wei$x), max(wei$x), by = (quantile(wei$x)[2]-quantile(wei$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  plot_dir_we_cat <-  weight_plot_cat + dir_plot_cat + plot_layout(ncol = 2) + plot_annotation( title = paste0("PPC",i), theme = theme(plot.title = element_text(face = "bold",hjust = 0.5)) )
  
  ggsave(filename = paste0(paste0(paste0(title_plt,paste0("PPC",i),"_cat")),format_plt),
         plot = plot_dir_we_cat,
         device = NULL,
         path = paste0(dir_res,subfolder_res),
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
  
  
  ppc = expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  predictive_factor = sum(results_ko$`Weights of PPCs`[[i]] * const_map, na.rm = TRUE)
  ppc_mapped_into_loading = predictive_factor*results_ko$`Directions of PPCs`[[i]]
  ppc$z <- (ppc_mapped_into_loading)[cbind(
    match(ppc$x, results_ko$`Function discrete evaluations points dim1`),  
    match(ppc$y, results_ko$`Function discrete evaluations points dim2`))]
  
  ppc_plot <- ggplot(ppc, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c( option = "plasma", na.value = "white",limits = c(z_min_const_map_effect, z_max_const_map_effect) ) +
    labs( x = x1_lab, y = x2_lab, fill = z_lab ) +
    labs( title = paste0("Effect of PPC",i) ) + 
    guides(fill = guide_colorbar(barwidth = 1, barheight = 8)) + 
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5 ),
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank() ) +
    geom_hline( yintercept = seq(min(ppc$y), max(ppc$y), by = (quantile(ppc$y)[2]-quantile(ppc$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(ppc$x), max(ppc$x), by = (quantile(ppc$x)[2]-quantile(ppc$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  
  ggsave(filename = paste0(paste0(paste0(title_plt,paste0("PPC",i),"_const_map_effect")),format_plt),
         plot = ppc_plot,
         device = NULL,
         path = paste0(dir_res,subfolder_res),
         scale = 1,
         width = 7,
         height = 10,
         dpi = 300)
}

#plot the mean function  
data_mean <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)

data_mean$z <- (results_ko$`Mean function`)[cbind(
  match(data_mean$x, results_ko$`Function discrete evaluations points dim1`),  
  match(data_mean$y, results_ko$`Function discrete evaluations points dim2`))]

plot_mean <- ggplot(data_mean, aes(x = x, y = y, fill = z)) +
  geom_tile() +
  scale_fill_viridis_c(option = "viridis", na.value = "white",limits = c(z_min_dir_pert, z_max_dir_pert)) +
  labs( title = "Mean function", x = x1_lab, y = x2_lab, fill = z_lab ) +
  guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) + 
  theme_minimal() +
  theme( plot.title = element_text(face = "bold",hjust = 0.5),
         legend.position = "bottom",
         panel.grid.major = element_blank(),  
         panel.grid.minor = element_blank()) +
  geom_hline( yintercept = seq(min(data_mean$y), max(data_mean$y), by = (quantile(data_mean$y)[2]-quantile(data_mean$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
  geom_vline( xintercept = seq(min(data_mean$x), max(data_mean$x), by = (quantile(data_mean$x)[2]-quantile(data_mean$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  

ggsave(filename = paste0(paste0(title_plt,"_mean_function"),format_plt),
       plot = plot_mean,
       device = NULL,
       path = paste0(dir_res,subfolder_res),
       scale = 1,
       width = 7,
       height = 10,
       dpi = 300)  




#PPCs as perturbation of the mean
for (i in 1:results_ko$`Number of PPCs retained`){
  #perturbation of the mean from the direction
  data_up_dir <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  
  data_lw_dir <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  
  data_lw_dir$z <- ( results_ko$`Mean function` - results_ko$`Directions of PPCs`[[i]]*results_ko$`Sd scores directions`[i])[cbind(
    match(data_lw_dir$x, results_ko$`Function discrete evaluations points dim1`),  
    match(data_lw_dir$y, results_ko$`Function discrete evaluations points dim2`))]
  data_up_dir$z <- ( results_ko$`Mean function` + results_ko$`Directions of PPCs`[[i]]*results_ko$`Sd scores directions`[i])[cbind(
    match(data_up_dir$x, results_ko$`Function discrete evaluations points dim1`),  
    match(data_up_dir$y, results_ko$`Function discrete evaluations points dim2`))]
  
  
  plot_up_dir <- ggplot(data_up_dir, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c(option = "viridis", na.value = "white",limits = c(z_min_dir_pert, z_max_dir_pert)) +
    labs( title = paste0(paste0("Direction a",i),", upper mean perturbation"), x = x1_lab, y = x2_lab, fill = z_lab ) +
    guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) +
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           legend.position = "bottom",
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank()) +
    geom_hline( yintercept = seq(min(data_up_dir$y), max(data_up_dir$y), by = (quantile(data_up_dir$y)[2]-quantile(data_up_dir$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(data_up_dir$x), max(data_up_dir$x), by = (quantile(data_up_dir$x)[2]-quantile(data_up_dir$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  plot_bw_dir <- ggplot(data_lw_dir, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c(option = "viridis", na.value = "white",limits = c(z_min_dir_pert, z_max_dir_pert)) +
    labs( title = paste0(paste0("Direction a",i),", lower mean perturbation"), x = x1_lab, y = x2_lab, fill = z_lab ) +
    guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) +
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           legend.position = "bottom",
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank()) +
    geom_hline( yintercept = seq(min(data_lw_dir$y), max(data_lw_dir$y), by = (quantile(data_lw_dir$y)[2]-quantile(data_lw_dir$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(data_lw_dir$x), max(data_lw_dir$x), by = (quantile(data_lw_dir$x)[2]-quantile(data_lw_dir$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  plot_dir_pert <-  plot_bw_dir + plot_up_dir + plot_layout(ncol = 2) + plot_annotation( title = paste0(paste0("PPC",i)," as mean perturbation"), theme = theme(plot.title = element_text(face = "bold",hjust = 0.5)) )
  
  ggsave(filename = paste0(paste0(title_plt,paste0(paste0("PPC",i),"_mean_pert_dir")),format_plt),
         plot = plot_dir_pert,
         device = NULL,
         path = paste0(dir_res,subfolder_res),
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
  
  
  
  
  
  data_up_wei <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  
  data_lw_wei <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  
  data_lw_wei$z <- ( results_ko$`Mean function` - results_ko$`Weights of PPCs`[[i]]*results_ko$`Sd scores weights`[i])[cbind(
    match(data_lw_wei$x, results_ko$`Function discrete evaluations points dim1`),  
    match(data_lw_wei$y, results_ko$`Function discrete evaluations points dim2`))]
  data_up_wei$z <- ( results_ko$`Mean function` + results_ko$`Weights of PPCs`[[i]]*results_ko$`Sd scores weights`[i])[cbind(
    match(data_up_wei$x, results_ko$`Function discrete evaluations points dim1`),  
    match(data_up_wei$y, results_ko$`Function discrete evaluations points dim2`))]
  
  
  plot_up_wei <- ggplot(data_up_wei, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c(option = "viridis", na.value = "white",limits = c(z_min_wei_pert, z_max_wei_pert)) + 
    labs( title = paste0(paste0("Weight b",i),", upper mean perturbation"), x = x1_lab, y = x2_lab, fill = z_lab ) +
    guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) +
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           legend.position = "bottom",
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank()) +
    geom_hline( yintercept = seq(min(data_up_wei$y), max(data_up_wei$y), by = (quantile(data_up_wei$y)[2]-quantile(data_up_wei$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(data_up_wei$x), max(data_up_wei$x), by = (quantile(data_up_wei$x)[2]-quantile(data_up_wei$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  plot_bw_wei<- ggplot(data_lw_wei, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c(option = "viridis", na.value = "white",limits = c(z_min_wei_pert, z_max_wei_pert)) +  
    labs( title = paste0(paste0("Weight b",i),", lower mean perturbation"), x = x1_lab, y = x2_lab, fill = z_lab ) +
    guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) +
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           legend.position = "bottom",
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank()) +
    geom_hline( yintercept = seq(min(data_lw_wei$y), max(data_lw_wei$y), by = (quantile(data_lw_wei$y)[2]-quantile(data_lw_wei$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(data_lw_wei$x), max(data_lw_wei$x), by = (quantile(data_lw_wei$x)[2]-quantile(data_lw_wei$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  
  plot_wei_pert <-  plot_bw_wei + plot_up_wei + plot_layout(ncol = 2) + plot_annotation( title = paste0(paste0("PPC",i)," as mean perturbation"), theme = theme(plot.title = element_text(face = "bold",hjust = 0.5)) )
  
  ggsave(filename = paste0(paste0(title_plt,paste0(paste0("PPC",i),"_mean_pert_wei")),format_plt),
         plot = plot_wei_pert,
         device = NULL,
         path = paste0(dir_res,subfolder_res),
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
  
}    




##-----mouth, diff 2, next instant----
results_ko = prediction_PPC_mouth_diff_2_next_instant
title_plt = "ppc_pred_mouth_zone_diff_2_"
subfolder_res = "/mouth_2diff"

results_ko$`Explanatory power PPCs`

z_min_wei = 0
z_max_wei = 0
z_min_dir = 0
z_max_dir = 0

for (i in 1:results_ko$`Number of PPCs retained`) { 
  z_min_wei = min(c(z_min_wei,results_ko$`Weights of PPCs`[[i]]),na.rm=TRUE)
  z_max_wei = max(c(z_max_wei,results_ko$`Weights of PPCs`[[i]]),na.rm=TRUE)
  z_min_dir = min(c(z_min_dir,results_ko$`Directions of PPCs`[[i]]),na.rm=TRUE)
  z_max_dir = max(c(z_max_dir,results_ko$`Directions of PPCs`[[i]]),na.rm=TRUE)
}

z_min_wei_pert = 0
z_max_wei_pert = 0
z_min_dir_pert = 0
z_max_dir_pert = 0

for (i in 1:results_ko$`Number of PPCs retained`) { 
  z_min_wei_pert = min(c(z_min_wei_pert, min( c(results_ko$`Mean function` - results_ko$`Weights of PPCs`[[i]]*results_ko$`Sd scores weights`[i],      results_ko$`Mean function` + results_ko$`Weights of PPCs`[[i]]*results_ko$`Sd scores weights`[i]), na.rm = TRUE )        ),na.rm=TRUE)
  z_max_wei_pert = max(c(z_max_wei_pert, max( c(results_ko$`Mean function` - results_ko$`Weights of PPCs`[[i]]*results_ko$`Sd scores weights`[i],      results_ko$`Mean function` + results_ko$`Weights of PPCs`[[i]]*results_ko$`Sd scores weights`[i]), na.rm = TRUE )        ),na.rm=TRUE)
  z_min_dir_pert = min(c(z_min_dir_pert, min( c(results_ko$`Mean function` - results_ko$`Directions of PPCs`[[i]]*results_ko$`Sd scores directions`[i],results_ko$`Mean function` + results_ko$`Directions of PPCs`[[i]]*results_ko$`Sd scores directions`[i]) ,na.rm = TRUE)   ),na.rm=TRUE)
  z_max_dir_pert = max(c(z_max_dir_pert, max( c(results_ko$`Mean function` - results_ko$`Directions of PPCs`[[i]]*results_ko$`Sd scores directions`[i],results_ko$`Mean function` + results_ko$`Directions of PPCs`[[i]]*results_ko$`Sd scores directions`[i]) ,na.rm = TRUE)   ),na.rm=TRUE)
}

const_map = matrix(1, nrow = dim(results_ko$`Mean function`)[1], ncol = dim(results_ko$`Mean function`)[2])
z_min_const_map_effect = 0
z_max_const_map_effect = 0
for (i in 1:results_ko$`Number of PPCs retained`){
  
  predictive_factor = sum(results_ko$`Weights of PPCs`[[i]] * const_map, na.rm = TRUE)
  ppc_mapped_into_loading = predictive_factor*results_ko$`Directions of PPCs`[[i]]
  z_min_const_map_effect = min(c(z_min_const_map_effect,min(ppc_mapped_into_loading,na.rm = TRUE)))
  z_max_const_map_effect = max(c(z_max_const_map_effect,max(ppc_mapped_into_loading,na.rm = TRUE)))
}

for (i in 1:results_ko$`Number of PPCs retained`){
  
  dir <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  wei <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  
  dir$z <- (results_ko$`Directions of PPCs`[[i]])[cbind(
    match(dir$x, results_ko$`Function discrete evaluations points dim1`),  
    match(dir$y, results_ko$`Function discrete evaluations points dim2`))]
  
  wei$z <- (results_ko$`Weights of PPCs`[[i]])[cbind(
    match(wei$x, results_ko$`Function discrete evaluations points dim1`),  
    match(wei$y, results_ko$`Function discrete evaluations points dim2`))]
  
  
  
  dir_plot <- ggplot(dir, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c( option = "viridis", na.value = "white",limits = c(z_min_dir, z_max_dir) ) +
    labs( x = x1_lab, y = x2_lab, fill = z_lab ) +
    labs( title = paste0("Direction a",i) ) + 
    guides(fill = guide_colorbar(barwidth = 1, barheight = 8)) + 
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5 ),
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank() ) +
    geom_hline( yintercept = seq(min(dir$y), max(dir$y), by = (quantile(dir$y)[2]-quantile(dir$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(dir$x), max(dir$x), by = (quantile(dir$x)[2]-quantile(dir$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  weight_plot <- ggplot(wei, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c(option = "viridis", na.value = "white",limits = c(z_min_wei, z_max_wei)) +
    labs( x = x1_lab, y = x2_lab, fill = z_lab ) +
    guides(fill = guide_colorbar(barwidth = 1, barheight = 8)) + 
    labs(title = paste0("Weight b",i)) + 
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank() ) +
    geom_hline( yintercept = seq(min(wei$y), max(wei$y), by = (quantile(wei$y)[2]-quantile(wei$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(wei$x), max(wei$x), by = (quantile(wei$x)[2]-quantile(wei$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  plot_dir_we <-  weight_plot + dir_plot + plot_layout(ncol = 2) + plot_annotation( title = paste0("PPC",i), theme = theme(plot.title = element_text(face = "bold",hjust = 0.5)) )
  
  
  ggsave(filename = paste0(paste0(title_plt,paste0("PPC",i)),format_plt),
         plot = plot_dir_we,
         device = NULL,
         path = paste0(dir_res,subfolder_res),
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
  
  
  dir$Category <- factor(ifelse(is.na(dir$z) | dir$z == 0, "Zero/NaN",
                                ifelse(dir$z > 0, "Positivo", "Negativo")))
  
  wei$Category <- factor(ifelse(is.na(wei$z) | wei$z == 0, "Zero/NaN",
                                ifelse(wei$z > 0, "Positivo", "Negativo")))
  
  dir_plot_cat = ggplot(dir, aes(x = x, y = y, fill = Category)) +
    geom_tile() +
    #annotation_map_tile() +
    scale_fill_manual(
      values = c("Negativo" = "blue", "Positivo" = "red", "Zero/NaN" = "white"),
      name = "Direction",
      labels = c("Negativo" = " Negative", 
                 "Positivo" = "Positive", 
                 "Zero/NaN" = "")) +
    theme_minimal() +
    labs(title = paste0("Direction a",i)) +
    labs( x = x1_lab, y = x2_lab) +
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank() ) +
    geom_hline( yintercept = seq(min(wei$y), max(wei$y), by = (quantile(wei$y)[2]-quantile(wei$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(wei$x), max(wei$x), by = (quantile(wei$x)[2]-quantile(wei$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  
  weight_plot_cat = ggplot(wei, aes(x = x, y = y, fill = Category)) +
    geom_tile() +
    scale_fill_manual(
      values = c("Negativo" = "blue", "Positivo" = "red", "Zero/NaN" = "white"),
      name = "Weight",
      labels = c("Negativo" = "Negative", 
                 "Positivo" = "Positive", 
                 "Zero/NaN" = "") ) +
    theme_minimal() +
    labs(title = paste0("Weight b",i)) +
    labs( x = x1_lab, y = x2_lab) +
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank() ) +
    geom_hline( yintercept = seq(min(wei$y), max(wei$y), by = (quantile(wei$y)[2]-quantile(wei$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(wei$x), max(wei$x), by = (quantile(wei$x)[2]-quantile(wei$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  plot_dir_we_cat <-  weight_plot_cat + dir_plot_cat + plot_layout(ncol = 2) + plot_annotation( title = paste0("PPC",i), theme = theme(plot.title = element_text(face = "bold",hjust = 0.5)) )
  
  ggsave(filename = paste0(paste0(paste0(title_plt,paste0("PPC",i),"_cat")),format_plt),
         plot = plot_dir_we_cat,
         device = NULL,
         path = paste0(dir_res,subfolder_res),
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
  
  ppc = expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  predictive_factor = sum(results_ko$`Weights of PPCs`[[i]] * const_map, na.rm = TRUE)
  ppc_mapped_into_loading = predictive_factor*results_ko$`Directions of PPCs`[[i]]
  ppc$z <- (ppc_mapped_into_loading)[cbind(
    match(ppc$x, results_ko$`Function discrete evaluations points dim1`),  
    match(ppc$y, results_ko$`Function discrete evaluations points dim2`))]
  
  ppc_plot <- ggplot(ppc, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c( option = "plasma", na.value = "white",limits = c(z_min_const_map_effect, z_max_const_map_effect) ) +
    labs( x = x1_lab, y = x2_lab, fill = z_lab ) +
    labs( title = paste0("Effect of PPC",i) ) + 
    guides(fill = guide_colorbar(barwidth = 1, barheight = 8)) + 
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5 ),
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank() ) +
    geom_hline( yintercept = seq(min(ppc$y), max(ppc$y), by = (quantile(ppc$y)[2]-quantile(ppc$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(ppc$x), max(ppc$x), by = (quantile(ppc$x)[2]-quantile(ppc$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  
  ggsave(filename = paste0(paste0(paste0(title_plt,paste0("PPC",i),"_const_map_effect")),format_plt),
         plot = ppc_plot,
         device = NULL,
         path = paste0(dir_res,subfolder_res),
         scale = 1,
         width = 7,
         height = 10,
         dpi = 300)
}

#plot the mean function  
data_mean <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)

data_mean$z <- (results_ko$`Mean function`)[cbind(
  match(data_mean$x, results_ko$`Function discrete evaluations points dim1`),  
  match(data_mean$y, results_ko$`Function discrete evaluations points dim2`))]

plot_mean <- ggplot(data_mean, aes(x = x, y = y, fill = z)) +
  geom_tile() +
  scale_fill_viridis_c(option = "viridis", na.value = "white",limits = c(z_min_dir_pert, z_max_dir_pert)) +
  labs( title = "Mean function", x = x1_lab, y = x2_lab, fill = z_lab ) +
  guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) + 
  theme_minimal() +
  theme( plot.title = element_text(face = "bold",hjust = 0.5),
         legend.position = "bottom",
         panel.grid.major = element_blank(),  
         panel.grid.minor = element_blank()) +
  geom_hline( yintercept = seq(min(data_mean$y), max(data_mean$y), by = (quantile(data_mean$y)[2]-quantile(data_mean$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
  geom_vline( xintercept = seq(min(data_mean$x), max(data_mean$x), by = (quantile(data_mean$x)[2]-quantile(data_mean$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  

ggsave(filename = paste0(paste0(title_plt,"_mean_function"),format_plt),
       plot = plot_mean,
       device = NULL,
       path = paste0(dir_res,subfolder_res),
       scale = 1,
       width = 7,
       height = 10,
       dpi = 300)  




#PPCs as perturbation of the mean
for (i in 1:results_ko$`Number of PPCs retained`){
  #perturbation of the mean from the direction
  data_up_dir <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  
  data_lw_dir <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  
  data_lw_dir$z <- ( results_ko$`Mean function` - results_ko$`Directions of PPCs`[[i]]*results_ko$`Sd scores directions`[i])[cbind(
    match(data_lw_dir$x, results_ko$`Function discrete evaluations points dim1`),  
    match(data_lw_dir$y, results_ko$`Function discrete evaluations points dim2`))]
  data_up_dir$z <- ( results_ko$`Mean function` + results_ko$`Directions of PPCs`[[i]]*results_ko$`Sd scores directions`[i])[cbind(
    match(data_up_dir$x, results_ko$`Function discrete evaluations points dim1`),  
    match(data_up_dir$y, results_ko$`Function discrete evaluations points dim2`))]
  
  
  plot_up_dir <- ggplot(data_up_dir, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c(option = "viridis", na.value = "white",limits = c(z_min_dir_pert, z_max_dir_pert)) +
    labs( title = paste0(paste0("Direction a",i),", upper mean perturbation"), x = x1_lab, y = x2_lab, fill = z_lab ) +
    guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) +
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           legend.position = "bottom",
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank()) +
    geom_hline( yintercept = seq(min(data_up_dir$y), max(data_up_dir$y), by = (quantile(data_up_dir$y)[2]-quantile(data_up_dir$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(data_up_dir$x), max(data_up_dir$x), by = (quantile(data_up_dir$x)[2]-quantile(data_up_dir$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  plot_bw_dir <- ggplot(data_lw_dir, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c(option = "viridis", na.value = "white",limits = c(z_min_dir_pert, z_max_dir_pert)) +
    labs( title = paste0(paste0("Direction a",i),", lower mean perturbation"), x = x1_lab, y = x2_lab, fill = z_lab ) +
    guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) +
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           legend.position = "bottom",
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank()) +
    geom_hline( yintercept = seq(min(data_lw_dir$y), max(data_lw_dir$y), by = (quantile(data_lw_dir$y)[2]-quantile(data_lw_dir$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(data_lw_dir$x), max(data_lw_dir$x), by = (quantile(data_lw_dir$x)[2]-quantile(data_lw_dir$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  plot_dir_pert <-  plot_bw_dir + plot_up_dir + plot_layout(ncol = 2) + plot_annotation( title = paste0(paste0("PPC",i)," as mean perturbation"), theme = theme(plot.title = element_text(face = "bold",hjust = 0.5)) )
  
  ggsave(filename = paste0(paste0(title_plt,paste0(paste0("PPC",i),"_mean_pert_dir")),format_plt),
         plot = plot_dir_pert,
         device = NULL,
         path = paste0(dir_res,subfolder_res),
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
  
  
  
  
  
  data_up_wei <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  
  data_lw_wei <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  
  data_lw_wei$z <- ( results_ko$`Mean function` - results_ko$`Weights of PPCs`[[i]]*results_ko$`Sd scores weights`[i])[cbind(
    match(data_lw_wei$x, results_ko$`Function discrete evaluations points dim1`),  
    match(data_lw_wei$y, results_ko$`Function discrete evaluations points dim2`))]
  data_up_wei$z <- ( results_ko$`Mean function` + results_ko$`Weights of PPCs`[[i]]*results_ko$`Sd scores weights`[i])[cbind(
    match(data_up_wei$x, results_ko$`Function discrete evaluations points dim1`),  
    match(data_up_wei$y, results_ko$`Function discrete evaluations points dim2`))]
  
  
  plot_up_wei <- ggplot(data_up_wei, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c(option = "viridis", na.value = "white",limits = c(z_min_wei_pert, z_max_wei_pert)) + 
    labs( title = paste0(paste0("Weight b",i),", upper mean perturbation"), x = x1_lab, y = x2_lab, fill = z_lab ) +
    guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) +
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           legend.position = "bottom",
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank()) +
    geom_hline( yintercept = seq(min(data_up_wei$y), max(data_up_wei$y), by = (quantile(data_up_wei$y)[2]-quantile(data_up_wei$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(data_up_wei$x), max(data_up_wei$x), by = (quantile(data_up_wei$x)[2]-quantile(data_up_wei$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  plot_bw_wei<- ggplot(data_lw_wei, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c(option = "viridis", na.value = "white",limits = c(z_min_wei_pert, z_max_wei_pert)) +  
    labs( title = paste0(paste0("Weight b",i),", lower mean perturbation"), x = x1_lab, y = x2_lab, fill = z_lab ) +
    guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) +
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           legend.position = "bottom",
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank()) +
    geom_hline( yintercept = seq(min(data_lw_wei$y), max(data_lw_wei$y), by = (quantile(data_lw_wei$y)[2]-quantile(data_lw_wei$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(data_lw_wei$x), max(data_lw_wei$x), by = (quantile(data_lw_wei$x)[2]-quantile(data_lw_wei$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  
  plot_wei_pert <-  plot_bw_wei + plot_up_wei + plot_layout(ncol = 2) + plot_annotation( title = paste0(paste0("PPC",i)," as mean perturbation"), theme = theme(plot.title = element_text(face = "bold",hjust = 0.5)) )
  
  ggsave(filename = paste0(paste0(title_plt,paste0(paste0("PPC",i),"_mean_pert_wei")),format_plt),
         plot = plot_wei_pert,
         device = NULL,
         path = paste0(dir_res,subfolder_res),
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
  
}  



##-----center, next instant----
results_ko = prediction_PPC_center_next_instant
title_plt = "ppc_pred_center_zone_"
subfolder_res = "/center"

results_ko$`Explanatory power PPCs`

z_min_wei = 0
z_max_wei = 0
z_min_dir = 0
z_max_dir = 0

for (i in 1:results_ko$`Number of PPCs retained`) { 
  z_min_wei = min(c(z_min_wei,results_ko$`Weights of PPCs`[[i]]),na.rm=TRUE)
  z_max_wei = max(c(z_max_wei,results_ko$`Weights of PPCs`[[i]]),na.rm=TRUE)
  z_min_dir = min(c(z_min_dir,results_ko$`Directions of PPCs`[[i]]),na.rm=TRUE)
  z_max_dir = max(c(z_max_dir,results_ko$`Directions of PPCs`[[i]]),na.rm=TRUE)
}

z_min_wei_pert = 0
z_max_wei_pert = 0
z_min_dir_pert = 0
z_max_dir_pert = 0

for (i in 1:results_ko$`Number of PPCs retained`) { 
  z_min_wei_pert = min(c(z_min_wei_pert, min( c(results_ko$`Mean function` - results_ko$`Weights of PPCs`[[i]]*results_ko$`Sd scores weights`[i],      results_ko$`Mean function` + results_ko$`Weights of PPCs`[[i]]*results_ko$`Sd scores weights`[i]), na.rm = TRUE )        ),na.rm=TRUE)
  z_max_wei_pert = max(c(z_max_wei_pert, max( c(results_ko$`Mean function` - results_ko$`Weights of PPCs`[[i]]*results_ko$`Sd scores weights`[i],      results_ko$`Mean function` + results_ko$`Weights of PPCs`[[i]]*results_ko$`Sd scores weights`[i]), na.rm = TRUE )        ),na.rm=TRUE)
  z_min_dir_pert = min(c(z_min_dir_pert, min( c(results_ko$`Mean function` - results_ko$`Directions of PPCs`[[i]]*results_ko$`Sd scores directions`[i],results_ko$`Mean function` + results_ko$`Directions of PPCs`[[i]]*results_ko$`Sd scores directions`[i]) ,na.rm = TRUE)   ),na.rm=TRUE)
  z_max_dir_pert = max(c(z_max_dir_pert, max( c(results_ko$`Mean function` - results_ko$`Directions of PPCs`[[i]]*results_ko$`Sd scores directions`[i],results_ko$`Mean function` + results_ko$`Directions of PPCs`[[i]]*results_ko$`Sd scores directions`[i]) ,na.rm = TRUE)   ),na.rm=TRUE)
}

const_map = matrix(1, nrow = dim(results_ko$`Mean function`)[1], ncol = dim(results_ko$`Mean function`)[2])
z_min_const_map_effect = 0
z_max_const_map_effect = 0
for (i in 1:results_ko$`Number of PPCs retained`){
  
  predictive_factor = sum(results_ko$`Weights of PPCs`[[i]] * const_map, na.rm = TRUE)
  ppc_mapped_into_loading = predictive_factor*results_ko$`Directions of PPCs`[[i]]
  z_min_const_map_effect = min(c(z_min_const_map_effect,min(ppc_mapped_into_loading,na.rm = TRUE)))
  z_max_const_map_effect = max(c(z_max_const_map_effect,max(ppc_mapped_into_loading,na.rm = TRUE)))
}

for (i in 1:results_ko$`Number of PPCs retained`){
  
  dir <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  wei <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  
  dir$z <- (results_ko$`Directions of PPCs`[[i]])[cbind(
    match(dir$x, results_ko$`Function discrete evaluations points dim1`),  
    match(dir$y, results_ko$`Function discrete evaluations points dim2`))]
  
  wei$z <- (results_ko$`Weights of PPCs`[[i]])[cbind(
    match(wei$x, results_ko$`Function discrete evaluations points dim1`),  
    match(wei$y, results_ko$`Function discrete evaluations points dim2`))]
  
  
  
  dir_plot <- ggplot(dir, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c( option = "viridis", na.value = "white",limits = c(z_min_dir, z_max_dir) ) +
    labs( x = x1_lab, y = x2_lab, fill = z_lab ) +
    labs( title = paste0("Direction a",i) ) + 
    guides(fill = guide_colorbar(barwidth = 1, barheight = 8)) + 
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5 ),
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank() ) +
    geom_hline( yintercept = seq(min(dir$y), max(dir$y), by = (quantile(dir$y)[2]-quantile(dir$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(dir$x), max(dir$x), by = (quantile(dir$x)[2]-quantile(dir$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  weight_plot <- ggplot(wei, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c(option = "viridis", na.value = "white",limits = c(z_min_wei, z_max_wei)) +
    labs( x = x1_lab, y = x2_lab, fill = z_lab ) +
    guides(fill = guide_colorbar(barwidth = 1, barheight = 8)) + 
    labs(title = paste0("Weight b",i)) + 
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank() ) +
    geom_hline( yintercept = seq(min(wei$y), max(wei$y), by = (quantile(wei$y)[2]-quantile(wei$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(wei$x), max(wei$x), by = (quantile(wei$x)[2]-quantile(wei$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  plot_dir_we <-  weight_plot + dir_plot + plot_layout(ncol = 2) + plot_annotation( title = paste0("PPC",i), theme = theme(plot.title = element_text(face = "bold",hjust = 0.5)) )
  
  
  ggsave(filename = paste0(paste0(title_plt,paste0("PPC",i)),format_plt),
         plot = plot_dir_we,
         device = NULL,
         path = paste0(dir_res,subfolder_res),
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
  
  
  dir$Category <- factor(ifelse(is.na(dir$z) | dir$z == 0, "Zero/NaN",
                                ifelse(dir$z > 0, "Positivo", "Negativo")))
  
  wei$Category <- factor(ifelse(is.na(wei$z) | wei$z == 0, "Zero/NaN",
                                ifelse(wei$z > 0, "Positivo", "Negativo")))
  
  dir_plot_cat = ggplot(dir, aes(x = x, y = y, fill = Category)) +
    geom_tile() +
    #annotation_map_tile() +
    scale_fill_manual(
      values = c("Negativo" = "blue", "Positivo" = "red", "Zero/NaN" = "white"),
      name = "Direction",
      labels = c("Negativo" = " Negative", 
                 "Positivo" = "Positive", 
                 "Zero/NaN" = "")) +
    theme_minimal() +
    labs(title = paste0("Direction a",i)) +
    labs( x = x1_lab, y = x2_lab) +
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank() ) +
    geom_hline( yintercept = seq(min(wei$y), max(wei$y), by = (quantile(wei$y)[2]-quantile(wei$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(wei$x), max(wei$x), by = (quantile(wei$x)[2]-quantile(wei$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  
  weight_plot_cat = ggplot(wei, aes(x = x, y = y, fill = Category)) +
    geom_tile() +
    scale_fill_manual(
      values = c("Negativo" = "blue", "Positivo" = "red", "Zero/NaN" = "white"),
      name = "Weight",
      labels = c("Negativo" = "Negative", 
                 "Positivo" = "Positive", 
                 "Zero/NaN" = "") ) +
    theme_minimal() +
    labs(title = paste0("Weight b",i)) +
    labs( x = x1_lab, y = x2_lab) +
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank() ) +
    geom_hline( yintercept = seq(min(wei$y), max(wei$y), by = (quantile(wei$y)[2]-quantile(wei$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(wei$x), max(wei$x), by = (quantile(wei$x)[2]-quantile(wei$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  plot_dir_we_cat <-  weight_plot_cat + dir_plot_cat + plot_layout(ncol = 2) + plot_annotation( title = paste0("PPC",i), theme = theme(plot.title = element_text(face = "bold",hjust = 0.5)) )
  
  ggsave(filename = paste0(paste0(paste0(title_plt,paste0("PPC",i),"_cat")),format_plt),
         plot = plot_dir_we_cat,
         device = NULL,
         path = paste0(dir_res,subfolder_res),
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
  
  ppc = expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  predictive_factor = sum(results_ko$`Weights of PPCs`[[i]] * const_map, na.rm = TRUE)
  ppc_mapped_into_loading = predictive_factor*results_ko$`Directions of PPCs`[[i]]
  ppc$z <- (ppc_mapped_into_loading)[cbind(
    match(ppc$x, results_ko$`Function discrete evaluations points dim1`),  
    match(ppc$y, results_ko$`Function discrete evaluations points dim2`))]
  
  ppc_plot <- ggplot(ppc, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c( option = "plasma", na.value = "white",limits = c(z_min_const_map_effect, z_max_const_map_effect) ) +
    labs( x = x1_lab, y = x2_lab, fill = z_lab ) +
    labs( title = paste0("Effect of PPC",i) ) + 
    guides(fill = guide_colorbar(barwidth = 1, barheight = 8)) + 
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5 ),
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank() ) +
    geom_hline( yintercept = seq(min(ppc$y), max(ppc$y), by = (quantile(ppc$y)[2]-quantile(ppc$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(ppc$x), max(ppc$x), by = (quantile(ppc$x)[2]-quantile(ppc$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  
  ggsave(filename = paste0(paste0(paste0(title_plt,paste0("PPC",i),"_const_map_effect")),format_plt),
         plot = ppc_plot,
         device = NULL,
         path = paste0(dir_res,subfolder_res),
         scale = 1,
         width = 7,
         height = 10,
         dpi = 300)
}

#plot the mean function  
data_mean <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)

data_mean$z <- (results_ko$`Mean function`)[cbind(
  match(data_mean$x, results_ko$`Function discrete evaluations points dim1`),  
  match(data_mean$y, results_ko$`Function discrete evaluations points dim2`))]

plot_mean <- ggplot(data_mean, aes(x = x, y = y, fill = z)) +
  geom_tile() +
  scale_fill_viridis_c(option = "viridis", na.value = "white",limits = c(z_min_dir_pert, z_max_dir_pert)) +
  labs( title = "Mean function", x = x1_lab, y = x2_lab, fill = z_lab ) +
  guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) + 
  theme_minimal() +
  theme( plot.title = element_text(face = "bold",hjust = 0.5),
         legend.position = "bottom",
         panel.grid.major = element_blank(),  
         panel.grid.minor = element_blank()) +
  geom_hline( yintercept = seq(min(data_mean$y), max(data_mean$y), by = (quantile(data_mean$y)[2]-quantile(data_mean$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
  geom_vline( xintercept = seq(min(data_mean$x), max(data_mean$x), by = (quantile(data_mean$x)[2]-quantile(data_mean$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  

ggsave(filename = paste0(paste0(title_plt,"_mean_function"),format_plt),
       plot = plot_mean,
       device = NULL,
       path = paste0(dir_res,subfolder_res),
       scale = 1,
       width = 7,
       height = 10,
       dpi = 300)  




#PPCs as perturbation of the mean
for (i in 1:results_ko$`Number of PPCs retained`){
  #perturbation of the mean from the direction
  data_up_dir <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  
  data_lw_dir <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  
  data_lw_dir$z <- ( results_ko$`Mean function` - results_ko$`Directions of PPCs`[[i]]*results_ko$`Sd scores directions`[i])[cbind(
    match(data_lw_dir$x, results_ko$`Function discrete evaluations points dim1`),  
    match(data_lw_dir$y, results_ko$`Function discrete evaluations points dim2`))]
  data_up_dir$z <- ( results_ko$`Mean function` + results_ko$`Directions of PPCs`[[i]]*results_ko$`Sd scores directions`[i])[cbind(
    match(data_up_dir$x, results_ko$`Function discrete evaluations points dim1`),  
    match(data_up_dir$y, results_ko$`Function discrete evaluations points dim2`))]
  
  
  plot_up_dir <- ggplot(data_up_dir, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c(option = "viridis", na.value = "white",limits = c(z_min_dir_pert, z_max_dir_pert)) +
    labs( title = paste0(paste0("Direction a",i),", upper mean perturbation"), x = x1_lab, y = x2_lab, fill = z_lab ) +
    guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) +
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           legend.position = "bottom",
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank()) +
    geom_hline( yintercept = seq(min(data_up_dir$y), max(data_up_dir$y), by = (quantile(data_up_dir$y)[2]-quantile(data_up_dir$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(data_up_dir$x), max(data_up_dir$x), by = (quantile(data_up_dir$x)[2]-quantile(data_up_dir$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  plot_bw_dir <- ggplot(data_lw_dir, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c(option = "viridis", na.value = "white",limits = c(z_min_dir_pert, z_max_dir_pert)) +
    labs( title = paste0(paste0("Direction a",i),", lower mean perturbation"), x = x1_lab, y = x2_lab, fill = z_lab ) +
    guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) +
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           legend.position = "bottom",
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank()) +
    geom_hline( yintercept = seq(min(data_lw_dir$y), max(data_lw_dir$y), by = (quantile(data_lw_dir$y)[2]-quantile(data_lw_dir$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(data_lw_dir$x), max(data_lw_dir$x), by = (quantile(data_lw_dir$x)[2]-quantile(data_lw_dir$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  plot_dir_pert <-  plot_bw_dir + plot_up_dir + plot_layout(ncol = 2) + plot_annotation( title = paste0(paste0("PPC",i)," as mean perturbation"), theme = theme(plot.title = element_text(face = "bold",hjust = 0.5)) )
  
  ggsave(filename = paste0(paste0(title_plt,paste0(paste0("PPC",i),"_mean_pert_dir")),format_plt),
         plot = plot_dir_pert,
         device = NULL,
         path = paste0(dir_res,subfolder_res),
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
  
  
  
  
  
  data_up_wei <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  
  data_lw_wei <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  
  data_lw_wei$z <- ( results_ko$`Mean function` - results_ko$`Weights of PPCs`[[i]]*results_ko$`Sd scores weights`[i])[cbind(
    match(data_lw_wei$x, results_ko$`Function discrete evaluations points dim1`),  
    match(data_lw_wei$y, results_ko$`Function discrete evaluations points dim2`))]
  data_up_wei$z <- ( results_ko$`Mean function` + results_ko$`Weights of PPCs`[[i]]*results_ko$`Sd scores weights`[i])[cbind(
    match(data_up_wei$x, results_ko$`Function discrete evaluations points dim1`),  
    match(data_up_wei$y, results_ko$`Function discrete evaluations points dim2`))]
  
  
  plot_up_wei <- ggplot(data_up_wei, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c(option = "viridis", na.value = "white",limits = c(z_min_wei_pert, z_max_wei_pert)) + 
    labs( title = paste0(paste0("Weight b",i),", upper mean perturbation"), x = x1_lab, y = x2_lab, fill = z_lab ) +
    guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) +
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           legend.position = "bottom",
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank()) +
    geom_hline( yintercept = seq(min(data_up_wei$y), max(data_up_wei$y), by = (quantile(data_up_wei$y)[2]-quantile(data_up_wei$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(data_up_wei$x), max(data_up_wei$x), by = (quantile(data_up_wei$x)[2]-quantile(data_up_wei$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  plot_bw_wei<- ggplot(data_lw_wei, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c(option = "viridis", na.value = "white",limits = c(z_min_wei_pert, z_max_wei_pert)) +  
    labs( title = paste0(paste0("Weight b",i),", lower mean perturbation"), x = x1_lab, y = x2_lab, fill = z_lab ) +
    guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) +
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           legend.position = "bottom",
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank()) +
    geom_hline( yintercept = seq(min(data_lw_wei$y), max(data_lw_wei$y), by = (quantile(data_lw_wei$y)[2]-quantile(data_lw_wei$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(data_lw_wei$x), max(data_lw_wei$x), by = (quantile(data_lw_wei$x)[2]-quantile(data_lw_wei$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  
  plot_wei_pert <-  plot_bw_wei + plot_up_wei + plot_layout(ncol = 2) + plot_annotation( title = paste0(paste0("PPC",i)," as mean perturbation"), theme = theme(plot.title = element_text(face = "bold",hjust = 0.5)) )
  
  ggsave(filename = paste0(paste0(title_plt,paste0(paste0("PPC",i),"_mean_pert_wei")),format_plt),
         plot = plot_wei_pert,
         device = NULL,
         path = paste0(dir_res,subfolder_res),
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
  
}  



##-----center, 1diff, next instant----
results_ko = prediction_PPC_center_diff_1_next_instant
title_plt = "ppc_pred_center_zone_diff_1"
subfolder_res = "/center_1diff"

results_ko$`Explanatory power PPCs`

z_min_wei = 0
z_max_wei = 0
z_min_dir = 0
z_max_dir = 0

for (i in 1:results_ko$`Number of PPCs retained`) { 
  z_min_wei = min(c(z_min_wei,results_ko$`Weights of PPCs`[[i]]),na.rm=TRUE)
  z_max_wei = max(c(z_max_wei,results_ko$`Weights of PPCs`[[i]]),na.rm=TRUE)
  z_min_dir = min(c(z_min_dir,results_ko$`Directions of PPCs`[[i]]),na.rm=TRUE)
  z_max_dir = max(c(z_max_dir,results_ko$`Directions of PPCs`[[i]]),na.rm=TRUE)
}

z_min_wei_pert = 0
z_max_wei_pert = 0
z_min_dir_pert = 0
z_max_dir_pert = 0

for (i in 1:results_ko$`Number of PPCs retained`) { 
  z_min_wei_pert = min(c(z_min_wei_pert, min( c(results_ko$`Mean function` - results_ko$`Weights of PPCs`[[i]]*results_ko$`Sd scores weights`[i],      results_ko$`Mean function` + results_ko$`Weights of PPCs`[[i]]*results_ko$`Sd scores weights`[i]), na.rm = TRUE )        ),na.rm=TRUE)
  z_max_wei_pert = max(c(z_max_wei_pert, max( c(results_ko$`Mean function` - results_ko$`Weights of PPCs`[[i]]*results_ko$`Sd scores weights`[i],      results_ko$`Mean function` + results_ko$`Weights of PPCs`[[i]]*results_ko$`Sd scores weights`[i]), na.rm = TRUE )        ),na.rm=TRUE)
  z_min_dir_pert = min(c(z_min_dir_pert, min( c(results_ko$`Mean function` - results_ko$`Directions of PPCs`[[i]]*results_ko$`Sd scores directions`[i],results_ko$`Mean function` + results_ko$`Directions of PPCs`[[i]]*results_ko$`Sd scores directions`[i]) ,na.rm = TRUE)   ),na.rm=TRUE)
  z_max_dir_pert = max(c(z_max_dir_pert, max( c(results_ko$`Mean function` - results_ko$`Directions of PPCs`[[i]]*results_ko$`Sd scores directions`[i],results_ko$`Mean function` + results_ko$`Directions of PPCs`[[i]]*results_ko$`Sd scores directions`[i]) ,na.rm = TRUE)   ),na.rm=TRUE)
}

const_map = matrix(1, nrow = dim(results_ko$`Mean function`)[1], ncol = dim(results_ko$`Mean function`)[2])
z_min_const_map_effect = 0
z_max_const_map_effect = 0
for (i in 1:results_ko$`Number of PPCs retained`){
  
  predictive_factor = sum(results_ko$`Weights of PPCs`[[i]] * const_map, na.rm = TRUE)
  ppc_mapped_into_loading = predictive_factor*results_ko$`Directions of PPCs`[[i]]
  z_min_const_map_effect = min(c(z_min_const_map_effect,min(ppc_mapped_into_loading,na.rm = TRUE)))
  z_max_const_map_effect = max(c(z_max_const_map_effect,max(ppc_mapped_into_loading,na.rm = TRUE)))
}

for (i in 1:results_ko$`Number of PPCs retained`){
  
  dir <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  wei <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  
  dir$z <- (results_ko$`Directions of PPCs`[[i]])[cbind(
    match(dir$x, results_ko$`Function discrete evaluations points dim1`),  
    match(dir$y, results_ko$`Function discrete evaluations points dim2`))]
  
  wei$z <- (results_ko$`Weights of PPCs`[[i]])[cbind(
    match(wei$x, results_ko$`Function discrete evaluations points dim1`),  
    match(wei$y, results_ko$`Function discrete evaluations points dim2`))]
  
  
  
  dir_plot <- ggplot(dir, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c( option = "viridis", na.value = "white",limits = c(z_min_dir, z_max_dir) ) +
    labs( x = x1_lab, y = x2_lab, fill = z_lab ) +
    labs( title = paste0("Direction a",i) ) + 
    guides(fill = guide_colorbar(barwidth = 1, barheight = 8)) + 
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5 ),
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank() ) +
    geom_hline( yintercept = seq(min(dir$y), max(dir$y), by = (quantile(dir$y)[2]-quantile(dir$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(dir$x), max(dir$x), by = (quantile(dir$x)[2]-quantile(dir$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  weight_plot <- ggplot(wei, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c(option = "viridis", na.value = "white",limits = c(z_min_wei, z_max_wei)) +
    labs( x = x1_lab, y = x2_lab, fill = z_lab ) +
    guides(fill = guide_colorbar(barwidth = 1, barheight = 8)) + 
    labs(title = paste0("Weight b",i)) + 
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank() ) +
    geom_hline( yintercept = seq(min(wei$y), max(wei$y), by = (quantile(wei$y)[2]-quantile(wei$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(wei$x), max(wei$x), by = (quantile(wei$x)[2]-quantile(wei$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  plot_dir_we <-  weight_plot + dir_plot + plot_layout(ncol = 2) + plot_annotation( title = paste0("PPC",i), theme = theme(plot.title = element_text(face = "bold",hjust = 0.5)) )
  
  
  ggsave(filename = paste0(paste0(title_plt,paste0("PPC",i)),format_plt),
         plot = plot_dir_we,
         device = NULL,
         path = paste0(dir_res,subfolder_res),
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
  
  
  dir$Category <- factor(ifelse(is.na(dir$z) | dir$z == 0, "Zero/NaN",
                                ifelse(dir$z > 0, "Positivo", "Negativo")))
  
  wei$Category <- factor(ifelse(is.na(wei$z) | wei$z == 0, "Zero/NaN",
                                ifelse(wei$z > 0, "Positivo", "Negativo")))
  
  dir_plot_cat = ggplot(dir, aes(x = x, y = y, fill = Category)) +
    geom_tile() +
    #annotation_map_tile() +
    scale_fill_manual(
      values = c("Negativo" = "blue", "Positivo" = "red", "Zero/NaN" = "white"),
      name = "Direction",
      labels = c("Negativo" = " Negative", 
                 "Positivo" = "Positive", 
                 "Zero/NaN" = "")) +
    theme_minimal() +
    labs(title = paste0("Direction a",i)) +
    labs( x = x1_lab, y = x2_lab) +
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank() ) +
    geom_hline( yintercept = seq(min(wei$y), max(wei$y), by = (quantile(wei$y)[2]-quantile(wei$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(wei$x), max(wei$x), by = (quantile(wei$x)[2]-quantile(wei$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  
  weight_plot_cat = ggplot(wei, aes(x = x, y = y, fill = Category)) +
    geom_tile() +
    scale_fill_manual(
      values = c("Negativo" = "blue", "Positivo" = "red", "Zero/NaN" = "white"),
      name = "Weight",
      labels = c("Negativo" = "Negative", 
                 "Positivo" = "Positive", 
                 "Zero/NaN" = "") ) +
    theme_minimal() +
    labs(title = paste0("Weight b",i)) +
    labs( x = x1_lab, y = x2_lab) +
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank() ) +
    geom_hline( yintercept = seq(min(wei$y), max(wei$y), by = (quantile(wei$y)[2]-quantile(wei$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(wei$x), max(wei$x), by = (quantile(wei$x)[2]-quantile(wei$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  plot_dir_we_cat <-  weight_plot_cat + dir_plot_cat + plot_layout(ncol = 2) + plot_annotation( title = paste0("PPC",i), theme = theme(plot.title = element_text(face = "bold",hjust = 0.5)) )
  
  ggsave(filename = paste0(paste0(paste0(title_plt,paste0("PPC",i),"_cat")),format_plt),
         plot = plot_dir_we_cat,
         device = NULL,
         path = paste0(dir_res,subfolder_res),
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
  
  ppc = expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  predictive_factor = sum(results_ko$`Weights of PPCs`[[i]] * const_map, na.rm = TRUE)
  ppc_mapped_into_loading = predictive_factor*results_ko$`Directions of PPCs`[[i]]
  ppc$z <- (ppc_mapped_into_loading)[cbind(
    match(ppc$x, results_ko$`Function discrete evaluations points dim1`),  
    match(ppc$y, results_ko$`Function discrete evaluations points dim2`))]
  
  ppc_plot <- ggplot(ppc, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c( option = "plasma", na.value = "white",limits = c(z_min_const_map_effect, z_max_const_map_effect) ) +
    labs( x = x1_lab, y = x2_lab, fill = z_lab ) +
    labs( title = paste0("Effect of PPC",i) ) + 
    guides(fill = guide_colorbar(barwidth = 1, barheight = 8)) + 
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5 ),
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank() ) +
    geom_hline( yintercept = seq(min(ppc$y), max(ppc$y), by = (quantile(ppc$y)[2]-quantile(ppc$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(ppc$x), max(ppc$x), by = (quantile(ppc$x)[2]-quantile(ppc$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  
  ggsave(filename = paste0(paste0(paste0(title_plt,paste0("PPC",i),"_const_map_effect")),format_plt),
         plot = ppc_plot,
         device = NULL,
         path = paste0(dir_res,subfolder_res),
         scale = 1,
         width = 7,
         height = 10,
         dpi = 300)
}

#plot the mean function  
data_mean <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)

data_mean$z <- (results_ko$`Mean function`)[cbind(
  match(data_mean$x, results_ko$`Function discrete evaluations points dim1`),  
  match(data_mean$y, results_ko$`Function discrete evaluations points dim2`))]

plot_mean <- ggplot(data_mean, aes(x = x, y = y, fill = z)) +
  geom_tile() +
  scale_fill_viridis_c(option = "viridis", na.value = "white",limits = c(z_min_dir_pert, z_max_dir_pert)) +
  labs( title = "Mean function", x = x1_lab, y = x2_lab, fill = z_lab ) +
  guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) + 
  theme_minimal() +
  theme( plot.title = element_text(face = "bold",hjust = 0.5),
         legend.position = "bottom",
         panel.grid.major = element_blank(),  
         panel.grid.minor = element_blank()) +
  geom_hline( yintercept = seq(min(data_mean$y), max(data_mean$y), by = (quantile(data_mean$y)[2]-quantile(data_mean$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
  geom_vline( xintercept = seq(min(data_mean$x), max(data_mean$x), by = (quantile(data_mean$x)[2]-quantile(data_mean$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  

ggsave(filename = paste0(paste0(title_plt,"_mean_function"),format_plt),
       plot = plot_mean,
       device = NULL,
       path = paste0(dir_res,subfolder_res),
       scale = 1,
       width = 7,
       height = 10,
       dpi = 300)  




#PPCs as perturbation of the mean
for (i in 1:results_ko$`Number of PPCs retained`){
  #perturbation of the mean from the direction
  data_up_dir <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  
  data_lw_dir <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  
  data_lw_dir$z <- ( results_ko$`Mean function` - results_ko$`Directions of PPCs`[[i]]*results_ko$`Sd scores directions`[i])[cbind(
    match(data_lw_dir$x, results_ko$`Function discrete evaluations points dim1`),  
    match(data_lw_dir$y, results_ko$`Function discrete evaluations points dim2`))]
  data_up_dir$z <- ( results_ko$`Mean function` + results_ko$`Directions of PPCs`[[i]]*results_ko$`Sd scores directions`[i])[cbind(
    match(data_up_dir$x, results_ko$`Function discrete evaluations points dim1`),  
    match(data_up_dir$y, results_ko$`Function discrete evaluations points dim2`))]
  
  
  plot_up_dir <- ggplot(data_up_dir, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c(option = "viridis", na.value = "white",limits = c(z_min_dir_pert, z_max_dir_pert)) +
    labs( title = paste0(paste0("Direction a",i),", upper mean perturbation"), x = x1_lab, y = x2_lab, fill = z_lab ) +
    guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) +
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           legend.position = "bottom",
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank()) +
    geom_hline( yintercept = seq(min(data_up_dir$y), max(data_up_dir$y), by = (quantile(data_up_dir$y)[2]-quantile(data_up_dir$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(data_up_dir$x), max(data_up_dir$x), by = (quantile(data_up_dir$x)[2]-quantile(data_up_dir$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  plot_bw_dir <- ggplot(data_lw_dir, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c(option = "viridis", na.value = "white",limits = c(z_min_dir_pert, z_max_dir_pert)) +
    labs( title = paste0(paste0("Direction a",i),", lower mean perturbation"), x = x1_lab, y = x2_lab, fill = z_lab ) +
    guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) +
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           legend.position = "bottom",
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank()) +
    geom_hline( yintercept = seq(min(data_lw_dir$y), max(data_lw_dir$y), by = (quantile(data_lw_dir$y)[2]-quantile(data_lw_dir$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(data_lw_dir$x), max(data_lw_dir$x), by = (quantile(data_lw_dir$x)[2]-quantile(data_lw_dir$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  plot_dir_pert <-  plot_bw_dir + plot_up_dir + plot_layout(ncol = 2) + plot_annotation( title = paste0(paste0("PPC",i)," as mean perturbation"), theme = theme(plot.title = element_text(face = "bold",hjust = 0.5)) )
  
  ggsave(filename = paste0(paste0(title_plt,paste0(paste0("PPC",i),"_mean_pert_dir")),format_plt),
         plot = plot_dir_pert,
         device = NULL,
         path = paste0(dir_res,subfolder_res),
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
  
  
  
  
  
  data_up_wei <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  
  data_lw_wei <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  
  data_lw_wei$z <- ( results_ko$`Mean function` - results_ko$`Weights of PPCs`[[i]]*results_ko$`Sd scores weights`[i])[cbind(
    match(data_lw_wei$x, results_ko$`Function discrete evaluations points dim1`),  
    match(data_lw_wei$y, results_ko$`Function discrete evaluations points dim2`))]
  data_up_wei$z <- ( results_ko$`Mean function` + results_ko$`Weights of PPCs`[[i]]*results_ko$`Sd scores weights`[i])[cbind(
    match(data_up_wei$x, results_ko$`Function discrete evaluations points dim1`),  
    match(data_up_wei$y, results_ko$`Function discrete evaluations points dim2`))]
  
  
  plot_up_wei <- ggplot(data_up_wei, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c(option = "viridis", na.value = "white",limits = c(z_min_wei_pert, z_max_wei_pert)) + 
    labs( title = paste0(paste0("Weight b",i),", upper mean perturbation"), x = x1_lab, y = x2_lab, fill = z_lab ) +
    guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) +
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           legend.position = "bottom",
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank()) +
    geom_hline( yintercept = seq(min(data_up_wei$y), max(data_up_wei$y), by = (quantile(data_up_wei$y)[2]-quantile(data_up_wei$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(data_up_wei$x), max(data_up_wei$x), by = (quantile(data_up_wei$x)[2]-quantile(data_up_wei$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  plot_bw_wei<- ggplot(data_lw_wei, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c(option = "viridis", na.value = "white",limits = c(z_min_wei_pert, z_max_wei_pert)) +  
    labs( title = paste0(paste0("Weight b",i),", lower mean perturbation"), x = x1_lab, y = x2_lab, fill = z_lab ) +
    guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) +
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           legend.position = "bottom",
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank()) +
    geom_hline( yintercept = seq(min(data_lw_wei$y), max(data_lw_wei$y), by = (quantile(data_lw_wei$y)[2]-quantile(data_lw_wei$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(data_lw_wei$x), max(data_lw_wei$x), by = (quantile(data_lw_wei$x)[2]-quantile(data_lw_wei$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  
  plot_wei_pert <-  plot_bw_wei + plot_up_wei + plot_layout(ncol = 2) + plot_annotation( title = paste0(paste0("PPC",i)," as mean perturbation"), theme = theme(plot.title = element_text(face = "bold",hjust = 0.5)) )
  
  ggsave(filename = paste0(paste0(title_plt,paste0(paste0("PPC",i),"_mean_pert_wei")),format_plt),
         plot = plot_wei_pert,
         device = NULL,
         path = paste0(dir_res,subfolder_res),
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
  
}  



##-----center, 2diff, next instant----
results_ko = prediction_PPC_center_diff_2_next_instant
title_plt = "ppc_pred_center_zone_diff_2"
subfolder_res = "/center_2diff"

results_ko$`Explanatory power PPCs`

z_min_wei = 0
z_max_wei = 0
z_min_dir = 0
z_max_dir = 0

for (i in 1:results_ko$`Number of PPCs retained`) { 
  z_min_wei = min(c(z_min_wei,results_ko$`Weights of PPCs`[[i]]),na.rm=TRUE)
  z_max_wei = max(c(z_max_wei,results_ko$`Weights of PPCs`[[i]]),na.rm=TRUE)
  z_min_dir = min(c(z_min_dir,results_ko$`Directions of PPCs`[[i]]),na.rm=TRUE)
  z_max_dir = max(c(z_max_dir,results_ko$`Directions of PPCs`[[i]]),na.rm=TRUE)
}

z_min_wei_pert = 0
z_max_wei_pert = 0
z_min_dir_pert = 0
z_max_dir_pert = 0

for (i in 1:results_ko$`Number of PPCs retained`) { 
  z_min_wei_pert = min(c(z_min_wei_pert, min( c(results_ko$`Mean function` - results_ko$`Weights of PPCs`[[i]]*results_ko$`Sd scores weights`[i],      results_ko$`Mean function` + results_ko$`Weights of PPCs`[[i]]*results_ko$`Sd scores weights`[i]), na.rm = TRUE )        ),na.rm=TRUE)
  z_max_wei_pert = max(c(z_max_wei_pert, max( c(results_ko$`Mean function` - results_ko$`Weights of PPCs`[[i]]*results_ko$`Sd scores weights`[i],      results_ko$`Mean function` + results_ko$`Weights of PPCs`[[i]]*results_ko$`Sd scores weights`[i]), na.rm = TRUE )        ),na.rm=TRUE)
  z_min_dir_pert = min(c(z_min_dir_pert, min( c(results_ko$`Mean function` - results_ko$`Directions of PPCs`[[i]]*results_ko$`Sd scores directions`[i],results_ko$`Mean function` + results_ko$`Directions of PPCs`[[i]]*results_ko$`Sd scores directions`[i]) ,na.rm = TRUE)   ),na.rm=TRUE)
  z_max_dir_pert = max(c(z_max_dir_pert, max( c(results_ko$`Mean function` - results_ko$`Directions of PPCs`[[i]]*results_ko$`Sd scores directions`[i],results_ko$`Mean function` + results_ko$`Directions of PPCs`[[i]]*results_ko$`Sd scores directions`[i]) ,na.rm = TRUE)   ),na.rm=TRUE)
}

const_map = matrix(1, nrow = dim(results_ko$`Mean function`)[1], ncol = dim(results_ko$`Mean function`)[2])
z_min_const_map_effect = 0
z_max_const_map_effect = 0
for (i in 1:results_ko$`Number of PPCs retained`){
  
  predictive_factor = sum(results_ko$`Weights of PPCs`[[i]] * const_map, na.rm = TRUE)
  ppc_mapped_into_loading = predictive_factor*results_ko$`Directions of PPCs`[[i]]
  z_min_const_map_effect = min(c(z_min_const_map_effect,min(ppc_mapped_into_loading,na.rm = TRUE)))
  z_max_const_map_effect = max(c(z_max_const_map_effect,max(ppc_mapped_into_loading,na.rm = TRUE)))
}

for (i in 1:results_ko$`Number of PPCs retained`){
  
  dir <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  wei <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  
  dir$z <- (results_ko$`Directions of PPCs`[[i]])[cbind(
    match(dir$x, results_ko$`Function discrete evaluations points dim1`),  
    match(dir$y, results_ko$`Function discrete evaluations points dim2`))]
  
  wei$z <- (results_ko$`Weights of PPCs`[[i]])[cbind(
    match(wei$x, results_ko$`Function discrete evaluations points dim1`),  
    match(wei$y, results_ko$`Function discrete evaluations points dim2`))]
  
  
  
  dir_plot <- ggplot(dir, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c( option = "viridis", na.value = "white",limits = c(z_min_dir, z_max_dir) ) +
    labs( x = x1_lab, y = x2_lab, fill = z_lab ) +
    labs( title = paste0("Direction a",i) ) + 
    guides(fill = guide_colorbar(barwidth = 1, barheight = 8)) + 
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5 ),
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank() ) +
    geom_hline( yintercept = seq(min(dir$y), max(dir$y), by = (quantile(dir$y)[2]-quantile(dir$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(dir$x), max(dir$x), by = (quantile(dir$x)[2]-quantile(dir$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  weight_plot <- ggplot(wei, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c(option = "viridis", na.value = "white",limits = c(z_min_wei, z_max_wei)) +
    labs( x = x1_lab, y = x2_lab, fill = z_lab ) +
    guides(fill = guide_colorbar(barwidth = 1, barheight = 8)) + 
    labs(title = paste0("Weight b",i)) + 
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank() ) +
    geom_hline( yintercept = seq(min(wei$y), max(wei$y), by = (quantile(wei$y)[2]-quantile(wei$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(wei$x), max(wei$x), by = (quantile(wei$x)[2]-quantile(wei$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  plot_dir_we <-  weight_plot + dir_plot + plot_layout(ncol = 2) + plot_annotation( title = paste0("PPC",i), theme = theme(plot.title = element_text(face = "bold",hjust = 0.5)) )
  
  
  ggsave(filename = paste0(paste0(title_plt,paste0("PPC",i)),format_plt),
         plot = plot_dir_we,
         device = NULL,
         path = paste0(dir_res,subfolder_res),
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
  
  
  dir$Category <- factor(ifelse(is.na(dir$z) | dir$z == 0, "Zero/NaN",
                                ifelse(dir$z > 0, "Positivo", "Negativo")))
  
  wei$Category <- factor(ifelse(is.na(wei$z) | wei$z == 0, "Zero/NaN",
                                ifelse(wei$z > 0, "Positivo", "Negativo")))
  
  dir_plot_cat = ggplot(dir, aes(x = x, y = y, fill = Category)) +
    geom_tile() +
    #annotation_map_tile() +
    scale_fill_manual(
      values = c("Negativo" = "blue", "Positivo" = "red", "Zero/NaN" = "white"),
      name = "Direction",
      labels = c("Negativo" = " Negative", 
                 "Positivo" = "Positive", 
                 "Zero/NaN" = "")) +
    theme_minimal() +
    labs(title = paste0("Direction a",i)) +
    labs( x = x1_lab, y = x2_lab) +
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank() ) +
    geom_hline( yintercept = seq(min(wei$y), max(wei$y), by = (quantile(wei$y)[2]-quantile(wei$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(wei$x), max(wei$x), by = (quantile(wei$x)[2]-quantile(wei$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  
  weight_plot_cat = ggplot(wei, aes(x = x, y = y, fill = Category)) +
    geom_tile() +
    scale_fill_manual(
      values = c("Negativo" = "blue", "Positivo" = "red", "Zero/NaN" = "white"),
      name = "Weight",
      labels = c("Negativo" = "Negative", 
                 "Positivo" = "Positive", 
                 "Zero/NaN" = "") ) +
    theme_minimal() +
    labs(title = paste0("Weight b",i)) +
    labs( x = x1_lab, y = x2_lab) +
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank() ) +
    geom_hline( yintercept = seq(min(wei$y), max(wei$y), by = (quantile(wei$y)[2]-quantile(wei$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(wei$x), max(wei$x), by = (quantile(wei$x)[2]-quantile(wei$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  plot_dir_we_cat <-  weight_plot_cat + dir_plot_cat + plot_layout(ncol = 2) + plot_annotation( title = paste0("PPC",i), theme = theme(plot.title = element_text(face = "bold",hjust = 0.5)) )
  
  ggsave(filename = paste0(paste0(paste0(title_plt,paste0("PPC",i),"_cat")),format_plt),
         plot = plot_dir_we_cat,
         device = NULL,
         path = paste0(dir_res,subfolder_res),
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
  
  
  ppc = expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  predictive_factor = sum(results_ko$`Weights of PPCs`[[i]] * const_map, na.rm = TRUE)
  ppc_mapped_into_loading = predictive_factor*results_ko$`Directions of PPCs`[[i]]
  ppc$z <- (ppc_mapped_into_loading)[cbind(
    match(ppc$x, results_ko$`Function discrete evaluations points dim1`),  
    match(ppc$y, results_ko$`Function discrete evaluations points dim2`))]
  
  ppc_plot <- ggplot(ppc, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c( option = "plasma", na.value = "white",limits = c(z_min_const_map_effect, z_max_const_map_effect) ) +
    labs( x = x1_lab, y = x2_lab, fill = z_lab ) +
    labs( title = paste0("Effect of PPC",i) ) + 
    guides(fill = guide_colorbar(barwidth = 1, barheight = 8)) + 
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5 ),
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank() ) +
    geom_hline( yintercept = seq(min(ppc$y), max(ppc$y), by = (quantile(ppc$y)[2]-quantile(ppc$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(ppc$x), max(ppc$x), by = (quantile(ppc$x)[2]-quantile(ppc$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  
  ggsave(filename = paste0(paste0(paste0(title_plt,paste0("PPC",i),"_const_map_effect")),format_plt),
         plot = ppc_plot,
         device = NULL,
         path = paste0(dir_res,subfolder_res),
         scale = 1,
         width = 7,
         height = 10,
         dpi = 300)
}

#plot the mean function  
data_mean <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)

data_mean$z <- (results_ko$`Mean function`)[cbind(
  match(data_mean$x, results_ko$`Function discrete evaluations points dim1`),  
  match(data_mean$y, results_ko$`Function discrete evaluations points dim2`))]

plot_mean <- ggplot(data_mean, aes(x = x, y = y, fill = z)) +
  geom_tile() +
  scale_fill_viridis_c(option = "viridis", na.value = "white",limits = c(z_min_dir_pert, z_max_dir_pert)) +
  labs( title = "Mean function", x = x1_lab, y = x2_lab, fill = z_lab ) +
  guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) + 
  theme_minimal() +
  theme( plot.title = element_text(face = "bold",hjust = 0.5),
         legend.position = "bottom",
         panel.grid.major = element_blank(),  
         panel.grid.minor = element_blank()) +
  geom_hline( yintercept = seq(min(data_mean$y), max(data_mean$y), by = (quantile(data_mean$y)[2]-quantile(data_mean$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
  geom_vline( xintercept = seq(min(data_mean$x), max(data_mean$x), by = (quantile(data_mean$x)[2]-quantile(data_mean$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  

ggsave(filename = paste0(paste0(title_plt,"_mean_function"),format_plt),
       plot = plot_mean,
       device = NULL,
       path = paste0(dir_res,subfolder_res),
       scale = 1,
       width = 7,
       height = 10,
       dpi = 300)  




#PPCs as perturbation of the mean
for (i in 1:results_ko$`Number of PPCs retained`){
  #perturbation of the mean from the direction
  data_up_dir <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  
  data_lw_dir <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  
  data_lw_dir$z <- ( results_ko$`Mean function` - results_ko$`Directions of PPCs`[[i]]*results_ko$`Sd scores directions`[i])[cbind(
    match(data_lw_dir$x, results_ko$`Function discrete evaluations points dim1`),  
    match(data_lw_dir$y, results_ko$`Function discrete evaluations points dim2`))]
  data_up_dir$z <- ( results_ko$`Mean function` + results_ko$`Directions of PPCs`[[i]]*results_ko$`Sd scores directions`[i])[cbind(
    match(data_up_dir$x, results_ko$`Function discrete evaluations points dim1`),  
    match(data_up_dir$y, results_ko$`Function discrete evaluations points dim2`))]
  
  
  plot_up_dir <- ggplot(data_up_dir, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c(option = "viridis", na.value = "white",limits = c(z_min_dir_pert, z_max_dir_pert)) +
    labs( title = paste0(paste0("Direction a",i),", upper mean perturbation"), x = x1_lab, y = x2_lab, fill = z_lab ) +
    guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) +
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           legend.position = "bottom",
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank()) +
    geom_hline( yintercept = seq(min(data_up_dir$y), max(data_up_dir$y), by = (quantile(data_up_dir$y)[2]-quantile(data_up_dir$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(data_up_dir$x), max(data_up_dir$x), by = (quantile(data_up_dir$x)[2]-quantile(data_up_dir$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  plot_bw_dir <- ggplot(data_lw_dir, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c(option = "viridis", na.value = "white",limits = c(z_min_dir_pert, z_max_dir_pert)) +
    labs( title = paste0(paste0("Direction a",i),", lower mean perturbation"), x = x1_lab, y = x2_lab, fill = z_lab ) +
    guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) +
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           legend.position = "bottom",
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank()) +
    geom_hline( yintercept = seq(min(data_lw_dir$y), max(data_lw_dir$y), by = (quantile(data_lw_dir$y)[2]-quantile(data_lw_dir$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(data_lw_dir$x), max(data_lw_dir$x), by = (quantile(data_lw_dir$x)[2]-quantile(data_lw_dir$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  plot_dir_pert <-  plot_bw_dir + plot_up_dir + plot_layout(ncol = 2) + plot_annotation( title = paste0(paste0("PPC",i)," as mean perturbation"), theme = theme(plot.title = element_text(face = "bold",hjust = 0.5)) )
  
  ggsave(filename = paste0(paste0(title_plt,paste0(paste0("PPC",i),"_mean_pert_dir")),format_plt),
         plot = plot_dir_pert,
         device = NULL,
         path = paste0(dir_res,subfolder_res),
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
  
  
  
  
  
  data_up_wei <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  
  data_lw_wei <- expand.grid(x = results_ko$`Function discrete evaluations points dim1`, y = results_ko$`Function discrete evaluations points dim2`)
  
  data_lw_wei$z <- ( results_ko$`Mean function` - results_ko$`Weights of PPCs`[[i]]*results_ko$`Sd scores weights`[i])[cbind(
    match(data_lw_wei$x, results_ko$`Function discrete evaluations points dim1`),  
    match(data_lw_wei$y, results_ko$`Function discrete evaluations points dim2`))]
  data_up_wei$z <- ( results_ko$`Mean function` + results_ko$`Weights of PPCs`[[i]]*results_ko$`Sd scores weights`[i])[cbind(
    match(data_up_wei$x, results_ko$`Function discrete evaluations points dim1`),  
    match(data_up_wei$y, results_ko$`Function discrete evaluations points dim2`))]
  
  
  plot_up_wei <- ggplot(data_up_wei, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c(option = "viridis", na.value = "white",limits = c(z_min_wei_pert, z_max_wei_pert)) + 
    labs( title = paste0(paste0("Weight b",i),", upper mean perturbation"), x = x1_lab, y = x2_lab, fill = z_lab ) +
    guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) +
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           legend.position = "bottom",
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank()) +
    geom_hline( yintercept = seq(min(data_up_wei$y), max(data_up_wei$y), by = (quantile(data_up_wei$y)[2]-quantile(data_up_wei$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(data_up_wei$x), max(data_up_wei$x), by = (quantile(data_up_wei$x)[2]-quantile(data_up_wei$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  plot_bw_wei<- ggplot(data_lw_wei, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c(option = "viridis", na.value = "white",limits = c(z_min_wei_pert, z_max_wei_pert)) +  
    labs( title = paste0(paste0("Weight b",i),", lower mean perturbation"), x = x1_lab, y = x2_lab, fill = z_lab ) +
    guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) +
    theme_minimal() +
    theme( plot.title = element_text(face = "bold",hjust = 0.5),
           legend.position = "bottom",
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank()) +
    geom_hline( yintercept = seq(min(data_lw_wei$y), max(data_lw_wei$y), by = (quantile(data_lw_wei$y)[2]-quantile(data_lw_wei$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
    geom_vline( xintercept = seq(min(data_lw_wei$x), max(data_lw_wei$x), by = (quantile(data_lw_wei$x)[2]-quantile(data_lw_wei$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  
  
  
  plot_wei_pert <-  plot_bw_wei + plot_up_wei + plot_layout(ncol = 2) + plot_annotation( title = paste0(paste0("PPC",i)," as mean perturbation"), theme = theme(plot.title = element_text(face = "bold",hjust = 0.5)) )
  
  ggsave(filename = paste0(paste0(title_plt,paste0(paste0("PPC",i),"_mean_pert_wei")),format_plt),
         plot = plot_wei_pert,
         device = NULL,
         path = paste0(dir_res,subfolder_res),
         scale = 1,
         width = 14,
         height = 10,
         dpi = 300)
  
}  