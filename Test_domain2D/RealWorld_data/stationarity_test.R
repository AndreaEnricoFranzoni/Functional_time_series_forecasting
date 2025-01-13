rm(list=ls())
graphics.off()
cat("\014")

set.seed(23032000)



#######################################################
#### ADF test for original data, zones and diff ts ####
#######################################################


library(PPCKO)

#put here the path of the local copy of the directory
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"
format = ".png"
#where the data are
dir_data = paste0(dir_w,"/Test_domain2D/RealWorld_data/data")

# upload data
load(paste0(dir_data,"/BS.Rdata"))
load(paste0(dir_data,"/BS_diff_1.Rdata"))
load(paste0(dir_data,"/BS_diff_2.Rdata"))

# where to store results
path_store_res = paste0(dir_w,"/Test_domain2D/RealWorld_data/results/adf_p_val")


## ----- complete data ----
subfolder_res = paste0(path_store_res,"/complete")
data = data_2d_wrapper_from_list(Xt[1:99])
data_diff_1 = Xt_diff_1[,1:98]
data_diff_2 = Xt_diff_2[,1:97]


adf_pval = KO_check_hps_2d(data,length(x1.grid),length(x2.grid))
adf_pval_diff_1 = KO_check_hps_2d(data_diff_1,length(x1.grid),length(x2.grid))
adf_pval_diff_2 = KO_check_hps_2d(data_diff_2,length(x1.grid),length(x2.grid))



## pvalue original data
title = "Pointwise ADF test p-value"
data <- expand.grid(x = lon, y = lat)
# Usa gli indici per ottenere i valori da m$`Pvalues ADF`
data$z <- (adf_pval$`P-values ADF`)[cbind(
  match(data$x, lon),  # Trova gli indici di x in x_coords
  match(data$y, lat)   # Trova gli indici di y in y_coords
)]

plot_pv <- ggplot(data, aes(x = x, y = y)) +
  geom_raster(aes(fill = z), na.rm = TRUE) +
  stat_contour(aes(z = z, color = ..level..), binwidth = 0.1, na.rm = TRUE) +
  scale_fill_viridis_c( na.value = "white", option = "viridis", name = "Value", limits = c(0, 1), breaks = seq(0, 1, by = 0.1) ) +
  scale_color_viridis_c(option = "viridis", name = "Value", limits = c(0, 1), breaks = seq(0, 1, by = 0.1) ) +
  labs( title = title, x = 'Lon', y = 'Lat' ) +
  guides( fill = guide_legend(override.aes = list(color = NA)), color = guide_legend() ) +
  theme_minimal() +
  theme( plot.title = element_text(face = "bold",hjust = 0.5),
         legend.position = "right",
         legend.key.size = unit(0.5, "cm"),
         legend.title = element_text(size = 10),
         legend.text = element_text(size = 7),
         legend.box = "vertical",
         legend.box.background = element_rect(fill = "white", color = "black"),
         legend.margin = margin(5, 5, 5, 5), 
         panel.grid.major = element_blank(),  
         panel.grid.minor = element_blank() ) +
  geom_hline( yintercept = seq(min(data$y), max(data$y), by = (quantile(data$y)[2]-quantile(data$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
  geom_vline( xintercept = seq(min(data$x), max(data$x), by = (quantile(data$x)[2]-quantile(data$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  

print(plot_pv)

title = "ADF_p_val_data_complete"
ggsave(filename = paste0(title,format),
       plot = plot_pv,
       device = NULL,
       path = subfolder_res,
       scale = 1,
       width = 14,
       height = 10,
       dpi = 300)



## pvalue diff 1 data
title = "Pointwise ADF test p-value diff 1"
data <- expand.grid(x = lon, y = lat)

# Usa gli indici per ottenere i valori da m$`Pvalues ADF`
data$z <- (adf_pval_diff_1$`P-values ADF`)[cbind(
  match(data$x, lon),  # Trova gli indici di x in x_coords
  match(data$y, lat)   # Trova gli indici di y in y_coords
)]

plot_pv <- ggplot(data, aes(x = x, y = y)) +
  geom_raster(aes(fill = z), na.rm = TRUE) +
  stat_contour(aes(z = z, color = ..level..), binwidth = 0.1, na.rm = TRUE) +
  scale_fill_viridis_c( na.value = "white", option = "viridis", name = "Value", limits = c(0, 1), breaks = seq(0, 1, by = 0.1) ) +
  scale_color_viridis_c(option = "viridis", name = "Value", limits = c(0, 1), breaks = seq(0, 1, by = 0.1) ) +
  labs( title = title, x = 'Lon', y = 'Lat' ) +
  guides( fill = guide_legend(override.aes = list(color = NA)), color = guide_legend() ) +
  theme_minimal() +
  theme( plot.title = element_text(face = "bold",hjust = 0.5),
         legend.position = "right",
         legend.key.size = unit(0.5, "cm"),
         legend.title = element_text(size = 10),
         legend.text = element_text(size = 7),
         legend.box = "vertical",
         legend.box.background = element_rect(fill = "white", color = "black"),
         legend.margin = margin(5, 5, 5, 5), 
         panel.grid.major = element_blank(),  
         panel.grid.minor = element_blank() ) +
  geom_hline( yintercept = seq(min(data$y), max(data$y), by = (quantile(data$y)[2]-quantile(data$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
  geom_vline( xintercept = seq(min(data$x), max(data$x), by = (quantile(data$x)[2]-quantile(data$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  


print(plot_pv)

title = "ADF_p_val_data_diff_1_complete"
ggsave(filename = paste0(title,format),
       plot = plot_pv,
       device = NULL,
       path = subfolder_res,
       scale = 1,
       width = 14,
       height = 10,
       dpi = 300)


## pvalue diff 2 data
title = "Pointwise ADF test p-value diff 2"
data <- expand.grid(x = lon, y = lat)

# Usa gli indici per ottenere i valori da m$`Pvalues ADF`
data$z <- (adf_pval_diff_2$`P-values ADF`)[cbind(
  match(data$x, lon),  # Trova gli indici di x in x_coords
  match(data$y, lat)   # Trova gli indici di y in y_coords
)]

plot_pv <- ggplot(data, aes(x = x, y = y)) +
  geom_raster(aes(fill = z), na.rm = TRUE) +
  stat_contour(aes(z = z, color = ..level..), binwidth = 0.1, na.rm = TRUE) +
  scale_fill_viridis_c( na.value = "white", option = "viridis", name = "Value", limits = c(0, 1), breaks = seq(0, 1, by = 0.1) ) +
  scale_color_viridis_c(option = "viridis", name = "Value", limits = c(0, 1), breaks = seq(0, 1, by = 0.1) ) +
  labs( title = title, x = 'Lon', y = 'Lat' ) +
  guides( fill = guide_legend(override.aes = list(color = NA)), color = guide_legend() ) +
  theme_minimal() +
  theme( plot.title = element_text(face = "bold",hjust = 0.5),
         legend.position = "right",
         legend.key.size = unit(0.5, "cm"),
         legend.title = element_text(size = 10),
         legend.text = element_text(size = 7),
         legend.box = "vertical",
         legend.box.background = element_rect(fill = "white", color = "black"),
         legend.margin = margin(5, 5, 5, 5), 
         panel.grid.major = element_blank(),  
         panel.grid.minor = element_blank() ) +
  geom_hline( yintercept = seq(min(data$y), max(data$y), by = (quantile(data$y)[2]-quantile(data$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
  geom_vline( xintercept = seq(min(data$x), max(data$x), by = (quantile(data$x)[2]-quantile(data$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  

print(plot_pv)

title = "ADF_p_val_data_diff_2_complete"
ggsave(filename = paste0(title,format),
       plot = plot_pv,
       device = NULL,
       path = subfolder_res,
       scale = 1,
       width = 14,
       height = 10,
       dpi = 300)






## ----- mouth data ----
rm(list=ls())
graphics.off()
cat("\014")

set.seed(23032000)

library(PPCKO)

#change here
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"
format = ".png"
#where the data are
dir_data = paste0(dir_w,"/Test_domain2D/RealWorld_data/data")

# upload data
load(paste0(dir_data,"/mouth/BS_mouth.Rdata"))
load(paste0(dir_data,"/mouth/BS_mouth_diff_1.Rdata"))
load(paste0(dir_data,"/mouth/BS_mouth_diff_2.Rdata"))

# where to store results
path_store_res = paste0(dir_w,"/Test_domain2D/RealWorld_data/results/adf_p_val")


subfolder_res = paste0(path_store_res,"/mouth")

#data = data_2d_wrapper_from_list(Xt[1:99])
#data_diff_1 = Xt_diff_1[,1:98]
#data_diff_2 = Xt_diff_2[,1:97]
data = data_2d_wrapper_from_list(Xt_mouth[1:99])
data_diff_1 = Xt_mouth_diff_1[,1:98]
data_diff_2 = Xt_mouth_diff_2[,1:97]

lon = lon_mouth
lat = lat_mouth
x1.grid = lon_mouth
x2.grid = lat_mouth

adf_pval = KO_check_hps_2d(data,length(x1.grid),length(x2.grid))
adf_pval_diff_1 = KO_check_hps_2d(data_diff_1,length(x1.grid),length(x2.grid))
adf_pval_diff_2 = KO_check_hps_2d(data_diff_2,length(x1.grid),length(x2.grid))



## pvalue original data
title = "Pointwise ADF test p-value mouth"
data <- expand.grid(x = lon, y = lat)
# Usa gli indici per ottenere i valori da m$`Pvalues ADF`
data$z <- (adf_pval$`P-values ADF`)[cbind(
  match(data$x, lon),  # Trova gli indici di x in x_coords
  match(data$y, lat)   # Trova gli indici di y in y_coords
)]

plot_pv <- ggplot(data, aes(x = x, y = y)) +
  geom_raster(aes(fill = z), na.rm = TRUE) +
  stat_contour(aes(z = z, color = ..level..), binwidth = 0.1, na.rm = TRUE) +
  scale_fill_viridis_c( na.value = "white", option = "viridis", name = "Value", limits = c(0, 1), breaks = seq(0, 1, by = 0.1) ) +
  scale_color_viridis_c(option = "viridis", name = "Value", limits = c(0, 1), breaks = seq(0, 1, by = 0.1) ) +
  labs( title = title, x = 'Lon', y = 'Lat' ) +
  guides( fill = guide_legend(override.aes = list(color = NA)), color = guide_legend() ) +
  theme_minimal() +
  theme( plot.title = element_text(face = "bold",hjust = 0.5),
         legend.position = "right",
         legend.key.size = unit(0.5, "cm"),
         legend.title = element_text(size = 10),
         legend.text = element_text(size = 7),
         legend.box = "vertical",
         legend.box.background = element_rect(fill = "white", color = "black"),
         legend.margin = margin(5, 5, 5, 5), 
         panel.grid.major = element_blank(),  
         panel.grid.minor = element_blank() ) +
  geom_hline( yintercept = seq(min(data$y), max(data$y), by = (quantile(data$y)[2]-quantile(data$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
  geom_vline( xintercept = seq(min(data$x), max(data$x), by = (quantile(data$x)[2]-quantile(data$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  


print(plot_pv)

title = "ADF_p_val_data_mouth"
ggsave(filename = paste0(title,format),
       plot = plot_pv,
       device = NULL,
       path = subfolder_res,
       scale = 1,
       width = 14,
       height = 10,
       dpi = 300)



## pvalue diff 1 data
title = "Pointwise ADF test p-value diff 1 mouth"
data <- expand.grid(x = lon, y = lat)

# Usa gli indici per ottenere i valori da m$`Pvalues ADF`
data$z <- (adf_pval_diff_1$`P-values ADF`)[cbind(
  match(data$x, lon),  # Trova gli indici di x in x_coords
  match(data$y, lat)   # Trova gli indici di y in y_coords
)]

plot_pv <- ggplot(data, aes(x = x, y = y)) +
  geom_raster(aes(fill = z), na.rm = TRUE) +
  stat_contour(aes(z = z, color = ..level..), binwidth = 0.1, na.rm = TRUE) +
  scale_fill_viridis_c( na.value = "white", option = "viridis", name = "Value", limits = c(0, 1), breaks = seq(0, 1, by = 0.1) ) +
  scale_color_viridis_c(option = "viridis", name = "Value", limits = c(0, 1), breaks = seq(0, 1, by = 0.1) ) +
  labs( title = title, x = 'Lon', y = 'Lat' ) +
  guides( fill = guide_legend(override.aes = list(color = NA)), color = guide_legend() ) +
  theme_minimal() +
  theme( plot.title = element_text(face = "bold",hjust = 0.5),
         legend.position = "right",
         legend.key.size = unit(0.5, "cm"),
         legend.title = element_text(size = 10),
         legend.text = element_text(size = 7),
         legend.box = "vertical",
         legend.box.background = element_rect(fill = "white", color = "black"),
         legend.margin = margin(5, 5, 5, 5), 
         panel.grid.major = element_blank(),  
         panel.grid.minor = element_blank() ) +
  geom_hline( yintercept = seq(min(data$y), max(data$y), by = (quantile(data$y)[2]-quantile(data$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
  geom_vline( xintercept = seq(min(data$x), max(data$x), by = (quantile(data$x)[2]-quantile(data$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  


print(plot_pv)

title = "ADF_p_val_data_diff_1_mouth"
ggsave(filename = paste0(title,format),
       plot = plot_pv,
       device = NULL,
       path = subfolder_res,
       scale = 1,
       width = 14,
       height = 10,
       dpi = 300)


## pvalue diff 2 data
title = "Pointwise ADF test p-value diff 2 mouth"
data <- expand.grid(x = lon, y = lat)

# Usa gli indici per ottenere i valori da m$`Pvalues ADF`
data$z <- (adf_pval_diff_2$`P-values ADF`)[cbind(
  match(data$x, lon),  # Trova gli indici di x in x_coords
  match(data$y, lat)   # Trova gli indici di y in y_coords
)]

plot_pv <- ggplot(data, aes(x = x, y = y)) +
  geom_raster(aes(fill = z), na.rm = TRUE) +
  stat_contour(aes(z = z, color = ..level..), binwidth = 0.1, na.rm = TRUE) +
  scale_fill_viridis_c( na.value = "white", option = "viridis", name = "Value", limits = c(0, 1), breaks = seq(0, 1, by = 0.1) ) +
  scale_color_viridis_c(option = "viridis", name = "Value", limits = c(0, 1), breaks = seq(0, 1, by = 0.1) ) +
  labs( title = title, x = 'Lon', y = 'Lat' ) +
  guides( fill = guide_legend(override.aes = list(color = NA)), color = guide_legend() ) +
  theme_minimal() +
  theme( plot.title = element_text(face = "bold",hjust = 0.5),
         legend.position = "right",
         legend.key.size = unit(0.5, "cm"),
         legend.title = element_text(size = 10),
         legend.text = element_text(size = 7),
         legend.box = "vertical",
         legend.box.background = element_rect(fill = "white", color = "black"),
         legend.margin = margin(5, 5, 5, 5), 
         panel.grid.major = element_blank(),  
         panel.grid.minor = element_blank() ) +
  geom_hline( yintercept = seq(min(data$y), max(data$y), by = (quantile(data$y)[2]-quantile(data$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
  geom_vline( xintercept = seq(min(data$x), max(data$x), by = (quantile(data$x)[2]-quantile(data$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  

print(plot_pv)

title = "ADF_p_val_data_diff_2_mouth"
ggsave(filename = paste0(title,format),
       plot = plot_pv,
       device = NULL,
       path = subfolder_res,
       scale = 1,
       width = 14,
       height = 10,
       dpi = 300)



## ----- center data ----
rm(list=ls())
graphics.off()
cat("\014")

set.seed(23032000)

library(PPCKO)

#change here
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"
format = ".png"
#where the data are
dir_data = paste0(dir_w,"/Test_domain2D/RealWorld_data/data")

# upload data
load(paste0(dir_data,"/center/BS_center.Rdata"))
load(paste0(dir_data,"/center/BS_center_diff_1.Rdata"))
load(paste0(dir_data,"/center/BS_center_diff_2.Rdata"))

# where to store results
path_store_res = paste0(dir_w,"/Test_domain2D/RealWorld_data/results/adf_p_val")


subfolder_res = paste0(path_store_res,"/center")

#data = data_2d_wrapper_from_list(Xt[1:99])
#data_diff_1 = Xt_diff_1[,1:98]
#data_diff_2 = Xt_diff_2[,1:97]
data = data_2d_wrapper_from_list(Xt_center[1:99])
data_diff_1 = Xt_center_diff_1[,1:98]
data_diff_2 = Xt_center_diff_2[,1:97]

lon = lon_center
lat = lat_center
x1.grid = lon_center
x2.grid = lat_center

adf_pval = KO_check_hps_2d(data,length(x1.grid),length(x2.grid))
adf_pval_diff_1 = KO_check_hps_2d(data_diff_1,length(x1.grid),length(x2.grid))
adf_pval_diff_2 = KO_check_hps_2d(data_diff_2,length(x1.grid),length(x2.grid))



## pvalue original data
title = "Pointwise ADF test p-value center"
data <- expand.grid(x = lon, y = lat)
# Usa gli indici per ottenere i valori da m$`Pvalues ADF`
data$z <- (adf_pval$`P-values ADF`)[cbind(
  match(data$x, lon),  # Trova gli indici di x in x_coords
  match(data$y, lat)   # Trova gli indici di y in y_coords
)]

plot_pv <- ggplot(data, aes(x = x, y = y)) +
  geom_raster(aes(fill = z), na.rm = TRUE) +
  stat_contour(aes(z = z, color = ..level..), binwidth = 0.1, na.rm = TRUE) +
  scale_fill_viridis_c( na.value = "white", option = "viridis", name = "Value", limits = c(0, 1), breaks = seq(0, 1, by = 0.1) ) +
  scale_color_viridis_c(option = "viridis", name = "Value", limits = c(0, 1), breaks = seq(0, 1, by = 0.1) ) +
  labs( title = title, x = 'Lon', y = 'Lat' ) +
  guides( fill = guide_legend(override.aes = list(color = NA)), color = guide_legend() ) +
  theme_minimal() +
  theme( plot.title = element_text(face = "bold",hjust = 0.5),
         legend.position = "right",
         legend.key.size = unit(0.5, "cm"),
         legend.title = element_text(size = 10),
         legend.text = element_text(size = 7),
         legend.box = "vertical",
         legend.box.background = element_rect(fill = "white", color = "black"),
         legend.margin = margin(5, 5, 5, 5), 
         panel.grid.major = element_blank(),  
         panel.grid.minor = element_blank() ) +
  geom_hline( yintercept = seq(min(data$y), max(data$y), by = (quantile(data$y)[2]-quantile(data$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
  geom_vline( xintercept = seq(min(data$x), max(data$x), by = (quantile(data$x)[2]-quantile(data$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  


print(plot_pv)

title = "ADF_p_val_data_center"
ggsave(filename = paste0(title,format),
       plot = plot_pv,
       device = NULL,
       path = subfolder_res,
       scale = 1,
       width = 14,
       height = 10,
       dpi = 300)



## pvalue diff 1 data
title = "Pointwise ADF test p-value diff 1 center"
data <- expand.grid(x = lon, y = lat)

# Usa gli indici per ottenere i valori da m$`Pvalues ADF`
data$z <- (adf_pval_diff_1$`P-values ADF`)[cbind(
  match(data$x, lon),  # Trova gli indici di x in x_coords
  match(data$y, lat)   # Trova gli indici di y in y_coords
)]

plot_pv <- ggplot(data, aes(x = x, y = y)) +
  geom_raster(aes(fill = z), na.rm = TRUE) +
  stat_contour(aes(z = z, color = ..level..), binwidth = 0.1, na.rm = TRUE) +
  scale_fill_viridis_c( na.value = "white", option = "viridis", name = "Value", limits = c(0, 1), breaks = seq(0, 1, by = 0.1) ) +
  scale_color_viridis_c(option = "viridis", name = "Value", limits = c(0, 1), breaks = seq(0, 1, by = 0.1) ) +
  labs( title = title, x = 'Lon', y = 'Lat' ) +
  guides( fill = guide_legend(override.aes = list(color = NA)), color = guide_legend() ) +
  theme_minimal() +
  theme( plot.title = element_text(face = "bold",hjust = 0.5),
         legend.position = "right",
         legend.key.size = unit(0.5, "cm"),
         legend.title = element_text(size = 10),
         legend.text = element_text(size = 7),
         legend.box = "vertical",
         legend.box.background = element_rect(fill = "white", color = "black"),
         legend.margin = margin(5, 5, 5, 5), 
         panel.grid.major = element_blank(),  
         panel.grid.minor = element_blank() ) +
  geom_hline( yintercept = seq(min(data$y), max(data$y), by = (quantile(data$y)[2]-quantile(data$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
  geom_vline( xintercept = seq(min(data$x), max(data$x), by = (quantile(data$x)[2]-quantile(data$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  


print(plot_pv)

title = "ADF_p_val_data_diff_1_center"
ggsave(filename = paste0(title,format),
       plot = plot_pv,
       device = NULL,
       path = subfolder_res,
       scale = 1,
       width = 14,
       height = 10,
       dpi = 300)


## pvalue diff 2 data
title = "Pointwise ADF test p-value diff 2 center"
data <- expand.grid(x = lon, y = lat)

# Usa gli indici per ottenere i valori da m$`Pvalues ADF`
data$z <- (adf_pval_diff_2$`P-values ADF`)[cbind(
  match(data$x, lon),  # Trova gli indici di x in x_coords
  match(data$y, lat)   # Trova gli indici di y in y_coords
)]

plot_pv <- ggplot(data, aes(x = x, y = y)) +
  geom_raster(aes(fill = z), na.rm = TRUE) +
  stat_contour(aes(z = z, color = ..level..), binwidth = 0.1, na.rm = TRUE) +
  scale_fill_viridis_c( na.value = "white", option = "viridis", name = "Value", limits = c(0, 1), breaks = seq(0, 1, by = 0.1) ) +
  scale_color_viridis_c(option = "viridis", name = "Value", limits = c(0, 1), breaks = seq(0, 1, by = 0.1) ) +
  labs( title = title, x = 'Lon', y = 'Lat' ) +
  guides( fill = guide_legend(override.aes = list(color = NA)), color = guide_legend() ) +
  theme_minimal() +
  theme( plot.title = element_text(face = "bold",hjust = 0.5),
         legend.position = "right",
         legend.key.size = unit(0.5, "cm"),
         legend.title = element_text(size = 10),
         legend.text = element_text(size = 7),
         legend.box = "vertical",
         legend.box.background = element_rect(fill = "white", color = "black"),
         legend.margin = margin(5, 5, 5, 5), 
         panel.grid.major = element_blank(),  
         panel.grid.minor = element_blank() ) +
  geom_hline( yintercept = seq(min(data$y), max(data$y), by = (quantile(data$y)[2]-quantile(data$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
  geom_vline( xintercept = seq(min(data$x), max(data$x), by = (quantile(data$x)[2]-quantile(data$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 )  

print(plot_pv)

title = "ADF_p_val_data_diff_2_center"
ggsave(filename = paste0(title,format),
       plot = plot_pv,
       device = NULL,
       path = subfolder_res,
       scale = 1,
       width = 14,
       height = 10,
       dpi = 300)
