rm(list=ls())
graphics.off()
cat("\014")

set.seed(23032000)


#######################################################
#### generating the surface FTS for the two zones  ####
#######################################################


#put here the path of the local copy of the directory
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"

#where the data are
dir_data = paste0(dir_w,"/Test_domain2D/RealWorld_data")

# upload data
load(paste0(dir_data,"/data/BS.Rdata"))

#address to save data
path_res_mouth = paste0(dir_data,"/data/mouth")
path_res_center = paste0(dir_data,"/data/center")



# plot one day
path_store_plt = dir_data

first_day_first_train = "20170101"
idx_first_day_first_train = which(dates==first_day_first_train)

data <- expand.grid(x = lon, y = lat)
plot_instant = Xt[[idx_first_day_first_train]]

data$z <- plot_instant[cbind(
  match(data1$x, lon),  
  match(data1$y, lat))]

data_cmp = plot_instant[which(!is.na(plot_instant))]

min_v = min(data_cmp )
max_v = max(data_cmp )

plot_pv <- ggplot(data, aes(x = x, y = y)) +
  geom_raster(aes(fill = z), na.rm = TRUE) +
  #stat_contour(aes(z = z, color = ..level..), binwidth = 0.1, na.rm = TRUE) +
  scale_fill_viridis_c( na.value = "white", option = "viridis", name = "SLA [m]", limits = c(-0.2,0.2) ) +
  scale_color_viridis_c(option = "viridis", name = "SLA [m[", limits = c(-0.2,0.2) ) +
  labs( title = "Black Sea SLA on 01-01-2017", x = "Longitude [°]", y = "Latitude [°]") +
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

ggsave(filename = "BS_SLA_01_01_17.jpg",
       plot = plot_pv,
       device = NULL,
       path = path_store_plt,
       scale = 1,
       width = 14,
       height = 10,
       dpi = 300)



##-----data around Dnepr and Danubio mouth-----
idx_lat_mouth = which(lat==44):which(lat==max(lat)) # lat: 44 : 46.875
lat_mouth     = lat[idx_lat_mouth]
idx_lon_mouth = which(lon==29.5):which(lon==33.75)  # lon: 29.5 : 33.75
lon_mouth     = lon[idx_lon_mouth]

Xt_mouth = list()
for (i in 1:length(Xt)) {
  x_mouth = Xt[[i]][idx_lon_mouth,idx_lat_mouth]
  Xt_mouth[[i]] = x_mouth
}

save(Xt_mouth,lon_mouth,lat_mouth,file=paste0(path_res_mouth,"/BS_mouth.Rdata"))


# takes differences for mouth
n_tot = length(Xt_mouth)
n_points = dim(Xt_mouth[[1]])[1]*dim(Xt_mouth[[1]])[2]
Xt_mat_mouth = matrix(NA, nrow=n_points, ncol=n_tot)

for(t in 1:n_tot){
  Xt_mat_mouth[,t] = as.vector(Xt_mouth[[t]])
}

# single
lag = 1
differences = 1
n_tot_new = n_tot - lag - (differences-1)

Xt_mouth_diff_1 = matrix(NA, nrow=n_points, ncol=n_tot_new)

# differentiate
for(ii in 1:n_points){
  Xt_mouth_diff_1[ii,] = diff(Xt_mat_mouth[ii,], lag=lag, differences = differences)
}

save(Xt_mouth_diff_1,file=paste0(path_res_mouth,"/BS_mouth_diff_1.Rdata"))


# double
lag = 1
differences = 2
n_tot_new = n_tot - lag - (differences-1)

Xt_mouth_diff_2 = matrix(NA, nrow=n_points, ncol=n_tot_new)

# differentiate
for(ii in 1:n_points){
  Xt_mouth_diff_2[ii,] = diff(Xt_mat_mouth[ii,], lag=lag, differences = differences)
}

save(Xt_mouth_diff_2,file=paste0(path_res_mouth,"/BS_mouth_diff_2.Rdata"))




## -----data in the center of the Black Sea------
idx_lat_center = which(lat==41.5):which(lat==42.5)  # lat: 41.5 : 42.5
lat_center     = lat[idx_lat_center]
idx_lon_center = which(lon==28):which(lon==31)      # lon: 38   : 31
lon_center     = lon[idx_lon_center]

Xt_center = list()
for (i in 1:length(Xt)) {
  x_center = Xt[[i]][idx_lon_center,idx_lat_center]
  Xt_center[[i]] = x_center
}

save(Xt_center,lon_center,lat_center,file=paste0(path_res_center,"/BS_center.Rdata"))


# takes differences for center
n_tot = length(Xt_center)
n_points = dim(Xt_center[[1]])[1]*dim(Xt_center[[1]])[2]
Xt_mat_center = matrix(NA, nrow=n_points, ncol=n_tot)

for(t in 1:n_tot){
  Xt_mat_center[,t] = as.vector(Xt_center[[t]])
}

# single
lag = 1
differences = 1
n_tot_new = n_tot - lag - (differences-1)

Xt_center_diff_1 = matrix(NA, nrow=n_points, ncol=n_tot_new)

# differentiate
for(ii in 1:n_points){
  Xt_center_diff_1[ii,] = diff(Xt_mat_center[ii,], lag=lag, differences = differences)
}

save(Xt_center_diff_1,file=paste0(path_res_center,"/BS_center_diff_1.Rdata"))


# double
lag = 1
differences = 2
n_tot_new = n_tot - lag - (differences-1)

Xt_center_diff_2 = matrix(NA, nrow=n_points, ncol=n_tot_new)

# differentiate
for(ii in 1:n_points){
  Xt_center_diff_2[ii,] = diff(Xt_mat_center[ii,], lag=lag, differences = differences)
}

save(Xt_center_diff_2,file=paste0(path_res_center,"/BS_center_diff_2.Rdata"))
