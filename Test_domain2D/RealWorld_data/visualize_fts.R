rm(list=ls())
graphics.off()
cat("\014")

set.seed(23032000)



#########################################
#### visualize one instant of BS FTS ####
#########################################


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
FTS = data_2d_wrapper_from_list(Xt)

day_idx = 2346


data_plot = Xt[[day_idx]]
day = dates.asDate[day_idx]



min_lat_mouth = 44
max_lat_mouth = max(lat)
min_lon_mouth = 29.5
max_lon_mouth = 33.75

min_lat_center = 41.5
max_lat_center = 42.5
min_lon_center = 28
max_lon_center = 31


idx_lat_mouth = which(lat==min_lat_mouth):which(lat==max_lat_mouth) # lat: 44 : 46.875
lat_mouth     = lat[idx_lat_mouth]
idx_lon_mouth = which(lon==min_lon_mouth):which(lon==max_lon_mouth)  # lon: 29.5 : 33.75
lon_mouth     = lon[idx_lon_mouth]

idx_lat_center = which(lat==min_lat_center):which(lat==max_lat_center)  # lat: 41.5 : 42.5
lat_center     = lat[idx_lat_center]
idx_lon_center = which(lon==min_lon_center):which(lon==max_lon_center)      # lon: 38   : 31
lon_center     = lon[idx_lon_center]


data <- expand.grid(x = lon, y = lat)

#f_n
data$z <- (data_plot)[cbind(
  match(data$x, lon),  
  match(data$y, lat))]


z_min <- min(data$z, na.rm = TRUE)
z_max <- max(data$z, na.rm = TRUE)

plot <- ggplot(data, aes(x = x, y = y, fill = z)) +
  geom_tile() +
  scale_fill_viridis_c( option = "viridis", na.value = "white", limits = c(z_min, z_max) ) +
  labs( x = "Longitude [°]", y = "Latitude [°]", fill = "SLA [m]" ) +
  labs( title = paste0("Black Sea SLA on ",as.character(day))) +
  theme_minimal() +
  theme( plot.title = element_text(face = "bold",hjust = 0.5),
         legend.position = "right",
         panel.grid.major = element_blank(),  
         panel.grid.minor = element_blank())+
  geom_hline( yintercept = seq(min(data$y), max(data$y), by = (quantile(data$y)[2]-quantile(data$y)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) +  
  geom_vline( xintercept = seq(min(data$x), max(data$x), by = (quantile(data$x)[2]-quantile(data$x)[1])/2), linetype = "solid", color = "grey", size = 0.1 ) + 
  geom_rect(aes(xmin = min_lon_mouth, xmax = max_lon_mouth, ymin = min_lat_mouth, ymax = max_lat_mouth),color = "red", fill = NA, linewidth = 0.5) +
  geom_rect(aes(xmin = min_lon_center, xmax = max_lon_center, ymin = min_lat_center, ymax = max_lat_center),color = "orange", fill = NA, linewidth = 0.5) 

quartz()
print(plot)
