rm(list=ls())
graphics.off()
cat("\014")

set.seed(23032000)

#change here
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"

#where the data are
dir_data = paste0(dir_w,"/Test_domain2D/RealWorld_data")

# upload data
load(paste0(dir_data,"/data/BS.Rdata"))

#address to save data
path_res_mouth = paste0(dir_data,"/data/mouth")
path_res_center = paste0(dir_data,"/data/center")




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
idx_lat_center = which(lat==42.5):which(lat==44.5)  # lat: 42.5 : 44.5
lat_center     = lat[idx_lat_center]
idx_lon_center = which(lon==36):which(lon==38)      # lon: 36   : 38
lon_center     = lon[idx_lon_center]

Xt_center = list()
for (i in 1:length(Xt)) {
  x_center = Xt[[i]][idx_lon_center,idx_lat_center]
  Xt_center[[i]] = x_center
}

save(Xt_center,lon_center,lat_center,file=paste0(path_res_center,"/BS_center.Rdata"))


# takes differences for mouth
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
