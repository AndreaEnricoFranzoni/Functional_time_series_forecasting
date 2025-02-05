rm(list=ls())
graphics.off()
cat("\014")
set.seed(23032000)


##############################################################
#### Computing PPC prediction as indicated in the readme  ####
##############################################################


library(PPCKO)


#put here the path of the local copy of the directory
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"

#save res
save_res = TRUE
folder_res = "/PPC"

#where the data are
dir_data = paste0(dir_w,"/Test_domain2D/RealWorld_data/data")
path_data_mouth = paste0(dir_data,"/mouth")
path_data_center = paste0(dir_data,"/center")

#where to store results
dir_res = paste0(dir_w,"/Test_domain2D/RealWorld_data/results/results_next_instant_prediction")
path_store_res = dir_res


#load the dataset
load(paste0(dir_data,"/BS.Rdata"))

#load data for mouth zone
load(paste0(path_data_mouth,"/BS_mouth.Rdata"))
load(paste0(path_data_mouth,"/BS_mouth_diff_1.Rdata"))
load(paste0(path_data_mouth,"/BS_mouth_diff_2.Rdata"))

#load data for center zone
load(paste0(path_data_center,"/BS_center.Rdata"))
load(paste0(path_data_center,"/BS_center_diff_1.Rdata"))
load(paste0(path_data_center,"/BS_center_diff_2.Rdata"))


first_day_first_train = "20170101"
idx_first_day_first_train = which(dates==first_day_first_train)
window_train_set = 99
days_to_be_pred = 1000


# paramters for PPC KO
id_CV_ = "CV"
alpha_vec = c(0.0001,0.001,0.01,0.1,1,10)


##-----mouth prediction-----

# time serie
mouth = data_2d_wrapper_from_list(Xt_mouth)

train_set = mouth[,(dim(mouth)[2]-window_train_set):dim(mouth)[2]]

prediction_PPC_mouth_next_instant = PPC_KO_2d( X = train_set,
                       id_CV = id_CV_,
                       alpha_vec = alpha_vec,
                       disc_ev_x1 = lon_mouth,
                       num_disc_ev_x1 = length(lon_mouth),
                       disc_ev_x2 = lat_mouth,
                       num_disc_ev_x2 = length(lat_mouth),
                       left_extreme_x1 = min(lon_mouth),
                       right_extreme_x1 = max(lon_mouth),
                       left_extreme_x2 = min(lat_mouth),
                       right_extreme_x2 = max(lat_mouth))

# save results
if(save_res){
  save(prediction_PPC_mouth_next_instant, file = paste0(path_store_res,"/prediction_PPC_mouth_next_instant.Rdata"))
}


# diff 1
train_set = Xt_mouth_diff_1[,(dim(Xt_mouth_diff_1)[2]-window_train_set):dim(Xt_mouth_diff_1)[2]]

prediction_PPC_mouth_diff_1_next_instant = PPC_KO_2d( X = train_set,
                       id_CV = id_CV_,
                       alpha_vec = alpha_vec,
                       disc_ev_x1 = lon_mouth,
                       num_disc_ev_x1 = length(lon_mouth),
                       disc_ev_x2 = lat_mouth,
                       num_disc_ev_x2 = length(lat_mouth),
                       left_extreme_x1 = min(lon_mouth),
                       right_extreme_x1 = max(lon_mouth),
                       left_extreme_x2 = min(lat_mouth),
                       right_extreme_x2 = max(lat_mouth))

# save results
if(save_res){
  save(prediction_PPC_mouth_diff_1_next_instant, file = paste0(path_store_res,"/prediction_PPC_mouth_diff_1_next_instant.Rdata"))
}

# diff 2
train_set = Xt_mouth_diff_2[,(dim(Xt_mouth_diff_2)[2]-window_train_set):dim(Xt_mouth_diff_2)[2]]

prediction_PPC_mouth_diff_2_next_instant = PPC_KO_2d( X = train_set,
                                                      id_CV = id_CV_,
                                                      alpha_vec = alpha_vec,
                                                      disc_ev_x1 = lon_mouth,
                                                      num_disc_ev_x1 = length(lon_mouth),
                                                      disc_ev_x2 = lat_mouth,
                                                      num_disc_ev_x2 = length(lat_mouth),
                                                      left_extreme_x1 = min(lon_mouth),
                                                      right_extreme_x1 = max(lon_mouth),
                                                      left_extreme_x2 = min(lat_mouth),
                                                      right_extreme_x2 = max(lat_mouth))

# save results
if(save_res){
  save(prediction_PPC_mouth_diff_2_next_instant, file = paste0(path_store_res,"/prediction_PPC_mouth_diff_2_next_instant.Rdata"))
}


##-----center prediction-----

# time serie
center = data_2d_wrapper_from_list(Xt_center)

train_set = center[,(dim(center)[2]-window_train_set):dim(center)[2]]

prediction_PPC_center_next_instant = PPC_KO_2d( X = train_set,
                       id_CV = id_CV_,
                       alpha_vec = alpha_vec,
                       disc_ev_x1 = lon_center,
                       num_disc_ev_x1 = length(lon_center),
                       disc_ev_x2 = lat_center,
                       num_disc_ev_x2 = length(lat_center),
                       left_extreme_x1 = min(lon_center),
                       right_extreme_x1 = max(lon_center),
                       left_extreme_x2 = min(lat_center),
                       right_extreme_x2 = max(lat_center))

# save results
if(save_res){
  save(prediction_PPC_center_next_instant, file = paste0(path_store_res,"/prediction_PPC_center_next_instant.Rdata"))
}


# diff 1
train_set = Xt_center_diff_1[,(dim(Xt_center_diff_1)[2]-window_train_set):dim(Xt_center_diff_1)[2]]

prediction_PPC_center_diff_1_next_instant = PPC_KO_2d( X = train_set,
                                                id_CV = id_CV_,
                                                alpha_vec = alpha_vec,
                                                disc_ev_x1 = lon_center,
                                                num_disc_ev_x1 = length(lon_center),
                                                disc_ev_x2 = lat_center,
                                                num_disc_ev_x2 = length(lat_center),
                                                left_extreme_x1 = min(lon_center),
                                                right_extreme_x1 = max(lon_center),
                                                left_extreme_x2 = min(lat_center),
                                                right_extreme_x2 = max(lat_center))

# save results
if(save_res){
  save(prediction_PPC_center_diff_1_next_instant, file = paste0(path_store_res,"/prediction_PPC_center_diff_1_next_instant.Rdata"))
}


# diff 2
train_set = Xt_center_diff_2[,(dim(Xt_center_diff_2)[2]-window_train_set):dim(Xt_center_diff_2)[2]]

prediction_PPC_center_diff_2_next_instant = PPC_KO_2d( X = train_set,
                                                       id_CV = id_CV_,
                                                       alpha_vec = alpha_vec,
                                                       disc_ev_x1 = lon_center,
                                                       num_disc_ev_x1 = length(lon_center),
                                                       disc_ev_x2 = lat_center,
                                                       num_disc_ev_x2 = length(lat_center),
                                                       left_extreme_x1 = min(lon_center),
                                                       right_extreme_x1 = max(lon_center),
                                                       left_extreme_x2 = min(lat_center),
                                                       right_extreme_x2 = max(lat_center))

# save results
if(save_res){
  save(prediction_PPC_center_diff_2_next_instant, file = paste0(path_store_res,"/prediction_PPC_center_diff_2_next_instant.Rdata"))
}


# plot the PPC
KO_show_results_2d(results_ko = prediction_PPC_mouth_next_instant,
                   x1_lab = "Longitude [°]",
                   x2_lab = "Latitude [°]",
                   z_lab = "SLA")

KO_show_results_2d(results_ko = prediction_PPC_mouth_diff_1_next_instant,
                   x1_lab = "Longitude [°]",
                   x2_lab = "Latitude [°]",
                   z_lab = "SLA")

KO_show_results_2d(results_ko = prediction_PPC_mouth_diff_2_next_instant,
                   x1_lab = "Longitude [°]",
                   x2_lab = "Latitude [°]",
                   z_lab = "SLA")

KO_show_results_2d(results_ko = prediction_PPC_center_next_instant,
                   x1_lab = "Longitude [°]",
                   x2_lab = "Latitude [°]",
                   z_lab = "SLA")

KO_show_results_2d(results_ko = prediction_PPC_center_diff_1_next_instant,
                   x1_lab = "Longitude [°]",
                   x2_lab = "Latitude [°]",
                   z_lab = "SLA")

KO_show_results_2d(results_ko = prediction_PPC_center_diff_2_next_instant,
                   x1_lab = "Longitude [°]",
                   x2_lab = "Latitude [°]",
                   z_lab = "SLA")
