rm(list=ls())
graphics.off()
cat("\014")
set.seed(23032000)


#change here
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"
dir_stor_res = "/Test_domain1D/Artificial_data/results/results_prediction"
name_folder_res = "/KE"


#if you want to save the result 
save_res = TRUE
#where to store the results
path_stor_res = paste0(paste0(dir_w,dir_stor_res),name_folder_res)              
#saving results
{
  file_saving_KE_gau_0_5 = "/prediction_KE_gau_0_5.Rdata"
  file_saving_KE_gau_0_8 = "/prediction_KE_gau_0_8.Rdata"
  file_saving_KE_id_0_5  = "/prediction_KE_id_0_5.Rdata"
  file_saving_KE_id_0_8  = "/prediction_KE_id_0_8.Rdata"
  file_saving_KE_spt_0_5 = "/prediction_KE_spt_0_5.Rdata"
  file_saving_KE_spt_0_8 = "/prediction_KE_spt_0_8.Rdata"
  file_saving_KE_sps_0_5 = "/prediction_KE_sps_0_5.Rdata"
  file_saving_KE_sps_0_8 = "/prediction_KE_sps_0_8.Rdata"
}



source(paste0(dir_w,"/Test_domain1D/Artificial_data/utils/EstimatedKernel_predictor.R"))       #load parameter to generate data according to a strategy
source(paste0(dir_w,"/Test_domain1D/Artificial_data/utils/KE_cv.R"))       #load parameter to generate data according to a strategy

source(paste0(dir_w,"/Test_domain1D/Artificial_data/utils/far_1_1d.R"))         #load functions to generate the FAR(1) process
source(paste0(dir_w,"/Test_domain1D/Artificial_data/utils/prediction_error.R")) #load functions to evaluate the prediction error
source(paste0(dir_w,"/Test_domain1D/Artificial_data/utils/data_param.R"))       #load parameter to generate data according to a strategy
path_stor_res = paste0(paste0(dir_w,dir_stor_res),name_folder_res)              #where to store the results


# parameters for doing CV for EK
p_vector = c(2,3,4,5,6)


#storing prediction
{
  prediction_KE_gau_0_5 <- lapply((1):N-1,function(x) NULL)
  prediction_KE_gau_0_8 <- lapply((1):N-1,function(x) NULL)
  prediction_KE_id_0_5  <- lapply((1):N-1,function(x) NULL)
  prediction_KE_id_0_8  <- lapply((1):N-1,function(x) NULL)
  prediction_KE_spt_0_5 <- lapply((1):N-1,function(x) NULL)
  prediction_KE_spt_0_8 <- lapply((1):N-1,function(x) NULL)
  prediction_KE_sps_0_5 <- lapply((1):N-1,function(x) NULL)
  prediction_KE_sps_0_8 <- lapply((1):N-1,function(x) NULL)
}

#storing errors
{
  err_KE_gau_0_5_en <- numeric(N)     
  err_KE_gau_0_5_rn <- numeric(N)
  err_KE_id_0_5_en  <- numeric(N)     
  err_KE_id_0_5_rn  <- numeric(N)
  err_KE_spt_0_5_en <- numeric(N)     
  err_KE_spt_0_5_rn <- numeric(N)
  err_KE_sps_0_5_en <- numeric(N)     
  err_KE_sps_0_5_rn <- numeric(N)
  err_KE_gau_0_8_en <- numeric(N)     
  err_KE_gau_0_8_rn <- numeric(N)
  err_KE_id_0_8_en  <- numeric(N)     
  err_KE_id_0_8_rn  <- numeric(N)
  err_KE_spt_0_8_en <- numeric(N)     
  err_KE_spt_0_8_rn <- numeric(N)
  err_KE_sps_0_8_en <- numeric(N)     
  err_KE_sps_0_8_rn <- numeric(N)
}






# ----- data generation Gaussian Kernel norm 0.5 -----
{
  #feats of data
  id_kernel <- "gaussian"       #way of generating data ("gaussian", "identity", "sp_t" and "sp_s" kernel available)
  norm      <- 0.5          #norm of the Kernel (that has to be <1)
  id_noise  <- "1"          #error of the FAR(1) process ("1", "2" or "3")
  
  
  proc = feat_far_1_process(id_kernel,norm)
  id_kernel   <- proc$kernel
  a           <- proc$constant
  name_kernel <- proc$name
  
  ##Simulate a stationary FAR(1) process according to a specific kernel
  X.sample <- far_1_1D(kernel_id = id_kernel, noise_id = id_noise, n = n, t.grid = t.grid, a = a, burnin = burnin)
}



string_message = "
                  KE prediction data Gaussian Kernel, norm 0.5 "
for (b in 1:N) {
  
  train_set = X.sample[,1:(N-1+b)]
  valid_set = X.sample[,N+b] 
  
  predictor = cv_EK( X = train_set,
                     grid_eval = t.grid,
                     p_vector = p_vector,
                     improved = FALSE)
  
  prediction_KE_gau_0_5[[b]] = list(Prediction = predictor$prediction, N_comp = predictor$N_PCs_ret)
  err_KE_gau_0_5_en[b] = En(prediction_KE_gau_0_5[[b]]$Prediction,valid_set,t.grid)
  err_KE_gau_0_5_rn[b] = Rn(prediction_KE_gau_0_5[[b]]$Prediction,valid_set,t.grid)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_KE_gau_0_5 = list(Prediction = prediction_KE_gau_0_5, En = err_KE_gau_0_5_en, Rn = err_KE_gau_0_5_rn)
  file_saving = paste0(path_stor_res,file_saving_KE_gau_0_5)
  save(res_KE_gau_0_5, file = file_saving)
}





# ----- data generation Gaussian Kernel norm 0.8 -----
{
  #feats of data
  id_kernel <- "gaussian"       #way of generating data ("gaussian", "identity", "sp_t" and "sp_s" kernel available)
  norm      <- 0.8          #norm of the Kernel (that has to be <1)
  id_noise  <- "1"          #error of the FAR(1) process ("1", "2" or "3")
  
  #grid for the kernel for generating FAR(1)
  #s.grid <- seq(0,1, length.out=dim_grid)
  #grid   <- expand.grid(t.grid, s.grid)
  
  
  proc = feat_far_1_process(id_kernel,norm)
  id_kernel   <- proc$kernel
  a           <- proc$constant
  name_kernel <- proc$name
  
  ##Simulate a stationary FAR(1) process according to a specific kernel
  X.sample <- far_1_1D(kernel_id = id_kernel, noise_id = id_noise, n = n, t.grid = t.grid, a = a, burnin = burnin)
}



string_message = "
                  KE prediction data Gaussian Kernel, norm 0.8 "
for (b in 1:N) {
  
  train_set = X.sample[,1:(N-1+b)]
  valid_set = X.sample[,N+b] 
  
  predictor = cv_EK( X = train_set,
                     grid_eval = t.grid,
                     p_vector = p_vector,
                     improved = FALSE)
  
  prediction_KE_gau_0_8[[b]] = list(Prediction = predictor$prediction, N_comp = predictor$N_PCs_ret)
  err_KE_gau_0_8_en[b] = En(prediction_KE_gau_0_8[[b]]$Prediction,valid_set,t.grid)
  err_KE_gau_0_8_rn[b] = Rn(prediction_KE_gau_0_8[[b]]$Prediction,valid_set,t.grid)
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_KE_gau_0_8 = list(Prediction = prediction_KE_gau_0_8, En = err_KE_gau_0_8_en, Rn = err_KE_gau_0_8_rn)
  file_saving = paste0(path_stor_res,file_saving_KE_gau_0_8)
  save(res_KE_gau_0_8, file = file_saving)
}





# ----- data generation idenity Kernel norm 0.5 -----
{
  #feats of data
  id_kernel <- "identity"       #way of generating data ("gaussian", "identity", "sp_t" and "sp_s" kernel available)
  norm      <- 0.5          #norm of the Kernel (that has to be <1)
  id_noise  <- "1"          #error of the FAR(1) process ("1", "2" or "3")
  
  #grid for the kernel for generating FAR(1)
  #s.grid <- seq(0,1, length.out=dim_grid)
  #grid   <- expand.grid(t.grid, s.grid)
  
  
  proc = feat_far_1_process(id_kernel,norm)
  id_kernel   <- proc$kernel
  a           <- proc$constant
  name_kernel <- proc$name
  
  ##Simulate a stationary FAR(1) process according to a specific kernel
  X.sample <- far_1_1D(kernel_id = id_kernel, noise_id = id_noise, n = n, t.grid = t.grid, a = a, burnin = burnin)
}



string_message = "
                  KE prediction data identity Kernel, norm 0.5 "
for (b in 1:N) {
  
  train_set = X.sample[,1:(N-1+b)]
  valid_set = X.sample[,N+b] 
  
  predictor = cv_EK( X = train_set,
                     grid_eval = t.grid,
                     p_vector = p_vector,
                     improved = FALSE)
  
  prediction_KE_id_0_5[[b]] = list(Prediction = predictor$prediction, N_comp = predictor$N_PCs_ret)
  err_KE_id_0_5_en[b] = En(prediction_KE_id_0_5[[b]]$Prediction,valid_set,t.grid)
  err_KE_id_0_5_rn[b] = Rn(prediction_KE_id_0_5[[b]]$Prediction,valid_set,t.grid)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_KE_id_0_5 = list(Prediction = prediction_KE_id_0_5, En = err_KE_id_0_5_en, Rn = err_KE_id_0_5_rn)
  file_saving = paste0(path_stor_res,file_saving_KE_id_0_5)
  save(res_KE_id_0_5, file = file_saving)
}





# ----- data generation idenity Kernel norm 0.8 -----
{
  #feats of data
  id_kernel <- "identity"       #way of generating data ("gaussian", "identity", "sp_t" and "sp_s" kernel available)
  norm      <- 0.8          #norm of the Kernel (that has to be <1)
  id_noise  <- "1"          #error of the FAR(1) process ("1", "2" or "3")
  
  #grid for the kernel for generating FAR(1)
  #s.grid <- seq(0,1, length.out=dim_grid)
  #grid   <- expand.grid(t.grid, s.grid)
  
  
  proc = feat_far_1_process(id_kernel,norm)
  id_kernel   <- proc$kernel
  a           <- proc$constant
  name_kernel <- proc$name
  
  ##Simulate a stationary FAR(1) process according to a specific kernel
  X.sample <- far_1_1D(kernel_id = id_kernel, noise_id = id_noise, n = n, t.grid = t.grid, a = a, burnin = burnin)
}



string_message = "
                  KE prediction data identity Kernel, norm 0.8 "
for (b in 1:N) {
  
  train_set = X.sample[,1:(N-1+b)]
  valid_set = X.sample[,N+b] 
  
  predictor = cv_EK( X = train_set,
                     grid_eval = t.grid,
                     p_vector = p_vector,
                     improved = FALSE)
  
  prediction_KE_id_0_8[[b]] = list(Prediction = predictor$prediction, N_comp = predictor$N_PCs_ret)
  err_KE_id_0_8_en[b] = En(prediction_KE_id_0_8[[b]]$Prediction,valid_set,t.grid)
  err_KE_id_0_8_rn[b] = Rn(prediction_KE_id_0_8[[b]]$Prediction,valid_set,t.grid)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_KE_id_0_8 = list(Prediction = prediction_KE_id_0_8, En = err_KE_id_0_8_en, Rn = err_KE_id_0_8_rn)
  file_saving = paste0(path_stor_res,file_saving_KE_id_0_8)
  save(res_KE_id_0_8, file = file_saving)
}





# ----- data generation slopint plane t Kernel norm 0.5 -----
{
  #feats of data
  id_kernel <- "sp_t"       #way of generating data ("gaussian", "identity", "sp_t" and "sp_s" kernel available)
  norm      <- 0.5          #norm of the Kernel (that has to be <1)
  id_noise  <- "1"          #error of the FAR(1) process ("1", "2" or "3")
  
  #grid for the kernel for generating FAR(1)
  #s.grid <- seq(0,1, length.out=dim_grid)
  #grid   <- expand.grid(t.grid, s.grid)
  
  
  proc = feat_far_1_process(id_kernel,norm)
  id_kernel   <- proc$kernel
  a           <- proc$constant
  name_kernel <- proc$name
  
  ##Simulate a stationary FAR(1) process according to a specific kernel
  X.sample <- far_1_1D(kernel_id = id_kernel, noise_id = id_noise, n = n, t.grid = t.grid, a = a, burnin = burnin)
}



string_message = "
                  KE prediction data sloping plane t Kernel, norm 0.5 "
for (b in 1:N) {
  
  train_set = X.sample[,1:(N-1+b)]
  valid_set = X.sample[,N+b] 
  
  predictor = cv_EK( X = train_set,
                     grid_eval = t.grid,
                     p_vector = p_vector,
                     improved = FALSE)
  
  prediction_KE_spt_0_5[[b]] = list(Prediction = predictor$prediction, N_comp = predictor$N_PCs_ret)
  err_KE_spt_0_5_en[b] = En(prediction_KE_spt_0_5[[b]]$Prediction,valid_set,t.grid)
  err_KE_spt_0_5_rn[b] = Rn(prediction_KE_spt_0_5[[b]]$Prediction,valid_set,t.grid)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_KE_spt_0_5 = list(Prediction = prediction_KE_spt_0_5, En = err_KE_spt_0_5_en, Rn = err_KE_spt_0_5_rn)
  file_saving = paste0(path_stor_res,file_saving_KE_spt_0_5)
  save(res_KE_spt_0_5, file = file_saving)
}





# ----- data generation slopint plane t Kernel norm 0.8 -----
{
  #feats of data
  id_kernel <- "sp_t"       #way of generating data ("gaussian", "identity", "sp_t" and "sp_s" kernel available)
  norm      <- 0.8          #norm of the Kernel (that has to be <1)
  id_noise  <- "1"          #error of the FAR(1) process ("1", "2" or "3")
  
  #grid for the kernel for generating FAR(1)
  #s.grid <- seq(0,1, length.out=dim_grid)
  #grid   <- expand.grid(t.grid, s.grid)
  
  
  proc = feat_far_1_process(id_kernel,norm)
  id_kernel   <- proc$kernel
  a           <- proc$constant
  name_kernel <- proc$name
  
  ##Simulate a stationary FAR(1) process according to a specific kernel
  X.sample <- far_1_1D(kernel_id = id_kernel, noise_id = id_noise, n = n, t.grid = t.grid, a = a, burnin = burnin)
}



string_message = "
                  KE prediction data sloping plane t Kernel, norm 0.8 "
for (b in 1:N) {
  
  train_set = X.sample[,1:(N-1+b)]
  valid_set = X.sample[,N+b] 
  
  predictor = cv_EK( X = train_set,
                     grid_eval = t.grid,
                     p_vector = p_vector,
                     improved = FALSE)
  
  prediction_KE_spt_0_8[[b]] = list(Prediction = predictor$prediction, N_comp = predictor$N_PCs_ret)
  err_KE_spt_0_8_en[b] = En(prediction_KE_spt_0_8[[b]]$Prediction,valid_set,t.grid)
  err_KE_spt_0_8_rn[b] = Rn(prediction_KE_spt_0_8[[b]]$Prediction,valid_set,t.grid)
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_KE_spt_0_8 = list(Prediction = prediction_KE_spt_0_8, En = err_KE_spt_0_8_en, Rn = err_KE_spt_0_8_rn)
  file_saving = paste0(path_stor_res,file_saving_KE_spt_0_8)
  save(res_KE_spt_0_8, file = file_saving)
}





# ----- data generation slopint plane s Kernel norm 0.5 -----
{
  #feats of data
  id_kernel <- "sp_s"       #way of generating data ("gaussian", "identity", "sp_t" and "sp_s" kernel available)
  norm      <- 0.5          #norm of the Kernel (that has to be <1)
  id_noise  <- "1"          #error of the FAR(1) process ("1", "2" or "3")
  
  #grid for the kernel for generating FAR(1)
  #s.grid <- seq(0,1, length.out=dim_grid)
  #grid   <- expand.grid(t.grid, s.grid)
  
  
  proc = feat_far_1_process(id_kernel,norm)
  id_kernel   <- proc$kernel
  a           <- proc$constant
  name_kernel <- proc$name
  
  ##Simulate a stationary FAR(1) process according to a specific kernel
  X.sample <- far_1_1D(kernel_id = id_kernel, noise_id = id_noise, n = n, t.grid = t.grid, a = a, burnin = burnin)
}



string_message = "
                  KE prediction data sloping plane s Kernel, norm 0.5 "
for (b in 1:N) {
  
  train_set = X.sample[,1:(N-1+b)]
  valid_set = X.sample[,N+b] 
  
  predictor = cv_EK( X = train_set,
                     grid_eval = t.grid,
                     p_vector = p_vector,
                     improved = FALSE)
  
  prediction_KE_sps_0_5[[b]] = list(Prediction = predictor$prediction, N_comp = predictor$N_PCs_ret)
  err_KE_sps_0_5_en[b] = En(prediction_KE_sps_0_5[[b]]$Prediction,valid_set,t.grid)
  err_KE_sps_0_5_rn[b] = Rn(prediction_KE_sps_0_5[[b]]$Prediction,valid_set,t.grid)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_KE_sps_0_5 = list(Prediction = prediction_KE_sps_0_5, En = err_KE_sps_0_5_en, Rn = err_KE_sps_0_5_rn)
  file_saving = paste0(path_stor_res,file_saving_KE_sps_0_5)
  save(res_KE_sps_0_5, file = file_saving)
}





# ----- data generation slopint plane s Kernel norm 0.8 -----
{
  #feats of data
  id_kernel <- "sp_t"       #way of generating data ("gaussian", "identity", "sp_t" and "sp_s" kernel available)
  norm      <- 0.8          #norm of the Kernel (that has to be <1)
  id_noise  <- "1"          #error of the FAR(1) process ("1", "2" or "3")
  
  #grid for the kernel for generating FAR(1)
  #s.grid <- seq(0,1, length.out=dim_grid)
  #grid   <- expand.grid(t.grid, s.grid)
  
  
  proc = feat_far_1_process(id_kernel,norm)
  id_kernel   <- proc$kernel
  a           <- proc$constant
  name_kernel <- proc$name
  
  ##Simulate a stationary FAR(1) process according to a specific kernel
  X.sample <- far_1_1D(kernel_id = id_kernel, noise_id = id_noise, n = n, t.grid = t.grid, a = a, burnin = burnin)
}



string_message = "
                  KE prediction data sloping plane s Kernel, norm 0.8 "
for (b in 1:N) {
  
  train_set = X.sample[,1:(N-1+b)]
  valid_set = X.sample[,N+b] 
  
  predictor = cv_EK( X = train_set,
                     grid_eval = t.grid,
                     p_vector = p_vector,
                     improved = FALSE)
  
  prediction_KE_sps_0_8[[b]] = list(Prediction = predictor$prediction, N_comp = predictor$N_PCs_ret)
  err_KE_sps_0_8_en[b] = En(prediction_KE_sps_0_8[[b]]$Prediction,valid_set,t.grid)
  err_KE_sps_0_8_rn[b] = Rn(prediction_KE_sps_0_8[[b]]$Prediction,valid_set,t.grid)
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_KE_sps_0_8 = list(Prediction = prediction_KE_sps_0_8, En = err_KE_sps_0_8_en, Rn = err_KE_sps_0_8_rn)
  file_saving = paste0(path_stor_res,file_saving_KE_sps_0_8)
  save(res_KE_sps_0_8, file = file_saving)
}

