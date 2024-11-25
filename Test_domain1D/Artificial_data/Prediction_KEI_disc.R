rm(list=ls())
graphics.off()
cat("\014")
set.seed(23032000)

library(KePredictor)

#change here
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"
dir_stor_res = "/Test_domain1D/Artificial_data/results/results_prediction"
name_folder_res = "/KEI"


#if you want to save the result 
save_res = TRUE
#where to store the results
path_stor_res = paste0(paste0(dir_w,dir_stor_res),name_folder_res)              
#saving results
{
  file_saving_KEI_gau_0_5 = "/prediction_KEI_gau_0_5.Rdata"
  file_saving_KEI_gau_0_8 = "/prediction_KEI_gau_0_8.Rdata"
  file_saving_KEI_id_0_5  = "/prediction_KEI_id_0_5.Rdata"
  file_saving_KEI_id_0_8  = "/prediction_KEI_id_0_8.Rdata"
  file_saving_KEI_spt_0_5 = "/prediction_KEI_spt_0_5.Rdata"
  file_saving_KEI_spt_0_8 = "/prediction_KEI_spt_0_8.Rdata"
  file_saving_KEI_sps_0_5 = "/prediction_KEI_sps_0_5.Rdata"
  file_saving_KEI_sps_0_8 = "/prediction_KEI_sps_0_8.Rdata"
}



source(paste0(dir_w,"/Test_domain1D/Artificial_data/utils/far_1_1d.R"))         #load functions to generate the FAR(1) process
source(paste0(dir_w,"/Test_domain1D/Artificial_data/utils/prediction_error.R")) #load functions to evaluate the prediction error
source(paste0(dir_w,"/Test_domain1D/Artificial_data/utils/data_param.R"))       #load parameter to generate data according to a strategy
path_stor_res = paste0(paste0(dir_w,dir_stor_res),name_folder_res)              #where to store the results




#storing prediction
{
  prediction_KEI_gau_0_5 <- lapply((1):N-1,function(x) NULL)
  prediction_KEI_gau_0_8 <- lapply((1):N-1,function(x) NULL)
  prediction_KEI_id_0_5  <- lapply((1):N-1,function(x) NULL)
  prediction_KEI_id_0_8  <- lapply((1):N-1,function(x) NULL)
  prediction_KEI_spt_0_5 <- lapply((1):N-1,function(x) NULL)
  prediction_KEI_spt_0_8 <- lapply((1):N-1,function(x) NULL)
  prediction_KEI_sps_0_5 <- lapply((1):N-1,function(x) NULL)
  prediction_KEI_sps_0_8 <- lapply((1):N-1,function(x) NULL)
}

#storing errors
{
  err_KEI_gau_0_5_en <- numeric(N)     
  err_KEI_gau_0_5_rn <- numeric(N)
  err_KEI_id_0_5_en  <- numeric(N)     
  err_KEI_id_0_5_rn  <- numeric(N)
  err_KEI_spt_0_5_en <- numeric(N)     
  err_KEI_spt_0_5_rn <- numeric(N)
  err_KEI_sps_0_5_en <- numeric(N)     
  err_KEI_sps_0_5_rn <- numeric(N)
  err_KEI_gau_0_8_en <- numeric(N)     
  err_KEI_gau_0_8_rn <- numeric(N)
  err_KEI_id_0_8_en  <- numeric(N)     
  err_KEI_id_0_8_rn  <- numeric(N)
  err_KEI_spt_0_8_en <- numeric(N)     
  err_KEI_spt_0_8_rn <- numeric(N)
  err_KEI_sps_0_8_en <- numeric(N)     
  err_KEI_sps_0_8_rn <- numeric(N)
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
                  KEI prediction data Gaussian Kernel, norm 0.5 "
for (b in 1:N) {
  
  train_set = X.sample[,1:(N-1+b)]
  valid_set = X.sample[,N+b] 
  
  KEI_predictor = KEI_1d( X = train_set)
  
  prediction_KEI_gau_0_5[[b]] = list( Prediction = KEI_predictor$`One-step ahead prediction`, N_comp = KEI_predictor$`Number of Components retained`, Exp_Pow = KEI_predictor$`Explained variance`  )
  err_KEI_gau_0_5_en[b] = En(KEI_predictor$`One-step ahead prediction`,valid_set,t.grid)
  err_KEI_gau_0_5_rn[b] = Rn(KEI_predictor$`One-step ahead prediction`,valid_set,t.grid)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_KEI_gau_0_5 = list(Prediction = prediction_KEI_gau_0_5, En = err_KEI_gau_0_5_en, Rn = err_KEI_gau_0_5_rn)
  file_saving = paste0(path_stor_res,file_saving_KEI_gau_0_5)
  save(res_KEI_gau_0_5, file = file_saving)
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
                  KEI prediction data Gaussian Kernel, norm 0.8 "
for (b in 1:N) {
  
  train_set = X.sample[,1:(N-1+b)]
  valid_set = X.sample[,N+b] 
  
  KEI_predictor = KEI_1d( X = train_set)
  
  prediction_KEI_gau_0_8[[b]] = list( Prediction = KEI_predictor$`One-step ahead prediction`, N_comp = KEI_predictor$`Number of Components retained`, Exp_Pow = KEI_predictor$`Explained variance` )
  err_KEI_gau_0_8_en[b] = En(KEI_predictor$`One-step ahead prediction`,valid_set,t.grid)
  err_KEI_gau_0_8_rn[b] = Rn(KEI_predictor$`One-step ahead prediction`,valid_set,t.grid)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_KEI_gau_0_8 = list(Prediction = prediction_KEI_gau_0_8, En = err_KEI_gau_0_8_en, Rn = err_KEI_gau_0_8_rn)
  file_saving = paste0(path_stor_res,file_saving_KEI_gau_0_8)
  save(res_KEI_gau_0_8, file = file_saving)
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
                  KEI prediction data identity Kernel, norm 0.5 "
for (b in 1:N) {
  
  train_set = X.sample[,1:(N-1+b)]
  valid_set = X.sample[,N+b] 
  
  KEI_predictor = KEI_1d( X = train_set)
  
  prediction_KEI_id_0_5[[b]] = list( Prediction = KEI_predictor$`One-step ahead prediction`, N_comp = KEI_predictor$`Number of Components retained`, Exp_Pow = KEI_predictor$`Explained variance`  )
  err_KEI_id_0_5_en[b] = En(KEI_predictor$`One-step ahead prediction`,valid_set,t.grid)
  err_KEI_id_0_5_rn[b] = Rn(KEI_predictor$`One-step ahead prediction`,valid_set,t.grid)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_KEI_id_0_5 = list(Prediction = prediction_KEI_id_0_5, En = err_KEI_id_0_5_en, Rn = err_KEI_id_0_5_rn)
  file_saving = paste0(path_stor_res,file_saving_KEI_id_0_5)
  save(res_KEI_id_0_5, file = file_saving)
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
                  KEI prediction data identity Kernel, norm 0.8 "
for (b in 1:N) {
  
  train_set = X.sample[,1:(N-1+b)]
  valid_set = X.sample[,N+b] 
  
  KEI_predictor = KEI_1d( X = train_set)
  
  prediction_KEI_id_0_8[[b]] = list( Prediction = KEI_predictor$`One-step ahead prediction`, N_comp = KEI_predictor$`Number of Components retained`, Exp_Pow = KEI_predictor$`Explained variance` )
  err_KEI_id_0_8_en[b] = En(KEI_predictor$`One-step ahead prediction`,valid_set,t.grid)
  err_KEI_id_0_8_rn[b] = Rn(KEI_predictor$`One-step ahead prediction`,valid_set,t.grid)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_KEI_id_0_8 = list(Prediction = prediction_KEI_id_0_8, En = err_KEI_id_0_8_en, Rn = err_KEI_id_0_8_rn)
  file_saving = paste0(path_stor_res,file_saving_KEI_id_0_8)
  save(res_KEI_id_0_8, file = file_saving)
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
                  KEI prediction data sloping plane t Kernel, norm 0.5 "
for (b in 1:N) {
  
  train_set = X.sample[,1:(N-1+b)]
  valid_set = X.sample[,N+b] 
  
  KEI_predictor = KEI_1d( X = train_set)
  
  
  prediction_KEI_spt_0_5[[b]] = list( Prediction = KEI_predictor$`One-step ahead prediction`, N_comp = KEI_predictor$`Number of Components retained`, Exp_Pow = KEI_predictor$`Explained variance` )
  err_KEI_spt_0_5_en[b] = En(KEI_predictor$`One-step ahead prediction`,valid_set,t.grid)
  err_KEI_spt_0_5_rn[b] = Rn(KEI_predictor$`One-step ahead prediction`,valid_set,t.grid)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_KEI_spt_0_5 = list(Prediction = prediction_KEI_spt_0_5, En = err_KEI_spt_0_5_en, Rn = err_KEI_spt_0_5_rn)
  file_saving = paste0(path_stor_res,file_saving_KEI_spt_0_5)
  save(res_KEI_spt_0_5, file = file_saving)
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
                  KEI prediction data sloping plane t Kernel, norm 0.8 "
for (b in 1:N) {
  
  train_set = X.sample[,1:(N-1+b)]
  valid_set = X.sample[,N+b] 
  
  KEI_predictor = KEI_1d( X = train_set)
  
  
  prediction_KEI_spt_0_8[[b]] = list( Prediction = KEI_predictor$`One-step ahead prediction`, N_comp = KEI_predictor$`Number of Components retained`, Exp_Pow = KEI_predictor$`Explained variance` )
  err_KEI_spt_0_8_en[b] = En(KEI_predictor$`One-step ahead prediction`,valid_set,t.grid)
  err_KEI_spt_0_8_rn[b] = Rn(KEI_predictor$`One-step ahead prediction`,valid_set,t.grid)
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_KEI_spt_0_8 = list(Prediction = prediction_KEI_spt_0_8, En = err_KEI_spt_0_8_en, Rn = err_KEI_spt_0_8_rn)
  file_saving = paste0(path_stor_res,file_saving_KEI_spt_0_8)
  save(res_KEI_spt_0_8, file = file_saving)
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
  
  KEI_predictor = KEI_1d( X = train_set)
  
  
  prediction_KEI_sps_0_5[[b]] = list( Prediction = KEI_predictor$`One-step ahead prediction`, N_comp = KEI_predictor$`Number of Components retained`, Exp_Pow = KEI_predictor$`Explained variance` )
  err_KEI_sps_0_5_en[b] = En(KEI_predictor$`One-step ahead prediction`,valid_set,t.grid)
  err_KEI_sps_0_5_rn[b] = Rn(KEI_predictor$`One-step ahead prediction`,valid_set,t.grid)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_KEI_sps_0_5 = list(Prediction = prediction_KEI_sps_0_5, En = err_KEI_sps_0_5_en, Rn = err_KEI_sps_0_5_rn)
  file_saving = paste0(path_stor_res,file_saving_KEI_sps_0_5)
  save(res_KEI_sps_0_5, file = file_saving)
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
  
  KEI_predictor = KEI_1d( X = train_set)
  
  
  prediction_KEI_sps_0_8[[b]] = list( Prediction = KEI_predictor$`One-step ahead prediction`, N_comp = KEI_predictor$`Number of Components retained`, Exp_Pow = KEI_predictor$`Explained variance` )
  err_KEI_sps_0_8_en[b] = En(KEI_predictor$`One-step ahead prediction`,valid_set,t.grid)
  err_KEI_sps_0_8_rn[b] = Rn(KEI_predictor$`One-step ahead prediction`,valid_set,t.grid)
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_KEI_sps_0_8 = list(Prediction = prediction_KEI_sps_0_8, En = err_KEI_sps_0_8_en, Rn = err_KEI_sps_0_8_rn)
  file_saving = paste0(path_stor_res,file_saving_KEI_sps_0_8)
  save(res_KEI_sps_0_8, file = file_saving)
}
