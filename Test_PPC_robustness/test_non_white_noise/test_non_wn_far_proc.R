rm(list=ls())
graphics.off()
cat("\014")
set.seed(23032000)

###############################################################################
#### PPC robustness ti non compact AR operator as indicated in the readme  ####
###############################################################################

library(PPCKO)

#put here the path of the local copy of the directory
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"

#if you want to save the result 
save_res = TRUE


#where to store the results
dir_stor_res = "/Test_PPC_robustness/test_non_white_noise/results/prediction/"
path_stor_res = paste0(dir_w,dir_stor_res)

#file for saving results
{
  file_saving_PPC_gau_no_wn = "/prediction_PPC_gau_no_wn.Rdata"
  file_saving_PPC_id_no_wn  = "/prediction_PPC_id_no_wn.Rdata"
  file_saving_PPC_spt_no_wn = "/prediction_PPC_spt_no_wn.Rdata"
  file_saving_PPC_sps_no_wn = "/prediction_PPC_sps_no_wn.Rdata"
  file_saving_EX_gau_no_wn  = "/prediction_EX_gau_no_wn.Rdata"
  file_saving_EX_id_no_wn   = "/prediction_EX_id_no_wn.Rdata"
  file_saving_EX_spt_no_wn  = "/prediction_EX_spt_no_wn.Rdata"
  file_saving_EX_sps_no_wn  = "/prediction_EX_sps_no_wn.Rdata"
}

#uploading functions for generating data
source(paste0(dir_w,"/Test_PPC_robustness/test_non_white_noise/utils/far_1_1d.R"))         #load functions to generate the FAR(1) process
source(paste0(dir_w,"/Test_PPC_robustness/test_non_white_noise/utils/data_param.R"))       #load parameter to generate data according to a strategy


#PPCKO parameters 
{
  id_CV_ko      <- "CV"           #Ko algorithm
  alpha         <- 0.1            #regularization parameter
  k             <- 0              #if a nnumber of PPCs has to be retained
  threshold_ppc <- 0.95           #threshold of explanatory power that has to be explained through PPCs
  alpha_vec     <- NULL           #alpha values for CV on regularization parameter
  k_vec         <- NULL           #k values for CV on number of PPCs
  toll          <- 1e-4           #toll for stopping adding others PPCs during CV
  disc_ev       <- t.grid         #discrete evaluations of the functional domain (has to be the same as t.grid)
  left_extreme  <- left_ex        #left extreme of the domain of the functional data (has to be the same as left_ex)
  right_extreme <- right_ex       #right extreme of the domain of the functional data (has to be the same as right_ex)
  err_ret       <- 1              #if CV errors have to be retained
  id_rem_nan    <- NULL
}


#storing prediction
{
  prediction_PPC_gau_no_wn <- lapply((1):N-1,function(x) NULL)
  prediction_EX_gau_no_wn  <- lapply((1):N-1,function(x) NULL)
  prediction_PPC_id_no_wn  <- lapply((1):N-1,function(x) NULL)
  prediction_EX_id_no_wn   <- lapply((1):N-1,function(x) NULL)
  prediction_PPC_spt_no_wn <- lapply((1):N-1,function(x) NULL)
  prediction_EX_spt_no_wn  <- lapply((1):N-1,function(x) NULL)
  prediction_PPC_sps_no_wn <- lapply((1):N-1,function(x) NULL)
  prediction_EX_sps_no_wn  <- lapply((1):N-1,function(x) NULL)
}

#storing errors
{
  err_PPC_gau_no_wn_en <- numeric(N)     
  err_PPC_gau_no_wn_rn <- numeric(N)
  err_PPC_id_no_wn_en  <- numeric(N)     
  err_PPC_id_no_wn_rn  <- numeric(N)
  err_PPC_spt_no_wn_en <- numeric(N)     
  err_PPC_spt_no_wn_rn <- numeric(N)
  err_PPC_sps_no_wn_en <- numeric(N)     
  err_PPC_sps_no_wn_rn <- numeric(N)
  err_EX_gau_no_wn_en  <- numeric(N)     
  err_EX_gau_no_wn_rn  <- numeric(N)
  err_EX_id_no_wn_en   <- numeric(N)     
  err_EX_id_no_wn_rn   <- numeric(N)
  err_EX_spt_no_wn_en  <- numeric(N)     
  err_EX_spt_no_wn_rn  <- numeric(N)
  err_EX_sps_no_wn_en  <- numeric(N)     
  err_EX_sps_no_wn_rn  <- numeric(N)
}






# ----- data generation Gaussian Kernel norm 0.5 -----
{
  #feats of data
  id_kernel <- "gaussian"       #way of generating data ("gaussian", "identity", "sp_t" and "sp_s" kernel available)
  norm      <- 0.5          #norm of the Kernel (that has to be <1)
  id_noise  <- "4"          #error of the FAR(1) process ("1", "2" or "3")
  
  #grid for the kernel for generating FAR(1)
  #s.grid <- seq(0,1, length.out=dim_grid)
  #grid   <- expand.grid(t.grid, s.grid)
  
  
  proc = feat_far_1_process(id_kernel,norm)
  id_kernel   <- proc$kernel
  a           <- proc$constant
  name_kernel <- proc$name
  
  ##Simulate a stationary FAR(1) process according to a specific kernel
  X.sample_gau <- far_1_1D(kernel_id = id_kernel, noise_id = id_noise, n = n, t.grid = t.grid, a = a, burnin = burnin)
}

# ----- data generation idenity Kernel norm 0.5 -----
{
  #feats of data
  id_kernel <- "identity"       #way of generating data ("gaussian", "identity", "sp_t" and "sp_s" kernel available)
  norm      <- 0.5          #norm of the Kernel (that has to be <1)
  id_noise  <- "5"          #error of the FAR(1) process ("1", "2" or "3")
  
  #grid for the kernel for generating FAR(1)
  #s.grid <- seq(0,1, length.out=dim_grid)
  #grid   <- expand.grid(t.grid, s.grid)
  
  
  proc = feat_far_1_process(id_kernel,norm)
  id_kernel   <- proc$kernel
  a           <- proc$constant
  name_kernel <- proc$name
  
  ##Simulate a stationary FAR(1) process according to a specific kernel
  X.sample_id <- far_1_1D(kernel_id = id_kernel, noise_id = id_noise, n = n, t.grid = t.grid, a = a, burnin = burnin)
}

# ----- data generation slopint plane t Kernel norm 0.5 -----
{
  #feats of data
  id_kernel <- "sp_t"       #way of generating data ("gaussian", "identity", "sp_t" and "sp_s" kernel available)
  norm      <- 0.5          #norm of the Kernel (that has to be <1)
  id_noise  <- "4"          #error of the FAR(1) process ("1", "2" or "3")
  
  #grid for the kernel for generating FAR(1)
  #s.grid <- seq(0,1, length.out=dim_grid)
  #grid   <- expand.grid(t.grid, s.grid)
  
  
  proc = feat_far_1_process(id_kernel,norm)
  id_kernel   <- proc$kernel
  a           <- proc$constant
  name_kernel <- proc$name
  
  ##Simulate a stationary FAR(1) process according to a specific kernel
  X.sample_spt <- far_1_1D(kernel_id = id_kernel, noise_id = id_noise, n = n, t.grid = t.grid, a = a, burnin = burnin)
}

# ----- data generation slopint plane s Kernel norm 0.5 -----
{
  #feats of data
  id_kernel <- "sp_s"       #way of generating data ("gaussian", "identity", "sp_t" and "sp_s" kernel available)
  norm      <- 0.5          #norm of the Kernel (that has to be <1)
  id_noise  <- "5"          #error of the FAR(1) process ("1", "2" or "3")
  
  #grid for the kernel for generating FAR(1)
  #s.grid <- seq(0,1, length.out=dim_grid)
  #grid   <- expand.grid(t.grid, s.grid)
  
  
  proc = feat_far_1_process(id_kernel,norm)
  id_kernel   <- proc$kernel
  a           <- proc$constant
  name_kernel <- proc$name
  
  ##Simulate a stationary FAR(1) process according to a specific kernel
  X.sample_sps <- far_1_1D(kernel_id = id_kernel, noise_id = id_noise, n = n, t.grid = t.grid, a = a, burnin = burnin)
}









string_message = "
                  PPC prediction data Gaussian Kernel, norm 0.5 "
for (b in (n-N-1):(n-1)) {
  
  train_set = X.sample_gau[,1:b]
  valid_set = X.sample_gau[,b+1] 
  
  PPC_predictor = PPC_KO( X = train_set,
                          id_CV = id_CV_ko,
                          disc_ev = t.grid,
                          left_extreme = left_extreme,
                          right_extreme = right_extreme,
                          err_ret = err_ret)
  
  prediction_PPC_gau_no_wn[[b]] = PPC_predictor
  err_PPC_gau_no_wn_en[b] = sqrt(MLmetrics::MSE(PPC_predictor$`One-step ahead prediction`,valid_set)) 
  err_PPC_gau_no_wn_rn[b] = MLmetrics::MAE(PPC_predictor$`One-step ahead prediction`,valid_set)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_PPC_gau_no_wn = list(Prediction = prediction_PPC_gau_no_wn, En = err_PPC_gau_no_wn_en, Rn = err_PPC_gau_no_wn_rn)
  file_saving = paste0(path_stor_res,file_saving_PPC_gau_no_wn)
  save(res_PPC_gau_no_wn, file = file_saving)
}



string_message = "
                  EX prediction data Gaussian Kernel, norm 0.5 "
for (b in (n-N-1):(n-1)) {
  
  train_set = X.sample_gau[,b]
  valid_set = X.sample_gau[,b+1] 
  
  exact_pred = innovation(id_kernel)
  EX_predictor = exact_pred(y=train_set, t.grid=t.grid, a=a)    
  
  prediction_EX_gau_no_wn[[b]] = EX_predictor
  err_EX_gau_no_wn_en[b] = sqrt(MLmetrics::MSE(EX_predictor,valid_set)) 
  err_EX_gau_no_wn_rn[b] = MLmetrics::MAE(EX_predictor,valid_set)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_EX_gau_no_wn = list(Prediction = prediction_EX_gau_no_wn, En = err_EX_gau_no_wn_en, Rn = err_EX_gau_no_wn_rn)
  file_saving = paste0(path_stor_res,file_saving_EX_gau_no_wn)
  save(res_EX_gau_no_wn, file = file_saving)
}









string_message = "
                  PPC prediction data identity Kernel, norm 0.5 "
for (b in (n-N-1):(n-1)) {
  
  train_set = X.sample_id[,1:b]
  valid_set = X.sample_id[,b+1] 
  
  PPC_predictor = PPC_KO( X = train_set,
                          id_CV = id_CV_ko,
                          disc_ev = t.grid,
                          left_extreme = left_extreme,
                          right_extreme = right_extreme,
                          err_ret = err_ret)
  
  prediction_PPC_id_no_wn[[b]] = PPC_predictor
  err_PPC_id_no_wn_en[b] = sqrt(MLmetrics::MSE(PPC_predictor$`One-step ahead prediction`,valid_set))
  err_PPC_id_no_wn_rn[b] = MLmetrics::MAE(PPC_predictor$`One-step ahead prediction`,valid_set)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_PPC_id_no_wn = list(Prediction = prediction_PPC_id_no_wn, En = err_PPC_id_no_wn_en, Rn = err_PPC_id_no_wn_rn)
  file_saving = paste0(path_stor_res,file_saving_PPC_id_no_wn)
  save(res_PPC_id_no_wn, file = file_saving)
}


string_message = "
                  EX prediction data Identity Kernel, norm 0.5 "
for (b in (n-N-1):(n-1)) {
  
  train_set = X.sample_id[,b]
  valid_set = X.sample_id[,b+1] 
  
  exact_pred = innovation(id_kernel)
  EX_predictor = exact_pred(y=train_set, t.grid=t.grid, a=a)    
  
  prediction_EX_id_no_wn[[b]] = EX_predictor
  err_EX_id_no_wn_en[b] = sqrt(MLmetrics::MSE(EX_predictor,valid_set)) 
  err_EX_id_no_wn_rn[b] = MLmetrics::MAE(EX_predictor,valid_set)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_EX_id_no_wn = list(Prediction = prediction_EX_id_no_wn, En = err_EX_id_no_wn_en, Rn = err_EX_id_no_wn_rn)
  file_saving = paste0(path_stor_res,file_saving_EX_id_no_wn)
  save(res_EX_id_no_wn, file = file_saving)
}











string_message = "
                  PPC prediction data sloping plane t Kernel, norm 1.5 "
for (b in (n-N-1):(n-1)) {
  
  train_set = X.sample_spt[,1:b]
  valid_set = X.sample_spt[,b+1] 
  
  PPC_predictor = PPC_KO( X = train_set,
                          id_CV = id_CV_ko,
                          disc_ev = t.grid,
                          left_extreme = left_extreme,
                          right_extreme = right_extreme,
                          err_ret = err_ret)
  
  prediction_PPC_spt_no_wn[[b]] = PPC_predictor
  err_PPC_spt_no_wn_en[b] = sqrt(MLmetrics::MSE(PPC_predictor$`One-step ahead prediction`,valid_set))
  err_PPC_spt_no_wn_rn[b] = MLmetrics::MAE(PPC_predictor$`One-step ahead prediction`,valid_set)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_PPC_spt_no_wn = list(Prediction = prediction_PPC_spt_no_wn, En = err_PPC_spt_no_wn_en, Rn = err_PPC_spt_no_wn_rn)
  file_saving = paste0(path_stor_res,file_saving_PPC_spt_no_wn)
  save(res_PPC_spt_no_wn, file = file_saving)
}


string_message = "
                  EX prediction data Sloping Plane t Kernel, norm 0.5 "
for (b in (n-N-1):(n-1)) {
  
  train_set = X.sample_spt[,b]
  valid_set = X.sample_spt[,b+1] 
  
  exact_pred = innovation(id_kernel)
  EX_predictor = exact_pred(y=train_set, t.grid=t.grid, a=a)    
  
  prediction_EX_spt_no_wn[[b]] = EX_predictor
  err_EX_spt_no_wn_en[b] = sqrt(MLmetrics::MSE(EX_predictor,valid_set)) 
  err_EX_spt_no_wn_rn[b] = MLmetrics::MAE(EX_predictor,valid_set)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_EX_spt_no_wn = list(Prediction = prediction_EX_spt_no_wn, En = err_EX_spt_no_wn_en, Rn = err_EX_spt_no_wn_rn)
  file_saving = paste0(path_stor_res,file_saving_EX_spt_no_wn)
  save(res_EX_spt_no_wn, file = file_saving)
}










string_message = "
                  PPC prediction data sloping plane s Kernel, norm 0.5 "
for (b in (n-N-1):(n-1)) {
  
  train_set = X.sample_sps[,1:b]
  valid_set = X.sample_sps[,b+1] 
  
  PPC_predictor = PPC_KO( X = train_set,
                          id_CV = id_CV_ko,
                          disc_ev = t.grid,
                          left_extreme = left_extreme,
                          right_extreme = right_extreme,
                          err_ret = err_ret)
  
  prediction_PPC_sps_no_wn[[b]] = PPC_predictor
  err_PPC_sps_no_wn_en[b] = sqrt(MLmetrics::MSE(PPC_predictor$`One-step ahead prediction`,valid_set))
  err_PPC_sps_no_wn_rn[b] = MLmetrics::MAE(PPC_predictor$`One-step ahead prediction`,valid_set)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_PPC_sps_no_wn = list(Prediction = prediction_PPC_sps_no_wn, En = err_PPC_sps_no_wn_en, Rn = err_PPC_sps_no_wn_rn)
  file_saving = paste0(path_stor_res,file_saving_PPC_sps_no_wn)
  save(res_PPC_sps_no_wn, file = file_saving)
}




string_message = "
                  EX prediction data Sloping Plane s Kernel, norm 0.5 "
for (b in (n-N-1):(n-1)) {
  
  train_set = X.sample_sps[,b]
  valid_set = X.sample_sps[,b+1] 
  
  exact_pred = innovation(id_kernel)
  EX_predictor = exact_pred(y=train_set, t.grid=t.grid, a=a)    
  
  prediction_EX_sps_no_wn[[b]] = EX_predictor
  err_EX_sps_no_wn_en[b] = sqrt(MLmetrics::MSE(EX_predictor,valid_set)) 
  err_EX_sps_no_wn_rn[b] = MLmetrics::MAE(EX_predictor,valid_set)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_EX_sps_no_wn = list(Prediction = prediction_EX_sps_no_wn, En = err_EX_sps_no_wn_en, Rn = err_EX_sps_no_wn_rn)
  file_saving = paste0(path_stor_res,file_saving_EX_sps_no_wn)
  save(res_EX_sps_no_wn, file = file_saving)
}
