rm(list=ls())
graphics.off()
cat("\014")
set.seed(23032000)

###############################################################################
#### PPC robustness if non compact AR operator as indicated in the readme  ####
###############################################################################

library(PPCKO)

#put here the path of the local copy of the directory
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"

#if you want to save the result 
save_res = TRUE


#where to store the results
dir_stor_res = "/Test_PPC_robustness/no_boundedness/results/predictions/train_all_story"
dir_stor_res_mw = "/Test_PPC_robustness/no_boundedness/results/predictions/train_moving_window"
path_stor_res = paste0(dir_w,dir_stor_res)
path_stor_res_mw = paste0(dir_w,dir_stor_res_mw)



#file for saving results
{
  file_saving_PPC_gau_no_bound = "/prediction_PPC_gau_no_bound.Rdata"
  file_saving_PPC_id_no_bound  = "/prediction_PPC_id_no_bound.Rdata"
  file_saving_PPC_spt_no_bound = "/prediction_PPC_spt_no_bound.Rdata"
  file_saving_PPC_sps_no_bound = "/prediction_PPC_sps_no_bound.Rdata"
  file_saving_PPC_gau_no_bound_mw = "/prediction_PPC_gau_no_bound_mw.Rdata"
  file_saving_PPC_id_no_bound_mw  = "/prediction_PPC_id_no_bound_mw.Rdata"
  file_saving_PPC_spt_no_bound_mw = "/prediction_PPC_spt_no_bound_mw.Rdata"
  file_saving_PPC_sps_no_bound_mw = "/prediction_PPC_sps_no_bound_mw.Rdata"
  
  file_saving_EX_gau_no_bound = "/prediction_EX_gau_no_bound.Rdata"
  file_saving_EX_id_no_bound  = "/prediction_EX_id_no_bound.Rdata"
  file_saving_EX_spt_no_bound = "/prediction_EX_spt_no_bound.Rdata"
  file_saving_EX_sps_no_bound = "/prediction_EX_sps_no_bound.Rdata"
}

#uploading functions for generating data
source(paste0(dir_w,"/Test_PPC_robustness/no_boundedness/utils/far_1_1d.R"))         #load functions to generate the FAR(1) process
source(paste0(dir_w,"/Test_PPC_robustness/no_boundedness/utils/data_param.R"))       #load parameter to generate data according to a strategy

last_train_set_instant = (n-N):(n-1)
predicted_instant = last_train_set_instant+1
dim_mw = 50



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
  prediction_PPC_gau_no_bound <- lapply((1):N-1,function(x) NULL)
  prediction_PPC_id_no_bound  <- lapply((1):N-1,function(x) NULL)
  prediction_PPC_spt_no_bound <- lapply((1):N-1,function(x) NULL)
  prediction_PPC_sps_no_bound <- lapply((1):N-1,function(x) NULL)
  
  prediction_PPC_gau_no_bound_mw <- lapply((1):N-1,function(x) NULL)
  prediction_PPC_id_no_bound_mw  <- lapply((1):N-1,function(x) NULL)
  prediction_PPC_spt_no_bound_mw <- lapply((1):N-1,function(x) NULL)
  prediction_PPC_sps_no_bound_mw <- lapply((1):N-1,function(x) NULL)
  
  prediction_EX_gau_no_bound <- lapply((1):N-1,function(x) NULL)
  prediction_EX_id_no_bound  <- lapply((1):N-1,function(x) NULL)
  prediction_EX_spt_no_bound <- lapply((1):N-1,function(x) NULL)
  prediction_EX_sps_no_bound <- lapply((1):N-1,function(x) NULL)
}

#storing errors
{
  err_PPC_gau_no_bound_en <- numeric(N)     
  err_PPC_gau_no_bound_rn <- numeric(N)
  err_PPC_id_no_bound_en  <- numeric(N)     
  err_PPC_id_no_bound_rn  <- numeric(N)
  err_PPC_spt_no_bound_en <- numeric(N)     
  err_PPC_spt_no_bound_rn <- numeric(N)
  err_PPC_sps_no_bound_en <- numeric(N)     
  err_PPC_sps_no_bound_rn <- numeric(N)
  
  err_PPC_gau_no_bound_en_mw <- numeric(N)     
  err_PPC_gau_no_bound_rn_mw <- numeric(N)
  err_PPC_id_no_bound_en_mw  <- numeric(N)     
  err_PPC_id_no_bound_rn_mw  <- numeric(N)
  err_PPC_spt_no_bound_en_mw <- numeric(N)     
  err_PPC_spt_no_bound_rn_mw <- numeric(N)
  err_PPC_sps_no_bound_en_mw <- numeric(N)     
  err_PPC_sps_no_bound_rn_mw <- numeric(N)
  
  err_EX_gau_no_bound_en <- numeric(N)     
  err_EX_gau_no_bound_rn <- numeric(N)
  err_EX_id_no_bound_en  <- numeric(N)     
  err_EX_id_no_bound_rn  <- numeric(N)
  err_EX_spt_no_bound_en <- numeric(N)     
  err_EX_spt_no_bound_rn <- numeric(N)
  err_EX_sps_no_bound_en <- numeric(N)     
  err_EX_sps_no_bound_rn <- numeric(N)
}






# ----- data generation Gaussian Kernel norm 1.025 -----
{
  #feats of data
  id_kernel <- "gaussian"       #way of generating data ("gaussian", "identity", "sp_t" and "sp_s" kernel available)
  norm      <- 1.025          #norm of the Kernel (that has to be <1)
  id_noise  <- "1"          #error of the FAR(1) process ("1", "2" or "3")
  
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


# ----- data generation identity Kernel norm 1.015 -----
{
  #feats of data
  id_kernel <- "identity"       #way of generating data ("gaussian", "identity", "sp_t" and "sp_s" kernel available)
  #norm      <- 1.025          #norm of the Kernel (that has to be <1)
  norm      <- 1.02          #norm of the Kernel (that has to be <1)
  id_noise  <- "1"          #error of the FAR(1) process ("1", "2" or "3")
  
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

# ----- data generation slopint plane t Kernel norm 1.178 -----
{
  #feats of data
  id_kernel <- "sp_t"       #way of generating data ("gaussian", "identity", "sp_t" and "sp_s" kernel available)
  norm      <- 1.178          #norm of the Kernel (that has to be <1)
  id_noise  <- "1"          #error of the FAR(1) process ("1", "2" or "3")
  
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

# ----- data generation slopint plane s Kernel norm 1.178 -----
{
  #feats of data
  id_kernel <- "sp_s"       #way of generating data ("gaussian", "identity", "sp_t" and "sp_s" kernel available)
  norm      <- 1.178          #norm of the Kernel (that has to be <1)
  id_noise  <- "1"          #error of the FAR(1) process ("1", "2" or "3")
  
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





##----GA----
data = X.sample_gau
id_kernel = "gaussian"

string_message = "
                  PPC prediction data Gaussian Kernel, no bnd "
for (b in 1:N) {
  
  pred_inst = predicted_instant[b]
  first_inst_mw = pred_inst - dim_mw
  
  
  train_set_ppc_all_story = data[,1:(pred_inst-1)]
  train_set_ppc_mw = data[,first_inst_mw:(pred_inst-1)]
  train_set_ex = data[,(pred_inst-1)]
  
  valid_set = data[,pred_inst] 
  
  PPC_predictor = PPC_KO( X = train_set_ppc_all_story,
                          id_CV = id_CV_ko,
                          disc_ev = t.grid,
                          left_extreme = left_extreme,
                          right_extreme = right_extreme,
                          err_ret = err_ret)
  
  prediction_PPC_gau_no_bound[[b]] = PPC_predictor
  err_PPC_gau_no_bound_en[b] = sqrt(MLmetrics::MSE(PPC_predictor$`One-step ahead prediction`,valid_set)) 
  err_PPC_gau_no_bound_rn[b] = MLmetrics::MAE(PPC_predictor$`One-step ahead prediction`,valid_set)
  
  
  PPC_predictor_mw = PPC_KO( X = train_set_ppc_mw,
                             id_CV = id_CV_ko,
                             disc_ev = t.grid,
                             left_extreme = left_extreme,
                             right_extreme = right_extreme,
                             err_ret = err_ret)
  
  prediction_PPC_gau_no_bound_mw[[b]] = PPC_predictor_mw
  err_PPC_gau_no_bound_en_mw[b] = sqrt(MLmetrics::MSE(PPC_predictor_mw$`One-step ahead prediction`,valid_set)) 
  err_PPC_gau_no_bound_rn_mw[b] = MLmetrics::MAE(PPC_predictor_mw$`One-step ahead prediction`,valid_set)
  
  
  exact_pred = innovation(id_kernel)
  EX_predictor = exact_pred(y=train_set_ex, t.grid=t.grid, a=a) 
  
  prediction_EX_gau_no_bound[[b]] = EX_predictor
  err_EX_gau_no_bound_en[b] = sqrt(MLmetrics::MSE(EX_predictor,valid_set)) 
  err_EX_gau_no_bound_rn[b] = MLmetrics::MAE(EX_predictor,valid_set)
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_PPC_gau_no_bound = list(Prediction = prediction_PPC_gau_no_bound, En = err_PPC_gau_no_bound_en, Rn = err_PPC_gau_no_bound_rn)
  file_saving = paste0(path_stor_res,file_saving_PPC_gau_no_bound)
  save(res_PPC_gau_no_bound, file = file_saving)
  
  res_PPC_gau_no_bound_mw = list(Prediction = prediction_PPC_gau_no_bound_mw, En = err_PPC_gau_no_bound_en_mw, Rn = err_PPC_gau_no_bound_rn_mw)
  file_saving = paste0(path_stor_res_mw,file_saving_PPC_gau_no_bound_mw)
  save(res_PPC_gau_no_bound_mw, file = file_saving)
  
  res_EX_gau_no_bound = list(Prediction = prediction_EX_gau_no_bound, En = err_EX_gau_no_bound_en, Rn = err_EX_gau_no_bound_rn)
  file_saving = paste0(path_stor_res,file_saving_EX_gau_no_bound)
  save(res_EX_gau_no_bound, file = file_saving)
}




##----ID----
data = X.sample_id
id_kernel = "identity"

string_message = "
                  PPC prediction data Identity Kernel, no bnd "
for (b in 1:N) {
  
  pred_inst = predicted_instant[b]
  first_inst_mw = pred_inst - dim_mw
  
  
  train_set_ppc_all_story = data[,1:(pred_inst-1)]
  train_set_ppc_mw = data[,first_inst_mw:(pred_inst-1)]
  train_set_ex = data[,(pred_inst-1)]
  
  valid_set = data[,pred_inst] 
  
  PPC_predictor = PPC_KO( X = train_set_ppc_all_story,
                          id_CV = id_CV_ko,
                          disc_ev = t.grid,
                          left_extreme = left_extreme,
                          right_extreme = right_extreme,
                          err_ret = err_ret)
  
  prediction_PPC_id_no_bound[[b]] = PPC_predictor
  err_PPC_id_no_bound_en[b] = sqrt(MLmetrics::MSE(PPC_predictor$`One-step ahead prediction`,valid_set)) 
  err_PPC_id_no_bound_rn[b] = MLmetrics::MAE(PPC_predictor$`One-step ahead prediction`,valid_set)
  
  
  PPC_predictor_mw = PPC_KO( X = train_set_ppc_mw,
                             id_CV = id_CV_ko,
                             disc_ev = t.grid,
                             left_extreme = left_extreme,
                             right_extreme = right_extreme,
                             err_ret = err_ret)
  
  prediction_PPC_id_no_bound_mw[[b]] = PPC_predictor_mw
  err_PPC_id_no_bound_en_mw[b] = sqrt(MLmetrics::MSE(PPC_predictor_mw$`One-step ahead prediction`,valid_set)) 
  err_PPC_id_no_bound_rn_mw[b] = MLmetrics::MAE(PPC_predictor_mw$`One-step ahead prediction`,valid_set)
  
  
  exact_pred = innovation(id_kernel)
  EX_predictor = exact_pred(y=train_set_ex, t.grid=t.grid, a=a) 
  
  prediction_EX_id_no_bound[[b]] = EX_predictor
  err_EX_id_no_bound_en[b] = sqrt(MLmetrics::MSE(EX_predictor,valid_set)) 
  err_EX_id_no_bound_rn[b] = MLmetrics::MAE(EX_predictor,valid_set)
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_PPC_id_no_bound = list(Prediction = prediction_PPC_id_no_bound, En = err_PPC_id_no_bound_en, Rn = err_PPC_id_no_bound_rn)
  file_saving = paste0(path_stor_res,file_saving_PPC_id_no_bound)
  save(res_PPC_id_no_bound, file = file_saving)
  
  res_PPC_id_no_bound_mw = list(Prediction = prediction_PPC_id_no_bound_mw, En = err_PPC_id_no_bound_en_mw, Rn = err_PPC_id_no_bound_rn_mw)
  file_saving = paste0(path_stor_res_mw,file_saving_PPC_id_no_bound_mw)
  save(res_PPC_id_no_bound_mw, file = file_saving)
  
  res_EX_id_no_bound = list(Prediction = prediction_EX_id_no_bound, En = err_EX_id_no_bound_en, Rn = err_EX_id_no_bound_rn)
  file_saving = paste0(path_stor_res,file_saving_EX_id_no_bound)
  save(res_EX_id_no_bound, file = file_saving)
}




##----spt----
data = X.sample_spt
id_kernel = "sp_t"

string_message = "
                  PPC prediction data Sloping plane t Kernel, no bnd "
for (b in 1:N) {
  
  pred_inst = predicted_instant[b]
  first_inst_mw = pred_inst - dim_mw
  
  
  train_set_ppc_all_story = data[,1:(pred_inst-1)]
  train_set_ppc_mw = data[,first_inst_mw:(pred_inst-1)]
  train_set_ex = data[,(pred_inst-1)]
  
  valid_set = data[,pred_inst] 
  
  PPC_predictor = PPC_KO( X = train_set_ppc_all_story,
                          id_CV = id_CV_ko,
                          disc_ev = t.grid,
                          left_extreme = left_extreme,
                          right_extreme = right_extreme,
                          err_ret = err_ret)
  
  prediction_PPC_spt_no_bound[[b]] = PPC_predictor
  err_PPC_spt_no_bound_en[b] = sqrt(MLmetrics::MSE(PPC_predictor$`One-step ahead prediction`,valid_set)) 
  err_PPC_spt_no_bound_rn[b] = MLmetrics::MAE(PPC_predictor$`One-step ahead prediction`,valid_set)
  
  
  PPC_predictor_mw = PPC_KO( X = train_set_ppc_mw,
                             id_CV = id_CV_ko,
                             disc_ev = t.grid,
                             left_extreme = left_extreme,
                             right_extreme = right_extreme,
                             err_ret = err_ret)
  
  prediction_PPC_spt_no_bound_mw[[b]] = PPC_predictor_mw
  err_PPC_spt_no_bound_en_mw[b] = sqrt(MLmetrics::MSE(PPC_predictor_mw$`One-step ahead prediction`,valid_set)) 
  err_PPC_spt_no_bound_rn_mw[b] = MLmetrics::MAE(PPC_predictor_mw$`One-step ahead prediction`,valid_set)
  
  
  exact_pred = innovation(id_kernel)
  EX_predictor = exact_pred(y=train_set_ex, t.grid=t.grid, a=a) 
  
  prediction_EX_spt_no_bound[[b]] = EX_predictor
  err_EX_spt_no_bound_en[b] = sqrt(MLmetrics::MSE(EX_predictor,valid_set)) 
  err_EX_spt_no_bound_rn[b] = MLmetrics::MAE(EX_predictor,valid_set)
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_PPC_spt_no_bound = list(Prediction = prediction_PPC_spt_no_bound, En = err_PPC_spt_no_bound_en, Rn = err_PPC_spt_no_bound_rn)
  file_saving = paste0(path_stor_res,file_saving_PPC_spt_no_bound)
  save(res_PPC_spt_no_bound, file = file_saving)
  
  res_PPC_spt_no_bound_mw = list(Prediction = prediction_PPC_spt_no_bound_mw, En = err_PPC_spt_no_bound_en_mw, Rn = err_PPC_spt_no_bound_rn_mw)
  file_saving = paste0(path_stor_res_mw,file_saving_PPC_spt_no_bound_mw)
  save(res_PPC_spt_no_bound_mw, file = file_saving)
  
  res_EX_spt_no_bound = list(Prediction = prediction_EX_spt_no_bound, En = err_EX_spt_no_bound_en, Rn = err_EX_spt_no_bound_rn)
  file_saving = paste0(path_stor_res,file_saving_EX_spt_no_bound)
  save(res_EX_spt_no_bound, file = file_saving)
}




##----sps----
data = X.sample_sps
id_kernel = "sp_s"

string_message = "
                  PPC prediction data Sloping plane s Kernel, no bnd "
for (b in 1:N) {
  
  pred_inst = predicted_instant[b]
  first_inst_mw = pred_inst - dim_mw
  
  
  train_set_ppc_all_story = data[,1:(pred_inst-1)]
  train_set_ppc_mw = data[,first_inst_mw:(pred_inst-1)]
  train_set_ex = data[,(pred_inst-1)]
  
  valid_set = data[,pred_inst] 
  
  PPC_predictor = PPC_KO( X = train_set_ppc_all_story,
                          id_CV = id_CV_ko,
                          disc_ev = t.grid,
                          left_extreme = left_extreme,
                          right_extreme = right_extreme,
                          err_ret = err_ret)
  
  prediction_PPC_sps_no_bound[[b]] = PPC_predictor
  err_PPC_sps_no_bound_en[b] = sqrt(MLmetrics::MSE(PPC_predictor$`One-step ahead prediction`,valid_set)) 
  err_PPC_sps_no_bound_rn[b] = MLmetrics::MAE(PPC_predictor$`One-step ahead prediction`,valid_set)
  
  
  PPC_predictor_mw = PPC_KO( X = train_set_ppc_mw,
                             id_CV = id_CV_ko,
                             disc_ev = t.grid,
                             left_extreme = left_extreme,
                             right_extreme = right_extreme,
                             err_ret = err_ret)
  
  prediction_PPC_sps_no_bound_mw[[b]] = PPC_predictor_mw
  err_PPC_sps_no_bound_en_mw[b] = sqrt(MLmetrics::MSE(PPC_predictor_mw$`One-step ahead prediction`,valid_set)) 
  err_PPC_sps_no_bound_rn_mw[b] = MLmetrics::MAE(PPC_predictor_mw$`One-step ahead prediction`,valid_set)
  
  
  exact_pred = innovation(id_kernel)
  EX_predictor = exact_pred(y=train_set_ex, t.grid=t.grid, a=a) 
  
  prediction_EX_sps_no_bound[[b]] = EX_predictor
  err_EX_sps_no_bound_en[b] = sqrt(MLmetrics::MSE(EX_predictor,valid_set)) 
  err_EX_sps_no_bound_rn[b] = MLmetrics::MAE(EX_predictor,valid_set)
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_PPC_sps_no_bound = list(Prediction = prediction_PPC_sps_no_bound, En = err_PPC_sps_no_bound_en, Rn = err_PPC_sps_no_bound_rn)
  file_saving = paste0(path_stor_res,file_saving_PPC_sps_no_bound)
  save(res_PPC_sps_no_bound, file = file_saving)
  
  res_PPC_sps_no_bound_mw = list(Prediction = prediction_PPC_sps_no_bound_mw, En = err_PPC_sps_no_bound_en_mw, Rn = err_PPC_sps_no_bound_rn_mw)
  file_saving = paste0(path_stor_res_mw,file_saving_PPC_sps_no_bound_mw)
  save(res_PPC_sps_no_bound_mw, file = file_saving)
  
  res_EX_sps_no_bound = list(Prediction = prediction_EX_sps_no_bound, En = err_EX_sps_no_bound_en, Rn = err_EX_sps_no_bound_rn)
  file_saving = paste0(path_stor_res,file_saving_EX_sps_no_bound)
  save(res_EX_sps_no_bound, file = file_saving)
}






















##-----smaller moving window-----



dim_mw = 10






##----GA----
data = X.sample_gau
id_kernel = "gaussian"

string_message = "
                  PPC prediction data Gaussian Kernel, no bnd, smaller window "
for (b in 1:N) {
  
  pred_inst = predicted_instant[b]
  first_inst_mw = pred_inst - dim_mw

  train_set_ppc_mw = data[,first_inst_mw:(pred_inst-1)]
  
  valid_set = data[,pred_inst] 
  
  
  
  PPC_predictor_mw = PPC_KO( X = train_set_ppc_mw,
                             id_CV = id_CV_ko,
                             disc_ev = t.grid,
                             left_extreme = left_extreme,
                             right_extreme = right_extreme,
                             err_ret = err_ret)
  
  prediction_PPC_gau_no_bound_mw[[b]] = PPC_predictor_mw
  err_PPC_gau_no_bound_en_mw[b] = sqrt(MLmetrics::MSE(PPC_predictor_mw$`One-step ahead prediction`,valid_set)) 
  err_PPC_gau_no_bound_rn_mw[b] = MLmetrics::MAE(PPC_predictor_mw$`One-step ahead prediction`,valid_set)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){

  res_PPC_gau_no_bound_small_mw = list(Prediction = prediction_PPC_gau_no_bound_mw, En = err_PPC_gau_no_bound_en_mw, Rn = err_PPC_gau_no_bound_rn_mw)
  file_saving = paste0(path_stor_res_mw,file_saving_PPC_gau_no_bound_mw)
  save(res_PPC_gau_no_bound_small_mw, file = file_saving)
}




##----ID----
data = X.sample_id
id_kernel = "identity"

string_message = "
                  PPC prediction data Identity Kernel, norm 0.5, no bnd smaller window "
for (b in 1:N) {
  
  pred_inst = predicted_instant[b]
  first_inst_mw = pred_inst - dim_mw

  train_set_ppc_mw = data[,first_inst_mw:(pred_inst-1)]
  
  valid_set = data[,pred_inst] 

  
  PPC_predictor_mw = PPC_KO( X = train_set_ppc_mw,
                             id_CV = id_CV_ko,
                             disc_ev = t.grid,
                             left_extreme = left_extreme,
                             right_extreme = right_extreme,
                             err_ret = err_ret)
  
  prediction_PPC_id_no_bound_mw[[b]] = PPC_predictor_mw
  err_PPC_id_no_bound_en_mw[b] = sqrt(MLmetrics::MSE(PPC_predictor_mw$`One-step ahead prediction`,valid_set)) 
  err_PPC_id_no_bound_rn_mw[b] = MLmetrics::MAE(PPC_predictor_mw$`One-step ahead prediction`,valid_set)

  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){

  res_PPC_id_no_bound_small_mw = list(Prediction = prediction_PPC_id_no_bound_mw, En = err_PPC_id_no_bound_en_mw, Rn = err_PPC_id_no_bound_rn_mw)
  file_saving = paste0(path_stor_res_mw,file_saving_PPC_id_no_bound_mw)
  save(res_PPC_id_no_bound_small_mw, file = file_saving)
}




##----spt----
data = X.sample_spt
id_kernel = "sp_t"

string_message = "
                  PPC prediction data Sloping plane t Kernel, no bnd, smaller window "
for (b in 1:N) {
  
  pred_inst = predicted_instant[b]
  first_inst_mw = pred_inst - dim_mw
  
  train_set_ppc_mw = data[,first_inst_mw:(pred_inst-1)]
  
  valid_set = data[,pred_inst] 
  
  
  PPC_predictor_mw = PPC_KO( X = train_set_ppc_mw,
                             id_CV = id_CV_ko,
                             disc_ev = t.grid,
                             left_extreme = left_extreme,
                             right_extreme = right_extreme,
                             err_ret = err_ret)
  
  prediction_PPC_spt_no_bound_mw[[b]] = PPC_predictor_mw
  err_PPC_spt_no_bound_en_mw[b] = sqrt(MLmetrics::MSE(PPC_predictor_mw$`One-step ahead prediction`,valid_set)) 
  err_PPC_spt_no_bound_rn_mw[b] = MLmetrics::MAE(PPC_predictor_mw$`One-step ahead prediction`,valid_set)

  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){

  res_PPC_spt_no_bound_smaller_mw = list(Prediction = prediction_PPC_spt_no_bound_mw, En = err_PPC_spt_no_bound_en_mw, Rn = err_PPC_spt_no_bound_rn_mw)
  file_saving = paste0(path_stor_res_mw,file_saving_PPC_spt_no_bound_mw)
  save(res_PPC_spt_no_bound_smaller_mw, file = file_saving)

}




##----sps----
data = X.sample_sps
id_kernel = "sp_s"

string_message = "
                  PPC prediction data Sloping plane s Kernel, no bnd, smaller window "
for (b in 1:N) {
  
  pred_inst = predicted_instant[b]
  first_inst_mw = pred_inst - dim_mw

  train_set_ppc_mw = data[,first_inst_mw:(pred_inst-1)]
  
  valid_set = data[,pred_inst] 

  
  PPC_predictor_mw = PPC_KO( X = train_set_ppc_mw,
                             id_CV = id_CV_ko,
                             disc_ev = t.grid,
                             left_extreme = left_extreme,
                             right_extreme = right_extreme,
                             err_ret = err_ret)
  
  prediction_PPC_sps_no_bound_mw[[b]] = PPC_predictor_mw
  err_PPC_sps_no_bound_en_mw[b] = sqrt(MLmetrics::MSE(PPC_predictor_mw$`One-step ahead prediction`,valid_set)) 
  err_PPC_sps_no_bound_rn_mw[b] = MLmetrics::MAE(PPC_predictor_mw$`One-step ahead prediction`,valid_set)

  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){

  res_PPC_sps_no_bound_small_mw = list(Prediction = prediction_PPC_sps_no_bound_mw, En = err_PPC_sps_no_bound_en_mw, Rn = err_PPC_sps_no_bound_rn_mw)
  file_saving = paste0(path_stor_res_mw,file_saving_PPC_sps_no_bound_mw)
  save(res_PPC_sps_no_bound_mw, file = file_saving)

}