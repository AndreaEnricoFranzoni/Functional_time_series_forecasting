rm(list=ls())
graphics.off()
cat("\014")
set.seed(23032000)


#########################################################################
#### Computing PPC gep_solver prediction as indicated in the readme  ####
#########################################################################

library(PPCKO)

#put here the path of the local copy of the directory
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"
#if you want to save the result 
save_res = TRUE


#where to store the results
dir_stor_res = "/Test_domain1D/Artificial_data/results/results_prediction"
name_folder_res = "/PPC_gen"
path_stor_res = paste0(paste0(dir_w,dir_stor_res),name_folder_res)  

#file for saving results
{
  file_saving_PPC_gen_gau_0_5 = "/prediction_PPC_gen_gau_0_5.Rdata"
  file_saving_PPC_gen_gau_0_8 = "/prediction_PPC_gen_gau_0_8.Rdata"
  file_saving_PPC_gen_id_0_5  = "/prediction_PPC_gen_id_0_5.Rdata"
  file_saving_PPC_gen_id_0_8  = "/prediction_PPC_gen_id_0_8.Rdata"
  file_saving_PPC_gen_spt_0_5 = "/prediction_PPC_gen_spt_0_5.Rdata"
  file_saving_PPC_gen_spt_0_8 = "/prediction_PPC_gen_spt_0_8.Rdata"
  file_saving_PPC_gen_sps_0_5 = "/prediction_PPC_gen_sps_0_5.Rdata"
  file_saving_PPC_gen_sps_0_8 = "/prediction_PPC_gen_sps_0_8.Rdata"
}


source(paste0(dir_w,"/Test_domain1D/Artificial_data/utils/far_1_1d.R"))         #load functions to generate the FAR(1) process
source(paste0(dir_w,"/Test_domain1D/Artificial_data/utils/prediction_error.R")) #load functions to evaluate the prediction error
source(paste0(dir_w,"/Test_domain1D/Artificial_data/utils/data_param.R"))       #load parameter to generate data according to a strategy


first_instant_train_set = burnin
last_instant_train_set  = n-1
dim_train_set = first_instant_train_set:last_instant_train_set

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
  err_ret       <- FALSE          #if CV errors have to be retained
  ex_slv        <- FALSE
  id_rem_nan    <- NULL
}


#storing prediction
{
  prediction_PPC_gen_gau_0_5 <- lapply((1):N-1,function(x) NULL)
  prediction_PPC_gen_gau_0_8 <- lapply((1):N-1,function(x) NULL)
  prediction_PPC_gen_id_0_5  <- lapply((1):N-1,function(x) NULL)
  prediction_PPC_gen_id_0_8  <- lapply((1):N-1,function(x) NULL)
  prediction_PPC_gen_spt_0_5 <- lapply((1):N-1,function(x) NULL)
  prediction_PPC_gen_spt_0_8 <- lapply((1):N-1,function(x) NULL)
  prediction_PPC_gen_sps_0_5 <- lapply((1):N-1,function(x) NULL)
  prediction_PPC_gen_sps_0_8 <- lapply((1):N-1,function(x) NULL)
}

#storing errors
{
  err_PPC_gen_gau_0_5_en <- numeric(N)     
  err_PPC_gen_gau_0_5_rn <- numeric(N)
  err_PPC_gen_id_0_5_en  <- numeric(N)     
  err_PPC_gen_id_0_5_rn  <- numeric(N)
  err_PPC_gen_spt_0_5_en <- numeric(N)     
  err_PPC_gen_spt_0_5_rn <- numeric(N)
  err_PPC_gen_sps_0_5_en <- numeric(N)     
  err_PPC_gen_sps_0_5_rn <- numeric(N)
  err_PPC_gen_gau_0_8_en <- numeric(N)     
  err_PPC_gen_gau_0_8_rn <- numeric(N)
  err_PPC_gen_id_0_8_en  <- numeric(N)     
  err_PPC_gen_id_0_8_rn  <- numeric(N)
  err_PPC_gen_spt_0_8_en <- numeric(N)     
  err_PPC_gen_spt_0_8_rn <- numeric(N)
  err_PPC_gen_sps_0_8_en <- numeric(N)     
  err_PPC_gen_sps_0_8_rn <- numeric(N)
}






# ----- data generation Gaussian Kernel norm 0.5 -----
{
  #feats of data
  id_kernel <- "gaussian"       #way of generating data ("gaussian", "identity", "sp_t" and "sp_s" kernel available)
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

counter = 1
string_message = "
                  PPC gen prediction data Gaussian Kernel, norm 0.5 "
for (b in dim_train_set) {
  
  train_set = X.sample[,1:b]
  valid_set = X.sample[,b+1] 
  
  PPC_predictor = PPC_KO( X = train_set,
                          id_CV = id_CV_ko,
                          disc_ev = t.grid,
                          left_extreme = left_extreme,
                          right_extreme = right_extreme,
                          ex_solver = ex_slv)
  
  prediction_PPC_gen_gau_0_5[[counter]] = list(Prediction = PPC_predictor$`One-step ahead prediction`, Alpha = PPC_predictor$Alpha, N_PPCs = PPC_predictor$`Number of PPCs retained`, Exp_Pow = PPC_predictor$`Explanatory power PPCs` )
  err_PPC_gen_gau_0_5_en[counter] = En(PPC_predictor$`One-step ahead prediction`,valid_set,t.grid)
  err_PPC_gen_gau_0_5_rn[counter] = Rn(PPC_predictor$`One-step ahead prediction`,valid_set,t.grid)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , counter, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), counter)
  cat("\r", message)
}

#save results
if(save_res){
  res_PPC_gen_gau_0_5 = list(Prediction = prediction_PPC_gen_gau_0_5, En = err_PPC_gen_gau_0_5_en, Rn = err_PPC_gen_gau_0_5_rn)
  file_saving = paste0(path_stor_res,file_saving_PPC_gen_gau_0_5)
  save(res_PPC_gen_gau_0_5, file = file_saving)
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

counter = 1
string_message = "
                  PPC gen prediction data Gaussian Kernel, norm 0.8 "
for (b in dim_train_set) {
  
  train_set = X.sample[,1:b]
  valid_set = X.sample[,b+1] 
  
  PPC_predictor = PPC_KO( X = train_set,
                          id_CV = id_CV_ko,
                          disc_ev = t.grid,
                          left_extreme = left_extreme,
                          right_extreme = right_extreme,
                          ex_solver = ex_slv)
  
  prediction_PPC_gen_gau_0_8[[counter]] = list(Prediction = PPC_predictor$`One-step ahead prediction`, Alpha = PPC_predictor$Alpha, N_PPCs = PPC_predictor$`Number of PPCs retained`, Exp_Pow = PPC_predictor$`Explanatory power PPCs`  )
  err_PPC_gen_gau_0_8_en[counter] = En(PPC_predictor$`One-step ahead prediction`,valid_set,t.grid)
  err_PPC_gen_gau_0_8_rn[counter] = Rn(PPC_predictor$`One-step ahead prediction`,valid_set,t.grid)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , counter, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), counter)
  cat("\r", message)
}

#save results
if(save_res){
  res_PPC_gen_gau_0_8 = list(Prediction = prediction_PPC_gen_gau_0_8, En = err_PPC_gen_gau_0_8_en, Rn = err_PPC_gen_gau_0_8_rn)
  file_saving = paste0(path_stor_res,file_saving_PPC_gen_gau_0_8)
  save(res_PPC_gen_gau_0_8, file = file_saving)
}




set.seed(29011999)
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

counter = 1
string_message = "
                  PPC gen prediction data identity Kernel, norm 0.5 "
for (b in 1:N) {
  
  train_set = X.sample[,1:b]
  valid_set = X.sample[,b+1] 
  
  PPC_predictor = PPC_KO( X = train_set,
                          id_CV = id_CV_ko,
                          disc_ev = t.grid,
                          left_extreme = left_extreme,
                          right_extreme = right_extreme,
                          ex_solver = ex_slv)
  
  prediction_PPC_gen_id_0_5[[counter]] = list(Prediction = PPC_predictor$`One-step ahead prediction`, Alpha = PPC_predictor$Alpha, N_PPCs = PPC_predictor$`Number of PPCs retained`, Exp_Pow = PPC_predictor$`Explanatory power PPCs`  )
  err_PPC_gen_id_0_5_en[counter] = En(PPC_predictor$`One-step ahead prediction`,valid_set,t.grid)
  err_PPC_gen_id_0_5_rn[counter] = Rn(PPC_predictor$`One-step ahead prediction`,valid_set,t.grid)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , counter, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), counter)
  cat("\r", message)
}

#save results
if(save_res){
  res_PPC_gen_id_0_5 = list(Prediction = prediction_PPC_gen_id_0_5, En = err_PPC_gen_id_0_5_en, Rn = err_PPC_gen_id_0_5_rn)
  file_saving = paste0(path_stor_res,file_saving_PPC_gen_id_0_5)
  save(res_PPC_gen_id_0_5, file = file_saving)
}




set.seed(23032000)
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

counter = 1
string_message = "
                  PPC gen prediction data identity Kernel, norm 0.8 "
for (b in dim_train_set) {
  
  train_set = X.sample[,1:b]
  valid_set = X.sample[,b+1] 
  
  PPC_predictor = PPC_KO( X = train_set,
                          id_CV = id_CV_ko,
                          disc_ev = t.grid,
                          left_extreme = left_extreme,
                          right_extreme = right_extreme,
                          ex_solver = ex_slv)
  
  prediction_PPC_gen_id_0_8[[counter]] = list(Prediction = PPC_predictor$`One-step ahead prediction`, Alpha = PPC_predictor$Alpha, N_PPCs = PPC_predictor$`Number of PPCs retained`, Exp_Pow = PPC_predictor$`Explanatory power PPCs`  )
  err_PPC_gen_id_0_8_en[counter] = En(PPC_predictor$`One-step ahead prediction`,valid_set,t.grid)
  err_PPC_gen_id_0_8_rn[counter] = Rn(PPC_predictor$`One-step ahead prediction`,valid_set,t.grid)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , counter, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), counter)
  cat("\r", message)
}

#save results
if(save_res){
  res_PPC_gen_id_0_8 = list(Prediction = prediction_PPC_gen_id_0_8, En = err_PPC_gen_id_0_8_en, Rn = err_PPC_gen_id_0_8_rn)
  file_saving = paste0(path_stor_res,file_saving_PPC_gen_id_0_8)
  save(res_PPC_gen_id_0_8, file = file_saving)
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

counter = 1
string_message = "
                  PPC gen prediction data sloping plane t Kernel, norm 0.5 "
for (b in dim_train_set) {
  
  train_set = X.sample[,1:b]
  valid_set = X.sample[,b+1] 
  
  PPC_predictor = PPC_KO( X = train_set,
                          id_CV = id_CV_ko,
                          disc_ev = t.grid,
                          left_extreme = left_extreme,
                          right_extreme = right_extreme,
                          ex_solver = ex_slv)
  
  prediction_PPC_gen_spt_0_5[[counter]] = list(Prediction = PPC_predictor$`One-step ahead prediction`, Alpha = PPC_predictor$Alpha, N_PPCs = PPC_predictor$`Number of PPCs retained`, Exp_Pow = PPC_predictor$`Explanatory power PPCs`  )
  err_PPC_gen_spt_0_5_en[counter] = En(PPC_predictor$`One-step ahead prediction`,valid_set,t.grid)
  err_PPC_gen_spt_0_5_rn[counter] = Rn(PPC_predictor$`One-step ahead prediction`,valid_set,t.grid)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , counter, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), counter)
  cat("\r", message)
}

#save results
if(save_res){
  res_PPC_gen_spt_0_5 = list(Prediction = prediction_PPC_gen_spt_0_5, En = err_PPC_gen_spt_0_5_en, Rn = err_PPC_gen_spt_0_5_rn)
  file_saving = paste0(path_stor_res,file_saving_PPC_gen_spt_0_5)
  save(res_PPC_gen_spt_0_5, file = file_saving)
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
                  PPC gen prediction data sloping plane t Kernel, norm 0.8 "
for (b in dim_train_set) {
  
  train_set = X.sample[,1:b]
  valid_set = X.sample[,b+1] 
  
  PPC_predictor = PPC_KO( X = train_set,
                          id_CV = id_CV_ko,
                          disc_ev = t.grid,
                          left_extreme = left_extreme,
                          right_extreme = right_extreme,
                          ex_solver = ex_slv)
  
  prediction_PPC_gen_spt_0_8[[counter]] = list(Prediction = PPC_predictor$`One-step ahead prediction`, Alpha = PPC_predictor$Alpha, N_PPCs = PPC_predictor$`Number of PPCs retained`, Exp_Pow = PPC_predictor$`Explanatory power PPCs`  )
  err_PPC_gen_spt_0_8_en[counter] = En(PPC_predictor$`One-step ahead prediction`,valid_set,t.grid)
  err_PPC_gen_spt_0_8_rn[counter] = Rn(PPC_predictor$`One-step ahead prediction`,valid_set,t.grid)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , counter, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), counter)
  cat("\r", message)
}

#save results
if(save_res){
  res_PPC_gen_spt_0_8 = list(Prediction = prediction_PPC_gen_spt_0_8, En = err_PPC_gen_spt_0_8_en, Rn = err_PPC_gen_spt_0_8_rn)
  file_saving = paste0(path_stor_res,file_saving_PPC_gen_spt_0_8)
  save(res_PPC_gen_spt_0_8, file = file_saving)
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

counter = 1
string_message = "
                  PPC gen prediction data sloping plane s Kernel, norm 0.5 "
for (b in dim_train_set) {
  
  train_set = X.sample[,1:b]
  valid_set = X.sample[,b+1] 
  
  PPC_predictor = PPC_KO( X = train_set,
                          id_CV = id_CV_ko,
                          disc_ev = t.grid,
                          left_extreme = left_extreme,
                          right_extreme = right_extreme,
                          ex_solver = ex_slv)
  
  prediction_PPC_gen_sps_0_5[[counter]] = list(Prediction = PPC_predictor$`One-step ahead prediction`, Alpha = PPC_predictor$Alpha, N_PPCs = PPC_predictor$`Number of PPCs retained`, Exp_Pow = PPC_predictor$`Explanatory power PPCs`  )
  err_PPC_gen_sps_0_5_en[counter] = En(PPC_predictor$`One-step ahead prediction`,valid_set,t.grid)
  err_PPC_gen_sps_0_5_rn[counter] = Rn(PPC_predictor$`One-step ahead prediction`,valid_set,t.grid)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , counter, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), counter)
  cat("\r", message)
}

#save results
if(save_res){
  res_PPC_gen_sps_0_5 = list(Prediction = prediction_PPC_gen_sps_0_5, En = err_PPC_gen_sps_0_5_en, Rn = err_PPC_gen_sps_0_5_rn)
  file_saving = paste0(path_stor_res,file_saving_PPC_gen_sps_0_5)
  save(res_PPC_gen_sps_0_5, file = file_saving)
}





# ----- data generation slopint plane s Kernel norm 0.8 -----
{
  #feats of data
  id_kernel <- "sp_s"       #way of generating data ("gaussian", "identity", "sp_t" and "sp_s" kernel available)
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

counter = 1
string_message = "
                  PPC gen prediction data sloping plane s Kernel, norm 0.8 "
for (b in dim_train_set) {
  
  train_set = X.sample[,1:b]
  valid_set = X.sample[,b+1] 
  
  PPC_predictor = PPC_KO( X = train_set,
                          id_CV = id_CV_ko,
                          disc_ev = t.grid,
                          left_extreme = left_extreme,
                          right_extreme = right_extreme,
                          ex_solver = ex_slv)
  
  prediction_PPC_gen_sps_0_8[[counter]] = list(Prediction = PPC_predictor$`One-step ahead prediction`, Alpha = PPC_predictor$Alpha, N_PPCs = PPC_predictor$`Number of PPCs retained`, Exp_Pow = PPC_predictor$`Explanatory power PPCs`  )
  err_PPC_gen_sps_0_8_en[counter] = En(PPC_predictor$`One-step ahead prediction`,valid_set,t.grid)
  err_PPC_gen_sps_0_8_rn[counter] = Rn(PPC_predictor$`One-step ahead prediction`,valid_set,t.grid)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , counter, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), counter)
  cat("\r", message)
}

#save results
if(save_res){
  res_PPC_gen_sps_0_8 = list(Prediction = prediction_PPC_gen_sps_0_8, En = err_PPC_gen_sps_0_8_en, Rn = err_PPC_gen_sps_0_8_rn)
  file_saving = paste0(path_stor_res,file_saving_PPC_gen_sps_0_8)
  save(res_PPC_gen_sps_0_8, file = file_saving)
}