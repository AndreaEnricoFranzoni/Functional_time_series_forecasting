rm(list=ls())
graphics.off()
cat("\014")
set.seed(23032000)

library(PPCKO)

#change here
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"
dir_stor_res = "/Test_domain1D/Artificial_data/results/results_valid_err_alpha"
name_folder_res = "/PPC"


#if you want to save the result 
save_res = TRUE
#where to store the results
path_stor_res = dir_stor_res            
#saving results
{
  file_saving_cv_alpha_gau_0_5 = "/valid_err_alpha_cv_alpha_PPC_gau_0_5.Rdata"
  file_saving_cv_alpha_gau_0_8 = "/valid_err_alpha_cv_alpha_PPC_gau_0_8.Rdata"
  file_saving_cv_alpha_id_0_5  = "/valid_err_alpha_cv_alpha_PPC_id_0_5.Rdata"
  file_saving_cv_alpha_id_0_8  = "/valid_err_alpha_cv_alpha_PPC_id_0_8.Rdata"
  file_saving_cv_alpha_spt_0_5 = "/valid_err_alpha_cv_alpha_PPC_spt_0_5.Rdata"
  file_saving_cv_alpha_spt_0_8 = "/valid_err_alpha_cv_alpha_PPC_spt_0_8.Rdata"
  file_saving_cv_alpha_sps_0_5 = "/valid_err_alpha_cv_alpha_PPC_sps_0_5.Rdata"
  file_saving_cv_alpha_sps_0_8 = "/valid_err_alpha_cv_alpha_PPC_sps_0_8.Rdata"
  
  file_saving_cv_gau_0_5 = "/valid_err_alpha_cv_PPC_gau_0_5.Rdata"
  file_saving_cv_gau_0_8 = "/valid_err_alpha_cv_PPC_gau_0_8.Rdata"
  file_saving_cv_id_0_5  = "/valid_err_alpha_cv_PPC_id_0_5.Rdata"
  file_saving_cv_id_0_8  = "/valid_err_alpha_cv_PPC_id_0_8.Rdata"
  file_saving_cv_spt_0_5 = "/valid_err_alpha_cv_PPC_spt_0_5.Rdata"
  file_saving_cv_spt_0_8 = "/valid_err_alpha_cv_PPC_spt_0_8.Rdata"
  file_saving_cv_sps_0_5 = "/valid_err_alpha_cv_PPC_sps_0_5.Rdata"
  file_saving_cv_sps_0_8 = "/valid_err_alpha_cv_PPC_sps_0_8.Rdata"
}


source(paste0(dir_w,"/Test_domain1D/Artificial_data/utils/far_1_1d.R"))         #load functions to generate the FAR(1) process
source(paste0(dir_w,"/Test_domain1D/Artificial_data/utils/prediction_error.R")) #load functions to evaluate the prediction error
source(paste0(dir_w,"/Test_domain1D/Artificial_data/utils/data_param.R"))       #load parameter to generate data according to a strategy
path_stor_res = paste0(dir_w,dir_stor_res)              #where to store the results


#PPCKO parameters 
{
  id_CV_ko      <- "CV"           #Ko algorithm
  alpha         <- 0.1            #regularization parameter
  k             <- 0              #if a nnumber of PPCs has to be retained
  threshold_ppc <- 0.95           #threshold of explanatory power that has to be explained through PPCs
  alpha_vec     <- NULL           #alpha values for CV on regularization parameter
  len_alphas    <- 21             #default input space for alpha length
  k_vec         <- NULL           #k values for CV on number of PPCs
  toll          <- 1e-4           #toll for stopping adding others PPCs during CV
  disc_ev       <- t.grid         #discrete evaluations of the functional domain (has to be the same as t.grid)
  left_extreme  <- left_ex        #left extreme of the domain of the functional data (has to be the same as left_ex)
  right_extreme <- right_ex       #right extreme of the domain of the functional data (has to be the same as right_ex)
  err_ret       <- 0              #if CV errors have to be retained
  id_rem_nan    <- NULL
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
  
  #storing errors
  err_cv_alpha = matrix(data = 0,nrow = len_alphas,ncol = N)
  err_cv = matrix(data = 0,nrow = len_alphas,ncol = N)
}


string_message = "
                  PPC validation erros data Gaussian Kernel, norm 0.5 "
for (b in 1:N) {
  
  train_set = X.sample[,1:(N-1+b)]
  valid_set = X.sample[,N+b] 
  
  PPC_predictor_cv_alpha = PPC_KO( X = train_set,
                                   id_CV = "CV_alpha",
                                   threshold_ppc = 0.9,
                                   disc_ev = t.grid,
                                   left_extreme = left_extreme,
                                   right_extreme = right_extreme,
                                   err_ret = TRUE)
  
  err_cv_alpha[,b] = PPC_predictor_cv_alpha$`Validation errors`
  
  PPC_predictor_cv = PPC_KO( X = train_set,
                            id_CV = id_CV_ko,
                            disc_ev = t.grid,
                            left_extreme = left_extreme,
                            right_extreme = right_extreme,
                            err_ret = TRUE,
                            ex_solver = FALSE)
  
  for (i in 1:len_alphas) {
    err_cv[i,b] = PPC_predictor_cv$`Validation errors`[[i]][PPC_predictor_cv$`Number of PPCs retained`]
  }
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  valid_err_cv_alpha_PPC_gau_0_5 = err_cv_alpha
  valid_err_cv_PPC_gau_0_5 = err_cv
  file_saving_cv_alpha = paste0(paste0(path_stor_res,"/cv_alpha"),file_saving_cv_alpha_gau_0_5)
  file_saving_cv = paste0(paste0(path_stor_res,"/cv_both"),file_saving_cv_gau_0_5)
  save(valid_err_cv_alpha_PPC_gau_0_5, file = file_saving_cv_alpha)
  save(valid_err_cv_PPC_gau_0_5, file = file_saving_cv)
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
  
  #storing errors
  err_cv_alpha = matrix(data = 0,nrow = len_alphas,ncol = N)
  err_cv = matrix(data = 0,nrow = len_alphas,ncol = N)
}


string_message = "
                  PPC validation errors data Gaussian Kernel, norm 0.8 "
for (b in 1:N) {
  
  train_set = X.sample[,1:(N-1+b)]
  valid_set = X.sample[,N+b] 
  
  PPC_predictor_cv_alpha = PPC_KO( X = train_set,
                                   id_CV = "CV_alpha",
                                   threshold_ppc = 0.9,
                                   disc_ev = t.grid,
                                   left_extreme = left_extreme,
                                   right_extreme = right_extreme,
                                   err_ret = TRUE)
  
  err_cv_alpha[,b] = PPC_predictor_cv_alpha$`Validation errors`
  
  PPC_predictor_cv = PPC_KO( X = train_set,
                             id_CV = id_CV_ko,
                             disc_ev = t.grid,
                             left_extreme = left_extreme,
                             right_extreme = right_extreme,
                             err_ret = TRUE,
                             ex_solver = FALSE)
  
  for (i in 1:len_alphas) {
    err_cv[i,b] = PPC_predictor_cv$`Validation errors`[[i]][PPC_predictor_cv$`Number of PPCs retained`]
  }
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  valid_err_cv_alpha_PPC_gau_0_8 = err_cv_alpha
  valid_err_cv_PPC_gau_0_8 = err_cv
  file_saving_cv_alpha = paste0(paste0(path_stor_res,"/cv_alpha"),file_saving_cv_alpha_gau_0_8)
  file_saving_cv = paste0(paste0(path_stor_res,"/cv_both"),file_saving_cv_gau_0_8)
  save(valid_err_cv_alpha_PPC_gau_0_8, file = file_saving_cv_alpha)
  save(valid_err_cv_PPC_gau_0_8, file = file_saving_cv)
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
  
  #storing errors
  err_cv_alpha = matrix(data = 0,nrow = len_alphas,ncol = N)
  err_cv = matrix(data = 0,nrow = len_alphas,ncol = N)
}


string_message = "
                  PPC validation errors data identity Kernel, norm 0.5 "
for (b in 1:N) {
  
  train_set = X.sample[,1:(N-1+b)]
  valid_set = X.sample[,N+b] 
  
  PPC_predictor_cv_alpha = PPC_KO( X = train_set,
                                   id_CV = "CV_alpha",
                                   threshold_ppc = 0.9,
                                   disc_ev = t.grid,
                                   left_extreme = left_extreme,
                                   right_extreme = right_extreme,
                                   err_ret = TRUE)
  
  err_cv_alpha[,b] = PPC_predictor_cv_alpha$`Validation errors`
  
  PPC_predictor_cv = PPC_KO( X = train_set,
                             id_CV = id_CV_ko,
                             disc_ev = t.grid,
                             left_extreme = left_extreme,
                             right_extreme = right_extreme,
                             err_ret = TRUE,
                             ex_solver = FALSE)
  
  for (i in 1:len_alphas) {
    err_cv[i,b] = PPC_predictor_cv$`Validation errors`[[i]][PPC_predictor_cv$`Number of PPCs retained`]
  }
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  valid_err_cv_alpha_PPC_id_0_5 = err_cv_alpha
  valid_err_cv_PPC_id_0_5 = err_cv
  file_saving_cv_alpha = paste0(paste0(path_stor_res,"/cv_alpha"),file_saving_cv_alpha_id_0_5)
  file_saving_cv = paste0(paste0(path_stor_res,"/cv_both"),file_saving_cv_id_0_5)
  save(valid_err_cv_alpha_PPC_id_0_5, file = file_saving_cv_alpha)
  save(valid_err_cv_PPC_id_0_5, file = file_saving_cv)
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
  
  #storing errors
  err_cv_alpha = matrix(data = 0,nrow = len_alphas,ncol = N)
  err_cv = matrix(data = 0,nrow = len_alphas,ncol = N)
}


string_message = "
                  PPC validation errors data identity Kernel, norm 0.8 "
for (b in 1:N) {
  
  train_set = X.sample[,1:(N-1+b)]
  valid_set = X.sample[,N+b] 
  
  PPC_predictor_cv_alpha = PPC_KO( X = train_set,
                                   id_CV = "CV_alpha",
                                   threshold_ppc = 0.9,
                                   disc_ev = t.grid,
                                   left_extreme = left_extreme,
                                   right_extreme = right_extreme,
                                   err_ret = TRUE)
  
  err_cv_alpha[,b] = PPC_predictor_cv_alpha$`Validation errors`
  
  PPC_predictor_cv = PPC_KO( X = train_set,
                             id_CV = id_CV_ko,
                             disc_ev = t.grid,
                             left_extreme = left_extreme,
                             right_extreme = right_extreme,
                             err_ret = TRUE,
                             ex_solver = FALSE)
  
  for (i in 1:len_alphas) {
    err_cv[i,b] = PPC_predictor_cv$`Validation errors`[[i]][PPC_predictor_cv$`Number of PPCs retained`]
  }
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  valid_err_cv_alpha_PPC_id_0_8 = err_cv_alpha
  valid_err_cv_PPC_id_0_8 = err_cv
  file_saving_cv_alpha = paste0(paste0(path_stor_res,"/cv_alpha"),file_saving_cv_alpha_id_0_8)
  file_saving_cv = paste0(paste0(path_stor_res,"/cv_both"),file_saving_cv_id_0_8)
  save(valid_err_cv_alpha_PPC_id_0_8, file = file_saving_cv_alpha)
  save(valid_err_cv_PPC_id_0_8, file = file_saving_cv)
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
  
  #storing errors
  err_cv_alpha = matrix(data = 0,nrow = len_alphas,ncol = N)
  err_cv = matrix(data = 0,nrow = len_alphas,ncol = N)
}


string_message = "
                  PPC validation errors data sloping plane t Kernel, norm 0.5 "
for (b in 1:N) {
  
  train_set = X.sample[,1:(N-1+b)]
  valid_set = X.sample[,N+b] 
  
  PPC_predictor_cv_alpha = PPC_KO( X = train_set,
                                   id_CV = "CV_alpha",
                                   threshold_ppc = 0.9,
                                   disc_ev = t.grid,
                                   left_extreme = left_extreme,
                                   right_extreme = right_extreme,
                                   err_ret = TRUE)
  
  err_cv_alpha[,b] = PPC_predictor_cv_alpha$`Validation errors`
  
  PPC_predictor_cv = PPC_KO( X = train_set,
                             id_CV = id_CV_ko,
                             disc_ev = t.grid,
                             left_extreme = left_extreme,
                             right_extreme = right_extreme,
                             err_ret = TRUE,
                             ex_solver = FALSE)
  
  for (i in 1:len_alphas) {
    err_cv[i,b] = PPC_predictor_cv$`Validation errors`[[i]][PPC_predictor_cv$`Number of PPCs retained`]
  }
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  valid_err_cv_alpha_PPC_spt_0_5 = err_cv_alpha
  valid_err_cv_PPC_spt_0_5 = err_cv
  file_saving_cv_alpha = paste0(paste0(path_stor_res,"/cv_alpha"),file_saving_cv_alpha_spt_0_5)
  file_saving_cv = paste0(paste0(path_stor_res,"/cv_both"),file_saving_cv_spt_0_5)
  save(valid_err_cv_alpha_PPC_spt_0_5, file = file_saving_cv_alpha)
  save(valid_err_cv_PPC_spt_0_5, file = file_saving_cv)
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
  
  #storing errors
  err_cv_alpha = matrix(data = 0,nrow = len_alphas,ncol = N)
  err_cv = matrix(data = 0,nrow = len_alphas,ncol = N)
}


string_message = "
                  PPC validation errors data sloping plane t Kernel, norm 0.8 "
for (b in 1:N) {
  
  train_set = X.sample[,1:(N-1+b)]
  valid_set = X.sample[,N+b] 
  
  PPC_predictor_cv_alpha = PPC_KO( X = train_set,
                                   id_CV = "CV_alpha",
                                   threshold_ppc = 0.9,
                                   disc_ev = t.grid,
                                   left_extreme = left_extreme,
                                   right_extreme = right_extreme,
                                   err_ret = TRUE)
  
  err_cv_alpha[,b] = PPC_predictor_cv_alpha$`Validation errors`
  
  PPC_predictor_cv = PPC_KO( X = train_set,
                             id_CV = id_CV_ko,
                             disc_ev = t.grid,
                             left_extreme = left_extreme,
                             right_extreme = right_extreme,
                             err_ret = TRUE,
                             ex_solver = FALSE)
  
  for (i in 1:len_alphas) {
    err_cv[i,b] = PPC_predictor_cv$`Validation errors`[[i]][PPC_predictor_cv$`Number of PPCs retained`]
  }
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  valid_err_cv_alpha_PPC_spt_0_8 = err_cv_alpha
  valid_err_cv_PPC_spt_0_8 = err_cv
  file_saving_cv_alpha = paste0(paste0(path_stor_res,"/cv_alpha"),file_saving_cv_alpha_spt_0_8)
  file_saving_cv = paste0(paste0(path_stor_res,"/cv_both"),file_saving_cv_spt_0_8)
  save(valid_err_cv_alpha_PPC_spt_0_8, file = file_saving_cv_alpha)
  save(valid_err_cv_PPC_spt_0_8, file = file_saving_cv)
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
  
  #storing errors
  err_cv_alpha = matrix(data = 0,nrow = len_alphas,ncol = N)
  err_cv = matrix(data = 0,nrow = len_alphas,ncol = N)
}


string_message = "
                  PPC prediction data sloping plane s Kernel, norm 0.5 "
for (b in 1:N) {
  
  train_set = X.sample[,1:(N-1+b)]
  valid_set = X.sample[,N+b] 
  
  PPC_predictor_cv_alpha = PPC_KO( X = train_set,
                                   id_CV = "CV_alpha",
                                   threshold_ppc = 0.9,
                                   disc_ev = t.grid,
                                   left_extreme = left_extreme,
                                   right_extreme = right_extreme,
                                   err_ret = TRUE)
  
  err_cv_alpha[,b] = PPC_predictor_cv_alpha$`Validation errors`
  
  PPC_predictor_cv = PPC_KO( X = train_set,
                             id_CV = id_CV_ko,
                             disc_ev = t.grid,
                             left_extreme = left_extreme,
                             right_extreme = right_extreme,
                             err_ret = TRUE,
                             ex_solver = FALSE)
  
  for (i in 1:len_alphas) {
    err_cv[i,b] = PPC_predictor_cv$`Validation errors`[[i]][PPC_predictor_cv$`Number of PPCs retained`]
  }
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  valid_err_cv_alpha_PPC_sps_0_5 = err_cv_alpha
  valid_err_cv_PPC_sps_0_5 = err_cv
  file_saving_cv_alpha = paste0(paste0(path_stor_res,"/cv_alpha"),file_saving_cv_alpha_sps_0_5)
  file_saving_cv = paste0(paste0(path_stor_res,"/cv_both"),file_saving_cv_sps_0_5)
  save(valid_err_cv_alpha_PPC_sps_0_5, file = file_saving_cv_alpha)
  save(valid_err_cv_PPC_sps_0_5, file = file_saving_cv)
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
  
  #storing errors
  err_cv_alpha = matrix(data = 0,nrow = len_alphas,ncol = N)
  err_cv = matrix(data = 0,nrow = len_alphas,ncol = N)
}


string_message = "
                  PPC prediction data sloping plane s Kernel, norm 0.8 "
for (b in 1:N) {
  
  train_set = X.sample[,1:(N-1+b)]
  valid_set = X.sample[,N+b] 
  
  PPC_predictor_cv_alpha = PPC_KO( X = train_set,
                                   id_CV = "CV_alpha",
                                   threshold_ppc = 0.9,
                                   disc_ev = t.grid,
                                   left_extreme = left_extreme,
                                   right_extreme = right_extreme,
                                   err_ret = TRUE)
  
  err_cv_alpha[,b] = PPC_predictor_cv_alpha$`Validation errors`
  
  PPC_predictor_cv = PPC_KO( X = train_set,
                             id_CV = id_CV_ko,
                             disc_ev = t.grid,
                             left_extreme = left_extreme,
                             right_extreme = right_extreme,
                             err_ret = TRUE,
                             ex_solver = FALSE)
  
  for (i in 1:len_alphas) {
    err_cv[i,b] = PPC_predictor_cv$`Validation errors`[[i]][PPC_predictor_cv$`Number of PPCs retained`]
  }
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  valid_err_cv_alpha_PPC_sps_0_8 = err_cv_alpha
  valid_err_cv_PPC_sps_0_8 = err_cv
  file_saving_cv_alpha = paste0(paste0(path_stor_res,"/cv_alpha"),file_saving_cv_alpha_sps_0_8)
  file_saving_cv = paste0(paste0(path_stor_res,"/cv_both"),file_saving_cv_sps_0_8)
  save(valid_err_cv_alpha_PPC_sps_0_8, file = file_saving_cv_alpha)
  save(valid_err_cv_PPC_sps_0_8, file = file_saving_cv)
}