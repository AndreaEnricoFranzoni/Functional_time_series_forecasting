rm(list=ls())
graphics.off()
cat("\014")
set.seed(23032000)


#change here
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"
dir_stor_res = "/Test_domain1D/Artificial_data/results/results_prediction"
name_folder_res = "/NP"


#if you want to save the result 
save_res = FALSE
#where to store the results
path_stor_res = paste0(paste0(dir_w,dir_stor_res),name_folder_res)              
#saving results
{
  file_saving_NP_gau_0_5 = "/prediction_NP_gau_0_5.Rdata"
  file_saving_NP_gau_0_8 = "/prediction_NP_gau_0_8.Rdata"
  file_saving_NP_id_0_5  = "/prediction_NP_id_0_5.Rdata"
  file_saving_NP_id_0_8  = "/prediction_NP_id_0_8.Rdata"
  file_saving_NP_spt_0_5 = "/prediction_NP_spt_0_5.Rdata"
  file_saving_NP_spt_0_8 = "/prediction_NP_spt_0_8.Rdata"
  file_saving_NP_sps_0_5 = "/prediction_NP_sps_0_5.Rdata"
  file_saving_NP_sps_0_8 = "/prediction_NP_sps_0_8.Rdata"
}



source(paste0(dir_w,"/Test_domain1D/Artificial_data/utils/far_1_1d.R"))         #load functions to generate the FAR(1) process
source(paste0(dir_w,"/Test_domain1D/Artificial_data/utils/prediction_error.R")) #load functions to evaluate the prediction error
source(paste0(dir_w,"/Test_domain1D/Artificial_data/utils/data_param.R"))       #load parameter to generate data according to a strategy




#storing predictions
{
  prediction_NP_gau_0_5 <- lapply((1):N-1,function(x) NULL)
  prediction_NP_gau_0_8 <- lapply((1):N-1,function(x) NULL)
  prediction_NP_id_0_5  <- lapply((1):N-1,function(x) NULL)
  prediction_NP_id_0_8  <- lapply((1):N-1,function(x) NULL)
  prediction_NP_spt_0_5 <- lapply((1):N-1,function(x) NULL)
  prediction_NP_spt_0_8 <- lapply((1):N-1,function(x) NULL)
  prediction_NP_sps_0_5 <- lapply((1):N-1,function(x) NULL)
  prediction_NP_sps_0_8 <- lapply((1):N-1,function(x) NULL)
}

#storing errors
{
  err_NP_gau_0_5_en <- numeric(N)     
  err_NP_gau_0_5_rn <- numeric(N)
  err_NP_id_0_5_en  <- numeric(N)     
  err_NP_id_0_5_rn  <- numeric(N)
  err_NP_spt_0_5_en <- numeric(N)     
  err_NP_spt_0_5_rn <- numeric(N)
  err_NP_sps_0_5_en <- numeric(N)     
  err_NP_sps_0_5_rn <- numeric(N)
  err_NP_gau_0_8_en <- numeric(N)     
  err_NP_gau_0_8_rn <- numeric(N)
  err_NP_id_0_8_en  <- numeric(N)     
  err_NP_id_0_8_rn  <- numeric(N)
  err_NP_spt_0_8_en <- numeric(N)     
  err_NP_spt_0_8_rn <- numeric(N)
  err_NP_sps_0_8_en <- numeric(N)     
  err_NP_sps_0_8_rn <- numeric(N)
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
                  NP prediction data Gaussian Kernel, norm 0.5 "
for (b in 1:N) {
  
  train_set = X.sample[,1:(N-1+b)]      #train set
  valid_set = X.sample[,N+b]            #validation set
  
  NP_predictor = train_set[,N+b-1]    #predictor
  
  prediction_NP_gau_0_5[[b]] = list( Prediction = NP_predictor )  #estimated prediction
  err_NP_gau_0_5_en[b] = En(NP_predictor,valid_set,t.grid)        #estimated En
  err_NP_gau_0_5_rn[b] = Rn(NP_predictor,valid_set,t.grid)        #estimated Rn
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_gau_0_5 = list(Prediction = prediction_NP_gau_0_5, En = err_NP_gau_0_5_en, Rn = err_NP_gau_0_5_rn)
  file_saving = paste0(path_stor_res,file_saving_NP_gau_0_5)
  save(res_gau_0_5, file = file_saving)
}





# ----- data generation Gaussian Kernel norm 0.8 -----
{
  #feats of data
  id_kernel <- "gaussian"       #way of generating data ("gaussian", "identity", "sp_t" and "sp_s" kernel available)
  norm      <- 0.8          #norm of the Kernel (that has to be <1)
  id_noise  <- "1"          #error of the FAR(1) process ("1", "2" or "3")
  
  
  proc = feat_far_1_process(id_kernel,norm)
  id_kernel   <- proc$kernel
  a           <- proc$constant
  name_kernel <- proc$name
  
  ##Simulate a stationary FAR(1) process according to a specific kernel
  X.sample <- far_1_1D(kernel_id = id_kernel, noise_id = id_noise, n = n, t.grid = t.grid, a = a, burnin = burnin)
}



string_message = "
                  NP prediction data Gaussian Kernel, norm 0.8 "
for (b in 1:N) {
  
  train_set = X.sample[,1:(N-1+b)]      #train set
  valid_set = X.sample[,N+b]            #validation set
  
  NP_predictor = train_set[,N+b-1]    #predictor
  
  prediction_NP_gau_0_8[[b]] = list( Prediction = NP_predictor )  #estimated prediction
  err_NP_gau_0_8_en[b] = En(NP_predictor,valid_set,t.grid)        #estimated En
  err_NP_gau_0_8_rn[b] = Rn(NP_predictor,valid_set,t.grid)        #estimated Rn
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_gau_0_8 = list(Prediction = prediction_NP_gau_0_8, En = err_NP_gau_0_8_en, Rn = err_NP_gau_0_8_rn)
  file_saving = paste0(path_stor_res,file_saving_NP_gau_0_8)
  save(res_gau_0_8, file = file_saving)
}





# ----- data generation Identity Kernel norm 0.5 -----
{
  #feats of data
  id_kernel <- "identity"       #way of generating data ("gaussian", "identity", "sp_t" and "sp_s" kernel available)
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
                  NP prediction data Identity Kernel, norm 0.5 "
for (b in 1:N) {
  
  train_set = X.sample[,1:(N-1+b)]      #train set
  valid_set = X.sample[,N+b]            #validation set
  
  NP_predictor = train_set[,N+b-1]    #predictor
  
  prediction_NP_id_0_5[[b]] = list( Prediction = NP_predictor )  #estimated prediction
  err_NP_id_0_5_en[b] = En(NP_predictor,valid_set,t.grid)        #estimated En
  err_NP_id_0_5_rn[b] = Rn(NP_predictor,valid_set,t.grid)        #estimated Rn
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_id_0_5 = list(Prediction = prediction_NP_id_0_5, En = err_NP_id_0_5_en, Rn = err_NP_id_0_5_rn)
  file_saving = paste0(path_stor_res,file_saving_NP_id_0_5)
  save(res_id_0_5, file = file_saving)
}






# ----- data generation Identity Kernel norm 0.8 -----
{
  #feats of data
  id_kernel <- "identity"       #way of generating data ("gaussian", "identity", "sp_t" and "sp_s" kernel available)
  norm      <- 0.8          #norm of the Kernel (that has to be <1)
  id_noise  <- "1"          #error of the FAR(1) process ("1", "2" or "3")
  
  
  proc = feat_far_1_process(id_kernel,norm)
  id_kernel   <- proc$kernel
  a           <- proc$constant
  name_kernel <- proc$name
  
  ##Simulate a stationary FAR(1) process according to a specific kernel
  X.sample <- far_1_1D(kernel_id = id_kernel, noise_id = id_noise, n = n, t.grid = t.grid, a = a, burnin = burnin)
}



string_message = "
                  NP prediction data Identity Kernel, norm 0.8 "
for (b in 1:N) {
  
  train_set = X.sample[,1:(N-1+b)]      #train set
  valid_set = X.sample[,N+b]            #validation set
  
  NP_predictor = train_set[,N+b-1]    #predictor
  
  prediction_NP_id_0_8[[b]] = list( Prediction = NP_predictor )  #estimated prediction
  err_NP_id_0_8_en[b] = En(NP_predictor,valid_set,t.grid)        #estimated En
  err_NP_id_0_8_rn[b] = Rn(NP_predictor,valid_set,t.grid)        #estimated Rn
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_id_0_8 = list(Prediction = prediction_NP_id_0_8, En = err_NP_id_0_8_en, Rn = err_NP_id_0_8_rn)
  file_saving = paste0(path_stor_res,file_saving_NP_id_0_8)
  save(res_id_0_8, file = file_saving)
}





# ----- data generation Sloping Plane t Kernel norm 0.5 -----
{
  #feats of data
  id_kernel <- "sp_t"       #way of generating data ("gaussian", "identity", "sp_t" and "sp_s" kernel available)
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
                  NP prediction data Sloping Plane t Kernel, norm 0.5 "
for (b in 1:N) {
  
  train_set = X.sample[,1:(N-1+b)]      #train set
  valid_set = X.sample[,N+b]            #validation set
  
  NP_predictor = train_set[,N+b-1]    #predictor
  
  prediction_NP_spt_0_5[[b]] = list( Prediction = NP_predictor )  #estimated prediction
  err_NP_spt_0_5_en[b] = En(NP_predictor,valid_set,t.grid)        #estimated En
  err_NP_spt_0_5_rn[b] = Rn(NP_predictor,valid_set,t.grid)        #estimated Rn
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_spt_0_5 = list(Prediction = prediction_NP_spt_0_5, En = err_NP_spt_0_5_en, Rn = err_NP_spt_0_5_rn)
  file_saving = paste0(path_stor_res,file_saving_NP_spt_0_5)
  save(res_spt_0_5, file = file_saving)
}





# ----- data generation Sloping Plane t Kernel norm 0.8 -----
{
  #feats of data
  id_kernel <- "sp_t"       #way of generating data ("gaussian", "identity", "sp_t" and "sp_s" kernel available)
  norm      <- 0.8          #norm of the Kernel (that has to be <1)
  id_noise  <- "1"          #error of the FAR(1) process ("1", "2" or "3")
  
  
  proc = feat_far_1_process(id_kernel,norm)
  id_kernel   <- proc$kernel
  a           <- proc$constant
  name_kernel <- proc$name
  
  ##Simulate a stationary FAR(1) process according to a specific kernel
  X.sample <- far_1_1D(kernel_id = id_kernel, noise_id = id_noise, n = n, t.grid = t.grid, a = a, burnin = burnin)
}



string_message = "
                  NP prediction data Sloping Plane t Kernel, norm 0.8 "
for (b in 1:N) {
  
  train_set = X.sample[,1:(N-1+b)]      #train set
  valid_set = X.sample[,N+b]            #validation set
  
  NP_predictor = train_set[,N+b-1]    #predictor
  
  prediction_NP_spt_0_8[[b]] = list( Prediction = NP_predictor )  #estimated prediction
  err_NP_spt_0_8_en[b] = En(NP_predictor,valid_set,t.grid)        #estimated En
  err_NP_spt_0_8_rn[b] = Rn(NP_predictor,valid_set,t.grid)        #estimated Rn
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_spt_0_8 = list(Prediction = prediction_NP_spt_0_8, En = err_NP_spt_0_8_en, Rn = err_NP_spt_0_8_rn)
  file_saving = paste0(path_stor_res,file_saving_NP_spt_0_8)
  save(res_spt_0_8, file = file_saving)
}





# ----- data generation Sloping Plane s Kernel norm 0.5 -----
{
  #feats of data
  id_kernel <- "sp_s"       #way of generating data ("gaussian", "identity", "sp_t" and "sp_s" kernel available)
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
                  NP prediction data Sloping Plane s Kernel, norm 0.5 "
for (b in 1:N) {
  
  train_set = X.sample[,1:(N-1+b)]      #train set
  valid_set = X.sample[,N+b]            #validation set
  
  NP_predictor = train_set[,N+b-1]    #predictor
  
  prediction_NP_sps_0_5[[b]] = list( Prediction = NP_predictor )  #estimated prediction
  err_NP_sps_0_5_en[b] = En(NP_predictor,valid_set,t.grid)        #estimated En
  err_NP_sps_0_5_rn[b] = Rn(NP_predictor,valid_set,t.grid)        #estimated Rn
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_sps_0_5 = list(Prediction = prediction_NP_sps_0_5, En = err_NP_sps_0_5_en, Rn = err_NP_sps_0_5_rn)
  file_saving = paste0(path_stor_res,file_saving_NP_sps_0_5)
  save(res_sps_0_5, file = file_saving)
}





# ----- data generation Sloping Plane s Kernel norm 0.8 -----
{
  #feats of data
  id_kernel <- "sp_s"       #way of generating data ("gaussian", "identity", "sp_t" and "sp_s" kernel available)
  norm      <- 0.8          #norm of the Kernel (that has to be <1)
  id_noise  <- "1"          #error of the FAR(1) process ("1", "2" or "3")
  
  
  proc = feat_far_1_process(id_kernel,norm)
  id_kernel   <- proc$kernel
  a           <- proc$constant
  name_kernel <- proc$name
  
  ##Simulate a stationary FAR(1) process according to a specific kernel
  X.sample <- far_1_1D(kernel_id = id_kernel, noise_id = id_noise, n = n, t.grid = t.grid, a = a, burnin = burnin)
}



string_message = "
                  NP prediction data Sloping Plane s Kernel, norm 0.8 "
for (b in 1:N) {
  
  train_set = X.sample[,1:(N-1+b)]      #train set
  valid_set = X.sample[,N+b]            #validation set
  
  NP_predictor = train_set[,N+b-1]    #predictor
  
  prediction_NP_sps_0_8[[b]] = list( Prediction = NP_predictor )  #estimated prediction
  err_NP_sps_0_8_en[b] = En(NP_predictor,valid_set,t.grid)        #estimated En
  err_NP_sps_0_8_rn[b] = Rn(NP_predictor,valid_set,t.grid)        #estimated Rn
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_sps_0_8 = list(Prediction = prediction_NP_sps_0_8, En = err_NP_sps_0_8_en, Rn = err_NP_sps_0_8_rn)
  file_saving = paste0(path_stor_res,file_saving_NP_sps_0_8)
  save(res_sps_0_8, file = file_saving)
}