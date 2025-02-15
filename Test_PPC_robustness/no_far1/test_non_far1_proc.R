rm(list=ls())
graphics.off()
cat("\014")
set.seed(23032000)

######################################################################
#### PPC robustness if non FAR(1) process is generating the data  ####
######################################################################

library(PPCKO)
library(freqdom.fda)

#put here the path of the local copy of the directory
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"

#if you want to save the result 
save_res = TRUE


#where to store the results
dir_stor_res = "/Test_PPC_robustness/test_non_far1/results/prediction"
path_stor_res = paste0(dir_w,dir_stor_res)

#file for saving results
file_saving_PPC_no_far1 = "/prediction_PPC_no_far1.Rdata"
file_saving_EX_no_far1 = "/prediction_EX_no_far1.Rdata"


#data parameters: VAR(2) process
{
  left_ex = 0
  right_ex = 1
  dim_grid = 200
  n                  <- 300         #number of time instants                                   #time instants of the functional time series
  burnin             <- 50          #burnin iterations                                     #burnin iterations for FAR(1)
  N                  <- n - burnin  #number of time instants to be predicted
  t.grid = seq(0,1,length.out=dim_grid)
  
  psi_1 = matrix(data=c(0.8,0.3,0.3,
                          0.3,0.8,0.3,
                          0.3,0.3,0.8),
                   nrow=3,
                   ncol=3)
  
  psi_1 = psi_1/(2*norm(psi_1, type = "F"))
  
  
  psi_2 = matrix(data=c(0.5,0.1,0.1,
                          0.1,0.5,0.1,
                          0.1,0.1,0.5),
                   nrow=3,
                   ncol=3)
  
  psi_2 = psi_2/(2*norm(psi_2, type = "F"))
  
  
  psi = array(c(psi_1,psi_2),dim = c(3,3,2))
  
  noise_t = "t"
  df_t_st = 4
  
  scale_t_st = matrix(data=c(0.5,0.3,0.3,
                             0.3,0.5,0.3,
                             0.3,0.3,0.5),
                      nrow=3,
                      ncol=3)
  
  var_proc = fts.rar(n = n,
                     d = 3,
                     Psi = psi,
                     burnin = burnin,
                     noise = noise_t,
                     sigma = scale_t_st,
                     df = df_t_st)
  
  X.sample = matrix(data = NA,
                    nrow = dim_grid,
                    ncol = n)
  
  
  for (q in 1:dim_grid) {
    for (t in 1:n) {
      eval_fts = var_proc$coefs[1,t] + var_proc$coefs[2,t]*(sin(2*pi*t.grid[q])/(1/sqrt(2))) + var_proc$coefs[3,t]*(cos(2*pi*t.grid[q])/(1/sqrt(2)))
      X.sample[q,t] = eval_fts
    }
  }
}




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
prediction_PPC_no_far1 <- lapply((1):N-1,function(x) NULL)
prediction_EX_no_far1 <- lapply((1):N-1,function(x) NULL)


#storing errors
err_PPC_no_far1_en <- numeric(N)
err_PPC_no_far1_rn <- numeric(N)

err_EX_no_far1_en <- numeric(N)
err_EX_no_far1_rn <- numeric(N)


#predico da istante 51 a istante 300
string_message = "
                  PPC prediction non-far(1) data "
for (b in 1:N) {
  
  train_set = X.sample[,1:(burnin+b-1)]
  valid_set = X.sample[,burnin+b] 
  
  PPC_predictor = PPC_KO( X = train_set,
                          id_CV = id_CV_ko,
                          disc_ev = t.grid,
                          left_extreme = left_extreme,
                          right_extreme = right_extreme,
                          err_ret = err_ret)
  
  prediction_PPC_no_far1[[b]] = PPC_predictor
  err_PPC_no_far1_en[b] = sqrt(MLmetrics::MSE(PPC_predictor$`One-step ahead prediction`,valid_set)) 
  err_PPC_no_far1_rn[b] = MLmetrics::MAE(PPC_predictor$`One-step ahead prediction`,valid_set)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_PPC_no_far1 = list(Prediction = prediction_PPC_no_far1, En = err_PPC_no_far1_en, Rn = err_PPC_no_far1_rn)
  file_saving = paste0(path_stor_res,file_saving_PPC_no_far1)
  save(res_PPC_no_far1, file = file_saving)
}





string_message = "
                  EX prediction non-far(1) data "
for (b in 1:N) {
  
  actual_instant = burnin+b-1
  next_instant = burnin+b
  valid_set = X.sample[,next_instant]

  var_proc_eval = as.vector(psi_1%*%var_proc$coefs[,actual_instant] + psi_2%*%var_proc$coefs[,actual_instant-1])
  
  EX_predictor = numeric(dim_grid)
  for(q in 1:length(t.grid)){
    EX_predictor[q] = var_proc_eval[1] + var_proc_eval[2]*sqrt(2)*sin(2*pi*t.grid[q]) + var_proc_eval[3]*sqrt(2)*cos(2*pi*t.grid[q])
  }
  
  
  prediction_EX_no_far1[[b]] = EX_predictor
  err_EX_no_far1_en[b] = sqrt(MLmetrics::MSE(EX_predictor,valid_set)) 
  err_EX_no_far1_rn[b] = MLmetrics::MAE(EX_predictor,valid_set)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , b, N)
  setTxtProgressBar(txtProgressBar(min = 1, max = N, style = 3), b)
  cat("\r", message)
}

#save results
if(save_res){
  res_EX_no_far1 = list(Prediction = prediction_EX_no_far1, En = err_EX_no_far1_en, Rn = err_EX_no_far1_rn)
  file_saving = paste0(path_stor_res,file_saving_EX_no_far1)
  save(res_EX_no_far1, file = file_saving)
}
