rm(list=ls())
graphics.off()
cat("\014")
set.seed(12)



###########################################################
#### Computing predictions as indicated in the readme  ####
###########################################################

library(PPCKO)


#put here the path of the local copy of the directory
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"

# if storing results
save_res = TRUE
path_store_res = paste0(dir_w,"/Test_domain2D/Artificial_Data/results/results_prediction")

# load auxiliary functions to generate the data and perform EK prediction
{
  source(paste0(dir_w,"/Test_domain2D/Artificial_data/utils/far_1_2d.R"))
  source(paste0(dir_w,"/Test_domain2D/Artificial_data/utils/ex_pred.R"))
  source(paste0(dir_w,"/Test_domain2D/Artificial_data/utils/EstimatedKernel_predictor_2D.R"))       
  source(paste0(dir_w,"/Test_domain2D/Artificial_data/utils/KE_cv_2D.R"))       
}




## Data parameters
sample_size     = 100                                   # number of time instants
tot_iter        = 50                                    # number of simulations
first_dim_ts    = 50                                    # dimension of first training 
training_set_sz = seq(first_dim_ts,length.out=tot_iter) # dimension of training set used

burnin      <- 20                                       # burnin for data generation

# domain parameters
left_ex_x1  <- 0
right_ex_x1 <- 1
left_ex_x2  <- 0
right_ex_x2 <- 1
dim_grid_x1 <- 20
dim_grid_x2 <- 20


############### The code to generate FAR(1) of surfaces using tensor products
############### gently given by Nicolò Ajroldi for 
############### "Conformal prediction bands for two-dimensional functional time series" by
############### Ajroldi, N., Diquigiovanni, J., Fontana, M. and Vantini, S. 



## ----- data generation -----
{
  # grid on x1 and x2
  x1.grid = seq(from=left_ex_x1, to=right_ex_x1, length=dim_grid_x1)
  x2.grid = seq(from=left_ex_x2, to=right_ex_x2, length=dim_grid_x2)
  
  # number of basis in each domain
  nbasis.1.sim = 5
  nbasis.2.sim = 5
  
  # total number of tensor product basis
  nbasis.sim = nbasis.1.sim*nbasis.2.sim
  
  # type of basis
  basis.type = "fourier"
  
  # d = dimension of the underlying VAR(1)
  d=nbasis.sim 
  Psi1=matrix(0.3,d,d)
  diag(Psi1)=0.8
  Psi1=Psi1/norm(Psi1, type = "F")*0.3
  Psi=array(0,c(d,d,1))
  Psi[,,1]=Psi1
  my_Sigma=matrix(0.6,d,d)
  diag(my_Sigma)=1
  my_Sigma=my_Sigma/2
  
  ## simulate data ----
  out <- far_1_2D(n = sample_size, 
                  Psi = Psi, 
                  x1.grid = x1.grid, 
                  x2.grid = x2.grid, 
                  nbasis.1 = nbasis.1.sim, 
                  nbasis.2 = nbasis.2.sim, 
                  burnin = burnin,
                  noise = "mnorm",
                  df= 4,
                  sigma = my_Sigma,
                  basis.type = basis.type)
  

  # list of matrices: data
  Xt = out$Xt
  
  # data wrapped as matrix
  Xt_wrapped = PPCKO::data_2d_wrapper_from_list(Xt)
  
  
  # plot the first three realizations
  color = "seagreen3"
  
  maxx = max(sapply(Xt, max))+0.5
  minn = min(sapply(Xt, min))-0.5
  
  library(latex2exp)
  quartz()
  par(mfrow=c(1,3))
  line=-9
  persp(x=x1.grid, y=x2.grid, z=Xt[[1]], col=color,
        xlab=TeX(""),ylab="",zlab="",zlim=c(minn,maxx),
        ticktype='detailed')
  title(TeX("$f_{1}$"), outer = FALSE,line=line)
  persp(x=x1.grid, y=x2.grid, z=Xt[[2]], col=color,
        xlab="",ylab="",zlab="",zlim=c(minn,maxx),
        ticktype='detailed')
  title(TeX("$f_{2}$"), outer = FALSE,line=line)
  persp(x=x1.grid, y=x2.grid, z=Xt[[3]], col=color,
        xlab="",ylab="",zlab="",zlim=c(minn,maxx),
        ticktype='detailed')
  title(TeX("$f_{3}$"), outer = FALSE,line=line)
}


#PPC KO parameters
{
  id_CV            <- "CV"
  alpha            <- 0.1
  k                <- 0 
  threshold_ppc    <- 0.95
  alpha_vec        <- NULL
  k_vec            <- NULL
  toll             <- 1e-4
  disc_ev_x1       <- x1.grid
  disc_ev_x2       <- x2.grid
  left_extreme_x1  <- left_ex_x1
  right_extreme_x1 <- right_ex_x1
  left_extreme_x2  <- left_ex_x2
  right_extreme_x2 <- right_ex_x2
  num_disc_ev_x1   <- dim_grid_x1
  num_disc_ev_x2   <- dim_grid_x2
  err_ret          <- 0
  id_rem_nan       <- NULL
}


{
  p_vector = c(2,3,4,5,6)
}




## -----PPC-----

#store results
err_PPC_en <- numeric(tot_iter)
err_PPC_rn <- numeric(tot_iter)
pred_PPC   <- lapply(1:tot_iter,function(x) NULL)

string_message = "
                  PPC prediction "
for (i in 1:tot_iter) {
  
  dim_ts = training_set_sz[i]
  train  = Xt_wrapped[,1:dim_ts]
  valid  = Xt_wrapped[,dim_ts+1]
  
  predictor = PPC_KO_2d( X = train,
                         id_CV = id_CV,
                         alpha = alpha,
                         k = k,
                         threshold_ppc = threshold_ppc,
                         alpha_vec = alpha_vec,
                         k_vec = k_vec,
                         toll = toll,
                         disc_ev_x1 = x1.grid,
                         num_disc_ev_x1 = dim_grid_x1,
                         disc_ev_x2 = x2.grid,
                         num_disc_ev_x2 = dim_grid_x2,
                         left_extreme_x1 = left_ex_x1,
                         right_extreme_x1 = right_ex_x1,
                         left_extreme_x2 = left_ex_x2,
                         right_extreme_x2 = right_ex_x2,
                         err_ret = 0,
                         ex_solver = TRUE)
  
  prediction = predictor$`One-step ahead prediction`
  pred_PPC[[i]] = list(Prediction = prediction, Alpha = predictor$Alpha, N_PPCs = predictor$`Number of PPCs retained`, Exp_Pow = predictor$`Explanatory power PPCs` )
  
  
  err_PPC_en[i] = sqrt(MLmetrics::MSE(prediction,valid))
  err_PPC_rn[i] = MLmetrics::MAE(prediction,valid)
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, tot_iter)
  setTxtProgressBar(txtProgressBar(min = 1, max = tot_iter, style = 3), i)
  cat("\r", message)
}

if(save_res){
  save(pred_PPC,   file = paste0(path_store_res,"/PPC/PPC_pred.Rdata"))
  save(err_PPC_en, file = paste0(path_store_res,"/PPC/PPC_en.Rdata"))
  save(err_PPC_rn, file = paste0(path_store_res,"/PPC/PPC_rn.Rdata"))
}





##-----KE-----

#store results
err_EK_en = numeric(tot_iter)
err_EK_rn = numeric(tot_iter)

string_message = "
                  EK prediction "
for (i in 1:tot_iter) {
  
  dim_ts = training_set_sz[i]
  train  = Xt[1:dim_ts]
  valid  = Xt_wrapped[,dim_ts+1]
  
  predictor = cv_EK_2d( X = train,
                        grid_eval1 = x1.grid,
                        grid_eval2 = x2.grid,
                        p_vector = p_vector,
                        improved = FALSE)
  
  
  prediction = predictor$prediction
  
  err_EK_en[i] = sqrt(MLmetrics::MSE(prediction,valid))
  err_EK_rn[i] = MLmetrics::MAE(prediction,valid)
  
  
  
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, tot_iter)
  setTxtProgressBar(txtProgressBar(min = 1, max = tot_iter, style = 3), i)
  cat("\r", message)
  
}

if(save_res){
  save(err_EK_en, file = paste0(path_store_res,"/EK/EK_en.Rdata"))
  save(err_EK_rn, file = paste0(path_store_res,"/EK/EK_rn.Rdata"))
}





##-----KEI-----

#store results
err_EKI_en = numeric(tot_iter)
err_EKI_rn = numeric(tot_iter)

string_message = "
                  EKI prediction "
for (i in 1:tot_iter) {
  
  dim_ts = training_set_sz[i]
  train  = Xt[1:dim_ts]
  valid  = Xt_wrapped[,dim_ts+1]
  
  predictor = cv_EK_2d( X = train,
                        grid_eval1 = x1.grid,
                        grid_eval2 = x2.grid,
                        p_vector = p_vector,
                        improved = TRUE)
  
  
  prediction = predictor$prediction
  
  err_EKI_en[i] = sqrt(MLmetrics::MSE(prediction,valid))
  err_EKI_rn[i] = MLmetrics::MAE(prediction,valid)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, tot_iter)
  setTxtProgressBar(txtProgressBar(min = 1, max = tot_iter, style = 3), i)
  cat("\r", message)
  
}

if(save_res){
  save(err_EKI_en, file = paste0(path_store_res,"/EKI/EKI_en.Rdata"))
  save(err_EKI_rn, file = paste0(path_store_res,"/EKI/EKI_rn.Rdata"))
}





##-----EX-----

err_EX_en = numeric(tot_iter)
err_EX_rn = numeric(tot_iter)

string_message = "
                  EX prediction "
for (i in 1:tot_iter) {
  
  dim_ts = training_set_sz[i]
  Xt_list_of_vec = vector("list", length = dim_ts+1)
  
  # for each time
  for(t in seq(1,dim_ts+1))
  {
    Xt_list_of_vec[[t]] = vector("list", length = 1)
    Xt_list_of_vec[[t]][[1]] = as.vector(Xt[[t]])
  }
  
  train = Xt_list_of_vec[-(dim_ts+1)]
  valid = Xt_list_of_vec[dim_ts+1]
  
  predictor = ex_prediction_2D_FAR_oracle(data_y = train,
                                          indexes_not_NA = NULL,
                                          x1.grid = x1.grid,
                                          x2.grid = x2.grid,
                                          nbasis.1 = nbasis.1.sim,
                                          nbasis.2 = nbasis.2.sim,
                                          basis.type = basis.type,
                                          PSI = Psi1)
  
  prediction = predictor[[1]][[1]]
  
  err_EX_en[i] = sqrt(MLmetrics::MSE(prediction,valid[[1]][[1]]))
  err_EX_rn[i] = MLmetrics::MAE(prediction,valid[[1]][[1]])
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, tot_iter)
  setTxtProgressBar(txtProgressBar(min = 1, max = tot_iter, style = 3), i)
  cat("\r", message)
  
}

if(save_res){
  save(err_EX_en, file = paste0(path_store_res,"/EX/EX_en.Rdata"))
  save(err_EX_rn, file = paste0(path_store_res,"/EX/EX_rn.Rdata"))
}





##-----MP-----

#store results
err_MP_en = numeric(tot_iter)
err_MP_rn = numeric(tot_iter)

string_message = "
                  MP prediction "
for (i in 1:tot_iter) {
  
  dim_ts = training_set_sz[i]
  train = Xt_wrapped[,1:dim_ts]
  valid = Xt_wrapped[,dim_ts+1]
  
  prediction = rowMeans(train)
  
  err_MP_en[i] = sqrt(MLmetrics::MSE(prediction,valid))
  err_MP_rn[i] = MLmetrics::MAE(prediction,valid)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, tot_iter)
  setTxtProgressBar(txtProgressBar(min = 1, max = tot_iter, style = 3), i)
  cat("\r", message)
  
}

if(save_res){
  save(err_MP_en, file = paste0(path_store_res,"/MP/MP_en.Rdata"))
  save(err_MP_rn, file = paste0(path_store_res,"/MP/MP_rn.Rdata"))
}





##-----NP-----

#store results
err_NP_en = numeric(tot_iter)
err_NP_rn = numeric(tot_iter)

string_message = "
                  NP prediction "
for (i in 1:tot_iter) {
  
  dim_ts = training_set_sz[i]
  prediction = Xt_wrapped[,dim_ts]
  valid = Xt_wrapped[,dim_ts+1]

  
  err_NP_en[i] = sqrt(MLmetrics::MSE(prediction,valid))
  err_NP_rn[i] = MLmetrics::MAE(prediction,valid)
  
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, tot_iter)
  setTxtProgressBar(txtProgressBar(min = 1, max = tot_iter, style = 3), i)
  cat("\r", message)
  
}

if(save_res){
  save(err_NP_en, file = paste0(path_store_res,"/NP/NP_en.Rdata"))
  save(err_NP_rn, file = paste0(path_store_res,"/NP/NP_rn.Rdata"))
}





## -----PPC gen-----

#store results
err_PPC_gen_en <- numeric(tot_iter)
err_PPC_gen_rn <- numeric(tot_iter)
pred_PPC_gen   <- lapply(1:tot_iter,function(x) NULL)

string_message = "
                  PPC gen prediction "
for (i in 1:tot_iter) {
  
  dim_ts = training_set_sz[i]
  train  = Xt_wrapped[,1:dim_ts]
  valid  = Xt_wrapped[,dim_ts+1]
  
  predictor = PPC_KO_2d( X = train,
                         id_CV = id_CV,
                         alpha = alpha,
                         k = k,
                         threshold_ppc = threshold_ppc,
                         alpha_vec = alpha_vec,
                         k_vec = k_vec,
                         toll = toll,
                         disc_ev_x1 = x1.grid,
                         num_disc_ev_x1 = dim_grid_x1,
                         disc_ev_x2 = x2.grid,
                         num_disc_ev_x2 = dim_grid_x2,
                         left_extreme_x1 = left_ex_x1,
                         right_extreme_x1 = right_ex_x1,
                         left_extreme_x2 = left_ex_x2,
                         right_extreme_x2 = right_ex_x2,
                         err_ret = FALSE,
                         ex_solver = FALSE)
  
  prediction = predictor$`One-step ahead prediction`
  pred_PPC_gen[[i]] = list(Prediction = prediction, Alpha = predictor$Alpha, N_PPCs = predictor$`Number of PPCs retained`, Exp_Pow = predictor$`Explanatory power PPCs` )
  
  
  err_PPC_gen_en[i] = sqrt(MLmetrics::MSE(prediction,valid))
  err_PPC_gen_rn[i] = MLmetrics::MAE(prediction,valid)
  
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
  ") , i, tot_iter)
  setTxtProgressBar(txtProgressBar(min = 1, max = tot_iter, style = 3), i)
  cat("\r", message)
}

if(save_res){
  save(pred_PPC_gen,   file = paste0(path_store_res,"/PPC_gen/PPC_gen_pred.Rdata"))
  save(err_PPC_gen_en, file = paste0(path_store_res,"/PPC_gen/PPC_gen_en.Rdata"))
  save(err_PPC_gen_rn, file = paste0(path_store_res,"/PPC_gen/PPC_gen_rn.Rdata"))
}