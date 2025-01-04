rm(list=ls())
graphics.off()
cat("\014")

set.seed(23032000)
#change here
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"

# save results
save_res = TRUE
format = ".jpg"
path_res_pred = paste0(dir_w,"/Test_domain2D/Artificial_data/results/results_prediction")

files <- list.files(path = paste0(path_res_pred,paste0("/PPC")), full.names = TRUE)
for (file in files) {
  load(file)}

strange_alphas=c() 
for (i in 1:length(pred_PPC)) {
  if(pred_PPC[[i]]$Alpha > 10){strange_alphas=c(strange_alphas,i)}
}

summary(err_PPC_en)
for (i in strange_alphas) {
  print(err_PPC_en[i])
}


summary(err_PPC_rn)
for (i in strange_alphas) {
  print(err_PPC_rn[i])
}


for (i in strange_alphas) {
  print(pred_PPC[[i]]$N_PPCs)
}

for (i in strange_alphas) {
  print(pred_PPC[[i]]$Exp_Pow)
}


exp_pow = numeric(length = length(pred_PPC))

for (i in 1:length(pred_PPC)) {
  exp_pow[i] = pred_PPC[[i]]$Exp_Pow[pred_PPC[[i]]$N_PPCs]
}

summary(exp_pow)






library(PPCKO)


set.seed(23032000)

#change here
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"

# load auxiliary functions
{
  source(paste0(dir_w,"/Test_domain2D/Artificial_data/utils/far_1_2d.R"))
  source(paste0(dir_w,"/Test_domain2D/Artificial_data/utils/ex_pred.R"))
  source(paste0(dir_w,"/Test_domain2D/Artificial_data/utils/EstimatedKernel_predictor_2D.R"))       #load parameter to generate data according to a strategy
  source(paste0(dir_w,"/Test_domain2D/Artificial_data/utils/KE_cv_2D.R"))       #load parameter to generate data according to a strategy
  
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
}


{
  id_CV            <- "CV_alpha"
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



err_PPC_en_check_alpha <- numeric(length(strange_alphas))
err_PPC_rn_check_alpha <- numeric(length(strange_alphas))
pred_PPC_check_alpha   <- lapply(1:length(strange_alphas),function(x) NULL)


for (i in 1:length(strange_alphas)) {
  
  dim_ts = training_set_sz[strange_alphas[i]]
  train  = Xt_wrapped[,1:dim_ts]
  valid  = Xt_wrapped[,dim_ts+1]
  
  predictor = PPC_KO_2d( X = train,
                         id_CV = "CV_alpha",
                         alpha = alpha,
                         k = 0,
                         threshold_ppc = 0.9,
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
                         err_ret = 0)
  
  prediction = predictor$`One-step ahead prediction`
  pred_PPC_check_alpha[[i]] = list(Prediction = prediction, Alpha = predictor$Alpha, N_PPCs = predictor$`Number of PPCs retained`, Exp_Pow = predictor$`Explanatory power PPCs` )
  
  
  err_PPC_en_check_alpha[i] = sqrt(MLmetrics::MSE(prediction,valid))
  err_PPC_rn_check_alpha[i] = MLmetrics::MAE(prediction,valid)
  
  message <- sprintf(paste0("Check strange alphas","/ Progress: %d/%d
  ") , i, length(strange_alphas))
  setTxtProgressBar(txtProgressBar(min = 1, max = length(strange_alphas), style = 3), i)
  cat("\r", message)
}

