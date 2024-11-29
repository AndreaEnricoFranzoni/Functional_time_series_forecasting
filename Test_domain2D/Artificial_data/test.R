rm(list=ls())
graphics.off()
cat("\014")

set.seed(23032000)
#change here
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series"
source(paste0(dir_w,"/Test_domain2D/Artificial_data/far_1_2d.R"))
source(paste0(dir_w,"/Test_domain2D/Artificial_data/Point_Prediction/CP_point_prediction_2D_FAR_EK.R"))
source(paste0(dir_w,"/Test_domain2D/Artificial_data/Point_Prediction/CP_point_prediction_2D_FAR_oracle.R"))
source(paste0(dir_w,"/Test_domain2D/Artificial_data/Point_Prediction/CP_point_prediction_2D_FAR_VAR.R"))
source(paste0(dir_w,"/Test_domain2D/Artificial_data/Point_Prediction/CP_point_prediction_2D_FARp_oracle.R"))

library(PPCKO)

## Data parameters
sample_size_vec = seq(from=20,to=500,by=2)
N = length(sample_size_vec)
new_sample_size = 1 #dimension of test set (is 1)
burnin      <- 100

left_ex_x1  <- 0
right_ex_x1 <- 1
left_ex_x2  <- 0
right_ex_x2 <- 1
dim_grid_x1 <- 12
dim_grid_x2 <- 12

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
}


# CP algos parameters
{
  b = 1
  l_vec = 0.5*sample_size_vec
  center = TRUE
  detrend = FALSE
  fitted_order = 1
  FPCA_method = "discretization"
  cum_prop_var = NULL
  nharm = 4
  nbasis.1 = NULL
  nbasis.2 = NULL
}

#PPCKO parameters
{
  id_DC            <- "CV"
  alpha            <- 0.1
  k                <- 0 
  threshold_ppc    <- 0.95
  alpha_vec        <- c(1e-3,1e-2,1e-1,1,1e1)
  k_vec            <- NULL
  toll             <- 1e-4
  disc_ev_x1       <- NULL
  disc_ev_x2       <- NULL
  left_extreme_x1  <- left_ex_x1
  right_extreme_x1 <- right_ex_x1
  left_extreme_x2  <- left_ex_x2
  right_extreme_x2 <- right_ex_x2
  num_disc_ev_x1   <- dim_grid_x1
  num_disc_ev_x2   <- dim_grid_x2
  err_ret          <- 0
  id_rem_nan       <- NULL
}




#errors vector
{
  err.dPPC.KO_en <- numeric(N)     #PF:  Kargin-Onatski PPC
  err.dPPC.KO_rn <- numeric(N) 
  err.dEK_en     <- numeric(N)     #EK:  Kokoszka Estimated Kernel
  err.dEK_rn     <- numeric(N)
  err.dEKI_en    <- numeric(N)     #EKI: Kokoszka Estimated Kernel Improved
  err.dEKI_rn    <- numeric(N)
  err.dFAR1Co_en <- numeric(N)     #FAR-Concurrent:
  err.dFAR1Co_rn <- numeric(N)
  err.dFAR1VA_en <- numeric(N)     #FAR-VAr:
  err.dFAR1VA_rn <- numeric(N)
  err.perf_en    <- numeric(N)     #EX:  exact (oracle) prediction
  err.perf_rn    <- numeric(N)
  err.mean_en    <- numeric(N)     #MP:  mean prediction
  err.mean_rn    <- numeric(N)
  err.naive_en   <- numeric(N)     #NP:  naive prediction
  err.naive_rn   <- numeric(N)
}


for (i in 1:length(sample_size_vec)) {
  i=1
  sample_size = sample_size_vec[i]
  l = l_vec[i]
  l=0
  
  ## simulate data ----
  out <- far_1_2D(n = sample_size+new_sample_size, 
                  Psi = Psi, 
                  x1.grid = x1.grid, 
                  x2.grid = x2.grid, 
                  nbasis.1 = nbasis.1.sim, 
                  nbasis.2 = nbasis.2.sim, 
                  burnin = burnin,
                  sigma = my_Sigma,
                  basis.type = basis.type)
  
  # list of matrices: data
  Xt = out$Xt
  
  # data wrapped for CP
  # From list of matrices to list of lists of vectors --------------------------
  Xt_list_of_vec = vector("list", length = sample_size+new_sample_size)
  
  # for each time
  for(t in 1:(sample_size+new_sample_size))
  {
    Xt_list_of_vec[[t]] = vector("list", length = 1)
    Xt_list_of_vec[[t]][[1]] = as.vector(Xt[[t]])
  }
  
  train_dataset_CP  = Xt_list_of_vec[-(sample_size+new_sample_size)]
  test_dataset_CP = Xt_list_of_vec[sample_size+new_sample_size][[1]][[1]]
  
  
  # data wrapped for PPCKO
  data_wrapped = PPCKO::data_2d_wrapper_from_list(Xt)
  training_dataset_KO = data_wrapped[,1:sample_size]
  test_dataset_KO     = data_wrapped[,sample_size+new_sample_size]
  
  
  #KO
  pred_ko = PPCKO::PPC_KO_2d(X = training_dataset_KO,
                             id_CV = "CV",
                             alpha_vec = alpha_vec,
                             disc_ev_x1 = x1.grid,
                             disc_ev_x2 = x2.grid,
                             num_disc_ev_x1 = dim_grid_x1,
                             num_disc_ev_x2 = dim_grid_x2)
  
  
  l=0
  cp_pp_far_ek = CP_point_prediction_2D_FAR_EK(data_y = first_dataset,
                                               l = l,
                                               b = b,
                                               x1.grid = x1.grid,
                                               x2.grid = x2.grid,
                                               FPCA_method = FPCA_method,
                                               basis.type=basis.type,
                                               nbasis.1=nbasis.1.sim,
                                               nbasis.2=nbasis.2.sim,
                                               cum_prop_var = cum_prop_var,
                                               nharm=nharm,
                                               center = center
  )
  
  res_pp_far_ek = cp_pp_far_ek$predicted_fun_n_plus_1[[1]][[1]]
  
  
  
  
  
  # Mean
  print("mean")
  out_mean = sfSapply(x = seeds, 
                      fun = comparison_fun_MSE,
                      sample_size = sample_size,
                      l = l,
                      b = b,
                      # simulation params:
                      x1.grid = x1.grid,
                      x2.grid = x2.grid,
                      burnin = burnin,
                      nbasis.1.sim=nbasis.1.sim,
                      nbasis.2.sim=nbasis.2.sim,
                      basis.type=basis.type,
                      # point prediction params:
                      point_predictor = "mean",
                      # alpha
                      alpha = alpha)
  
  # Naive
  print("naive")
  out_naive = sfSapply(x = seeds, 
                       fun = comparison_fun_MSE,
                       sample_size = sample_size,
                       l = l,
                       b = b,
                       # simulation params:
                       x1.grid = x1.grid,
                       x2.grid = x2.grid,
                       burnin = burnin,
                       nbasis.1.sim=nbasis.1.sim,
                       nbasis.2.sim=nbasis.2.sim,
                       basis.type=basis.type,
                       # point prediction params:
                       point_predictor = "naive",
                       # alpha
                       alpha = alpha)
  
  # Concurrent
  print("concurrent")
  out_concurrent = sfSapply(x = seeds, 
                            fun = comparison_fun_MSE,
                            sample_size = sample_size,
                            l = l,
                            b = b,
                            # simulation params:
                            x1.grid = x1.grid,
                            x2.grid = x2.grid,
                            burnin = burnin,
                            nbasis.1.sim=nbasis.1.sim,
                            nbasis.2.sim=nbasis.2.sim,
                            basis.type=basis.type,
                            # point prediction params:
                            point_predictor = "concurrent",
                            center = center,
                            detrend = detrend,
                            FPCA_method = FPCA_method,
                            cum_prop_var = cum_prop_var,
                            nharm=nharm,
                            nbasis.1 = nbasis.1,
                            nbasis.2 = nbasis.2,
                            # alpha
                            alpha = alpha)
  # EK
  print("EK")
  out_EK = sfSapply(x = seeds, 
                    fun = comparison_fun_MSE,
                    sample_size = sample_size,
                    l = l,
                    b = b,
                    # simulation params:
                    x1.grid = x1.grid,
                    x2.grid = x2.grid,
                    burnin = burnin,
                    nbasis.1.sim=nbasis.1.sim,
                    nbasis.2.sim=nbasis.2.sim,
                    basis.type=basis.type,
                    # point prediction params:
                    point_predictor = "EK",
                    center = center,
                    detrend = detrend,
                    FPCA_method = FPCA_method,
                    cum_prop_var = cum_prop_var,
                    nharm=nharm,
                    nbasis.1 = nbasis.1,
                    nbasis.2 = nbasis.2,
                    # alpha
                    alpha = alpha)
  
  # EK improved
  print("EK_improved")
  out_EK_improved = sfSapply(x = seeds, 
                             fun = comparison_fun_MSE,
                             sample_size = sample_size,
                             l = l,
                             b = b,
                             # simulation params:
                             x1.grid = x1.grid,
                             x2.grid = x2.grid,
                             burnin = burnin,
                             nbasis.1.sim=nbasis.1.sim,
                             nbasis.2.sim=nbasis.2.sim,
                             # point prediction params:
                             point_predictor = "EK_improved",
                             center = center,
                             detrend = detrend,
                             FPCA_method = FPCA_method,
                             cum_prop_var = cum_prop_var,
                             nharm=nharm,
                             nbasis.1 = nbasis.1,
                             nbasis.2 = nbasis.2,
                             basis.type=basis.type,
                             # alpha
                             alpha = alpha)
  
  # VAR on EFPC's
  print("VAR_efpc")
  out_VAR_efpc = sfSapply(x = seeds, 
                          fun = comparison_fun_MSE,
                          sample_size = sample_size,
                          l = l,
                          b = b,
                          # simulation params:
                          x1.grid = x1.grid,
                          x2.grid = x2.grid,
                          burnin = burnin,
                          nbasis.1.sim=nbasis.1.sim,
                          nbasis.2.sim=nbasis.2.sim,
                          # point prediction params:
                          point_predictor = "VAR_efpc",
                          center = center,
                          detrend = detrend,
                          FPCA_method = FPCA_method,
                          cum_prop_var = cum_prop_var,
                          nharm=nharm,
                          nbasis.1 = nbasis.1,
                          nbasis.2 = nbasis.2,
                          basis.type=basis.type,
                          # alpha
                          alpha = alpha)
  
  # oracle
  print("oracle")
  out_oracle = CP_point_prediction_2D_FAR_oracle(data_y=Xt_list_of_vec,
                                                 indexes_not_NA=NULL,
                                                 l=0,
                                                 b=1,
                                                 seed_split=simulation_seed,
                                                 x1.grid=x1.grid,
                                                 x2.grid=x2.grid,
                                                 nbasis.1=nbasis.1.sim,
                                                 nbasis.2=nbasis.2.sim,
                                                 basis.type=basis.type,
                                                 PSI=Psi1)
  
}













# Droppo l'ultimo tempo:





#parameters for algorithm




