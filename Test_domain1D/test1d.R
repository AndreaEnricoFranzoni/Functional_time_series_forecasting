rm(list=ls())
graphics.off()
cat("\014")

set.seed(23032000)

#change here 
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series"
source(paste0(dir_w,"/Test_domain1D/Artificial_data/utils/far_1_1d.R"))
source(paste0(dir_w,"/Test_domain1D/Artificial_data/utils/functions.R"))
source(paste0(dir_w,"/Test_domain1D/Artificial_data/utils/prediction_error.R"))



## Data parameters 
left_ex            <- 0                                           #left extreme of the domain of the functional data
right_ex           <- 1                                           #right extreme of the domain of the functional data
dim_grid           <- 200                                         #number of discrete evaluations of the functional statistical units
n                  <- 100                                         #time instants of the functional time series
burnin             <- 50                                          #burnin iterations for FAR(1)
N                  <- n - burnin                                  #instants that will actually be taken into account  
t.grid             <- seq(left_ex,right_ex, length.out=dim_grid)  #grid for the discrete evaluation of the statistical units

#PPCKO parameters 
{
  id_CV_ko      <- "CV_alpha"           #Ko algorithm
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

#data (only four kernels, two norm, three errors implemented)
{
  #feats of data
  id_kernel <- "sp_s"       #way of generating data ("gaussian", "identity", "sp_t" and "sp_s" kernel available)
  norm      <- 0.8          #norm of the Kernel (that has to be <1)
  id_noise  <- "1"          #error of the FAR(1) process ("1", "2" or "3")
  
  #grid for the kernel for generating FAR(1)
  s.grid <- seq(0,1, length.out=dim_grid)
  grid   <- expand.grid(t.grid, s.grid)
}

# ----- data generation -----
proc = feat_far_1_process(id_kernel,norm)
id_kernel   <- proc$kernel
a           <- proc$constant
name_kernel <- proc$name

##Simulate a stationary FAR(1) process according to a specific kernel
X.sample <- far_1_1D(kernel_id = id_kernel, noise_id = id_noise, n = n, t.grid = t.grid, a = a, burnin = burnin)


test_1d_hp = PPCKO::KO_check_hps(X.sample)
test1d     = PPCKO::PPC_KO(X = X.sample,
                           id_CV = id_CV_ko,
                           alpha = alpha,
                           k = k,
                           threshold_ppc = threshold_ppc,
                           alpha_vec = alpha_vec,
                           k_vec = k_vec,
                           toll = toll,
                           disc_ev = t.grid,
                           left_extreme = left_ex,
                           right_extreme = right_ex,
                           err_ret = 1)


PPCKO::KO_show_results(test1d,test_1d_hp)
