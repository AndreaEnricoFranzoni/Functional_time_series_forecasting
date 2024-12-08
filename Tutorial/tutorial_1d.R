rm(list=ls())
graphics.off()
cat("\014")


# library for the package
library(PPCKO)

# upload the data from the package
data("data_1d")



## Data parameters 
left_ex            <- 0                                           #left extreme of the domain of the functional data
right_ex           <- 1                                           #right extreme of the domain of the functional data
dim_grid           <- dim(data_1d)[1]                             #number of discrete evaluations of the functional statistical units
n                  <- dim(data_1d)[2]                             #time instants of the functional time series
t.grid             <- seq(left_ex,right_ex, length.out=dim_grid)  #grid for the discrete evaluation of the statistical units

#PPCKO parameters 
{
  id_CV_ko      <- "CV_alpha"     #Ko algorithm
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


# obtain pointwise pvalues fo ADF 
test_1d_hp = KO_check_hps(data_1d)


# PPC KO
test1d     = PPC_KO(X = data_1d,
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

# plot results
KO_show_results(test1d,test_1d_hp)
