rm(list=ls())
graphics.off()
cat("\014")

set.seed(23032000)


#put here the directory of the file install (in this way every load will be coherent with the folders architecture)
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"

#installing packages
source(paste0(dir_w,"/requirements.R"))

#installing PPCKO
devtools::install_github("AndreaEnricoFranzoni/PPCforAutoregressiveOperator", force = TRUE)
library(PPCKO)






source(paste0(dir_w,"/Test_domain1D/Artificial_data/utils/far_1_1d.R"))         #load functions to generate the FAR(1) process
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
  n=100
  
  ##Simulate a stationary FAR(1) process according to a specific kernel
  X.sample <- far_1_1D(kernel_id = id_kernel, noise_id = id_noise, n = n, t.grid = seq(0,1, length.out=10000), a = a, burnin = 20)
}

start_time = Sys.time()
test = PPC_KO(X.sample,"CV",alpha_vec = c(0.0001,0.001,0.01,0.1,1,10),ex_solver=FALSE)
end_time = Sys.time()

as.double(end_time-start_time)