rm(list=ls())
graphics.off()
cat("\014")

set.seed(23032000)
#change here
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_prediction"
source(paste0(dir_w,"/utils/data/far_1_2d.R"))



## Data parameters
dim_grid_x1 <- 10  #dim grid x1
left_ex_x1  <- 0
right_ex_x1 <- 1
dim_grid_x2 <- 10  #dim grid x1
left_ex_x2  <- 0
right_ex_x2 <- 1
sample_size <- 19  #time_instants
burnin      <- 20
{
  # number of basis in each domain
  nbasis.1.sim = 5
  nbasis.2.sim = 5
  
  # total number of tensor product basis
  nbasis.sim = nbasis.1.sim*nbasis.2.sim
  
  # type of basis
  basis.type = "bspline"
  
  # grid on x1 and x2
  x1.grid = seq(from=left_ex_x1, to=right_ex_x1, length=dim_grid_x1)
  x2.grid = seq(from=left_ex_x2, to=right_ex_x2, length=dim_grid_x2)
  
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

#PPCKO parameters
{
  id_CV            <- "NoCV"
  alpha            <- 0.75
  k                <- 0 
  threshold_ppc    <- 0.95
  alpha_vec        <- NULL
  k_vec            <- NULL
  toll             <- 1e-4
  disc_ev_x1       <- NULL
  num_disc_ev_x1   <- dim_grid_x1
  disc_ev_x2       <- NULL
  num_disc_ev_x2   <- dim_grid_x2
  left_extreme_x1  <- left_ex_x1
  right_extreme_x1 <- right_ex_x1
  left_extreme_x2  <- left_ex_x2
  right_extreme_x2 <- right_ex_x2
  err_ret          <- 0
  id_rem_nan       <- NULL
}


## simulate data ----
out <- far_1_2D(n = sample_size, 
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


data_wrapped = PPCKO::data_2d_wrapper_from_list(Xt)

hp   <- PPCKO::KO_check_hps_2d(X = data_wrapped,
                               dim_x1 = dim_grid_x1,
                               dim_x2 = dim_grid_x2)

pred <- PPCKO::PPC_KO_2d(X = data_wrapped,
                         id_CV = id_CV,
                         alpha = alpha,
                         k = k,
                         threshold_ppc = threshold_ppc,
                         alpha_vec = alpha_vec,
                         k_vec = k_vec,
                         toll = toll,
                         disc_ev_x1 = disc_ev_x1,
                         num_disc_ev_x1 = num_disc_ev_x1,
                         disc_ev_x2 = disc_ev_x2,
                         num_disc_ev_x2 = num_disc_ev_x2,
                         left_extreme_x1 = left_ex_x1,
                         right_extreme_x1 = right_ex_x1,
                         left_extreme_x2 = left_ex_x2,
                         right_extreme_x2 = right_ex_x2,
                         err_ret = err_ret,
                         id_rem_nan = id_rem_nan)


PPCKO::KO_show_results_2d(pred,hp)
