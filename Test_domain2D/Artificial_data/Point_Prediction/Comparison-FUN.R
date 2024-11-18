
# SIMULATION FUNCTION ----------------------------------------------------------

comparison_fun_MSE = function(simulation_seed,
                              sample_size,
                              l,
                              b,
                              # simulation params:
                              x1.grid,
                              x2.grid,
                              burnin,
                              nbasis.1.sim=NULL,
                              nbasis.2.sim=NULL,
                              basis.type=NULL,
                              # point prediction params:
                              point_predictor,
                              cum_prop_var=NULL,
                              nharm=NULL,
                              center=NULL,
                              detrend=FALSE,
                              FPCA_method=NULL,
                              nbasis.1=NULL,
                              nbasis.2=NULL,
                              # alpha
                              alpha)
{
  
  # FAR-2D simulation function
  source("D:/Poli/TESI/Code/Time-Series-CP/FAR_2D/Simulation/simulate_FAR.R")
  
  # point predictors:
  source("D:/Poli/TESI/Code/Time-Series-CP/FAR_2D/Point_Prediction/CP_point_prediction_2D_FAR_EK.R")
  source("D:/Poli/TESI/Code/Time-Series-CP/FAR_1D/Point_Prediction/CP_point_prediction_mean.R")
  source("D:/Poli/TESI/Code/Time-Series-CP/FAR_1D/Point_Prediction/CP_point_prediction_naive.R")
  source("D:/Poli/TESI/Code/Time-Series-CP/FAR_1D/Point_Prediction/CP_point_prediction_FAR_nc_VAR.R")
  source("D:/Poli/TESI/Code/Time-Series-CP/FAR_1D/Point_Prediction/CP_point_prediction_FAR_concurrent.R")
  source('D:/Poli/TESI/Code/Time-Series-CP/FAR_2D/Point_Prediction/CP_point_prediction_2D_FAR_oracle.R')
  
  # other functions:
  source("D:/Poli/TESI/Code/Time-Series-CP/functions_scen2sim.R")
  
  # check
  if( !(point_predictor %in% c("mean","naive","concurrent","EK","EK_improved","VAR_efpc","oracle")) )
  {
    stop("Provide a valid point predictor")
  }
  
  # Data -----------------------------------------------------------------------
  
  # total number of basis functions for the simulation (tensor product basis)
  nbasis.sim = nbasis.1.sim*nbasis.2.sim
  
  # Parameters
  sample_size = sample_size
  new_sample_size = 1
  
  ## FAR(1) non-concurrent parameters ----
  
  # d = dimension of the underlying VAR(1)
  d=nbasis.sim 
  Psi1=matrix(0.3,d,d)
  diag(Psi1)=0.8
  Psi1=Psi1/norm(Psi1, type = "F")*0.9
  Psi=array(0,c(d,d,1))
  Psi[,,1]=Psi1
  my_Sigma=matrix(0.6,d,d)
  diag(my_Sigma)=1
  my_Sigma=my_Sigma/2
  
  # set seed
  set.seed(simulation_seed)
  
  ## simulate data ----
  out <- simulate_FAR(
    n = (sample_size+new_sample_size), 
    Psi = Psi, 
    x1.grid = x1.grid, 
    x2.grid = x2.grid, 
    nbasis.1 = nbasis.1.sim, 
    nbasis.2 = nbasis.2.sim, 
    sigma = my_Sigma,
    basis.type = basis.type,
    quiet = TRUE
  )
  
  # list of matrices
  Xt_list = out$Xt
  
  rm(Psi, my_Sigma, d)
  
  # From list of matrices to list of lists of vectors --------------------------
  Xt_list_of_vec = vector("list", length = sample_size+new_sample_size)
  
  # for each time
  for(t in 1:(sample_size+new_sample_size))
  {
    Xt_list_of_vec[[t]] = vector("list", length = 1)
    Xt_list_of_vec[[t]][[1]] = as.vector(Xt_list[[t]])
  }
  
  # Droppo l'ultimo tempo:
  first_dataset  = Xt_list_of_vec[-(sample_size+new_sample_size)]
  second_dataset = Xt_list_of_vec[sample_size+new_sample_size]
  
  # di solito il mean centering lo faccio con la rappresentazione matriciale
  # qui lo sto evitando perch? ? un casino
  
  # Point prediction  ----------------------------------------------------------
  
  ## Mean ----
  if(point_predictor == "mean")
  {
    # Initialize computational times
    start_time <- Sys.time()
    
    # Point prediction
    point_prediction = CP_point_prediction_mean(data_y=first_dataset, 
                                                l=l, 
                                                b=b, 
                                                seed_split=simulation_seed,
                                                center=center)
    # Save computation times
    end_time <- Sys.time()
    computational_time = as.double(end_time - start_time)
  }
  
  
  ## Naive ----
  if(point_predictor == "naive")
  {
    # Initialize computational times
    start_time <- Sys.time()
    
    # Point prediction
    point_prediction = CP_point_prediction_naive(data_y=first_dataset, 
                                                 l=l, 
                                                 b=b, 
                                                 seed_split=simulation_seed)
    # Save computation times
    end_time <- Sys.time()
    computational_time = as.double(end_time - start_time)
  }
  
  ## concurrent ----
  if(point_predictor == "concurrent")
  {
    # Initialize computational times
    start_time <- Sys.time()
    
    # Point prediction
    point_prediction = CP_point_prediction_concurrent(data_y=first_dataset, 
                                                      l=l, 
                                                      b=b, 
                                                      fitted_order=1,
                                                      seed_split=simulation_seed, 
                                                      center=center)
    # Save computation times
    end_time <- Sys.time()
    computational_time = as.double(end_time - start_time)
  }
  
  ## EK ----
  if(point_predictor == "EK")
  {
    # Initialize computational times
    start_time <- Sys.time()
    
    # Point prediction
    point_prediction = CP_point_prediction_2D_FAR_EK(data_y=first_dataset, 
                                                     indexes_not_NA=NULL,
                                                     l=l,
                                                     b=b,
                                                     seed_split=simulation_seed,
                                                     x1.grid=x1.grid, 
                                                     x2.grid=x2.grid, 
                                                     # FPCA:
                                                     FPCA_method=FPCA_method,
                                                     cum_prop_var=cum_prop_var,
                                                     nharm=nharm,
                                                     nbasis.1=nbasis.1,
                                                     nbasis.2=nbasis.2,
                                                     # other things:
                                                     center=center,
                                                     EK_improved=FALSE)
    # Save computation times
    end_time <- Sys.time()
    computational_time = as.double(end_time - start_time)
    computational_time = 1
  }
  
  ## EK improved ----
  if(point_predictor == "EK_improved")
  {
    # Initialize computational times
    start_time <- Sys.time()
    
    # Point prediction
    point_prediction = CP_point_prediction_2D_FAR_EK(data_y=first_dataset, 
                                                     indexes_not_NA=NULL,
                                                     l=l,
                                                     b=b,
                                                     seed_split=simulation_seed,
                                                     x1.grid=x1.grid, 
                                                     x2.grid=x2.grid, 
                                                     # FPCA:
                                                     FPCA_method=FPCA_method,
                                                     cum_prop_var=cum_prop_var,
                                                     nharm=nharm,
                                                     nbasis.1=nbasis.1,
                                                     nbasis.2=nbasis.2,
                                                     # other things:
                                                     center=center,
                                                     EK_improved=TRUE)
    # Save computation times
    end_time <- Sys.time()
    computational_time = as.double(end_time - start_time)
  }
  
  
  ## VAR(1) on EFPC's ----
  if(point_predictor == "VAR_efpc")
  {
    # Initialize computational times
    start_time <- Sys.time()
    
    # Point prediction
    point_prediction = CP_point_prediction_FAR_nc_VAR(data_y=first_dataset, 
                                                      l=l, 
                                                      b=b, 
                                                      seed_split=simulation_seed,
                                                      grid=my_grid,
                                                      # lag order:
                                                      fitted_order=1,
                                                      # FPCA:
                                                      FPCA_method=FPCA_method,
                                                      cum_prop_var=cum_prop_var,
                                                      nharm=nharm,
                                                      nbasis=nbasis,
                                                      center=center)
    # Save computation times
    end_time <- Sys.time()
    computational_time = as.double(end_time - start_time)
  }
  
  ## oracle ----
  if(point_predictor == "oracle")
  {
    # Initialize computational times
    start_time <- Sys.time()
    
    # Point prediction
    point_prediction = CP_point_prediction_2D_FAR_oracle(data_y=first_dataset,
                                                         indexes_not_NA=NULL,
                                                         l=l,
                                                         b=b,
                                                         seed_split=simulation_seed,
                                                         x1.grid=x1.grid,
                                                         x2.grid=x2.grid,
                                                         nbasis.1=nbasis.1.sim,
                                                         nbasis.2=nbasis.2.sim,
                                                         basis.type=basis.type,
                                                         PSI=Psi1)
    # Save computation times
    end_time <- Sys.time()
    computational_time = as.double(end_time - start_time)
  }
  
  
  # Output  --------------------------------------------------------------------
  
  library(MLmetrics)
  y_next = point_prediction$predicted_fun_n_plus_1
  y_next_pred = y_next[[1]][[1]]
  y_next_true = second_dataset[[1]][[1]]
  
  mse = MSE(y_next_pred, y_next_true)
  
  return(mse)
#  
#  my_list=list(
#    conformal_TS$average_width,
#    computational_time,
#    prediction_set$inclusion
#  )
#  
#  my_names=c(
#    "average_width",
#    "computational_time",
#    "inclusion"
#  )
#  
#  return(structure(.Data=my_list, names=my_names))
 
}
