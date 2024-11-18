#da qui veniva fuori il tempo in realtà

rm(list=ls())
graphics.off()
cat("\014")

set.seed(23032000)
#change here
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series"
source(paste0(dir_w,"/Test_domain2D/Artificial_data/far_1_2d.R"))

#dimensions
dimension_sr_grid <- c(10,20,30,40,50,100)
dimension_ti      <- c(19,49,99,499)

{
  
  m_loop = length(dimension_sr_grid)
  n_loop = length(dimension_ti)
  total_iterations <- m_loop * n_loop
  
  times_NoCV    <- matrix(nrow=length(dimension_sr_grid),ncol=length(dimension_ti))
  times_NoCV <- as.data.frame(times_NoCV)
  rownames(times_NoCV) = as.character(dimension_sr_grid^2)
  colnames(times_NoCV) = as.character(dimension_ti)
  
  times_CValpha    <- matrix(nrow=length(dimension_sr_grid),ncol=length(dimension_ti))
  times_CValpha <- as.data.frame(times_CValpha)
  rownames(times_CValpha) = as.character(dimension_sr_grid^2)
  colnames(times_CValpha) = as.character(dimension_ti)
  
  times_CVk    <- matrix(nrow=length(dimension_sr_grid),ncol=length(dimension_ti))
  times_CVk <- as.data.frame(times_CVk)
  rownames(times_CVk) = as.character(dimension_sr_grid^2)
  colnames(times_CVk) = as.character(dimension_ti)
  
  times_CV    <- matrix(nrow=length(dimension_sr_grid),ncol=length(dimension_ti))
  times_CV <- as.data.frame(times_CV)
  rownames(times_CV) = as.character(dimension_sr_grid^2)
  colnames(times_CV) = as.character(dimension_ti)
}



## Data parameters
left_ex_x1  <- 0
right_ex_x1 <- 1
left_ex_x2  <- 0
right_ex_x2 <- 1
burnin      <- 20
{
  # number of basis in each domain
  nbasis.1.sim = 5
  nbasis.2.sim = 5
  
  # total number of tensor product basis
  nbasis.sim = nbasis.1.sim*nbasis.2.sim
  
  # type of basis
  basis.type = "bspline"
  
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
  alpha            <- 0.1
  k                <- 0 
  threshold_ppc    <- 0.95
  alpha_vec        <- NULL
  k_vec            <- NULL
  toll             <- 1e-4
  disc_ev_x1       <- NULL
  disc_ev_x2       <- NULL
  left_extreme_x1  <- left_ex_x1
  right_extreme_x1 <- right_ex_x1
  left_extreme_x2  <- left_ex_x2
  right_extreme_x2 <- right_ex_x2
  err_ret          <- 0
  id_rem_nan       <- NULL
}


stop=1

##########################
### Simulations KO NoCV ##
##########################
{
counter <- 0

for(dim_g in 1:m_loop){
    
  dim_grid_x1 <- dimension_sr_grid[dim_g]  #dim grid x1
  dim_grid_x2 <- dimension_sr_grid[dim_g]  #dim grid x2
  # grid on x1 and x2
  x1.grid = seq(from=left_ex_x1, to=right_ex_x1, length=dim_grid_x1)
  x2.grid = seq(from=left_ex_x2, to=right_ex_x2, length=dim_grid_x2)
    
  num_disc_ev_x1   <- dim_grid_x1
  num_disc_ev_x2   <- dim_grid_x2
    
  for(t in 1:n_loop){
      
    sample_size <- dimension_ti[t]  #time_instants
      
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
      
      #KO
      time_start = Sys.time()
      pred <- PPCKO::PPC_KO_2d(X = data_wrapped,
                               id_CV = "NoCV",
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
      time_end = Sys.time()
      
      times_NoCV[dim_g,t] = as.double(time_end - time_start)
      

      #counter <- counter + 1
      #message <- sprintf("No CV test: Grid dimension = %d, Number of time instants = %d / Progress: %d/%d ", dim_grid_x1*dim_grid_x2, sample_size, counter, total_iterations)
      #setTxtProgressBar(txtProgressBar(min = 0, max = total_iterations, style = 3), counter)
      #cat("\r", message)
    }
  }
}

file_saving_times_KO_NoCV = paste0(dir_w,"/Test_domain2D/Artificial_data/times_KO_NoCV.Rdata")
save(times_NoCV, file = file_saving_times_KO_NoCV)


load(file_saving_times_KO_NoCV)