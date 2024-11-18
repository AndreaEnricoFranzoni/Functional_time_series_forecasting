
#'
#' Conformal prediction bands for functional time series. 
#' 
#' Pointwise prediction function.
#' 
#' Prediction method: FAR nonconcurrent oracle predictor, can be used if we 
#'                    knows the actual autoregressive operator, eg
#'                    if one simulates data.
#' 
#' The following function provides pointwise prediction functions, evaluated on the time grid.
#' It returns the prediction ("predicted_fun") for the observations in the training set, for one observation every "b" on the calibration set,
#' i.e. on the observations on which we are going to compute NC scores, and also for the (n+1) observation ("predicted_fun_n_plus_one")
#' 
#' WORKS ONLY WITH A FAR(1), FAR(p) is straightforward to implement, but still to do.
#' 

CP_point_prediction_2D_FAR_oracle = function(data_y,
                                               indexes_not_NA=NULL,
                                               l, 
                                               b, 
                                               seed_split=FALSE,
                                               x1.grid=NULL, 
                                               x2.grid=NULL, 
                                               nbasis.1=NULL,
                                               nbasis.2=NULL,
                                               basis.type=NULL,
                                               PSI)
{
  
  # CHECKS AND INITIALIZATION --------------------------------------------------
  
  if (is.null(seed_split)==TRUE || (seed_split!=FALSE & is.numeric(seed_split)==FALSE)) stop("Argument 'seed_split' must be either FALSE or an integer.")
  if(is.list(data_y)==FALSE || is.data.frame(data_y)==TRUE || is.list(data_y[[1]])==FALSE || is.data.frame(data_y[[1]])==TRUE){ #le prime due condizioni son per assicurarsi che sia una lista nel senso di "list" e non un dataframe (che ? una lista tecnicamente), 
    
    stop("data_y must be a list of lists. Specifically, data_y must be a list of 'n' lists. Each of the 'n' lists must be made up of 'p' lists. Each of the 'p' lists must contain a numeric vector expressing the evaluation of the function on a grid (whose length can be different in the 'p' dimensions).
         'n' (i.e. the sample size) must be greater or equal than 2. 'p'(i.e. the dimension of the multivariate function ) must be the same for all the multivariate functions.")} else{
           n=length(data_y) 
           p=length(data_y[[1]])
           grid_size=vapply(data_y[[1]],function(x) length(x),integer(1))
           
         }
  
  if (b<=0 || (b %% 1) !=0) stop("'b' must be a positive integer.")
  if (n <2) stop("'n'(i.e. the sample size) must be greater or equal than 2.")
  if (length(unique(vapply(data_y,length,integer(1))))!=1) stop("'p'(i.e. the dimension of the multivariate function) must be the same for all the multivariate functions.")
  if(!(all(apply(t(vapply(data_y,function(x) vapply(x,length,integer(1)),integer(p))),2, function(y) length(unique(y))==1)))) stop("The 'n' functions must be evaluated on the same p-variate grid. The grid can vary between the p domains.")
  
  if ((l+1) %% b !=0 || (l+1)/b-1 <=0 ) stop("(l+1)/b must be an integer >=2.")
  
  # DIVISION TRAINING-CALIBRATION ----------------------------------------------
  fitted_order=1
  burn_in=fitted_order
  
  m=n-l-burn_in
  
  if(seed_split!=FALSE){set.seed(seed_split)}
  training=sample((burn_in+1):n,m)
  training=sort(training)
  calibration=setdiff((burn_in+1):n,training)
  calibration=sort(calibration)
  excluded_set=1:burn_in
  
  # COMPUTATION ----------------------------------------------------------------
  
  ## 1 -- From list to matrix --------------------------------------------------
  
  # total number of points in the grid
  if(is.null(indexes_not_NA)){
    n_points = length(x1.grid) * length(x2.grid)
  } else {
    n_points = sum(indexes_not_NA)
  }
  
  # Vectorize data
  Xt_mat = vapply(simplify2array(data_y, as.numeric), as.numeric, numeric(length=n_points)) # n_points x n
  
  Xt_mat.train = Xt_mat[,training]
  Xt_mat.calibration.burnin = Xt_mat[,-training]
  
  ## 2 -- Tensor product basis -------------------------------------------------
  
  # total number of basis functions
  nbasis = nbasis.1*nbasis.2
  
  # if no grid is provided, I use [0,1]x[0,1]
  if(is.null(x1.grid))
    x1.grid <- seq(0, 1, by=0.05)
  if(is.null(x2.grid))
    x2.grid <- seq(0, 1, by=0.05)
  
  # build 1D basis functions on each of the 1D domains
  if(basis.type == "fourier"){
    basis.1 <- fda::create.fourier.basis(rangeval=range(x1.grid), nbasis=nbasis.1)
    basis.2 <- fda::create.fourier.basis(rangeval=range(x2.grid), nbasis=nbasis.2)
  } else if(basis.type == "bspline"){
    basis.1 <- fda::create.bspline.basis(rangeval=range(x1.grid), nbasis=nbasis.1)
    basis.2 <- fda::create.bspline.basis(rangeval=range(x2.grid), nbasis=nbasis.2)
  }
  
  # evaluate 1D basis functions on the grid
  basis.1.eval <- fda::eval.basis(evalarg=x1.grid, basisobj=basis.1)
  basis.2.eval <- fda::eval.basis(evalarg=x2.grid, basisobj=basis.2)
  
  # 2D basis functions evaluated on the grid
  basis.grid.eval <- vector("list", length = nbasis)
  grid.2D <- expand.grid(x1.grid, x2.grid)
  counter = 1
  
  # considero ogni combinazione di basi
  for(i in 1:nbasis.1)
  {
    base_i_j = matrix(nrow=length(x1.grid),ncol=length(x2.grid))
    for(j in 1:nbasis.2)
    {
      # tensor product (ma sui punti della griglia, ? li stess)
      base_i_j = outer(basis.1.eval[,i],basis.2.eval[,j])
      
      basis.grid.eval[[counter]] = base_i_j
      counter = counter+1
    }
  }
  rm(counter,i,j)
  
  # Vectorize basis
  if(is.null(indexes_not_NA)){
    basis_eval = vapply(basis.grid.eval, as.numeric, numeric(length=n_points)) # n_points x nbasis
  } else {
    basis_eval = vapply(basis.grid.eval, function(x) {x[indexes_not_NA]}, numeric(length=n_points)) # n_points x nbasis
  }
  
  ## 3 -- Project data on basis ------------------------------------------------
  
  B = crossprod(basis_eval)
  X = crossprod(basis_eval, Xt_mat)
  C = solve(B,X) # nbasis x n
  
  ## 4 -- Prediction initialization --------------------------------------------
  
  # Inizializzo la matrice che conterr? le previsioni per il training e calibration set
  predicted_fun_matrix=matrix(0,m+l+burn_in,n_points)
  predicted_fun_matrix[excluded_set,]=NA #no previsione per le osservazioni del "burn-in"
  
  ## 5 -- Predict training set -------------------------------------------------
  
  for (i in training){
    predicted_fun_matrix[i,] = as.numeric(basis_eval %*% PSI %*% C[,i-1])
  } 
  rm(i)
  
  ## 6 -- Predict calibration set ----------------------------------------------
  
  # Dal calibration, ottengo gli indici da cui calcolo i NCS (..._included) 
  # e quelli esclusi in accordo al parametro b (..._excluded)
  
  obs_calibration_included=sort(calibration)[seq(from=b,to=l,by=b)] #saranno (l+1)/b-1
  obs_calibration_excluded=setdiff(calibration,obs_calibration_included)
  
  # Calcolo le previsioni per gli '(l+1)/b-1' elementi del calibration, 
  # negli altri l-[(l+1)/b-1] ci metter? NA
  
  if(length(obs_calibration_excluded)!=0)  predicted_fun_matrix[obs_calibration_excluded,]=NA
  
  for (i in obs_calibration_included){
    predicted_fun_matrix[i,] = as.numeric(basis_eval %*% PSI %*% C[,i-1])
  } 
  rm(i)
  
  ## 7 -- Predict (n+1) time ---------------------------------------------------
  
  predicted_fun_n_plus_1 = as.numeric(basis_eval %*% PSI %*% C[,n])
  #print(paste0("length of prediction: ", length(predicted_fun_n_plus_1)))
  #print(paste0("dimension: ", dim(predicted_fun_n_plus_1)[1], "x", dim(predicted_fun_n_plus_1)[2]))
  
  ## 8 -- From matrix to list --------------------------------------------------
  
  predicted_fun=NULL
  for (i in 1:n){
    predicted_fun=c(predicted_fun,list(list(predicted_fun_matrix[i,])))
  }
  rm(predicted_fun_matrix)
  
  predicted_fun_n_plus_1=list(list(predicted_fun_n_plus_1))
  
  
  # OUTPUT----------------------------------------------------------------------
  
  return(structure(.Data=list(data_y,
                              predicted_fun, 
                              n,
                              burn_in,
                              m,
                              l,
                              b,
                              excluded_set,
                              training,
                              calibration,
                              obs_calibration_included,
                              obs_calibration_excluded,
                              seed_split,
                              fitted_order,
                              predicted_fun_n_plus_1),
                   names=c("data_y",
                           "predicted_fun",
                           "n",
                           "burn_in",
                           "m",
                           "l",
                           "b",
                           "excluded_set",
                           "training",
                           "calibration",
                           "obs_calibration_included",
                           "obs_calibration_excluded",
                           "seed_split",
                           "fitted_order",
                           "predicted_fun_n_plus_1")))
}



