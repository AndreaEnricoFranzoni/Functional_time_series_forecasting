
#'
#' Conformal prediction bands for functional time series. 
#' 
#' Pointwise prediction function.
#' 
#' Prediction method: FAR non-concurrent, estimated through EK method (or EK_improved is specified).
#' 
#' 
#' The following function provides pointwise prediction functions, evaluated on the time grid.
#' It returns the prediction ("predicted_fun") for the observations in the training set, for one observation every "b" on the calibration set,
#' i.e. on the observations on which we are going to compute NC scores, and also for the (n+1) observation ("predicted_fun_n_plus_one")
#' 
#' 
#' NOTA:
#'       indexes_not_NA (se specificato) deve essere una matrice di dimensioni length(x1.grid) x length(x2.grid), indica dove la funzione ? definita.
#'       ? utile se lavoriamo con dati che non sono definiti su un rettangolo, ma su un dominio particolare-
#'       
#'       - se indexes_not_NA non ? specificato, allora le funzioni vengono assunte definite su tutto il dominio x1.grid \times x2.grid.
#'         Il numero totale di puti sar? n_points=length(x1.grid)*length(x2.grid)
#'       
#'       - se invece indexes_not_NA ? specificato, allora le funzioni vengono considerate solo sulla griglia definita su indexes_not_NA
#'         In tal caso alcune modifiche avvengono:
#'         1) le funzioni vengono valutate sulo su indexes_not_NA
#'         2) n_points = sum(indexes_not_NA) anzich? length(x1.grid)*length(x2.grid)
#'         3) basis_eval viene valutata sulla griglia e tiene poi solo gli elementi non NA
#' 
#'
#' @param data_y: list of n lists, where n is the length of the time series. Each of the 'n' lists must be made up of 'p' lists. Each of the Each of the 'p' lists must contain a numeric vector expressing the evaluation of the function on a grid (whose length can be different in the 'p' dimensions)
#' @param indexes_not_NA: indexes of the two-dimensional grid where the functions are not NA
#' @param l: size of the calibration set
#' @param b: block size
#' @param seed_split: it must be either FALSE or an integer. It determines the seed of the split of the data into training set and calibration set
#' @param x1.grid: grid of the first 1D domain
#' @param x2.grid: grid of the second 1D domain
#' @param FPCA_method: method to use for FPCA, either "discretization" or "basis"
#' @param basis.type: meaningful only if if FPCA_method=="basis, either "fourier" or "bspline"
#' @param grid.step.num.int: meaningful only if if FPCA_method=="basis, it is the grid step for numerical integration of the matrix W
#' @param nbasis.1: meaningful only if if FPCA_method=="basis", select here the number of basis to use in the first 1D grid
#' @param nbasis.2: meaningful only if if FPCA_method=="basis", select here the number of basis to use in the second 1D grid
#' @param cum_prop_var: set this parameter if you want to select the numbers of FPC's by means of cumulative proportion of variance, this should be a number in (0,1]
#' @param nharm: set this parameter if you want to manually fix the numbers of FPC's. They will be fixed equal to nharm.
#' @param center: should we center functions? If TRUE, data are centered around the training set mean function.
#' @param EK_improved: logical, should we use the improved estimation techniques?
#' 
#' @return a list with members:
#'         data_y
#'         predicted_fun
#'         n
#'         burn_in
#'         m
#'         l
#'         b
#'         excluded_set
#'         training
#'         calibration
#'         obs_calibration_included
#'         obs_calibration_excluded
#'         seed_split
#'         predicted_fun_n_plus_1
#'

EK_pred_2D = function(data_y,
                                         indexes_not_NA=NULL,
                                         l, 
                                         b, 
                                         seed_split=FALSE,
                                         x1.grid=NULL, 
                                         x2.grid=NULL, 
                                         # FPCA:
                                         FPCA_method="discretization",
                                         basis.type="bspline",
                                         grid.step.num.int=0.001,
                                         nbasis.1=NULL,
                                         nbasis.2=NULL,
                                         cum_prop_var=NULL,
                                         nharm=NULL,
                                         # other things:
                                         center=FALSE,
                                         EK_improved=FALSE)
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
  fitted_order=2 # TODO: change!!!
  burn_in=fitted_order
  
  m=n-l-burn_in
  
  #print(paste0("n=", n))
  #print(paste0("m (train)=", m))
  #print(paste0("l (calib)=", l))
  #print(paste0("burn_in =", burn_in))
  
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
  
  ## 2 -- Mean centering -------------------------------------------------------
  
  if(center)
  {
    # mean is estimated on the training set
    mean.train = rowMeans(Xt_mat.train)
    
    Xt_mat.train = sweep(Xt_mat.train, 1, mean.train)
    Xt_mat.calibration.burnin = sweep(Xt_mat.calibration.burnin, 1, mean.train)
  }
  
  ## 3 -- FPCA -----------------------------------------------------------------
  
  if(!(FPCA_method %in% c("discretization","basis"))){
    stop("invalid method for FPCA. Please select one between discretization and basis")
  }
  
  if(FPCA_method == "discretization")
  {
    ### Discretization ---------------------------------------------------------
    
    # discretization step
    w = 1/(n_points-1)
    
    # pca
    pca = prcomp(t(Xt_mat.train), center=FALSE, scale.=FALSE)
    
    # eigenvalues of FPCA
    lambda = (pca$sdev^2) * w
    
    # number of EFPC's to consider
    if(is.null(nharm) & !is.null(cum_prop_var))
    {
      # choose the number of harmonics by cumulative proportion of variance
      nharm = as.integer(which.max(cumsum(lambda)/sum(lambda)>cum_prop_var))
      
      #print(paste0("nharm=",nharm))
      #print(cumsum(lambda)/sum(lambda))
    }
    
    # consider the first nharm eigenvalues
    lambda = lambda[1:nharm]
    
    # FPC's evaluated on the time grid
    efpc_eval = pca$rotation[,1:nharm] / sqrt(w) # length(grid) x nharm
    
    # projection of training data on FPC's
    scores.train = pca$x[,1:nharm] * sqrt(w) # n x nharm
    
    rm(pca)
    
  } else if(FPCA_method == "basis")
  {
    ### Basis ------------------------------------------------------------------
    
    #### Tensor product basis definition ---------------------------------------
    
    # total number of basis functions
    nbasis = nbasis.1*nbasis.2
    
    # build 1D basis functions on each of the 1D domain
    if(basis.type == "bspline")
    {
      basis.1 <- fda::create.bspline.basis(rangeval=range(x1.grid), nbasis=nbasis.1)
      basis.2 <- fda::create.bspline.basis(rangeval=range(x2.grid), nbasis=nbasis.2)
    } else if(basis.type == "fourier")
    {
      basis.1 <- fda::create.fourier.basis(rangeval=range(x1.grid), nbasis=nbasis.1)
      basis.2 <- fda::create.fourier.basis(rangeval=range(x2.grid), nbasis=nbasis.2)
    }
    
    # evaluate 1D basis functions on the grid
    basis.1.eval <- fda::eval.basis(evalarg=x1.grid, basisobj=basis.1)
    basis.2.eval <- fda::eval.basis(evalarg=x2.grid, basisobj=basis.2)
    
    # 2D basis functions evaluated on the grid
    basis.grid.eval <- vector("list", length = nbasis)
    grid.2D <- expand.grid(x1.grid, x2.grid)
    counter = 1
    
    # tensor product basis construction
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
    
    #### Project training set on the basis -------------------------------------
    
    B = crossprod(basis_eval)
    X = crossprod(basis_eval, Xt_mat.train)
    C = solve(B,X) # nbasis x n
    
    #### Compute W -------------------------------------------------------------
    
    if(basis.type == "fourier")
    {
      W = diag(nbasis)
    } else if(basis.type == "bspline")
    {
      # in order to obtain a better approximation of W, we resort to numerical 
      # integration on a more dense grid
      
      # Let's define a dicretization grid for numerical integration
      x1.discretized = seq(0,1,by=grid.step.num.int)
      x2.discretized = seq(0,1,by=grid.step.num.int)
      
      # n_points of dicretization grid
      n_points.discretized = length(x1.discretized) * length(x2.discretized)
      
      # evaluate 1D basis on such grid
      basis.1.eval.discretized <- fda::eval.basis(evalarg=x2.discretized, basisobj=basis.1)
      basis.2.eval.discretized <- fda::eval.basis(evalarg=x2.discretized, basisobj=basis.2)
      
      # 2D basis functions evaluated on the bidimensional grid
      basis.grid.eval.discretized <- vector("list", length = nbasis)
      grid.2D.discretized <- expand.grid(x1.discretized, x2.discretized)
      counter = 1
      
      # considero ogni combinazione di basi
      for(i in 1:nbasis.1){
        base_i_j = matrix(nrow=length(x1.discretized),ncol=length(x2.discretized))
        for(j in 1:nbasis.2){
          # outer product sui punti della griglia
          base_i_j = outer(basis.1.eval.discretized[,i],basis.2.eval.discretized[,j])
          basis.grid.eval.discretized[[counter]] = base_i_j
          counter = counter+1
        }
      }
      rm(counter)
      
      # Vectorize basis
      basis_eval.discretized = vapply(basis.grid.eval.discretized, as.numeric, numeric(length=n_points.discretized))
      
      # 2D discretization square
      w_discretized = 1/( (length(x1.discretized)-1) * (length(x2.discretized)-1))
      W_discretized = crossprod(basis_eval.discretized, basis_eval.discretized) * w_discretized
      
      # W matrix
      W = W_discretized
    }
    
    #### Eigenproblem ----------------------------------------------------------
    
    sample_size = length(training)
    
    # solve matricial eigenproblem with different variables
    eigen_dec = eigen(expm::sqrtm(W) %*% tcrossprod(C)%*% expm::sqrtm(W)/sample_size, symmetric=TRUE)
    lambda = eigen_dec$values # numeric(nbasis)
    efpc = eigen_dec$vectors # vectors, efpc[i,j] = <x_i, x_j>, nbasis x nbasis
    
    # back to original matricial eigenproblem
    efpc = solve(expm::sqrtm(W)) %*% efpc # nbasis x nbasis
    
    # number of EFPC's to consider
    if(is.null(nharm) & !is.null(cum_prop_var))
    {
      # choose the number of harmonics by cumulative proportion of variance
      nharm = which.max(cumsum(lambda)/sum(lambda) > cum_prop_var)
      
      #print(paste0("nharm=",nharm))
      #print(cumsum(lambda)/sum(lambda))
    }
    
    lambda = lambda[1:nharm] # numeric(nharm)
    efpc = efpc[,1:nharm] # nbasis x nharm 
    
    # Evaluate EFPCS on the grid
    efpc_eval = basis_eval %*% efpc # npoints x nharm
    
    # Project data onto the EFPCS
    B = crossprod(efpc_eval)
    X = crossprod(efpc_eval, Xt_mat.train)
    C_efpc = solve(B,X) # nharm x n
    
    # scores of training data on EFPC's
    scores.train = t(C_efpc) # n x nharm
    
    #print(paste0("dim(scores.train)=",dim(scores.train)[1],"x",dim(scores.train)[2]))
    
  }
  
  ## 4 -- Project calibration set onto FPC's -----------------------------------
  
  B = efpc_eval
  Y = Xt_mat.calibration.burnin
  
  Bmat = crossprod(B)
  Dmat = crossprod(B,Y)
  coef = solve(Bmat, Dmat) # nharm x (l+burn_in)
  
  scores.calibration.burnin = t(coef) # (l+burn_in) x nharm
  
  rm(B,Y,Bmat,Dmat,coef)
  
  ## 5 -- SCORES ---------------------------------------------------------------
  
  # Scores of all the n original functions projected on the first "nharm" EFPC's
  SCORES = matrix(NA, nrow=n, ncol=nharm)
  SCORES[training,] = scores.train
  SCORES[-training,] = scores.calibration.burnin
  
  #print(paste0("SCORES dim: ", dim(SCORES)[1], "x", dim(SCORES)[2]))
  
  
  ## 6 -- PSI Estimation -------------------------------------------------------
  
  if(EK_improved && nharm>=2) # TODO: risolvere il caso nharm<2
  {
    b_param = 1.5*(lambda[1]+lambda[2]) # do not name it "b", there is already a "b" variable!
    lambda[3:nharm] = lambda[3:nharm] + b_param
  }
  
  # Estimation of the LAG-1 operator PSI:
  
  # nharm x nharm matrix representing the expansion of the kernel on the EFPC's
  PSI = crossprod(SCORES[training,], SCORES[training-1,]) %*% diag(1/(lambda*m), nrow=nharm, ncol=nharm)
  
  # other possible estimators:
  #PSI_2 = crossprod(SCORES[training[-m]+1,], SCORES[training[-m],]) %*% diag(1/(lambda*(m-1)))
  #PSI_2 = crossprod(SCORES[training[2:m]+1,], SCORES[training[1:(m-1)],]) %*% diag(1/(lambda*(m-1)))
  
  #print(paste0("PSI dim: ", dim(PSI)[1], "x", dim(PSI)[2]))
  
  
  
  ## 10 -- Predict (n+1) time --------------------------------------------------
  
  predicted_fun_n_plus_1 = as.numeric(efpc_eval %*% (PSI %*% SCORES[n,]))
  #print(paste0("length of prediction: ", length(predicted_fun_n_plus_1)))
  #print(paste0("dimension: ", dim(predicted_fun_n_plus_1)[1], "x", dim(predicted_fun_n_plus_1)[2]))
  
  ### add mean -----------------------------------------------------------------
  if(center){
    predicted_fun_n_plus_1 = predicted_fun_n_plus_1 + mean.train
  }
  
  ## 11 -- From matrix to list -------------------------------------------------
  
  
  predicted_fun_n_plus_1=list(list(predicted_fun_n_plus_1))
  
  
  # OUTPUT----------------------------------------------------------------------
  
  return(structure(.Data=list(predicted_fun_n_plus_1),
                   names=c("predicted_fun_n_plus_1")))
  
  
  
}
