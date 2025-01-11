#'  FUNCTIONS COPYRIGHT OF AJROLDI N.
#'
#' Conformal prediction bands for functional time series. 
#' 
#' Pointwise prediction function.
#' 
#' Prediction method: FAR nonconcurrent, estimated through EK method or EK-improved
#' 
#' The following function provides pointwise prediction functions, evaluated on the time grid.
#' It returns the prediction ("predicted_fun") for the observations in the training set, for one observation every "b" on the calibration set,
#' i.e. on the observations on which we are going to compute NC scores, and also for the (n+1) observation ("predicted_fun_n_plus_one")
#' 
#' @param data_y: list of n lists, where n is the length of the time series. Each of the 'n' lists must be made up of 'p' lists. Each of the Each of the 'p' lists must contain a numeric vector expressing the evaluation of the function on a grid (whose length can be different in the 'p' dimensions)
#' @param l: size of the calibration set
#' @param b: block size
#' @param seed_split: it must be either FALSE or an integer. It determines the seed of the split of the data into training set and calibration set
#' @param grid: a single grid (TODO: farla diventare una lista di p griglie)
#' @param FPCA_method: method to use for FPCA, either "discretization" or "basis"
#' @param nbasis: if FPCA_method=="basis", select here the number of basis to use
#' @param cum_prop_var: set this parameter if you want to select the numbers of FPC's by means of cumulative proportion of variance, this should be a number in (0,1]
#' @param nharm: set this parameter if you want to manually fix the numbers of FPC's. They will be fixed equal to nharm.
#' @param center: should we center functions? If TRUE, data are centered around the training set mean function.
#' @param EK_improved: logical, should we use the improved estimation techniques?
#' 
#' @return a list with members:
#'         data_y, predicted_fun, n, burn_in, m, l, b, excluded_set, training, calibration, obs_calibration_included, obs_calibration_excluded, seed_split, predicted_fun_n_plus_1
#'

EK_pred = function(data_y, #NB: I DATI VANNO PASSATI STORATI PER RIGHE: OGNI RIGA E' UN ISTANTE
                                         l, 
                                         b, 
                                         seed_split=FALSE,
                                         grid=seq(from=0,to=1,length=length_grid), 
                                         # FPCA:
                                         FPCA_method="discretization",
                                         cum_prop_var=0.9,
                                         nharm=NULL,
                                         nbasis=NULL,
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
  
  #Il codice lo scrivo nel caso delle simulazioni che sto facendo, ossia con p=1. Non garantisco che funzioni con p>1
  
  ## 1 -- From list to matrix --------------------------------------------------
  
  y_mat=matrix(NA,n,grid_size[1])
  
  for (i in 1:n){
    y_mat[i,]=data_y[[i]][[1]]
  } 
  
  rm(i)
  
  y_mat.train = y_mat[training,]
  y_mat.calibration.burnin = y_mat[-training,]
  
  ## 2 -- Mean centering -------------------------------------------------------
  
  if(center)
  {
    # mean is estimated on the training set
    mean.train = colMeans(y_mat.train)
    
    y_mat.train = sweep(y_mat.train, 2, mean.train)
    y_mat.calibration.burnin = sweep(y_mat.calibration.burnin, 2, mean.train)
  }
  
  ## 3 -- FPCA -----------------------------------------------------------------
  
  if(!(FPCA_method %in% c("discretization","basis"))){
    stop("invalid method for FPCA. Please select one between discretization and basis")
  }
  
  if(FPCA_method == "discretization")
  {
    # discretization step
    w = 1/(length(grid)-1)
    
    # pca
    pca = prcomp(y_mat.train, center=FALSE, scale.=FALSE)
    
    # eigenvalues of FPCA
    lambda = (pca$sdev^2) * w
    
    # number of EFPC's to consider
    if(is.null(nharm) & !is.null(cum_prop_var))
    {
      # choose the number of harmonics by cumulative proportion of variance
      nharm = as.integer(which.max(cumsum(lambda)/sum(lambda)>cum_prop_var))
    }
    
    # consider the first nharm eigenvalues
    lambda = lambda[1:nharm]
    
    # projection of training data on FPC's
    scores.train = pca$x[,1:nharm] * sqrt(w) # n x nharm
    
    # FPC's evaluated on the time grid
    harmonics.eval.grid = pca$rotation[,1:nharm] / sqrt(w) # length(grid) x nharm
    
    rm(pca)
    
  } else if(FPCA_method == "basis")
  {
    
    library(fda)
    
    if(is.null(nbasis)){
      nbasis = m/2
    }
    
    # create basis functions
    basis.1 <- create.fourier.basis(rangeval=range(grid), nbasis=nbasis)
    
    # basis projection
    data.fd <- Data2fd(y = t(y_mat.train), 
                       argvals = grid,
                       basisobj = basis.1)
    
    # fpca
    fpca <- pca.fd(data.fd, nharm=nbasis, centerfns=FALSE)
    
    # number of EFPC's to consider
    if(is.null(nharm) & !is.null(cum_prop_var))
    {
      # choose the number of harmonics by cumulative proportion of variance
      nharm = as.integer(which.max(cumsum(lambda)/sum(lambda)>cum_prop_var))
    }
    
    # eigenvalues of FPCA
    lambda = fpca$values[1:nharm]
    
    # projection of training data on FPC's
    scores.train = fpca$scores[,1:nharm]
    
    # FPC's evaluated on the time grid
    harmonics.fd = fpca$harmonics
    harmonics.eval.grid = eval.fd(grid, harmonics.fd)[,1:nharm] 
    
    rm(fpca, data.fd)
  }
  
  ## 4 -- Project calibration set onto FPC's -----------------------------------
  
  # A matrix of discrete data is projected into the space spanned by the values of a set of basis functions. 
  # This amounts to a least squares regression of the data on to the values of the basis functions
  B = harmonics.eval.grid
  Y = t(y_mat.calibration.burnin)
  
  Bmat = crossprod(B)
  Dmat = crossprod(B,Y)
  coef = solve(Bmat, Dmat)
  
  scores.calibration.burnin = t(coef)
  
  rm(B,Y,Bmat,Dmat,coef)
  
  ## 5 -- SCORES ---------------------------------------------------------------
  
  # Scores of all the n original functions projected on the first "nharm" EFPC's
  SCORES = matrix(NA, nrow=n, ncol=nharm)
  SCORES[training,] = scores.train
  SCORES[-training,] = scores.calibration.burnin
  
  #print(paste0("SCORES dim: ", dim(SCORES)[1], "x", dim(SCORES)[2]))
  
  ## 6 -- PSI Estimation -------------------------------------------------------
  
  if(EK_improved && nharm>=2)
  {
    b_param = 1.5*(lambda[1]+lambda[2]) # do not name it "b", there is already a "b" variable!
    lambda[3:nharm] = lambda[3:nharm] + b_param
  }
  
  # Estimation of the LAG-1 operator PSI:
  
  # nharm x nharm matrix representing the expansion of the kernel on the EFPC's
  PSI = crossprod(SCORES[training,], SCORES[training-1,]) %*% diag(1/(lambda*m), nrow=nharm, ncol=nharm)
  
  # other possible methods:
  #PSI_2 = crossprod(SCORES[training[-m]+1,], SCORES[training[-m],]) %*% diag(1/(lambda*(m-1)))
  #PSI_2 = crossprod(SCORES[training[2:m]+1,], SCORES[training[1:(m-1)],]) %*% diag(1/(lambda*(m-1)))
  
  ## 10 -- Predict (n+1) time ---------------------------------------------------
  
  predicted_fun_n_plus_1 = as.numeric(harmonics.eval.grid %*% (PSI %*% SCORES[n,]))
  #print(is(predicted_fun_n_plus_1))
  #print(paste0("dimension: ", dim(predicted_fun_n_plus_1)[1], "x", dim(predicted_fun_n_plus_1)[2]))
  
  # add estimated mean if center==TRUE
  if(center){
    predicted_fun_n_plus_1 = predicted_fun_n_plus_1 + mean.train
  }
  
  ## 11 -- From matrix to list -------------------------------------------------
  
  # N: la previsione diventa una lista di dimensione n, ogni elemento ? una lista
  #    In teoria questa sottolista dovrebbe avere p elementi, ma in realt? poi ne ha solo 1 perch? p=1.
  predicted_fun_n_plus_1=list(list(predicted_fun_n_plus_1))
  
  
  # OUTPUT----------------------------------------------------------------------
  
  return(structure(.Data=list(predicted_fun_n_plus_1),
                   names=c("predicted_fun_n_plus_1")))
  
}
