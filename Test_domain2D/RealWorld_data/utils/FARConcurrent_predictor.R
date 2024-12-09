
#'
#' Conformal prediction bands for functional time series. 
#' 
#' Pointwise prediction function.
#' 
#' Prediction method: FAR concurrent
#' 
#' The following function provides pointwise prediction functions, evaluated on the time grid.
#' It returns the prediction ("predicted_fun") for the observations in the training set, for one observation every "b" on the calibration set,
#' i.e. on the observations on which we are going to compute NC scores, and also for the (n+1) observation ("predicted_fun_n_plus_one")
#' 

FAR_concurrent_predictor = function(data_y, 
                                          l, 
                                          b, 
                                          fitted_order,
                                          seed_split=FALSE,
                                          grid=seq(from=0,to=1,length=length_grid), 
                                          center=FALSE)
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
  burn_in=2 # TODO:change!!
  # burn_in=fitted_order
  
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
  
  ## 4 -- Stimo i parametri utilizzando il training set ------------------------
  
  my_coeff=matrix(NA,grid_size[1],fitted_order)
  
  # per ogni punto della griglia fitto un AR(fitted_order)
  for (j in 1:grid_size[1]){
    cov=NULL
    
    for (h in 1:fitted_order){ 
      cov=cbind(cov,y_mat[training-h,j])
    }
    
    my_coeff[j,]=lm(y_mat[training,j]~-1+cov)$coefficients
  } 
  
  rm(cov,j,h)
  
  
  
  ## 8 -- Predict time (n+1) ---------------------------------------------------
  
  predicted_fun_n_plus_1=numeric(grid_size[1])
  
  for (j in 1:grid_size[1]){
    cov=NULL
    for (h in 1:fitted_order){
      
      cov=c(cov,y_mat[n+1-h,j])
    } 
    predicted_fun_n_plus_1[j]= cov %*% my_coeff[j,]
  }
  
  ### add mean -----------------------------------------------------------------
  if(center){
    predicted_fun_n_plus_1 = predicted_fun_n_plus_1 + mean.train
  }
  
  ## 9 -- From matrix to list -------------------------------------------------
  
  # N: la previsione diventa una lista di dimensione n, ogni elemento ? una lista
  #    In teoria questa sottolista dovrebbe avere p elementi, ma in realt? poi ne ha solo 1 perch? p=1.
  
  
  predicted_fun_n_plus_1=list(list(predicted_fun_n_plus_1))
  
  
  # OUTPUT----------------------------------------------------------------------
  
  return(structure(.Data=list(predicted_fun_n_plus_1),
                   names=c("predicted_fun_n_plus_1")))
  
  
  
}