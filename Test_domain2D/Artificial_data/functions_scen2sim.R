# -- Funzione 1

#'
#' Conformal prediction bands for functional time series. 
#' 
#' The following function provides pointwise prediction functions, evaluated on the time grid.
#' It returns the prediction ("predicted_fun") for the observations in the training set, for one observation every "b" on the caibration set,
#' i.e. on the observations on which we are going to compute NC scores, and also for the (n+1) observation.
#' 
#' @param data_y: list of n lists, where n is the length of the time series. Each of the 'n' lists must be made up of 'p' lists. Each of the Each of the 'p' lists must contain a numeric vector expressing the evaluation of the function on a grid (whose length can be different in the 'p' dimensions)
#' @param l: size of the training set
#' @param b: block size
#' @param fitted_order: order for the underlying autoregressive model
#' @param seed_split: it must be either FALSE or an integer. It determines the seed of the split of the data into training set and calibration set
#' @param grid: per ora inutile, potrebbe avere un senso se fosse una lista con p-entries, cioè le p griglie. Ha senso nell'ottica di fittare un FAR(fitted_order), ancizhè fittare grid_size AR(fitted_order)
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
#'         fitted_order
#'         predicted_fun_n_plus_1
#'

point_prediction_conformal_TS_sc2_globalsplit=function(data_y, l, b, fitted_order, seed_split=FALSE,grid=seq(from=0,to=1,length=length_grid)){
  
  #----------------------------------------------CHECKS AND INITIALIZATION---------------------------------------------------
  
  if (is.null(seed_split)==TRUE || (seed_split!=FALSE & is.numeric(seed_split)==FALSE)) stop("Argument 'seed_split' must be either FALSE or an integer.")
  if(is.list(data_y)==FALSE || is.data.frame(data_y)==TRUE || is.list(data_y[[1]])==FALSE || is.data.frame(data_y[[1]])==TRUE){ #le prime due condizioni son per assicurarsi che sia una lista nel senso di "list" e non un dataframe (che è una lista tecnicamente), 
    
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
  
  #----------------------------DIVISION TRAINING-CALIBRATION--------------------
  burn_in=fitted_order
  
  m=n-l-burn_in
  
  if(seed_split!=FALSE){set.seed(seed_split)}
  training=sample((burn_in+1):n,m)
  training=sort(training)
  calibration=setdiff((burn_in+1):n,training)
  calibration=sort(calibration)
  excluded_set=1:burn_in
  
  
  
  #----------------------------------------------COMPUTATION---------------------------------------------------
  
  #Il codice lo scrivo nel caso delle simulazioni che sto facendo, ossia con p=1. Non garantisco che funzioni con p>1
  
  ### 1 -- Trasformo i dati da lista a matrice
  
  y_mat=matrix(NA,n,grid_size[1])
  
  for (i in 1:n){
    y_mat[i,]=data_y[[i]][[1]]
  } 
  
  rm(i)
  
  ### 2 -- Stimo i parametri utilizzando il training set
  
  my_coeff=matrix(NA,grid_size[1],fitted_order)
  
  for (j in 1:grid_size[1]){ # N: per ogni punto della griglia fitto un AR(fitted_order)
    cov=NULL
    
    for (h in 1:fitted_order){ 
      cov=cbind(cov,y_mat[training-h,j])
    }
    
    my_coeff[j,]=lm(y_mat[training,j]~-1+cov)$coefficients
  } 
  
  rm(cov,j,h)
  
  
  ### 3 -- Inizializzo la matrice che conterrà le previsioni per il training e calibration set
  
  predicted_fun_matrix=matrix(0,m+l+burn_in,grid_size[1])
  
  
  ### 4 -- Calcolo le previsioni per gli 'm' elementi del training (N: mi servono per la modulation function?)
  
  predicted_fun_matrix[excluded_set,]=NA #no previsione per le osservazioni del "burn-in"
  
  
  for (i in training){
    for (j in 1:grid_size[1]){
      cov=NULL
      for (h in 1:fitted_order){
        
        cov=c(cov,y_mat[i-h,j])
      } 
      predicted_fun_matrix[i,j]= cov %*% my_coeff[j,]
    }
  } 
  
  rm(cov,h,i,j)
  
  #5- dal calibration, ottengo gli indici da cui calcolo i NCS (..._included) e quelli esclusi in accordo al parametro b (..._excluded)
  
  obs_calibration_included=sort(calibration)[seq(from=b,to=l,by=b)] #saranno (l+1)/b-1
  obs_calibration_excluded=setdiff(calibration,obs_calibration_included)
  
  
  #6-Calcolo le previsioni per gli '(l+1)/b-1' elementi del calibration, negli altri l-[(l+1)/b-1] ci metterò NA
  
  if(length(obs_calibration_excluded)!=0)  predicted_fun_matrix[obs_calibration_excluded,]=NA #inserisco NA nelle osservazioni di cui non faccio la previsione
  
  for (i in obs_calibration_included){ # N: scorro le osservazioni su cui devo fare previsione
    for (j in 1:grid_size[1]){ # N: scorro la grid, per ogni valore faccio previsione col suo specifico AR(fitted_order)
      cov=NULL
      for (h in 1:fitted_order){
        
        cov=c(cov,y_mat[i-h,j])
        
      } 
      predicted_fun_matrix[i,j]= cov %*% my_coeff[j,]
    }
  } 
  
  
  rm(i,j,h,cov)
  
  
  #7 -- Calcolo le previsioni per la funzione al tempo n+1 
  
  predicted_fun_n_plus_1=numeric(grid_size[1])
  
  
  for (j in 1:grid_size[1]){
    cov=NULL
    for (h in 1:fitted_order){
      
      cov=c(cov,y_mat[n+1-h,j])
    } 
    predicted_fun_n_plus_1[j]= cov %*% my_coeff[j,]
  }
  
  
  
  rm(cov,h,j)
  
  
  
  #10 -- Trasformo le previsioni da matrici a liste
  
  # N: la previsione diventa una lista di dimensione n, ogni elemento è una lista
  #    In teoria questa sottolista dovrebbe avere p elementi, ma in realtà poi ne ha solo 1 perchè p=1.
  
  predicted_fun=NULL
  for (i in 1:n){
    predicted_fun=c(predicted_fun,list(list(predicted_fun_matrix[i,])))
  }
  rm(predicted_fun_matrix)
  
  
  predicted_fun_n_plus_1=list(list(predicted_fun_n_plus_1))
  
  
  
  #----------------------------------------------OUTPUT---------------------------------------------------
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











# -- Funzione 2

#' 
#' Conformal prediction bands for functional time series. 
#' 
#' Global split function. Computes k_s.
#' 
#' @param data_y: list of n lists, where n is the length of the time series. Each of the 'n' lists must be made up of 'p' lists. Each of the Each of the 'p' lists must contain a numeric vector expressing the evaluation of the function on a grid (whose length can be different in the 'p' dimensions)
#' @param hat_y: fitted data, should have the same data structure as "data_y"
#' @param t_values: must be either NULL or a list of 'p' atomic vectors of lenght equal to the size of the 'p' grids.
#' @param m: size of the training set
#' @param alpha: significance level alpha
#' @param s_type: the type of modulation function, should be one of the following: "identity", "st-dev", "alpha-max"
#' @param randomized: a logical value. Should the non-smoothed split algorithm (FALSE) or the smoothed split algorithm (TRUE) be performed?
#' @param seed_tau: it must be either FALSE or an integer. If randomized=TRUE, it determines the seed in creating 'tau'.
#' @param training: indices of the training set, it must be a subset of {1,...,n}, with n=length(data_y) be the sample size
#' @param obs_calibration_included: indices of the training set on which to compute the non-conformity scores
#' 
#' @return a list with members:
#'         s_type:
#'         s: 
#'         alpha
#'         randomized
#'         tau
#'         extremes_are_included
#'         average_width
#'         product_integral
#' 

functional_conformal_prediction_TS_sc2_globalsplit=function(data_y,hat_y,t_values=NULL,m,alpha=0.10,s_type,randomized=FALSE,seed_tau=FALSE,training,obs_calibration_included){
  
  
  #----------------------------------------------CHECKS AND INITIALIZATION---------------------------------------------------
  if (is.null(seed_tau)==TRUE || (seed_tau!=FALSE & is.numeric(seed_tau)==FALSE)) stop("Argument 'seed_tau' must be either FALSE or an integer.")
  
  
  if (is.null(randomized)==TRUE || randomized %in% c("TRUE","FALSE")==FALSE) stop("Argument 'randomized' must be either TRUE or FALSE")
  if(randomized==FALSE) {tau=1} else{
    if(seed_tau!=FALSE){set.seed(seed_tau)}
    tau=stats::runif(n=1,min=0,max=1)
  }
  
  if(is.list(data_y)==FALSE || is.data.frame(data_y)==TRUE || is.list(data_y[[1]])==FALSE || is.data.frame(data_y[[1]])==TRUE){ #le prime due condizioni son per assicurarsi che sia una lista nel senso di "list" e non un dataframe (che è una lista tecnicamente), 
    
    stop("data_y must be a list of lists. Specifically, data_y must be a list of 'n' lists. Each of the 'n' lists must be made up of 'p' lists. Each of the 'p' lists must contain a numeric vector expressing the evaluation of the function on a grid (whose length can be different in the 'p' dimensions).
         'n' (i.e. the sample size) must be greater or equal than 2. 'p'(i.e. the dimension of the multivariate function ) must be the same for all the multivariate functions.")} else{
           n=length(data_y) 
           p=length(data_y[[1]])
           grid_size=vapply(data_y[[1]],function(x) length(x),integer(1))
           
  }
  
  if(is.list(hat_y)==FALSE || is.data.frame(hat_y)==TRUE || is.list(hat_y[[1]])==FALSE || is.data.frame(hat_y[[1]])==TRUE){ #le prime due condizioni son per assicurarsi che sia una lista nel senso di "list" e non un dataframe (che è una lista tecnicamente), 
    
    stop("hat_y must be a list of lists. Specifically, hat_y must be a list of 'n' lists. Each of the 'n' lists must be made up of 'p' lists. Each of the 'p' lists must contain a numeric vector expressing the evaluation of the function on a grid (whose length can be different in the 'p' dimensions).
         'n' (i.e. the sample size) must be greater or equal than 2. 'p'(i.e. the dimension of the multivariate function ) must be the same for all the multivariate functions.") }
  
  if (n <2) stop("'n'(i.e. the sample size) must be greater or equal than 2.")
  if (length(unique(vapply(data_y,length,integer(1))))!=1 || length(unique(vapply(hat_y,length,integer(1))))!=1) stop("'p'(i.e. the dimension of the multivariate function) must be the same for all the multivariate functions. The same holds for 'hat_y'.")
  if(!(all(apply(t(vapply(data_y,function(x) vapply(x,length,integer(1)),integer(p))),2, function(y) length(unique(y))==1))) || !(all(apply(t(vapply(hat_y,function(x) vapply(x,length,integer(1)),integer(p))),2, function(y) length(unique(y))==1)))) stop("The 'n' functions in data_y must be evaluated on the same p-variate grid. The grid can vary between the p domains. The same holds for 'hat_y'.")
  
  if(length(hat_y)!=n) stop("the length of 'hat_y' must be equal to the length of 'data_y'.")
  if(length(hat_y[[1]])!=p) stop("the dimension of the multivariate functions in  'hat_y' must be equal to the dimension of the multivariate functions in 'data_y'.")
  if(!(all(vapply(hat_y[[1]],function(x) length(x),integer(1))==grid_size))) stop("The multivariate functions in 'hat_y' must be evalauted on the same grid on which the multivariate functions in 'data_y' are evaluated.")
  
  if (is.null(t_values)==FALSE & (is.list(t_values)==FALSE || (all(vapply(t_values,is.vector,logical(1))))==FALSE || (all(vapply(t_values,is.atomic,logical(1))))==FALSE || all(vapply(t_values,length,integer(1))==grid_size)==FALSE || length(t_values)!=p) ) stop("Argument 't_values' must be either NULL or a list of 'p' atomic vectors of lenght equal to the size of the 'p' grids.")#CONTROLLALOOOOO
  
  l_bar_minus_1=length(obs_calibration_included)
  
  if (alpha<tau/(l_bar_minus_1+1) & alpha>0) stop ("The prediction band obtained with such a small value of alpha is the entire space. 
                                                   If you are using the non randomized version of the algorithm, non-trivial prediction bands can be obtained by using an alpha greater or equal than 1/(l_bar_minus_1+1) and less than 1. 
                                                   If you are using the randomized version of the algorithm, non-trivial prediction bands can be obtained by using an alpha greater or equal than tau/(l_bar_minus_1+1) and less than (tau+l_bar_minus_1)/(l_bar_minus_1+1).")
  
  if (alpha>=(l_bar_minus_1+tau)/(l_bar_minus_1+1) || alpha<=0)       stop("The alpha value is not admissible.
                                                                           If you are using the non randomized version of the algorithm, non-trivial prediction bands can be obtained by using an alpha greater or equal than 1/(l_bar_minus_1+1) and less than 1. 
                                                                           If you are using the randomized version of the algorithm, non-trivial prediction bands can be obtained by using an alpha greater or equal than tau/(l_bar_minus_1+1) and less than (tau+l_bar_minus_1)/(l_bar_minus_1+1).")
  
  
  
  # N: ottengo una matrice per i valori osservati, predettti e per i residui
  #    dimensione delle matrici: n x sum(grid_size)
  
  vect_y=t(vapply(data_y,unlist,numeric(sum(grid_size)))) # unlists and checks that all values of FUN are compatible with the type and length of numeric(sum(grid_size))
  vect_hat_y=t(vapply(hat_y,unlist,numeric(sum(grid_size))))
  vect_residuals_y=vect_y-vect_hat_y
  
  # Compute modulation function and then vectorize it
  s=computing_s_regression(vec_residual=vect_residuals_y[training,],type=s_type,alpha=alpha,tau=tau,grid_size=grid_size)  
  vect_s=unlist(s) # N: has length sum(grid_size)
  
  #-----------------------------------COMPUTATION OF THE NCS IN THE CALIBRATION---------------------------------------
  
  rho=apply(vect_residuals_y[obs_calibration_included,],1,function(x) max(abs(x)/vect_s)) 
  k_s=sort(rho,decreasing=FALSE)[ceiling(l_bar_minus_1+tau-(l_bar_minus_1+1)*alpha)]
  
  if ((ceiling(l_bar_minus_1+tau-(l_bar_minus_1+1)*alpha))==1) v=0 else{v=sum(sort(rho,decreasing=FALSE)[1:(ceiling(l_bar_minus_1+tau-(l_bar_minus_1+1)*alpha)-1)]==k_s)}
  if ((ceiling(l_bar_minus_1+tau-(l_bar_minus_1+1)*alpha))==l_bar_minus_1) r=0 else{r=sum(sort(rho,decreasing=FALSE)[(ceiling(l_bar_minus_1+tau-(l_bar_minus_1+1)*alpha)+1):length(rho)]==k_s)}
  extremes_are_included= tau > (alpha*(l_bar_minus_1+1)-floor(alpha*(l_bar_minus_1+1)-tau)+r)/(r+v+2)  
  
  average_width=mean(2*k_s*vect_s)
  product_integral=exp(mean(log(2*k_s*vect_s))) # N: ???
  
  
  
  return(structure(.Data=list(k_s,s_type,s,alpha,randomized,tau,extremes_are_included,average_width,product_integral),
                   names=c("k_s","s_type","s","alpha","randomized","tau","extremes_are_included","average_width","product_integral")))
  
  }






# -- Funzione 3

#' 
#' Conformal prediction bands for functional time series. 
#' 
#' THe following function computes the modulation function s.
#' 
#' @param vec_residual: residuals evaluated on the time grid. Should be either a matrix, a dataframe or an atomic vector
#' @param type: type of modulation function. It must be either...
#' @param alpha: significance level alpha
#' @param tau: the value of tau
#' @param grid_size: size of the grid
#' 
#' @return a list of p vectors. The j-th element of the list contains values of the modulation function for the j-th dimension evaluated on the correspondent time grid.
#' 
computing_s_regression=function(vec_residual,type,alpha,tau,grid_size){
  
  #----------------------------------------------CHECKS ON data AND type---------------------------------------------------
  
  #check on 'vec_residual' argument  
  if( is.matrix(vec_residual)==FALSE & is.data.frame(vec_residual)==FALSE & (is.atomic(vec_residual)==FALSE || is.vector(vec_residual)==FALSE)) stop("vec_residual must be either a matrix, a dataframe or an atomic vector (naive case).") 
  
  
  
  #check on 'type' argument  
  possible_s_functions=c("identity","st-dev","alpha-max")
  if (is.null(type) || type %in% possible_s_functions==FALSE) {
    stop(c("The 'type' argument is not correct. Please select one of the following:",paste(possible_s_functions,collapse=", "),"."))
  }
  
  #-----------------------------------------------COMPUTATION-------------------------------------------------
  
  # N: indicator_grid contains integers from 1 to p, concatenates p vectors, each of size grid_size[i] with values equal to i
  indicator_grid=NULL 
  for (i in 1:length(grid_size)) indicator_grid=c(indicator_grid,rep(i,grid_size[i]))
  
  #----naive cases: just one observation and type %in% c("st-dev","alpha-max")
  if(is.atomic(vec_residual)==TRUE & is.vector(vec_residual)==TRUE & type=="st-dev") stop("st-dev can not be computed when the number of observations is equal to 1.")
  if(is.atomic(vec_residual)==TRUE & is.vector(vec_residual)==TRUE & type=="alpha-max") {
    
    out=split(abs(vec_residual),indicator_grid) # divides "vec_residuals" into lists defined by "indicator_grid"
    names(out)=paste("s_",1:length(grid_size),sep="")
    return(out)
  }
  #---non-naive cases
  if (type=="identity"){
    out=split(rep(1,sum(grid_size)),indicator_grid) # ??
    names(out)=paste("s_",1:length(grid_size),sep="")
    return(out)
  }
  
  if (type=="st-dev") {
    out=split(apply(vec_residual,2,sd),indicator_grid)
    names(out)=paste("s_",1:length(grid_size),sep="")
    return(out)
  }
  
  if (type=="alpha-max"){ 
    
    #----------------------------------------------CHECKS ON tau---------------------------------------------------
    
    if ((tau<0) || (tau>1)) stop("tau must belong to [0,1].")
    
    #---------------------------------------------OBTAINING |vec_residual|------------------------------------------------
    
    
    abs_vec_residual=abs(vec_residual)
    
    #----------------------------------------------CHECKS ON alpha-----------------------------------------------
    
    # N: capire sto check
    
    if(ceiling(dim(abs_vec_residual)[1]+tau-(dim(abs_vec_residual)[1]+1)*alpha) >= dim(abs_vec_residual)[1]) {
      out=split(apply(abs_vec_residual,2,max),indicator_grid) # N: max across times
      names(out)=paste("s_",1:length(grid_size),sep="")
      return(out)} 
    if(ceiling(dim(abs_vec_residual)[1]+tau-(dim(abs_vec_residual)[1]+1)*alpha) <= 0)           {
      out=split(rep(1,sum(grid_size)),indicator_grid)
      names(out)=paste("s_",1:length(grid_size),sep="")
      return(out)} 
    
    #----------------------------------------------S ALPHA-MAX----------------------------------------------------
    
    sequence_sup=apply(abs_vec_residual,1,max)
    gamma=sort(sequence_sup,decreasing=FALSE)[ceiling(dim(abs_vec_residual)[1]+tau-(dim(abs_vec_residual)[1]+1)*alpha)]
    position_functions_in_H=which(sequence_sup <= gamma)
    out=split(apply(abs_vec_residual[position_functions_in_H,],2,max),indicator_grid)
    names(out)=paste("s_",1:length(grid_size),sep="")
    return(out)
    
  }
  
  
}


# -- Funzione 4

#' 
#' Conformal prediction bands for functional time series. Computes conformal prediction bands.
#' If observed_y is not NULL, the function also returns a list of logical values: "inclusion".
#' inclusion[[j]] is TRUE if observed_y lies inside the prediction bands, FALSE otherwise.
#' 
#' @param hat_y: fitted values. It must be a list of n lists, where n is the length of the time series. Each of the 'n' lists must be made up of 'p' lists. Each of the Each of the 'p' lists must contain a numeric vector expressing the evaluation of the function on a grid (whose length can be different in the 'p' dimensions)
#' @param observed_y: Observed values. It must have the same data structure of "hat_y
#' @param k_s: ceiling((l+1)(1-alpha))th smallest value of the non-conformity scores of the calibration set
#' @param s: numeric, contains the consecutive evaluation of the modulation function on the time grids. It should have length euqal to sum(grid_size).
#' @param alpha: significance level alpha.
#' @param randomized: logical value. Should the non-smoothed split algorithm (FALSE) or the smoothed split algorithm (TRUE) be performed?
#' @param extremes_are_included: logical value. It determines whether or not the lower and upper bounds of the prediction band are included in the prediction band. If randomized=FALSE, it is always TRUE.
#' 
#' @return list with the following attributes:
#'         inf_lim:
#'         sup_lim:
#'         alpha:
#'         randomized:
#'         extremes_are_included: 
#'         inclusion: 
#' 

computation_conformal_prediction_set=function(hat_y,observed_y=NULL,k_s,s,alpha,randomized=FALSE,extremes_are_included=TRUE){
  
  sup_lim=lapply(hat_y,function(z) Map(function(x,y) x+k_s*y,z,s))
  inf_lim=lapply(hat_y,function(z) Map(function(x,y) x-k_s*y,z,s))
  
  if (is.null(observed_y)==FALSE){
    
    if(extremes_are_included==TRUE){
      point_belonging=Map(function(w,t,r) Map(function(x,y,z) ((x>=y) & (x<=z)),w,t,r),observed_y,inf_lim,sup_lim)
      inclusion=lapply(lapply(point_belonging,unlist),all)} else{
        point_belonging=Map(function(w,t,r) Map(function(x,y,z) ((x>y) & (x<z)),w,t,r),observed_y,inf_lim,sup_lim)
        inclusion=lapply(lapply(point_belonging,unlist),all)
      }
    return(structure(.Data=list(inf_lim,sup_lim,inclusion,alpha,randomized,extremes_are_included),
                     names=c("inf_lim","sup_lim","inclusion","alpha","randomized","extremes_are_included")))
    
  }
  return(structure(.Data=list(inf_lim,sup_lim,alpha,randomized,extremes_are_included),
                   names=c("inf_lim","sup_lim","alpha","randomized","extremes_are_included")))
}



