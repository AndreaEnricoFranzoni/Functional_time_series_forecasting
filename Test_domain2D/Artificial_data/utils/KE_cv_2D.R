# to perform cv on 2d EK


cv_EK_2d = function(X,   
                    grid_eval1,
                    grid_eval2,
                    p_vector,   #number of directions
                    improved=FALSE
                 )
{
  #X=train_set,grid_eval=t.grid,p_vector=c(2,3,4,5,6),improved = TRUE
  n = length(X)
  
  t_start = ceiling(n/2)
  t_end = n-1
  
  time_instants_cv = seq(t_start,t_end,by=1)
  n_cv_iter = length(time_instants_cv)
  err_cv = numeric(length(p_vector))
  
  for (j in 1:length(p_vector)) {
    
    p = p_vector[j]
    mse_val = numeric(n_cv_iter)
    count_err = 1
    
    for (t in time_instants_cv) {
      X_train = X[1:t]
      X_val   = as.vector(X[t+1][[1]])
      
      
      # From list of matrices to list of lists of vectors --------------------------
      Xt_list_of_vec = vector("list", length = t)
      
      # for each time
      for(i in 1:(t))
      {
        Xt_list_of_vec[[i]] = vector("list", length = 1)
        Xt_list_of_vec[[i]][[1]] = as.vector(X_train[[i]])
      }
      
      
      testing = EK_pred_2D(data_y=Xt_list_of_vec,
                            indexes_not_NA=NULL,
                            l=3, 
                            b=2, 
                            seed_split=FALSE,
                            x1.grid=grid_eval1, 
                            x2.grid=grid_eval2, 
                            FPCA_method="discretization",
                            basis.type="bspline",
                            grid.step.num.int=0.001,
                            nbasis.1=NULL,
                            nbasis.2=NULL,
                            cum_prop_var=NULL,
                            nharm=p,
                            center=TRUE,
                            EK_improved=improved)
      
      prediction_cv = testing$predicted_fun_n_plus_1[[1]][[1]]
      
      mse_val[count_err] = MLmetrics::MSE(prediction_cv,X_val)
      count_err = count_err+1
    }
    
    err_cv[j] = mean(mse_val)
    
  }
  
  p_best = p_vector[which(err_cv==min(err_cv))]
  
  
  Xt_list_of_vec = vector("list", length = n)
  
  # for each time
  for(t in 1:(n))
  {
    Xt_list_of_vec[[t]] = vector("list", length = 1)
    Xt_list_of_vec[[t]][[1]] = as.vector(X[[t]])
  }
   
          ke_opt = EK_pred_2D(data_y=Xt_list_of_vec,
                              indexes_not_NA=NULL,
                              l=3, 
                              b=2, 
                              seed_split=FALSE,
                              x1.grid=grid_eval1, 
                              x2.grid=grid_eval2, 
                              FPCA_method="discretization",
                              basis.type="bspline",
                              grid.step.num.int=0.001,
                              nbasis.1=NULL,
                              nbasis.2=NULL,
                              cum_prop_var=NULL,
                              nharm=p,
                              center=TRUE,
                              EK_improved=improved)
  
  result = list(prediction = ke_opt$predicted_fun_n_plus_1[[1]][[1]], N_PCs_ret = p_best)
  
  return(result)
}
