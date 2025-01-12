#function to perform cv on KE and KEI

cv_EK = function(X,   #passo normale: righe(valutazioni)xcolonne(istanti)
                 grid_eval,
                 p_vector,
                 improved=FALSE
                 )
{
  #X=train_set,grid_eval=t.grid,p_vector=c(2,3,4,5,6),improved = TRUE
  n = dim(X)[2]
  
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
      X_train = X[,1:t]
      X_val   = X[,t+1]
      
      
      # dati vanno trasposti per andare nella prossima funzione: ogni riga: time instant. Ogni colonna: evaluation
      X_ek = t(X_train)
      
      dataset_wrapped=NULL
      for (i in 1:t){
        dataset_wrapped=c(dataset_wrapped,list(list(X_ek[i,])))
      }
      
      testing = EK_pred(data_y = dataset_wrapped,
                        l = 3,
                        b = 2,
                        seed_split=FALSE,
                        grid=grid_eval, 
                        FPCA_method="discretization",
                        cum_prop_var=0.9,
                        nharm=NULL,
                        nbasis=NULL,
                        center=TRUE,
                        EK_improved=FALSE)
      
      prediction_cv = testing$predicted_fun_n_plus_1[[1]][[1]]
      
      mse_val[count_err] = MLmetrics::MSE(prediction_cv,X_val)
      count_err = count_err+1
    }
    
    err_cv[j] = mean(mse_val)
    
  }
  
  p_best = p_vector[which(err_cv==min(err_cv))]
  
  X_ek = t(X)
  
  dataset_wrapped=NULL
  for (i in 1:n){
    dataset_wrapped=c(dataset_wrapped,list(list(X_ek[i,])))
  }
   
          ke_opt = EK_pred(data_y = dataset_wrapped,
                        l = 3,
                        b = 2,
                        seed_split=FALSE,
                        grid=grid_eval, 
                        FPCA_method="discretization",
                        cum_prop_var=0.9,
                        nharm=NULL,
                        nbasis=NULL,
                        center=TRUE,
                        EK_improved=FALSE)
  
  result = list(prediction = ke_opt$predicted_fun_n_plus_1[[1]][[1]], N_PCs_ret = p_best)
  
  return(result)
}
