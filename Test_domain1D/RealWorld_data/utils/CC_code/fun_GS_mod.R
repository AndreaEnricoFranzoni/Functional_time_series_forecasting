## COPYRIGHT OF DIQUIGIOVANNI J.

#funzione 'point_prediction_conformal_TS'.

#Funzione che riceve in input:

#1-data_y: la lista degli n dati funzionali
#2-m
#3-b: ATTENTO: se usi b!=1 assicurati che size del calibration set sia scelto in modo tale da non creare problemi. Inoltre, se b!=1 non ho pensato benissimo a come gestire il fatto che MGS non pubblica i dati per una settimana, cio� magari ha senso prendere i blocchi considerando sta cosa!
#4-AR_order_v: ordine dell'AR pointwise per le curve di vendita. Default � 7.
#5-intercept_v: TRUE se si vuole includere l'intercetta nell' AR(AR_order) per le curve di vendita, FALSE se non la si vuole
#6-AR_order_a: ordine dell'AR pointwise per le curve di acquisto. Default � 7.
#7-intercept_a: TRUE se si vuole includere l'intercetta nell' AR(AR_order) per le curve di acquisto, FALSE se non la si vuole
#8-partial_calibration_rotation: TRUE se NON permetti il calcolo del NCS per le osservazioni del calibration che avrebbero come covariate nel futuro, FALSE altrimenti. ATTENTO: per il momento se TRUE ho semplicemente escluso le permutazioni che in testa avevano sequenze non crescenti, pensa se si pu� fare meglio. Inoltre attento se metti FALSE rischi che girando il dataset che sfasi tutto l'ordine se non hai un calibration set che sia multiplo di 7 tipo!
#9-common_grid: la griglia su cui le funzioni sono valutate. In questo caso passer� un vettore perch� assumo che curve di acquisto e curve di vendita siano sulla stessa griglia, volendo si pu� ovviamente generalizzare usando una lista contenente i due vettori per esempio. Questo parametro serve per rendere monotone le previsioni
#10-bivariate_model: TRUE se per prevedere le curve di vendita usi anche quelle passate di acquisto (e viceversa), FALSE altrimenti. ATTENTO: se AR_order_v e AR_order_a sono diversi non so cosa succeda nel codice, perch� quando calcoli i NCS nel calibration si sfaserebbero le due sequenze e diventa un problema forse. Sto considerando quindi AR_order_v=AR_order_a
#11-opp_order_v: ordine dell'AR pointwise delle curve di vendita nella predizione delle curve di acquisto. Default � 0, non mettere NULL o simili perch� senno c'� un errore. Viene usato solo se bivariate_model=TRUE. Per il momento deve essere <= di AR_order_a
#12-opp_order_a: ordine dell'AR pointwise delle curve di acquisto nella predizione delle curve di vendita. Default � 0, non mettere NULL o simili perch� senno c'� un errore. Viene usato solo se bivariate_model=TRUE. Per il momento deve essere <= di AR_order_v
#13-dummy_day: TRUE se si inserisce una dummy per il giorno della settimana, FALSE altrimenti. Se TRUE, in automatico inserisco l'intercetta nel modello e considero come baseline il giorno luned�
#14-weekdays_data: argomento che viene usato solo se dummy_day=TRUE. Riporta il giorno della settimana (luned�-marted� ecc...) delle n osservazioni del dataset. Serve per costuire le dummy del giorno della settimana.
#15-weekdays_fun_n_plus_8: argomento che viene usato solo se dummy_day=TRUE. Riporta il giorno della settimana (luned�-marted� ecc...) del giorno che voglio prevedere ossia n+8.
#16-inclusion_price: TRUE se si inseriscono i prezzi finali degli ultimi giorni (ossia gli ultimi giorni disponibili: al giorno t-1,t-2 ecc), FALSE altrimenti
#17-price_order: se inclusion_price � FALSE questo argomento deve essere 0. Se inclusion_price � TRUE, indica il numero di prezzi finali che bisogna prendere (se price_order=1 metter� prezzo al tempo t-1, se price_order=2 metter� prezzo al tempo t-1 e al tempo t-2 ecc...)
#18-data_prices: matrice di dimension n x price_order contenente alla riga i-esima gli 'price_order' prezzi finali piu recenti disponibili al giorno i (ossia il prezzo del giorno i-2,i-3,...,i-1-price order). Questo argomento viene preso in considerazione solo se inclusion_price=TRUE
#18-new_data_prices: vettore di lunghezza price_order contenente gli 'price_order' prezzi finali piu recenti disponibili al giorno che voglio prevedere (che essendo n+8, saranno dunque i prezzi del giorno n+6,n+5,..,n+7-price_order. Questo argomento viene preso in considerazione solo se inclusion_price=TRUE

#Funzione che restituisce in output:
#data_y: come da input
#predicted_fun: una lista di lunghezza n di liste di lunghezza 2(in quanto le curve da prevedere sono 2: vendita e acquisto)  contenenti la previsione funzionale per quelle funzioni (tra le n del dataset) di cui � utile avere una previsione. Quindi per quasi tutte le funzioni del training set in quanto possono servire per calcolare la funzione s(quasi tutte perch� fittando tipo un AR(7) pointwise per esempio non ottieni la previsione delle prime 7 osservazioni e anche delle successive 7 in quanto MGS pubblica i risultati 7 giorni dopo) e per l-bar_v e l-bar_a funzioni del calibration che serviranno per calcolare gli NCS. Per tutte le altre funzioni la previsione sar� NA.
#predicted_calibration_index_v:vettore  contenente gli indici delle funzioni del calibration per cui � stata calcolata la previsione per le curve di vendita. Tutti i valori saranno quindi compresi tra m+1 e m+l=n. ATTENTO: al momento sto differenziando tra predicted_calibration_index_v e predicted_calibration_index_a ma � piuttosto inutile in quanto poi potr� usare solo le funzioni per cui ho calcolato la previsione per entrambe le funzioni. I due index sono diversi solo se gli AR che fitto hanno ordine diverso per le curve di vendita e acquisto e se partial_calibration_rotation=TRUE credo. Insomma al momento lascio i due index separati ma sarebbe da sistemare
#predicted_calibration_index_a:corrispettivo per le curve di acquisto di 'predicted_calibration_index_v'.
#m: come da input
#predicted_fun_n_plus_8: una lista di lunghezza 1 formato da una lista di lunghezza 2 (in quanto le curve da prevedere sono 2: vendita e acquisto) contenente la previsione funzionale per la funzione n+8.
#AR_order_v: come da input
#AR_order_a: come da input

#Non sono presenti i controlli su tutto all'interno della funzione, solo alcune cose sono controllate.

point_prediction_conformal_TS_fstep_ppnaive_withpr_GS=function(data_y, l, b=1, seed_split=FALSE, AR_order_v=7, AR_order_a=7, common_grid, inclusion_price=FALSE,price_order=0,data_prices=NULL,new_data_prices=NULL){
  
  #----------------------------------------------CHECKS AND INITIALIZATION---------------------------------------------------
  
  if (is.null(seed_split)==TRUE || (seed_split!=FALSE & is.numeric(seed_split)==FALSE)) stop("Argument 'seed_split' must be either FALSE or an integer.")
  if(is.list(data_y)==FALSE || is.data.frame(data_y)==TRUE || is.list(data_y[[1]])==FALSE || is.data.frame(data_y[[1]])==TRUE){ #le prime due condizioni son per assicurarsi che sia una lista nel senso di "list" e non un dataframe (che � una lista tecnicamente), 
    
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
  burn_in=max(AR_order_v,AR_order_a)+7 
  
  m=n-l-burn_in
  
  if(seed_split!=FALSE){set.seed(seed_split)}
  training=sample((burn_in+1):n,m)
  training=sort(training)
  calibration=setdiff((burn_in+1):n,training)
  calibration=sort(calibration)
  excluded_set=1:burn_in
  
  #----------------------------------------------COMPUTATION---------------------------------------------------
  
  ### 1 -- Trasformo i dati da lista a matrice
  
  curves_v=matrix(NA,m+l+burn_in,grid_size[1])
  curves_a=matrix(NA,m+l+burn_in,grid_size[2])
  
  for (i in 1:n){
    curves_v[i,]=data_y[[i]]$p_cg_v
    curves_a[i,]=data_y[[i]]$p_cg_a
  }
  
  rm(i)
  ### 2 -- Stimo i parametri utilizzando il training set
  
  my_coeff_v=matrix(NA,grid_size[1], AR_order_v + inclusion_price*price_order)
  
  for (j in 1:grid_size[1]){
    cov_v=NULL
    
    for (h in 1:AR_order_v){
      cov_v=cbind(cov_v,curves_v[training-7-h,j]) 
    }
    
    if (inclusion_price==TRUE){ #qui non ritardo la covariata di 2 giorni perch� data_prices � stato costruito in modo che alla riga i-esima ho gi� i prezzi ritardati di 2 giorni (insomma le covariate "giuste")
      for (h in 1:price_order){
        cov_v=cbind(cov_v,data_prices[training,h])
      }
    }
    
    my_coeff_v[j,]=lm(curves_v[training,j]~-1+cov_v)$coefficients
  } 
  
  
  rm(cov_v,j,h)
  
  
  
  
  my_coeff_a=matrix(NA,grid_size[2], AR_order_a + inclusion_price*price_order)
  
  
  for (j in 1:grid_size[2]){
    cov_a=NULL
    
    for (h in 1:AR_order_a){
      cov_a=cbind(cov_a,curves_a[training-7-h,j])
    }
    
    if (inclusion_price==TRUE){ #qui non ritardo la covariata di 2 giorni perch� data_prices � stato costruito in modo che alla riga i-esima ho gi� i prezzi ritardati di 2 giorni (insomma le covariate "giuste")
      for (h in 1:price_order){
        cov_a=cbind(cov_a,data_prices[training,h])
      }
    }
    
    
    my_coeff_a[j,]=lm(curves_a[training,j]~-1+cov_a)$coefficients
    
  } 
  
  rm(cov_a,j,h)
  
  ### 3 -- Inizializzo le due matrici che conterranno le previsioni per il training e calibration set
  predicted_fun_matrix_v=matrix(0,m+l+burn_in,grid_size[1])
  predicted_fun_matrix_a=matrix(0,m+l+burn_in,grid_size[2])
  
  ### 4 -- Calcolo le previsioni per gli 'm' elementi del training, metto invece NA negli elemnti dell'excluded_set
  
  predicted_fun_matrix_v[excluded_set,]=NA #no previsione 
  predicted_fun_matrix_a[excluded_set,]=NA #no previsione 
  
  for (i in training){
    for (j in 1:grid_size[1]){
      cov_v=NULL
      for (h in 1:AR_order_v){
        
        cov_v=c(cov_v,curves_v[i-7-h,j])
      } 
      
      if (inclusion_price==TRUE){
        for (h in 1:price_order){
          cov_v=c(cov_v,data_prices[i,h])
        }
      }
      
      predicted_fun_matrix_v[i,j]= cov_v %*% my_coeff_v[j,]
    }
  } 
  
  rm(cov_v,h,i,j)
  
  
  for (i in training){
    for (j in 1:grid_size[2]){
      cov_a=NULL
      for (h in 1:AR_order_a){
        
        cov_a=c(cov_a,curves_a[i-7-h,j])
      } 
      
      if (inclusion_price==TRUE){
        for (h in 1:price_order){
          cov_a=c(cov_a,data_prices[i,h])
        }
      }
      predicted_fun_matrix_a[i,j]= cov_a %*% my_coeff_a[j,]
    }
  } 
  
  rm(cov_a,h,i,j)
  
  #5 -- Ottengo obs_calibration_included (ossia il vettore contenente le posizioni delle osservazioni del training set di cui calcolo il NCS) e obs_calibration_excluded (il vettore contenente la posizione delle altre osservazioni del calibration set). Se b=1 obs_calibration_excluded � vuoto 
  
  obs_calibration_included=sort(calibration)[seq(from=b,to=l,by=b)] #saranno (l+1)/b-1
  obs_calibration_excluded=setdiff(calibration,obs_calibration_included)
  
  
  #6 -- Calcolo le previsioni per gli (l+1)/b-1 elementi del calibration set le cui posizioni sono in obs_calibration_included, negli altri ci metter� NA
  
  if(length(obs_calibration_excluded)!=0)  predicted_fun_matrix_v[obs_calibration_excluded,]=NA #inserisco NA nelle osservazioni di cui non faccio la previsione
  
  
  for (i in obs_calibration_included){
    for (j in 1:grid_size[1]){
      cov_v=NULL
      for (h in 1:AR_order_v){
        
        cov_v=c(cov_v,curves_v[i-7-h,j]) 
        
      } 
      
      if (inclusion_price==TRUE){
        for (h in 1:price_order){
          cov_v=c(cov_v,data_prices[i,h])
        }
      }
      
      predicted_fun_matrix_v[i,j]= cov_v %*% my_coeff_v[j,]
    }
  } 
  
  rm(cov_v,h,i,j)
  
  if(length(obs_calibration_excluded)!=0)  predicted_fun_matrix_a[obs_calibration_excluded,]=NA #inserisco NA nelle osservazioni di cui non faccio la previsione
  
  
  for (i in obs_calibration_included){
    for (j in 1:grid_size[2]){
      cov_a=NULL
      for (h in 1:AR_order_a){
        
        cov_a=c(cov_a,curves_a[i-7-h,j]) 
        
      } 
      if (inclusion_price==TRUE){
        for (h in 1:price_order){
          cov_a=c(cov_a,data_prices[i,h])
        }
      }
      predicted_fun_matrix_a[i,j]= cov_a %*% my_coeff_a[j,]
    }
  } 
  
  rm(cov_a,h,i,j)
  
  
  
  
  #7 -- Calcolo le previsioni per la funzione al tempo n+8 (ATTENTO: la funzione � la n+8, quindi immagino proprio di essere al giorno n+7 e di voler prevedere le curve per il giorno successivo, ossia n+8. Quindi nel codice qui sotto le covariate saranno le funzioni al tempo l,l-1,l-2 ecc.. senza il consueto ritardo di tot giorni!)
  predicted_fun_n_plus_8_v=numeric(grid_size[1])
  predicted_fun_n_plus_8_a=numeric(grid_size[2])
  
  
  for (j in 1:grid_size[1]){
    cov_v=NULL
    for (h in 1:AR_order_v){
      
      cov_v=c(cov_v,curves_v[n+1-h,j])
    } 
    if (inclusion_price==TRUE){
      for (h in 1:price_order){
        cov_v=c(cov_v,new_data_prices[h])
      }
    }
    
    predicted_fun_n_plus_8_v[j]= cov_v %*% my_coeff_v[j,]
  }
  
  rm(cov_v,h,j)
  
  
  for (j in 1:grid_size[2]){
    cov_a=NULL
    for (h in 1:AR_order_a){
      
      cov_a=c(cov_a,curves_a[n+1-h,j])
    } 
    if (inclusion_price==TRUE){
      for (h in 1:price_order){
        cov_a=c(cov_a,new_data_prices[h])
      }
    }
    
    predicted_fun_n_plus_8_a[j]= cov_a %*% my_coeff_a[j,]
  }
  
  rm(cov_a,h,j)
  
  
  
  
  
  #8 -- Rendo monotone le mie previsioni in predicted_fun_matrix_v e predicted_fun_matrix_a 
  
  
  no_mon_curves_v=(1:nrow(predicted_fun_matrix_v))[apply(predicted_fun_matrix_v,1, function(x) !all(diff(x)>=0))] #posizione delle curve che non sono gi� monotone
  no_mon_curves_v=no_mon_curves_v[!is.na(no_mon_curves_v)]  
  
  for (i in no_mon_curves_v){  
    
    points_tbc=rep(NA,ncol(predicted_fun_matrix_v))  
    for (h in 2:ncol(predicted_fun_matrix_v))  {
      points_tbc[h]= (predicted_fun_matrix_v[i,h]<max(predicted_fun_matrix_v[i,1:(h-1)]))
    }
    
    
    for (j in which(points_tbc)){ #j quindi pu� andare da 2 a grid_size[1], indicher� la posizione dei punti che stanno pi� in basso rispetto al punto pi� in alto tra quelli che stanno a sinistra
      
      y1=max(predicted_fun_matrix_v[i,1:(j-1)])
      x1=common_grid[which.max(predicted_fun_matrix_v[i,1:(j-1)])]
      
      if(max(predicted_fun_matrix_v[i,j:grid_size[1]])<y1){ 
        predicted_fun_matrix_v[i,j:grid_size[1]]=y1
        break}      
      
      y2=predicted_fun_matrix_v[i,j+(which(predicted_fun_matrix_v[i,(j+1):grid_size[1]]>=y1))[1]]
      x2=common_grid[j+(which(predicted_fun_matrix_v[i,(j+1):grid_size[1]]>=y1))[1]]
      pend=(y2-y1)/(x2-x1)
      int_b=y1-pend*x1
      predicted_fun_matrix_v[i,j]=pend*common_grid[j]+int_b
      
    }
    
  } 
  
  rm(x1,x2,y1,y2,points_tbc,no_mon_curves_v,int_b,pend,i,j,h)
  
  
  
  
  
  
  
  
  no_mon_curves_a=(1:nrow(predicted_fun_matrix_a))[apply(predicted_fun_matrix_a,1, function(x) !all(diff(x)<=0))] #posizione delle curve che non sono gi� monotone
  no_mon_curves_a=no_mon_curves_a[!is.na(no_mon_curves_a)]  
  
  for (i in no_mon_curves_a){  
    
    points_tbc=rep(NA,ncol(predicted_fun_matrix_a))  
    for (h in 2:ncol(predicted_fun_matrix_a))  {
      points_tbc[h]= (predicted_fun_matrix_a[i,h]>min(predicted_fun_matrix_a[i,1:(h-1)]))
    }
    
    
    for (j in which(points_tbc)){ #j quindi pu� andare da 2 a grid_size[2], indicher� la posizione dei punti che stanno pi� in basso rispetto al punto pi� in alto tra quelli che stanno a sinistra
      
      y1=min(predicted_fun_matrix_a[i,1:(j-1)])
      x1=common_grid[which.min(predicted_fun_matrix_a[i,1:(j-1)])]
      
      if(min(predicted_fun_matrix_a[i,j:grid_size[2]])>y1){ 
        predicted_fun_matrix_a[i,j:grid_size[2]]=y1
        break}     
      
      y2=predicted_fun_matrix_a[i,j+(which(predicted_fun_matrix_a[i,(j+1):grid_size[2]]<=y1))[1]]
      x2=common_grid[j+(which(predicted_fun_matrix_a[i,(j+1):grid_size[2]]<=y1))[1]]
      pend=(y2-y1)/(x2-x1)
      int_b=y1-pend*x1
      predicted_fun_matrix_a[i,j]=pend*common_grid[j]+int_b
      
    }
    
  } 
  rm(x1,x2,y1,y2,points_tbc,no_mon_curves_a,int_b,pend,i,j,h)
  
  
  
  #9 -- Rendo monotone le previsioni al tempo n+8 (se non lo sono gi�)
  
  if(!all(diff(predicted_fun_n_plus_8_v)>=0)){
    
    points_tbc=rep(NA,length(predicted_fun_n_plus_8_v))  
    for (h in 2:length(predicted_fun_n_plus_8_v))  {
      points_tbc[h]= (predicted_fun_n_plus_8_v[h]<max(predicted_fun_n_plus_8_v[1:(h-1)]))
    }
    
    
    for (j in which(points_tbc)){ #j quindi pu� andare da 2 a grid_size[1], indicher� la posizione dei punti che stanno pi� in basso rispetto al punto pi� in alto tra quelli che stanno a sinistra
      
      y1=max(predicted_fun_n_plus_8_v[1:(j-1)])
      x1=common_grid[which.max(predicted_fun_n_plus_8_v[1:(j-1)])]
      
      if(max(predicted_fun_n_plus_8_v[j:grid_size[1]])<y1){ 
        predicted_fun_n_plus_8_v[j:grid_size[1]]=y1
        break}      
      
      y2=predicted_fun_n_plus_8_v[j+(which(predicted_fun_n_plus_8_v[(j+1):grid_size[1]]>=y1))[1]]
      x2=common_grid[j+(which(predicted_fun_n_plus_8_v[(j+1):grid_size[1]]>=y1))[1]]
      pend=(y2-y1)/(x2-x1)
      int_b=y1-pend*x1
      predicted_fun_n_plus_8_v[j]=pend*common_grid[j]+int_b
      
    }
    rm(x1,x2,y1,y2,points_tbc,int_b,pend,j,h)
  }
  
  
  
  
  
  
  if(!all(diff(predicted_fun_n_plus_8_a)<=0)){
    
    
    
    points_tbc=rep(NA,length(predicted_fun_n_plus_8_a))  
    for (h in 2:length(predicted_fun_n_plus_8_a))  {
      points_tbc[h]= (predicted_fun_n_plus_8_a[h]>min(predicted_fun_n_plus_8_a[1:(h-1)]))
    }
    
    
    for (j in which(points_tbc)){ #j quindi pu� andare da 2 a grid_size[2], indicher� la posizione dei punti che stanno pi� in basso rispetto al punto pi� in alto tra quelli che stanno a sinistra
      
      y1=min(predicted_fun_n_plus_8_a[1:(j-1)])
      x1=common_grid[which.min(predicted_fun_n_plus_8_a[1:(j-1)])]
      
      if(min(predicted_fun_n_plus_8_a[j:grid_size[2]])>y1){ 
        predicted_fun_n_plus_8_a[j:grid_size[2]]=y1
        break}     
      
      y2=predicted_fun_n_plus_8_a[j+(which(predicted_fun_n_plus_8_a[(j+1):grid_size[2]]<=y1))[1]]
      x2=common_grid[j+(which(predicted_fun_n_plus_8_a[(j+1):grid_size[2]]<=y1))[1]]
      pend=(y2-y1)/(x2-x1)
      int_b=y1-pend*x1
      predicted_fun_n_plus_8_a[j]=pend*common_grid[j]+int_b
      
    }
    
  } 
  rm(x1,x2,y1,y2,points_tbc,int_b,pend,j,h)
  
  #10 -- Trasformo le previsioni da matrici a liste
  
  
  predicted_fun=NULL
  for (i in 1:n){
    predicted_fun=c(predicted_fun,list(list(predicted_fun_matrix_v[i,],predicted_fun_matrix_a[i,])))
    names(predicted_fun[[i]])=c("predicted_p_cg_v","predicted_p_cg_a")
  }
  rm(predicted_fun_matrix_v,predicted_fun_matrix_a)
  
  
  predicted_fun_n_plus_8=list(list(predicted_fun_n_plus_8_v,predicted_fun_n_plus_8_a))
  names(predicted_fun_n_plus_8[[1]])=c("predicted_p_cg_v","predicted_p_cg_a")
  
  rm(predicted_fun_n_plus_8_v,predicted_fun_n_plus_8_a)
  
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
                              predicted_fun_n_plus_8,
                              AR_order_v,
                              AR_order_a),
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
                           "predicted_fun_n_plus_8",
                           "AR_order_v",
                           "AR_order_a")))
  
} 









#funzione 'functional_conformal_prediction_TS_fstep_ppnaive'.



functional_conformal_prediction_TS_fstep_ppnaive_GS=function(data_y,hat_y,t_values=NULL,m,alpha=0.10,s_type,randomized=FALSE,seed_tau=FALSE,training,obs_calibration_included){
  
  
  #----------------------------------------------CHECKS AND INITIALIZATION---------------------------------------------------
  if (is.null(seed_tau)==TRUE || (seed_tau!=FALSE & is.numeric(seed_tau)==FALSE)) stop("Argument 'seed_tau' must be either FALSE or an integer.")
  
  
  if (is.null(randomized)==TRUE || randomized %in% c("TRUE","FALSE")==FALSE) stop("Argument 'randomized' must be either TRUE or FALSE")
  if(randomized==FALSE) {tau=1} else{
    if(seed_tau!=FALSE){set.seed(seed_tau)}
    tau=stats::runif(n=1,min=0,max=1)
  }
  
  if(is.list(data_y)==FALSE || is.data.frame(data_y)==TRUE || is.list(data_y[[1]])==FALSE || is.data.frame(data_y[[1]])==TRUE){ #le prime due condizioni son per assicurarsi che sia una lista nel senso di "list" e non un dataframe (che � una lista tecnicamente), 
    
    stop("data_y must be a list of lists. Specifically, data_y must be a list of 'n' lists. Each of the 'n' lists must be made up of 'p' lists. Each of the 'p' lists must contain a numeric vector expressing the evaluation of the function on a grid (whose length can be different in the 'p' dimensions).
         'n' (i.e. the sample size) must be greater or equal than 2. 'p'(i.e. the dimension of the multivariate function ) must be the same for all the multivariate functions.")} else{
           n=length(data_y) 
           p=length(data_y[[1]])
           grid_size=vapply(data_y[[1]],function(x) length(x),integer(1))
           
  }
  
  if(is.list(hat_y)==FALSE || is.data.frame(hat_y)==TRUE || is.list(hat_y[[1]])==FALSE || is.data.frame(hat_y[[1]])==TRUE){ #le prime due condizioni son per assicurarsi che sia una lista nel senso di "list" e non un dataframe (che � una lista tecnicamente), 
    
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
  
  
  
  
  vect_y=t(vapply(data_y,unlist,numeric(sum(grid_size))))
  vect_hat_y=t(vapply(hat_y,unlist,numeric(sum(grid_size))))
  vect_residuals_y=vect_y-vect_hat_y
  
  
  s=computing_s_regression(vec_residual=vect_residuals_y[training,],type=s_type,alpha=alpha,tau=tau,grid_size=grid_size)  
  vect_s=unlist(s)
  
  #-----------------------------------COMPUTATION OF THE NCS IN THE CALIBRATION---------------------------------------
  
  rho=apply(vect_residuals_y[obs_calibration_included,],1,function(x) max(abs(x)/vect_s)) 
  k_s=sort(rho,decreasing=FALSE)[ceiling(l_bar_minus_1+tau-(l_bar_minus_1+1)*alpha)]
  
  
  if ((ceiling(l_bar_minus_1+tau-(l_bar_minus_1+1)*alpha))==1) v=0 else{v=sum(sort(rho,decreasing=FALSE)[1:(ceiling(l_bar_minus_1+tau-(l_bar_minus_1+1)*alpha)-1)]==k_s)}
  if ((ceiling(l_bar_minus_1+tau-(l_bar_minus_1+1)*alpha))==l_bar_minus_1) r=0 else{r=sum(sort(rho,decreasing=FALSE)[(ceiling(l_bar_minus_1+tau-(l_bar_minus_1+1)*alpha)+1):length(rho)]==k_s)}
  extremes_are_included= tau > (alpha*(l_bar_minus_1+1)-floor(alpha*(l_bar_minus_1+1)-tau)+r)/(r+v+2)  
  
  
  
  average_width=mean(2*k_s*vect_s)
  widths=vapply(1:length(s),function(x) mean(2*k_s*s[[x]]),numeric(1))
  product_integral=exp(mean(log(2*k_s*vect_s)))
  
  
  
  return(structure(.Data=list(rho,k_s,s_type,s,alpha,randomized,tau,extremes_are_included,widths,average_width,product_integral),
                   names=c("rho","k_s","s_type","s","alpha","randomized","tau","extremes_are_included","widths","average_width","product_integral")))
  
  }




#funzione 'computing_s_regression'


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
  
  indicator_grid=NULL 
  for (i in 1:length(grid_size)) indicator_grid=c(indicator_grid,rep(i,grid_size[i]))
  
  #----naive cases: just one observation and type %in% c("st-dev","alpha-max")
  if(is.atomic(vec_residual)==TRUE & is.vector(vec_residual)==TRUE & type=="st-dev") stop("st-dev can not be computed when the number of observations is equal to 1.")
  if(is.atomic(vec_residual)==TRUE & is.vector(vec_residual)==TRUE & type=="alpha-max") {
    
    out=split(abs(vec_residual),indicator_grid)
    names(out)=paste("s_",1:length(grid_size),sep="")
    return(out)
  }
  #---non-naive cases
  if (type=="identity"){
    out=split(rep(1,sum(grid_size)),indicator_grid)
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
    
    if(ceiling(dim(abs_vec_residual)[1]+tau-(dim(abs_vec_residual)[1]+1)*alpha) >= dim(abs_vec_residual)[1]) {
      out=split(apply(abs_vec_residual,2,max),indicator_grid)
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



#funzione 'computation_conformal_prediction_set_fstep_ppnaive' : qua hat_y � la previsione della funzione n+1!

computation_conformal_prediction_set_fstep_ppnaive=function(hat_y,observed_y=NULL,k_s,s,alpha,randomized=FALSE,extremes_are_included=TRUE){
  
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




