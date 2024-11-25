#correction_monotonic_function=function(inflim,suplim,type){
  
#  inflim_mod=rep(NA,length(inflim))
#  suplim_mod=rep(NA,length(suplim))  
#  if (type=="v"){
#    for(i in 1:length(inflim)){
#      inflim_mod[i]=max(inflim[1:i])
#      suplim_mod[i]=min(suplim[i:length(suplim)])
#    }
      
#  }
  
#  if (type=="a"){
#    for(i in 1:length(inflim)){
#      inflim_mod[i]=max(inflim[i:length(inflim)])
#      suplim_mod[i]=min(suplim[1:i])
#    }
  
#  }
  
#  return(list(inflim_mod,suplim_mod))
  
#}  
  
correction_monotonic_function=function(inflim,suplim,type){
  if (type=="v"){
      inflim_mod=vapply(1:length(inflim),function(i) max(inflim[1:i]),numeric(1))
      suplim_mod=vapply(1:length(inflim),function(i) min(suplim[i:length(suplim)]),numeric(1))
      
  }
  
  if (type=="a"){
      inflim_mod=vapply(1:length(inflim),function(i) max(inflim[i:length(inflim)]),numeric(1))
      suplim_mod=vapply(1:length(inflim),function(i) min(suplim[1:i]),numeric(1))

  }
  
  return(list(inflim_mod,suplim_mod))
  
}  


