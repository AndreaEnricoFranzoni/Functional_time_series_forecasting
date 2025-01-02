rm(list=ls())
graphics.off()
cat("\014")

library(PPCKO)

#change here 
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"
source(paste0(dir_w,"/Test_CompTime/utils/far_1_1d.R"))
source(paste0(dir_w,"/Test_CompTime/utils/graphic_window.R"))



#to eventually store results
save_res = FALSE
dir_store_res = paste0(dir_w,"/Test_CompTime/results")
path_store_res = dir_store_res



## ----parallel-----
number_threads_OMP = c(1,2,3,4,5,6,7,8)
# checking, for different values of discrete grid size and time instants, the computational time 
# for PPC KO

#each row: grid size; each col: number time instants
time_instants <- c(19,49,99,499)
grid_size     <- 400

alphas = list(seq(from=0.1,to=1.0,length.out=5),
              seq(from=0.1,to=1.0,length.out=10),
              seq(from=0.1,to=1.0,length.out=25),
              seq(from=0.1,to=1.0,length.out=50))
sizes_alphas = c(5,10,25,50)

times_CV = list()

counter = 0
tot_it = length(number_threads_OMP)
for (i in 1:length(number_threads_OMP)) {
  n_t = number_threads_OMP[i]
  
  times_thread = matrix(data=NA, nrow = length(sizes_alphas),ncol=length(time_instants))
  times_thread = as.data.frame(times_thread)
  row.names(times_thread) = as.character(sizes_alphas)
  colnames(times_thread)  = as.character(time_instants)
  
  for (j in 1:length(time_instants)) {
    {
      set.seed(23032000)
      dim_grid           <- grid_size                                        
      n                  <- time_instants[j]                                        
      t.grid             <- seq(0,1, length.out=dim_grid)
      X.sample <- far_1_1D(kernel_id = "gaussian", noise_id = "1", n = n, t.grid = t.grid, a = 0.5, burnin = 0)
      
      for(index_alpha in 1:length(alphas)){
        
        alpha_vec = alphas[[index_alpha]]
        
        start_time = Sys.time()
        test = PPC_KO(X.sample,"CV",alpha_vec = alpha_vec,num_threads = n_t)
        end_time = Sys.time()
        
        times_thread[index_alpha,j] = as.double(end_time-start_time)
      }
      
    }
    
  }
  
  times_CV[[n_t]] = times_thread
  
  counter = counter + 1
  string_message = "
                  Test time cv par vs seq "
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
    ") , counter, tot_it)
  setTxtProgressBar(txtProgressBar(min = 1, max = tot_it, style = 3), counter)
  cat("\r", message)
}






times_CV_gen = list()

counter = 0
tot_it = length(number_threads_OMP)
for (i in 1:length(number_threads_OMP)) {
  n_t = number_threads_OMP[i]
  
  times_thread = matrix(data=NA, nrow = length(sizes_alphas),ncol=length(time_instants))
  times_thread = as.data.frame(times_thread)
  row.names(times_thread) = as.character(sizes_alphas)
  colnames(times_thread)  = as.character(time_instants)
  
  for (j in 1:length(time_instants)) {
    {
      set.seed(23032000)
      dim_grid           <- grid_size                                        
      n                  <- time_instants[j]                                        
      t.grid             <- seq(0,1, length.out=dim_grid)
      X.sample <- far_1_1D(kernel_id = "gaussian", noise_id = "1", n = n, t.grid = t.grid, a = 0.5, burnin = 0)
      
      for(index_alpha in 1:length(alphas)){
        
        alpha_vec = alphas[[index_alpha]]
        
        start_time = Sys.time()
        test = PPC_KO(X.sample,"CV",alpha_vec = alpha_vec,ex_solver = FALSE,num_threads = n_t)
        end_time = Sys.time()
        
        times_thread[index_alpha,j] = as.double(end_time-start_time)
      }
      
    }
    
  }
  
  times_CV_gen[[n_t]] = times_thread
  
  counter = counter + 1
  string_message = "
                  Test time cv gen par vs seq "
  message <- sprintf(paste0(string_message,"/ Progress: %d/%d
    ") , counter, tot_it)
  setTxtProgressBar(txtProgressBar(min = 1, max = tot_it, style = 3), counter)
  cat("\r", message)
}



# some are in minutes: TO BE CONVERTED
# to be converted: do it manually
times_CV[[1]]
times_CV[[1]][1,4]   = times_CV[[1]][1,4]*60
times_CV[[1]][2,3:4] = times_CV[[1]][2,3:4]*60
times_CV[[1]][3,2:4] = times_CV[[1]][3,2:4]*60
times_CV[[1]][4,1:4] = times_CV[[1]][4,1:4]*60

times_CV[[2]]
times_CV[[2]][1,4]   = times_CV[[2]][1,4]*60
times_CV[[2]][2,4]   = times_CV[[2]][2,4]*60
times_CV[[2]][3,3:4] = times_CV[[2]][3,3:4]*60
times_CV[[2]][4,2:4] = times_CV[[2]][4,2:4]*60

times_CV[[3]]
times_CV[[3]][1,4]   = times_CV[[3]][1,4]*60
times_CV[[3]][2,4]   = times_CV[[3]][2,4]*60
times_CV[[3]][3,3:4] = times_CV[[3]][3,3:4]*60
times_CV[[3]][4,2:4] = times_CV[[3]][4,2:4]*60

times_CV[[4]]
times_CV[[4]][1,4]   = times_CV[[4]][1,4]*60
times_CV[[4]][2,4]   = times_CV[[4]][2,4]*60
times_CV[[4]][3,3:4] = times_CV[[4]][3,3:4]*60
times_CV[[4]][4,2:4] = times_CV[[4]][4,2:4]*60

times_CV[[5]]
times_CV[[5]][1,4]   = times_CV[[5]][1,4]*60
times_CV[[5]][2,4]   = times_CV[[5]][2,4]*60
times_CV[[5]][3,3:4] = times_CV[[5]][3,3:4]*60
times_CV[[5]][4,3:4] = times_CV[[5]][4,3:4]*60

times_CV[[6]]
times_CV[[6]][1,4]   = times_CV[[6]][1,4]*60
times_CV[[6]][2,4]   = times_CV[[6]][2,4]*60
times_CV[[6]][3,3:4] = times_CV[[6]][3,3:4]*60
times_CV[[6]][4,3:4] = times_CV[[6]][4,3:4]*60

times_CV[[7]]
times_CV[[7]][1,4]   = times_CV[[7]][1,4]*60
times_CV[[7]][2,4]   = times_CV[[7]][2,4]*60
times_CV[[7]][3,4]   = times_CV[[7]][3,4]*60
times_CV[[7]][4,3:4] = times_CV[[7]][4,3:4]*60

times_CV[[8]]
times_CV[[8]][1,4]   = times_CV[[8]][1,4]*60
times_CV[[8]][2,4]   = times_CV[[8]][2,4]*60
times_CV[[8]][3,3:4] = times_CV[[8]][3,3:4]*60
times_CV[[8]][4,3:4] = times_CV[[8]][4,3:4]*60


if(save_res){
  save(times_CV, file = paste0(path_store_res,"/times_CV.Rdata"))
}







# some are in minutes: TO BE CONVERTED
# to be converted: do it manually
times_CV_gen[[1]]
times_CV_gen[[1]][2,4] = times_CV_gen[[1]][2,4]*60
times_CV_gen[[1]][3,4] = times_CV_gen[[1]][3,4]*60
times_CV_gen[[1]][4,3:4] = times_CV_gen[[1]][4,3:4]*60

times_CV_gen[[2]]
times_CV_gen[[2]][1,4] = times_CV_gen[[2]][1,4]*60
times_CV_gen[[2]][2,4] = times_CV_gen[[2]][2,4]*60
times_CV_gen[[2]][3,4] = times_CV_gen[[2]][3,4]*60
times_CV_gen[[2]][4,4] = times_CV_gen[[2]][4,4]*60

times_CV_gen[[3]]
times_CV_gen[[3]][2,4] = times_CV_gen[[3]][2,4]*60
times_CV_gen[[3]][3,4] = times_CV_gen[[3]][3,4]*60
times_CV_gen[[3]][4,4] = times_CV_gen[[3]][4,4]*60

times_CV_gen[[4]]
times_CV_gen[[4]][2,4] = times_CV_gen[[4]][2,4]*60
times_CV_gen[[4]][3,4] = times_CV_gen[[4]][3,4]*60
times_CV_gen[[4]][4,4] = times_CV_gen[[4]][4,4]*60

times_CV_gen[[5]]
times_CV_gen[[5]][2,4] = times_CV_gen[[5]][2,4]*60
times_CV_gen[[5]][3,4] = times_CV_gen[[5]][3,4]*60
times_CV_gen[[5]][4,4] = times_CV_gen[[5]][4,4]*60

times_CV_gen[[6]]
times_CV_gen[[6]][2,4] = times_CV_gen[[6]][2,4]*60
times_CV_gen[[6]][3,4] = times_CV_gen[[6]][3,4]*60
times_CV_gen[[6]][4,4] = times_CV_gen[[6]][4,4]*60

times_CV_gen[[7]]
times_CV_gen[[7]][2,4] = times_CV_gen[[7]][2,4]*60
times_CV_gen[[7]][3,4] = times_CV_gen[[7]][3,4]*60
times_CV_gen[[7]][4,4] = times_CV_gen[[7]][4,4]*60

times_CV_gen[[8]]
times_CV_gen[[8]][2,4] = times_CV_gen[[8]][2,4]*60
times_CV_gen[[8]][3,4] = times_CV_gen[[8]][3,4]*60
times_CV_gen[[8]][4,4] = times_CV_gen[[8]][4,4]*60


if(save_res){
  save(times_CV_gen, file = paste0(path_store_res,"/times_CV_gen.Rdata"))
}