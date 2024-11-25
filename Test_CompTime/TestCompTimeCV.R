rm(list=ls())
graphics.off()
cat("\014")

library(PPCKO)

#change here 
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"
source(paste0(dir_w,"/Test_CompTime/utils/far_1_1d.R"))
source(paste0(dir_w,"/Test_CompTime/utils/graphic_window.R"))



#to eventually store results
save_res = TRUE
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

# times for 499 time instants are in minutes: TO BE CONVERTED
if(save_res){
  save(times_CV, file = paste0(path_store_res,"/times_CV.Rdata"))
}