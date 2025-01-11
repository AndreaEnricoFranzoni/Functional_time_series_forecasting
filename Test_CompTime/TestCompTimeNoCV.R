rm(list=ls())
graphics.off()
cat("\014")

##########################################################################################################################
#### Computational time of PPCKO no-cv version, for different values of number of FTS evaluations and time instants:  ####
#### comparing imposing k with ex_solver, selecting k with exp pow criterion, imposing k with gep_solver              ####
##########################################################################################################################

library(PPCKO)

#put here the path of the local copy of the directory
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"

#upload the functions to create FAR(1)
source(paste0(dir_w,"/Test_CompTime/utils/far_1_1d.R"))
source(paste0(dir_w,"/Test_CompTime/utils/graphic_window.R"))

#to eventually store results
save_res = TRUE

#directory storing results
dir_store_res = paste0(dir_w,"/Test_CompTime/results")
path_store_res = dir_store_res


#each row: grid size; each col: number time instants
time_instants <- c(19,49,99,499)
grid_size     <- c(100,400,900,1600,2500,10000)


## -----imposing k with ex_solver-----
times_NoCV_k_imp = matrix(data=NA,nrow = length(grid_size),ncol=length(time_instants))
times_NoCV_k_imp = as.data.frame(times_NoCV_k_imp)
row.names(times_NoCV_k_imp) = as.character(grid_size)
colnames(times_NoCV_k_imp)  = as.character(time_instants)

counter = 0
tot_it = length(time_instants)*length(grid_size)

for (i in 1:length(time_instants)) {
  for(j in 1:length(grid_size)){
    
    {
      set.seed(23032000)
      dim_grid           <- grid_size[j]                                         
      n                  <- time_instants[i]                                        
      t.grid             <- seq(0,1, length.out=dim_grid)
      X.sample <- far_1_1D(kernel_id = "gaussian", noise_id = "1", n = n, t.grid = t.grid, a = 0.5, burnin = 0)
    }
    
    #measure time
    start_time = Sys.time()
    test = PPC_KO(X.sample,"NoCV",k=3)
    end_time = Sys.time()
    
    times_NoCV_k_imp[j,i] = as.double(end_time-start_time)

    counter = counter + 1
    string_message = "
                  Test time no cv k imp: time instants: %d, grid size: %d "
    message <- sprintf(paste0(string_message,"/ Progress: %d/%d
    ") , n, dim_grid, counter, tot_it)
    setTxtProgressBar(txtProgressBar(min = 1, max = tot_it, style = 3), counter)
    cat("\r", message)
  }
}

# for the grid_size = 10000: time is measured in minutes: convert it 
times_NoCV_k_imp[which(grid_size==10000),] = times_NoCV_k_imp[which(grid_size==10000),]*60

if(save_res){
  save(times_NoCV_k_imp, file = paste0(path_store_res,"/times_NoCV_k_imp.Rdata"))
}



## -----imposing k with gep_solver-----
times_NoCV_k_imp_gen = matrix(data=NA,nrow = length(grid_size),ncol=length(time_instants))
times_NoCV_k_imp_gen = as.data.frame(times_NoCV_k_imp_gen)
row.names(times_NoCV_k_imp_gen) = as.character(grid_size)
colnames(times_NoCV_k_imp_gen)  = as.character(time_instants)


counter = 0
tot_it = length(time_instants)*length(grid_size)

for (i in 1:length(time_instants)) {
  for(j in 1:length(grid_size)){
    
    {
      set.seed(23032000)
      dim_grid           <- grid_size[j]                                         
      n                  <- time_instants[i]                                        
      t.grid             <- seq(0,1, length.out=dim_grid)
      X.sample <- far_1_1D(kernel_id = "gaussian", noise_id = "1", n = n, t.grid = t.grid, a = 0.5, burnin = 0)
    }
    
    #measure time
    start_time = Sys.time()
    test = PPC_KO(X.sample,"NoCV",k=3,ex_solver=FALSE)
    end_time = Sys.time()
    
    times_NoCV_k_imp_gen[j,i] = as.double(end_time-start_time)
    
    
    counter = counter + 1
    string_message = "
                  Test time no cv k imp gep solver: time instants: %d, grid size: %d "
    message <- sprintf(paste0(string_message,"/ Progress: %d/%d
    ") , n, dim_grid, counter, tot_it)
    setTxtProgressBar(txtProgressBar(min = 1, max = tot_it, style = 3), counter)
    cat("\r", message)
  }
}


# for the grid_size = 10000: time is measured in minutes: convert it 
times_NoCV_k_imp_gen[which(grid_size==10000),] = times_NoCV_k_imp_gen[which(grid_size==10000),]*60

if(save_res){
  save(times_NoCV_k_imp_gen, file = paste0(path_store_res,"/times_NoCV_k_imp_gen.Rdata"))
}



## -----selecting for k via explanatory power retained PPC-----
times_NoCV_k_no_imp = matrix(data=NA,nrow = length(grid_size),ncol=length(time_instants))
times_NoCV_k_no_imp = as.data.frame(times_NoCV_k_no_imp)
row.names(times_NoCV_k_no_imp) = as.character(grid_size)
colnames(times_NoCV_k_no_imp)  = as.character(time_instants)


counter = 0
tot_it = length(time_instants)*length(grid_size)

for (i in 1:length(time_instants)) {
  for(j in 1:length(grid_size)){
    
    {
      set.seed(23032000)
      dim_grid           <- grid_size[j]                                         
      n                  <- time_instants[i]                                        
      t.grid             <- seq(0,1, length.out=dim_grid)
      X.sample <- far_1_1D(kernel_id = "gaussian", noise_id = "1", n = n, t.grid = t.grid, a = 0.5, burnin = 0)
    }
    
    #measure time
    start_time = Sys.time()
    test = PPC_KO(X.sample,"NoCV",k=0)
    end_time = Sys.time()
    
    times_NoCV_k_no_imp[j,i] = as.double(end_time-start_time)
    
    
    counter = counter + 1
    string_message = "
                  Test time no cv k not imp: time instants: %d, grid size: %d "
    message <- sprintf(paste0(string_message,"/ Progress: %d/%d
    ") , n, dim_grid, counter, tot_it)
    setTxtProgressBar(txtProgressBar(min = 1, max = tot_it, style = 3), counter)
    cat("\r", message)
  }
}

# for the grid_size = 10000: time is measured in minutes: convert it 
times_NoCV_k_no_imp[which(grid_size==10000),] = times_NoCV_k_no_imp[which(grid_size==10000),]*60


# save results
if(save_res){
  save(times_NoCV_k_no_imp, file = paste0(path_store_res,"/times_NoCV_k_no_imp.Rdata"))
}
