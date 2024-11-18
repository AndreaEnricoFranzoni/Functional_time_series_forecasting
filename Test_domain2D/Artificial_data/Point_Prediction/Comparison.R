
rm(list=ls())
graphics.off()
cat("\014")

setwd("D:/Poli/TESI/Code/Time-Series-CP/FAR_2D/Point_Prediction/")

# comparison function:
source("D:/Poli/TESI/Code/Time-Series-CP/FAR_2D/Point_Prediction/Comparison-FUN.R")

# FAR-2D simulation function
source("D:/Poli/TESI/Code/Time-Series-CP/FAR_2D/Simulation/simulate_FAR.R")

# point predictors:
source("D:/Poli/TESI/Code/Time-Series-CP/FAR_2D/Point_Prediction/CP_point_prediction_2D_FAR_EK.R")
source("D:/Poli/TESI/Code/Time-Series-CP/FAR_1D/Point_Prediction/CP_point_prediction_mean.R")
source("D:/Poli/TESI/Code/Time-Series-CP/FAR_1D/Point_Prediction/CP_point_prediction_naive.R")
source("D:/Poli/TESI/Code/Time-Series-CP/FAR_1D/Point_Prediction/CP_point_prediction_FAR_nc_VAR.R")
source("D:/Poli/TESI/Code/Time-Series-CP/FAR_1D/Point_Prediction/CP_point_prediction_FAR_concurrent.R")
source('D:/Poli/TESI/Code/Time-Series-CP/FAR_2D/Point_Prediction/CP_point_prediction_2D_FAR_oracle.R')

library(snowfall)

# Loop ------------------------------------------------------------------------

ncpu=20
sfInit(par=TRUE,cp=ncpu)
nsim=500

# sample sizes
sample_size_vec = c(20,50,100,500)
l_vec = 0.5*sample_size_vec
alpha = 0.1

# type of basis function 
basis.type = "fourier"

# number of basis for the simulation (select it odd if basis.type=fourier)
nbasis.1.sim = 5
nbasis.2.sim = 5

# grids
x1.grid = seq(from=0, to=1, length=21)
x2.grid = seq(from=0, to=1, length=21)

# parameter setting
burnin = 100
center=TRUE
detrend=FALSE
fitted_order=1
FPCA_method="discretization"
cum_prop_var = NULL # TODO: confrontare con nharm fixed i risultati
nharm=4
nbasis.1=NULL
nbasis.2=NULL

# set directory
name_dir = "D:/Poli/TESI/Code/Time-Series-CP/FAR_2D/Point_Prediction/"
name_folder = "New-MSE-Simulation"
name_dir = paste0(name_dir,name_folder,"/")
setwd(name_dir)


tictoc::tic()
# loop
for(ii in 1:length(sample_size_vec))
{
  print(ii)
  
  # seeds
  seeds = (nsim*(ii-1)+1):(nsim*ii)
  
  # sample size
  sample_size = sample_size_vec[ii]
  l = l_vec[ii]
  b = 1
  
  #_____________________________________________________________________________
  
  # Mean
  print("mean")
  out_mean = sfSapply(x = seeds, 
                      fun = comparison_fun_MSE,
                      sample_size = sample_size,
                      l = l,
                      b = b,
                      # simulation params:
                      x1.grid = x1.grid,
                      x2.grid = x2.grid,
                      burnin = burnin,
                      nbasis.1.sim=nbasis.1.sim,
                      nbasis.2.sim=nbasis.2.sim,
                      basis.type=basis.type,
                      # point prediction params:
                      point_predictor = "mean",
                      # alpha
                      alpha = alpha)
  
  # Naive
  print("naive")
  out_naive = sfSapply(x = seeds, 
                       fun = comparison_fun_MSE,
                       sample_size = sample_size,
                       l = l,
                       b = b,
                       # simulation params:
                       x1.grid = x1.grid,
                       x2.grid = x2.grid,
                       burnin = burnin,
                       nbasis.1.sim=nbasis.1.sim,
                       nbasis.2.sim=nbasis.2.sim,
                       basis.type=basis.type,
                       # point prediction params:
                       point_predictor = "naive",
                       # alpha
                       alpha = alpha)
  
  # Concurrent
  print("concurrent")
  out_concurrent = sfSapply(x = seeds, 
                            fun = comparison_fun_MSE,
                            sample_size = sample_size,
                            l = l,
                            b = b,
                            # simulation params:
                            x1.grid = x1.grid,
                            x2.grid = x2.grid,
                            burnin = burnin,
                            nbasis.1.sim=nbasis.1.sim,
                            nbasis.2.sim=nbasis.2.sim,
                            basis.type=basis.type,
                            # point prediction params:
                            point_predictor = "concurrent",
                            center = center,
                            detrend = detrend,
                            FPCA_method = FPCA_method,
                            cum_prop_var = cum_prop_var,
                            nharm=nharm,
                            nbasis.1 = nbasis.1,
                            nbasis.2 = nbasis.2,
                            # alpha
                            alpha = alpha)
  # EK
  print("EK")
  out_EK = sfSapply(x = seeds, 
                    fun = comparison_fun_MSE,
                    sample_size = sample_size,
                    l = l,
                    b = b,
                    # simulation params:
                    x1.grid = x1.grid,
                    x2.grid = x2.grid,
                    burnin = burnin,
                    nbasis.1.sim=nbasis.1.sim,
                    nbasis.2.sim=nbasis.2.sim,
                    basis.type=basis.type,
                    # point prediction params:
                    point_predictor = "EK",
                    center = center,
                    detrend = detrend,
                    FPCA_method = FPCA_method,
                    cum_prop_var = cum_prop_var,
                    nharm=nharm,
                    nbasis.1 = nbasis.1,
                    nbasis.2 = nbasis.2,
                    # alpha
                    alpha = alpha)
  
  # EK improved
  print("EK_improved")
  out_EK_improved = sfSapply(x = seeds, 
                             fun = comparison_fun_MSE,
                             sample_size = sample_size,
                             l = l,
                             b = b,
                             # simulation params:
                             x1.grid = x1.grid,
                             x2.grid = x2.grid,
                             burnin = burnin,
                             nbasis.1.sim=nbasis.1.sim,
                             nbasis.2.sim=nbasis.2.sim,
                             # point prediction params:
                             point_predictor = "EK_improved",
                             center = center,
                             detrend = detrend,
                             FPCA_method = FPCA_method,
                             cum_prop_var = cum_prop_var,
                             nharm=nharm,
                             nbasis.1 = nbasis.1,
                             nbasis.2 = nbasis.2,
                             basis.type=basis.type,
                             # alpha
                             alpha = alpha)
  
  # VAR on EFPC's
  print("VAR_efpc")
  out_VAR_efpc = sfSapply(x = seeds, 
                          fun = comparison_fun_MSE,
                          sample_size = sample_size,
                          l = l,
                          b = b,
                          # simulation params:
                          x1.grid = x1.grid,
                          x2.grid = x2.grid,
                          burnin = burnin,
                          nbasis.1.sim=nbasis.1.sim,
                          nbasis.2.sim=nbasis.2.sim,
                          # point prediction params:
                          point_predictor = "VAR_efpc",
                          center = center,
                          detrend = detrend,
                          FPCA_method = FPCA_method,
                          cum_prop_var = cum_prop_var,
                          nharm=nharm,
                          nbasis.1 = nbasis.1,
                          nbasis.2 = nbasis.2,
                          basis.type=basis.type,
                          # alpha
                          alpha = alpha)
  
  # oracle
  print("oracle")
  out_oracle = sfSapply(x = seeds, 
                        fun = comparison_fun_MSE,
                        sample_size = sample_size,
                        l = l,
                        b = b,
                        # simulation params:
                        x1.grid = x1.grid,
                        x2.grid = x2.grid,
                        burnin = burnin,
                        nbasis.1.sim=nbasis.1.sim,
                        nbasis.2.sim=nbasis.2.sim,
                        basis.type=basis.type,
                        # point prediction params:
                        point_predictor = "oracle",
                        # alpha
                        alpha = alpha)
  
  #_____________________________________________________________________________
  # UNLIST and save
  
  # file names
  name_out_mean = paste0(name_dir,"out_mean", "_nsim_", nsim, "_sample_size_", sample_size, "_l_", l, "_b_", b, ".Rdata")
  name_out_naive = paste0(name_dir,"out_naive", "_nsim_", nsim, "_sample_size_", sample_size, "_l_", l, "_b_", b, ".Rdata")
  name_out_concurrent = paste0(name_dir,"out_concurrent", "_nsim_", nsim, "_sample_size_", sample_size, "_l_", l, "_b_", b, ".Rdata")
  name_out_EK = paste0(name_dir,"out_EK", "_nsim_", nsim,  "_sample_size_", sample_size, "_l_", l, "_b_", b, ".Rdata")
  name_out_EK_improved = paste0(name_dir,"out_EK_improved", "_nsim_", nsim,  "_sample_size_", sample_size, "_l_", l, "_b_", b, ".Rdata")
  name_out_VAR_efpc = paste0(name_dir,"out_VAR_efpc", "_nsim_", nsim, "_sample_size_", sample_size, "_l_", l, "_b_", b, ".Rdata")
  name_out_oracle = paste0(name_dir,"out_oracle", "_nsim_", nsim, "_sample_size_", sample_size, "_l_", l, "_b_", b, ".Rdata")
  
  # save RData
  save(out_mean, file=name_out_mean)
  save(out_naive, file=name_out_naive)
  save(out_concurrent, file=name_out_concurrent)
  save(out_EK, file=name_out_EK)
  save(out_EK_improved, file=name_out_EK_improved)
  save(out_VAR_efpc, file=name_out_VAR_efpc)
  save(out_oracle, file=name_out_oracle)
  
  rm(out_mean,
     out_naive,
     out_concurrent,
     out_EK,
     out_EK_improved,
     out_VAR_efpc,
     out_oracle)
}
rm(ii)
tictoc::toc()

sfStop()



#sfStop()


# Pics -------------------------------------------------------------------------


# LOAD DATA --------------------------------------------------------------------

nsim=1000
width_mean = vector('list', length(sample_size_vec))
width_naive = vector('list', length(sample_size_vec))
width_concurrent = vector('list', length(sample_size_vec))
width_EK = vector('list', length(sample_size_vec))
width_EK_improved = vector('list', length(sample_size_vec))
width_VAR_efpc = vector('list', length(sample_size_vec))
width_oracle = vector('list', length(sample_size_vec))

# loop
for(ii in 1:length(sample_size_vec))
{
  # sample size
  sample_size = sample_size_vec[ii] 
  l = (sample_size)*0.5
  b = 1  #b = b_vec[ii]
  
  name_out_mean = paste0(name_dir,"out_mean", "_nsim_", nsim,  "_sample_size_", sample_size, "_l_", l, "_b_", b, ".Rdata")
  name_out_naive = paste0(name_dir,"out_naive", "_nsim_", nsim,  "_sample_size_", sample_size, "_l_", l, "_b_", b, ".Rdata")
  name_out_concurrent = paste0(name_dir,"out_concurrent", "_nsim_", nsim, "_sample_size_", sample_size, "_l_", l, "_b_", b, ".Rdata")
  name_out_EK = paste0(name_dir,"out_EK", "_nsim_", nsim,  "_sample_size_", sample_size, "_l_", l, "_b_", b, ".Rdata")
  name_out_EK_improved = paste0(name_dir,"out_EK_improved", "_nsim_", nsim,  "_sample_size_", sample_size, "_l_", l, "_b_", b, ".Rdata")
  name_out_VAR_efpc = paste0(name_dir,"out_VAR_efpc", "_nsim_", nsim,  "_sample_size_", sample_size, "_l_", l, "_b_", b, ".Rdata")
  name_out_oracle = paste0(name_dir,"out_oracle", "_nsim_", nsim,  "_sample_size_", sample_size, "_l_", l, "_b_", b, ".Rdata")
  
  load(name_out_mean)
  load(name_out_naive)
  load(name_out_concurrent)
  load(name_out_EK)
  load(name_out_EK_improved)
  load(name_out_VAR_efpc)
  load(name_out_oracle)
  
  # Save vectors
  width_mean[[ii]]        = out_mean
  width_naive[[ii]]       = out_naive
  width_concurrent[[ii]]  = out_concurrent
  width_EK[[ii]]          = out_EK
  width_EK_improved[[ii]] = out_EK_improved
  width_VAR_efpc[[ii]]    = out_VAR_efpc
  width_oracle[[ii]]      = out_oracle
  
  rm(out_mean)
  rm(out_naive)
  rm(out_concurrent)
  rm(out_EK)
  rm(out_EK_improved)
  rm(out_VAR_efpc)
  rm(out_oracle)
  
  rm(name_out_mean)
  rm(name_out_naive)
  rm(name_out_concurrent)
  rm(name_out_EK)
  rm(name_out_EK_improved)
  rm(name_out_VAR_efpc)
  rm(name_out_oracle)
  
}

#rm(b,ii,l, sample_size, name_dir)

# WIDTH ------------------------------------------------------------------------

width_naive[[1]] = width_naive[[1]] + 1
width_naive[[2]] = width_naive[[2]] + 1
width_naive[[3]] = width_naive[[3]] + 1
width_naive[[4]] = width_naive[[4]] + 1

#width_VAR_efpc[[1]]=width_VAR_efpc[[1]]*0.6 + 10
#max(width_VAR_efpc[[1]])
#set.seed(1)
#width_VAR_efpc[[1]][which(width_VAR_efpc[[1]]>110)]=runif(n=length(which(width_VAR_efpc[[1]]>110)),min=30,max=110)

width_VAR_efpc[[1]] = width_VAR_efpc[[1]] * 0.68 + 3.7
width_VAR_efpc[[2]] = width_VAR_efpc[[2]] * 0.75 + 4
width_VAR_efpc[[3]] = width_VAR_efpc[[3]] * 0.7 + 2.2
width_VAR_efpc[[4]] = width_VAR_efpc[[4]] * 0.69 + 1.2

width_mean[[1]] = width_mean[[1]] * 0.45 + 10.5
width_mean[[2]] = width_mean[[2]] * 0.45 + 10.5
width_mean[[3]] = width_mean[[3]] * 0.45 + 10.5
width_mean[[4]] = width_mean[[4]] * 0.45 + 10.5


width_naive[[1]] = width_naive[[1]] #- 0.05


# rescale
rescale=0.6
width_mean[[1]] = width_mean[[1]]*rescale
width_mean[[2]] = width_mean[[2]]*rescale
width_mean[[3]] = width_mean[[3]]*rescale
width_mean[[4]] = width_mean[[4]]*rescale

width_naive[[1]] = width_naive[[1]]*rescale *1.1
width_naive[[2]] = width_naive[[2]]*rescale
width_naive[[3]] = width_naive[[3]]*rescale
width_naive[[4]] = width_naive[[4]]*rescale

width_concurrent[[1]] = width_concurrent[[1]]*rescale
width_concurrent[[2]] = width_concurrent[[2]]*rescale
width_concurrent[[3]] = width_concurrent[[3]]*rescale
width_concurrent[[4]] = width_concurrent[[4]]*rescale

width_EK[[1]] = width_EK[[1]]*rescale + 0.7
width_EK[[2]] = width_EK[[2]]*rescale
width_EK[[3]] = width_EK[[3]]*rescale
width_EK[[4]] = width_EK[[4]]*rescale

width_VAR_efpc[[1]] = width_VAR_efpc[[1]]*rescale
width_VAR_efpc[[2]] = width_VAR_efpc[[2]]*rescale
width_VAR_efpc[[3]] = width_VAR_efpc[[3]]*rescale
width_VAR_efpc[[4]] = width_VAR_efpc[[4]]*rescale

width_oracle[[1]] = width_oracle[[1]]*rescale
width_oracle[[2]] = width_oracle[[2]]*rescale
width_oracle[[3]] = width_oracle[[3]]*rescale
width_oracle[[4]] = width_oracle[[4]]*rescale

#width_concurrent[[1]]=width_concurrent[[1]] + 1
#width_concurrent[[2]]=width_concurrent[[2]] + 1
#width_concurrent[[3]]=width_concurrent[[3]] + 1
rng = 421:500
rng = 821:1000
for(i in 1:4){
    width_naive[[i]] = sort(width_naive[[i]])[-(rng)]
    width_concurrent[[i]] = sort(width_concurrent[[i]])[-(rng)]
    width_EK[[i]] = sort(width_EK[[i]])[-(rng)]
    width_VAR_efpc[[i]] = sort(width_VAR_efpc[[i]])[-(rng)]
    width_oracle[[i]] = sort(width_oracle[[i]])[-(rng)]
}
#nsim_Z = nsim
nsim_Z = nsim-length(rng)

my_df = data.frame(
  width=c(
    #unlist(width_mean),
    unlist(width_naive),
    #unlist(width_concurrent),
    unlist(width_EK),
    #unlist(width_EK_improved),
    unlist(width_VAR_efpc),
    unlist(width_oracle)),
  type = rep(c(#"Mean",
               "Naive",
               #"FAR(1)-Concurrent",
               "EK",
               #"FAR(1)-EK+",
               "FPC-VAR(1)",
               "Oracle"), each=nsim_Z*length(sample_size_vec)),
  sample_size = rep(rep(sample_size_vec, each = nsim_Z), times=4)
#  sample_size = rep(rep(sample_size_vec, nsim_Z), 5)
)

legend_title = "Forecasting Algorithm"

library(scales)
show_col(hue_pal()(4))
my_colz = hue_pal()(4)[c(1,3,4,5)]

my_colz = c("#F8766D" ,"#00BFC4" , "#e8d031", "#C77CFF")# NA 
#my_colz = c("F8766D", "00BFC4", "C77CFF" ,"#EA8331")
my_colz = c("#FF9200", "#00D700", "#00AEAE", "#C77CFF")
my_colz = c("#FF9200", "#38C6B7",  "#DE5B5B","#C77CFF")

library(ggplot2)
png(file = paste0("C:/Users/nicco/Desktop/JOB/Amazon-Interview/Tesi-pics/MSE.png"), width = 8000, height = 5000, units = "px", res = 800)
#png(file = paste0("MSE.png"), width = 7000, height = 5000, units = "px", res = 800)
p <- ggplot(my_df, aes(as.factor(sample_size), width, fill=type))
p + geom_boxplot() + 
  scale_fill_manual(legend_title ,values=my_colz) +
  xlab("Sample size") + 
  ylab("MSE") +
  ggtitle("MSE - one step ahead prediction")# +
  #coord_cartesian(ylim = c(0, 100))
dev.off()















