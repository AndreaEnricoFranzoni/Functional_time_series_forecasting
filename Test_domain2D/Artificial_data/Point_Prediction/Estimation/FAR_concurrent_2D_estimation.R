
#' 
#' Estimation of a FAR(1) concurrent model through OLS in each location.
#' We assume an AR(p) in each grid point, and estiate its paramaters.
#' 
#' We simulate data as lists of lists of matrices.
#' However the codes for conformal deal with lists if lists of numeric vectors.
#' Therefore, we will transform data from a list of matrices to a list
#' of lists of vectors, thus vectorizing the two-dimensional matrices!
#' 

rm(list=ls())

setwd("D:/Poli/TESI/Code/Time-Series-CP/FAR_2D")
source("D:/Poli/TESI/Code/Time-Series-CP/functions_scen2sim.R")
source("
simulate_FAR.R")

# Data -------------------------------------------------------------------------

n.basis.1 = 5
n.basis.2 = 5
n.basis = n.basis.1*n.basis.2

#definisco Psi1, Psi2, my_Sigma
Psi1=matrix(0.3,n.basis,n.basis)
diag(Psi1)=0.8
Psi1= Psi1/norm(Psi1, type = "F")/2

Psi=array(0,c(n.basis,n.basis,1))
Psi[,,1]=Psi1

my_Sigma=matrix(0.6,n.basis,n.basis)
diag(my_Sigma)=1
my_Sigma=my_Sigma/2

set.seed(0)
n = 50
x1.grid= seq(0, 1, by=0.05)
x2.grid= seq(0, 1, by=0.05)
out <- 
simulate_FAR(n = 50, Psi = NULL, x1.grid= x1.grid, x2.grid= x2.grid, 
             nbasis.1=n.basis.1, nbasis.2=n.basis.2, sigma=my_Sigma)
names(out)

# Change data structure --------------------------------------------------------

# ora devo cambiare forma a Xt
# per ora è una lista di matrici
# Deve divenire una lista di n liste. Ogni sottolista avrà un singolo elemento,
# il quale sarà un vettore. Questo vettore è la matrice sulla griglia, ma vettorizzata,
# tanto non mi interessa la dipendenza spaziale: ogni location è indipendente dalle altre.

Xt = out$Xt
my.data = vector("list", length = n)

# for each time
for(t in 1:n)
{
  my.data[[t]] = vector("list", length = 1)
  my.data[[t]][[1]] = as.vector(Xt[[t]])
}

# Droppo l'ultimo tempo:
y.true = my.data[n]
my.data = my.data[-n]

# Prediction ----------------------------------------------------------------------

#--- Point prediction
simulation_seed = 1
sample_size = n
l = sample_size*0.5
b = 1
fitted_order = 1
alpha = 0.1

point_prediction=point_prediction_conformal_TS_sc2_globalsplit(data_y=my.data, 
                                                               l=l, 
                                                               b=b, 
                                                               fitted_order=fitted_order, 
                                                               seed_split=simulation_seed,
                                                               grid=seq(from=0,to=1,length=length_grid) )
#--- Obtaining k,s ...
conformal_TS=functional_conformal_prediction_TS_sc2_globalsplit(data_y=point_prediction$data_y,
                                                                hat_y=point_prediction$predicted_fun,
                                                                t_values=NULL,
                                                                m=point_prediction$m,
                                                                alpha=alpha,
                                                                s_type="st-dev",
                                                                randomized=FALSE,
                                                                seed_tau=FALSE,
                                                                training=point_prediction$training,
                                                                obs_calibration_included=point_prediction$obs_calibration_included)

#--- Obtaining prediction sets
prediction_set=computation_conformal_prediction_set(hat_y=point_prediction$predicted_fun_n_plus_1,
                                                    observed_y=y.true,
                                                    k_s=conformal_TS$k_s,
                                                    s=conformal_TS$s,
                                                    alpha=conformal_TS$alpha,
                                                    randomized=conformal_TS$randomized,
                                                    extremes_are_included=conformal_TS$extremes_are_included)

#--- Output
y_pointwise = unlist(point_prediction$predicted_fun_n_plus_1)
y_inf = unlist(prediction_set$inf_lim)
y_sup = unlist(prediction_set$sup_lim)

# included?
prediction_set$extremes_are_included

# devo tornare a delle matrici
ngrid1 = length(x1.grid)
ngrid2 = length(x2.grid)
y_pointwise_matrix = matrix(y_pointwise, nrow=ngrid1, ncol=ngrid2) # byrow=TRUE)
y_inf_matrix = matrix(y_inf, nrow=ngrid1, ncol=ngrid2) # byrow=TRUE)
y_sup_matrix = matrix(y_sup, nrow=ngrid1, ncol=ngrid2) # byrow=TRUE)
y_true_matrix = matrix(unlist(y.true), nrow=ngrid1, ncol=ngrid2) # byrow=TRUE)
  
library(rgl)

# BANDS:
persp3d(x1.grid, x2.grid, y_inf_matrix, col='red', alpha=0.3,
        xlab = "", ylab = "", zlab = "")
persp3d(x1.grid, x2.grid, y_pointwise_matrix, col='yellow', add=TRUE, alpha=0.3)
persp3d(x1.grid, x2.grid, y_sup_matrix, col='red', add=TRUE, alpha=0.3)
persp3d(x1.grid, x2.grid, y_true_matrix, col='blue', add=TRUE)
legend3d("topright", legend = c("Point-prediction", paste0(100*(1-alpha),"%CI"), "Actual y(t)"), 
         pch = 16, col = c("yellow","red","blue"), cex=1)
rgl.snapshot(filename = "prediction_bands.png", fmt = "png", top = TRUE )




