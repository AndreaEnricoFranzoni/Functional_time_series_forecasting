
#' 
#' Estimation of a FAR(1) non-concurrent model through EK method.
#' 
#' The method relies on FPCA, so we have to extend it to two-dimensional
#' functional data. We rely in this case on an expansion on a basis system.
#' As basis system we choose the tensor product of two fourier basis systems.
#' Notice that such resulting system is orthonormal by construction.
#' If we instead use b-spline systems, we should add a matrix term W.
#' 
#' In other codes, we will use  also grid discretization for FPCA.
#' 
#' We simulate data as lists of lists of matrices.
#' However the codes for conformal deal with lists if lists of numeric vectors.
#' Therefore, we will transform data from a list of matrices to a list
#' of lists of vectors, thus vectorizing the two-dimensional matrices!
#' 

close3d()
rm(list=ls())
graphics.off()
cat("\014")

setwd("D:/Poli/TESI/Code/Time-Series-CP/FAR_2D")
source("
simulate_FAR.R")

# Simulating FAR ---------------------------------------------------------------

nbasis.1 = 4
nbasis.2 = 4
nbasis = nbasis.1*nbasis.2

#definisco Psi1, Psi2, my_Sigma
Psi1=matrix(0.3,nbasis,nbasis)
diag(Psi1)=0.8
Psi1= Psi1/norm(Psi1, type = "F")/2
Psi2=matrix(0.1,nbasis,nbasis)
diag(Psi2)=0.5
Psi2= Psi2/norm(Psi2, type = "F")/2
Psi=array(0,c(nbasis,nbasis,2))
Psi[,,1]=Psi1
Psi[,,2]=Psi2
my_Sigma=matrix(0.6,nbasis,nbasis)
diag(my_Sigma)=1
my_Sigma=my_Sigma/2

set.seed(0)
n = 30
out <- 
simulate_FAR(n = (n+1), Psi = NULL, x1.grid=NULL, x2.grid=NULL, 
             nbasis.1=nbasis.1, nbasis.2=nbasis.2, sigma=my_Sigma)

## removing last observation ----
Xt = out$Xt # list of matrices
Xt_list = Xt[1:n]
X_new_true_mat = Xt[[n+1]]

# 2D Basis definition ----------------------------------------------------------

# grid: [0,1]x[0,1]
x1.grid <- seq(0, 1, by=0.05)
x2.grid <- seq(0, 1, by=0.05)

# number of basis functions in each dimension
nbasis.1 = 5
nbasis.2 = 5
nbasis = nbasis.1*nbasis.2

# build 1D basis functions on each of the 1D domains
basis.1 <- fda::create.fourier.basis(rangeval=range(x1.grid), nbasis=nbasis.1)
basis.2 <- fda::create.fourier.basis(rangeval=range(x2.grid), nbasis=nbasis.2)

# evaluate 1D basis functions on the grid
basis.1.eval <- fda::eval.basis(evalarg=x1.grid, basisobj=basis.1)
basis.2.eval <- fda::eval.basis(evalarg=x2.grid, basisobj=basis.2)

# 2D basis functions evaluated on the grid
basis.grid.eval <- vector("list", length = nbasis)
grid.2D <- expand.grid(x1.grid, x2.grid)
counter = 1

# considero ogni combinazione di basi
for(i in 1:nbasis.1)
{
  base_i_j = matrix(nrow=length(x1.grid),ncol=length(x2.grid))
  for(j in 1:nbasis.2)
  {
    # tensor product (ma sui punti della griglia, è li stess)
    base_i_j = outer(basis.1.eval[,i],basis.2.eval[,j])
    
    basis.grid.eval[[counter]] = base_i_j
    counter = counter+1
  }
}

nbasis = nbasis.1 * nbasis.2
nbasis

# Project data on the basis ----------------------------------------------------

n_points = length(out$grid.1) * length(out$grid.2) # num of points in the grid
n_points

## Vectorize Xt ----

Xt_mat = vapply(Xt_list, as.numeric, numeric(length=n_points)) # n_points x n
dim(Xt_mat)

## Vectorize basis -----

basis_eval = vapply(basis.grid.eval, as.numeric, numeric(length=n_points)) # n_points x nbasis

## Project data on the basis ----

B = crossprod(basis_eval)
X = crossprod(basis_eval, Xt_mat)
C = solve(B,X) # nbasis x n

# solve vs fda::symsolve:
#B = matrix(rnorm(n=1e6), nrow=1e3, ncol=1e3)
#B = crossprod(B)
#X = matrix(rnorm(n=1e6), nrow=1e3, ncol=1e3)
#library(tictoc)
#tic()
#AAA=solve(B,X)
#toc()
#tic()
#BBB=fda::symsolve(B,X)
#toc()
#all.equal(AAA,BBB)

# EFPC -------------------------------------------------------------------------

eigen_dec = eigen(tcrossprod(C)/n, symmetric=TRUE)
lambda = eigen_dec$values # numeric(nbasis)
efpc = eigen_dec$vectors # nbasis x nbasis

# cumsum(lambda)/sum(lambda) # cumulative proportion of explained variance
cum_prop_var = 0.95
nharm = which.max(cumsum(lambda)/sum(lambda) > cum_prop_var)
nharm

lambda = lambda[1:nharm] # numeric(nharm)
efpc = efpc[,1:nharm] # nbasis x nharm 

## Evaluate EFPCS on the grid ----

efpc_eval = basis_eval %*% efpc # npoints x nharm

# Project data onto the EFPCS ----

B = crossprod(efpc_eval)
X = crossprod(efpc_eval, Xt_mat)
C_efpc = solve(B,X) # nharm x n

# scores of FPCA
SCORES = t(C_efpc) # n x nharm
dim(SCORES)

# KERNEL estimation ------------------------------------------------------------

# number of time steps in the dataset
N = n

# compute coefficients of PSI expanded on the EFPC's
PSI = crossprod(SCORES[2:N,], SCORES[1:(N-1),]) %*% diag(1/(lambda*(N-1)),nrow=nharm,ncol=nharm)

# check matricial product  with a double for cicle
PSI_2=matrix(NA,nharm,nharm)
for(i in 1:nharm){
  for(j in 1:nharm){
    PSI_2[i,j] = 1/(N-1) * sum( SCORES[1:(N-1),j]*SCORES[2:N,i] ) / lambda[j]
  }
}
all.equal(PSI,PSI_2)

# Prediction -------------------------------------------------------------------

# FPC's evaluated on the time grids
dim(efpc_eval) # n_points x nharm

# prediction on the grid
X_new_vec = as.numeric( efpc_eval %*% (PSI %*% SCORES[N,]) )

# check matricial product  with a double for cicle
X_new_vec_2 = numeric(n_points)
for(h in 1:n_points){
  X_new_vec_2[h] = sum(efpc_eval[h,] %*% PSI %*% SCORES[N,])
}
all.equal(X_new_vec, X_new_vec_2)

# prediction in matricial form
X_new_mat = matrix(X_new_vec, nrow=sqrt(n_points))
dim(X_new_mat)

## Plot X_(n+1) ----
library(rgl)
persp3d(x1.grid, x2.grid, X_new_mat, col='red')
persp3d(x1.grid, x2.grid, X_new_true_mat, col='yellow', add=TRUE)

