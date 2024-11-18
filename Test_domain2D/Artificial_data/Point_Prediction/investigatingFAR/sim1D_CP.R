
rm(list = ls())

# construct set of basis functions ----

x1.grid = seq(0, 1, by=0.05)
nbasis.1.sim = 5
basis.1 = fda::create.bspline.basis(rangeval=range(x1.grid), nbasis=nbasis.1.sim)
basis.1.eval = fda::eval.basis(evalarg=x1.grid, basisobj=basis.1) 
dim(basis.1.eval) # npoints x d

# simulate VAR(1) ----

d=nbasis.1.sim
Psi1=matrix(0.3,d,d)
diag(Psi1)=0.8
Psi1=Psi1/norm(Psi1, type = "F")*0.7
Psi=array(0,c(d,d,1))
Psi[,,1]=Psi1
# my_Sigma=matrix(0.1,d,d) ######################## TODOOOOOOOOOOOOO #############################
# my_Sigma=matrix(0.6,d,d)
# diag(my_Sigma)=1
# my_Sigma=my_Sigma/4

# d = nbasis.1.sim
# Psi1 = matrix(0,d,d)
# diag(Psi1)=1.0
# Psi1
# Psi1 = Psi1/norm(Psi1, type = "F")*0.7
# Psi1
# Psi = array(0,c(d,d,1))
# Psi[,,1] = Psi1
my_Sigma = diag(rep(1,d))
my_Sigma = my_Sigma/1000
my_Sigma
# my_Sigma=matrix(0.6,d,d)
# diag(my_Sigma)=1
# my_Sigma=my_Sigma/2


nbasis=nbasis.1.sim

n=100
burnin=0

# order of the autoregressive model
p=dim(Psi)[3]
noise = "mnorm"
df = 4
# sigma = diag((nbasis.1.sim):1)/(nbasis.1.sim)
# d = dimension of the underlying VAR(1)

# simulate a VAR(p) for the the coefficients of basis projection
simulation_seed = 1
set.seed(simulation_seed)
arg <- list()
arg[['n']] <- n
arg[['d']] <- nbasis
arg[['Psi']] <- Psi
arg[['burnin']] <- burnin
arg[['noise']] <- noise
arg[['sigma']] <- my_Sigma	
arg[['df']] <- df
X = do.call(freqdom::rar, arg) # (n x nbasis)

# build FAR(1) ----

Xfun = tcrossprod(X, basis.1.eval) # n x npoints
colfunc <- colorRampPalette(c("black", "white"))
colors = colfunc(n)
matplot(x1.grid, t(Xfun), 'l', col = colors, lty=rep(1,n))

# use CP_point_prediction -----------------------------------------------------

data_y = vector("list", length = n)
for(i in 1:n){
  data_y[[i]] = list(Xfun[i,])
}

# indexes_not_NA=NULL
l = floor(0.5*(n-1))
b=1
seed_split=1
# x1.grid=NULL
# x2.grid=NULL
fitted_order=1
# FPCA:
FPCA_method="discretization"
basis.type="bspline"
grid.step.num.int=0.001
nbasis.1=NULL
nbasis.2=NULL
cum_prop_var=NULL
nharm=4
# other things:
center=FALSE

# CHECKS AND INITIALIZATION --------------------------------------------------

if (is.null(seed_split)==TRUE || (seed_split!=FALSE & is.numeric(seed_split)==FALSE)) stop("Argument 'seed_split' must be either FALSE or an integer.")
if(is.list(data_y)==FALSE || is.data.frame(data_y)==TRUE || is.list(data_y[[1]])==FALSE || is.data.frame(data_y[[1]])==TRUE){ #le prime due condizioni son per assicurarsi che sia una lista nel senso di "list" e non un dataframe (che ? una lista tecnicamente), 
  
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

# DIVISION TRAINING-CALIBRATION ----------------------------------------------
# fitted_order=1
burn_in=fitted_order

m=n-l-burn_in

if(seed_split!=FALSE){set.seed(seed_split)}
training=sample((burn_in+1):n,m)
training=sort(training)
calibration=setdiff((burn_in+1):n,training)
calibration=sort(calibration)
excluded_set=1:burn_in

# COMPUTATION ----------------------------------------------------------------

## 1 -- From list to matrix --------------------------------------------------

# total number of points in the grid
# if(is.null(indexes_not_NA)){
#   n_points = length(x1.grid) * length(x2.grid)
# } else {
#   n_points = sum(indexes_not_NA)
# }
n_points = length(x1.grid)

# Vectorize data
Xt_mat = vapply(simplify2array(data_y, as.numeric), as.numeric, numeric(length=n_points)) # n_points x n
dim(Xt_mat)

Xt_mat.train = Xt_mat[,training]
Xt_mat.calibration.burnin = Xt_mat[,-training]

## 2 -- Mean centering -------------------------------------------------------

center = FALSE
if(center)
{
  # mean is estimated on the training set
  mean.train = rowMeans(Xt_mat.train)
  
  Xt_mat.train = sweep(Xt_mat.train, 1, mean.train)
  Xt_mat.calibration.burnin = sweep(Xt_mat.calibration.burnin, 1, mean.train)
}

## 3 -- FPCA -----------------------------------------------------------------

if(!(FPCA_method %in% c("discretization","basis"))){
  stop("invalid method for FPCA. Please select one between discretization and basis")
}

if(FPCA_method == "discretization")
{
  ### Discretization ---------------------------------------------------------
  
  # discretization step
  w = 1/(n_points-1)
  
  # pca
  pca = prcomp(t(Xt_mat.train), center=FALSE, scale.=FALSE)
  
  # eigenvalues of FPCA
  lambda = (pca$sdev^2) * w
  
  # number of EFPC's to consider
  if(is.null(nharm) & !is.null(cum_prop_var))
  {
    # choose the number of harmonics by cumulative proportion of variance
    nharm = as.integer(which.max(cumsum(lambda)/sum(lambda)>cum_prop_var))
    
    #print(paste0("nharm=",nharm))
    #print(cumsum(lambda)/sum(lambda))
  }
  
  # consider the first nharm eigenvalues
  lambda = lambda[1:nharm]
  
  # FPC's evaluated on the time grid
  efpc_eval = pca$rotation[,1:nharm] / sqrt(w) # length(grid) x nharm
  
  # projection of training data on FPC's
  scores.train = pca$x[,1:nharm] * sqrt(w) # n x nharm
  
  rm(pca)
  
} 


# NICO check projection

Xt_mat.train.reconstruct = tcrossprod(efpc_eval, scores.train)
dim(Xt_mat.train.reconstruct) 

colfunc <- colorRampPalette(c("black", "white"))
colors = colfunc(m)
matplot(x1.grid, Xt_mat.train, 'l', col = colors, lty=rep(1,m))
matplot(x1.grid, Xt_mat.train.reconstruct, 'l', col = colors, lty=rep(1,m))

## 4 -- Project calibration set onto FPC's -----------------------------------

B = efpc_eval
Y = Xt_mat.calibration.burnin

Bmat = crossprod(B)
Dmat = crossprod(B,Y)
coef = solve(Bmat, Dmat) # nharm x (l+burn_in)

scores.calibration.burnin = t(coef) # (l+burn_in) x nharm

rm(B,Y,Bmat,Dmat,coef)

# NICO check projection
Xt_mat.calibration.burnin.reconstruct = tcrossprod(efpc_eval, scores.calibration.burnin)
dim(Xt_mat.calibration.burnin) 
matplot(x1.grid, Xt_mat.calibration.burnin, 'l', col = colors, lty=rep(1,m))
matplot(x1.grid, Xt_mat.calibration.burnin.reconstruct, 'l', col = colors, lty=rep(1,m))

## 5 -- SCORES ---------------------------------------------------------------

# Scores of all the n original functions projected on the first "nharm" EFPC's
SCORES = matrix(NA, nrow=n, ncol=nharm)
SCORES[training,] = scores.train
SCORES[-training,] = scores.calibration.burnin

#print(paste0("SCORES dim: ", dim(SCORES)[1], "x", dim(SCORES)[2]))

## 6 -- Parameters Estimation ------------------------------------------------

# matrix of coefficients, row j contains coefficients for score j 
my_coeff = matrix(NA, nrow=nharm, ncol=(nharm)*fitted_order)

# build matrix of regressors. It is the same for all the scores
covariates = c()
for(h in 1:fitted_order){
  covariates = cbind(covariates, SCORES[training-h,])
}

# per ogni scores stimo i coefficienti di quella riga
for(j in 1:nharm){
  my_coeff[j,] = lm(SCORES[training,j] ~ -1 + covariates)$coefficients
}
#rm(j,h,covariates)

my_coeff
# my_coeff_dynml

# NICO check prediction
# SCORES_train_predict = tcrossprod(cbind(rep(1,m), SCORES[training-1,]), my_coeff)
SCORES_train_predict = tcrossprod(SCORES[training-1,], my_coeff)
SCORES_train_predict
SCORES[training,]
colors = colfunc(4)
matplot(1:m, SCORES[training,], 'l', col = colors, lty=rep(1,4))
matplot(1:m, SCORES_train_predict, 'l', col = colors, lty=rep(3,4))

{
  x11()
  par(mfrow=c(2,2))
  plot(SCORES[training,1])
}

{
x11()
par(mfrow=c(2,2))
plot(1:m, SCORES[training,1], 'l')
lines(1:m, SCORES_train_predict[,1], 'l', col="red")
plot(1:m, SCORES[training,2], 'l')
lines(1:m, SCORES_train_predict[,2], 'l', col="red")
plot(1:m, SCORES[training,3], 'l')
lines(1:m, SCORES_train_predict[,3], 'l', col="red")
plot(1:m, SCORES[training,4], 'l')
lines(1:m, SCORES_train_predict[,4], 'l', col="red")
}


{
  x11()
  par(mfrow=c(nharm,nharm))
  par(mar = c(1, 1, 1, 1))
  for(i in 1:nharm){
    for(j in 1:nharm){
      plot(SCORES[training,i] ~ SCORES[training-1,j])
      abline(lm(SCORES[training,i] ~ -1 +SCORES[training-1,j]))
    }
  }
}

## 7 -- Prediction initialization --------------------------------------------

# Inizializzo la matrice che conterr? le previsioni per il training e calibration set
predicted_fun_matrix=matrix(0,m+l+burn_in,n_points)
predicted_fun_matrix[excluded_set,]=NA #no previsione per le osservazioni del "burn-in"


## 8 -- Predict training set ------------------------------------------------

predicted_fun_matrix[excluded_set,]=NA #no previsione per le osservazioni del "burn-in"

# build matrix of regressors
covariates = c()
for(h in 1:fitted_order){
  covariates = cbind(covariates, SCORES[training-h,])
}
# covariates = cbind(rep(1,m), covariates)

# predict scores
SCORES_pred = tcrossprod(covariates, my_coeff) # m x nharm

# reconstruct values
predicted_fun_matrix[training,] = tcrossprod(SCORES_pred, efpc_eval) # m x n_points

# rm(h, covariates, SCORES_pred)

colors = colfunc(m)
matplot(x1.grid, Xt_mat.train, 'l', col = colors, lty=rep(1,m))
matplot(x1.grid, t(predicted_fun_matrix[training,]), 'l', col = colors, lty=rep(1,m))
# 
# x11()
# par(mfrow=c(2,2))
# plot(1:m, SCORES[training,1], 'l')
# lines(1:m, SCORES_pred[,1], 'l', col="red")
# plot(1:m, SCORES[training,2], 'l')
# lines(1:m, SCORES_pred[,2], 'l', col="red")
# plot(1:m, SCORES[training,3], 'l')
# lines(1:m, SCORES_pred[,3], 'l', col="red")
# plot(1:m, SCORES[training,4], 'l')
# lines(1:m, SCORES_pred[,4], 'l', col="red")


### add mean -----------------------------------------------------------------
if(center){
  predicted_fun_matrix[training,] = sweep(predicted_fun_matrix[training,], 2, mean.train, "+")
}

## 9 -- Predict calibration set ---------------------------------------------

# Dal calibration, ottengo gli indici da cui calcolo i NCS (..._included) 
# e quelli esclusi in accordo al parametro b (..._excluded)

obs_calibration_included=sort(calibration)[seq(from=b,to=l,by=b)] #saranno (l+1)/b-1
obs_calibration_excluded=setdiff(calibration,obs_calibration_included)

# build matrix of regressors
covariates = c()
for(h in 1:fitted_order){
  covariates = cbind(covariates, SCORES[obs_calibration_included-h,])
}
covariates = cbind(rep(1,l), covariates)

# predict scores
SCORES_pred = tcrossprod(covariates, my_coeff) # length(obs_calibration_included) x nharm

# reconstruct values
predicted_fun_matrix[obs_calibration_included,] = tcrossprod(SCORES_pred, efpc_eval) # m x n_points

rm(h, covariates, SCORES_pred)

colors = colfunc(m)
matplot(x1.grid, Xt_mat.calibration.burnin, 'l', col = colors, lty=rep(1,m))
matplot(x1.grid, t(predicted_fun_matrix[obs_calibration_included,]), 'l', col = colors, lty=rep(1,m))



### add mean -----------------------------------------------------------------
if(center){
  predicted_fun_matrix[obs_calibration_included,] = sweep(predicted_fun_matrix[obs_calibration_included,], 2, mean.train, "+")
}

## 10 -- Predict (n+1) time --------------------------------------------------

# build matrix of regressors
covariates = c()
for(h in 1:fitted_order){
  covariates = c(covariates, SCORES[n-h,])
}

# predict scores
SCORES_pred = tcrossprod(covariates, my_coeff) # length(obs_calibration_included) x nharm

#print(nharm)

# reconstruct values
predicted_fun_n_plus_1 = as.numeric(tcrossprod(SCORES_pred, efpc_eval)) # n_points x 1

rm(h, covariates, SCORES_pred)

### add mean -----------------------------------------------------------------
if(center){
  predicted_fun_n_plus_1 = predicted_fun_n_plus_1 + mean.train
}

## 11 -- From matrix to list -------------------------------------------------

predicted_fun=NULL
for (i in 1:n){
  predicted_fun=c(predicted_fun,list(list(predicted_fun_matrix[i,])))
}
rm(predicted_fun_matrix)

predicted_fun_n_plus_1=list(list(predicted_fun_n_plus_1))


# OUTPUT----------------------------------------------------------------------
