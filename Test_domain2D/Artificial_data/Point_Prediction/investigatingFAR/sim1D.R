
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
my_Sigma=matrix(0.6,d,d)
diag(my_Sigma)=1
my_Sigma=my_Sigma/2

nbasis=nbasis.1.sim

n=10
burnin=100

# order of the autoregressive model
p=dim(Psi)[3]
noise = "mnorm"
df = 4
sigma = diag((nbasis.1.sim):1)/(nbasis.1.sim)
basis.type = "fourier"
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
arg[['sigma']] <- sigma	
arg[['df']] <- df
X=do.call(freqdom::rar, arg) # (n x nbasis)

# build FAR(1) ----

Xfun = tcrossprod(X, basis.1.eval) # n x npoints

# FPCA: estimate fpca scores ----

# discretization step
n_points = length(x1.grid)
w = 1/(n_points-1)

# pca
pca = prcomp(Xfun, center=FALSE, scale.=FALSE) # Xfun is n x npoints, quindi nxp

# eigenvalues of FPCA
lambda = (pca$sdev^2) * w

# cum prop of variance
cumsum(lambda)/sum(lambda)

# number of EFPC's to consider
nharm = 4

# consider the first nharm eigenvalues
lambda = lambda[1:nharm]

# FPC's evaluated on the time grid
efpc_eval = pca$rotation[,1:nharm] / sqrt(w) # n_points x nharm

# projection of training data on FPC's
scores.train = pca$x[,1:nharm] * sqrt(w) # n x nharm

# predict VAR(1) on the fpca scores ----

psi = matrix(NA, nrow=nharm, ncol=nharm)
for(j in 1:nharm){
  psi[j,] = lm(scores.train[2:n,j] ~ -1 + scores.train[1:(n-1),])$coefficients
}
scores_pred = tcrossprod(scores.train[1:n,], psi)

dim(scores_pred) # n x nharm

# reconstruct functions ----

Xfun_pred = tcrossprod(scores_pred, efpc_eval)

dim(Xfun_pred) # n x npoints

# plot predicted -----

t = 10
plot(x1.grid, Xfun[t,], 'l')
lines(x1.grid, Xfun_pred[t-1,], 'l', col='red')

# Xfun        # n x npoints (t=1,...,n)
# Xfun_pred   # n x npoints (t=2,...,n+1)









