rm(list=ls())
graphics.off()
cat("\014")

set.seed(23032000)
#change here
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series"
source(paste0(dir_w,"/utils/data/far_1_1d.R"))
source(paste0(dir_w,"/utils/functions.R"))
source(paste0(dir_w,"/utils/prediction_error.R"))

##################################################################
################### REPRODUCTION OF KOKOSZKA PAPER ###############
##################################################################



## Data parameters 
left_ex            <- 0
right_ex           <- 1
dim_grid           <- 200
n                  <- 100                            #time instants of the functional temporal series
burnin             <- 50                             #burnin iterations for FAR(1)
N                  <- n - burnin                     #actual instants that will be taken into account  
traj_to_be_plotted <- 6
t.grid             <- seq(left_ex,right_ex, length.out=dim_grid)  #domain of the statistical units

#PPCKO parameters (default parameter, except for id_CV)
{
  id_CV_ko      <- "CV"           #Ko algorithm
  alpha         <- 0.75
  k             <- 0
  threshold_ppc <- 0.95
  alpha_vec     <- NULL
  k_vec         <- NULL
  toll          <- 1e-4
  disc_ev       <- NULL           #has to be the same as t.grid
  left_extreme  <- left_ex        #has to be the same left extreme as t.grid
  right_extreme <- right_ex       #has to be the same right extreme as t.grid
  err_ret       <- 0
  id_rem_nan    <- NULL
}

#data (only four kernels, two norm, three errors implemented)
{
  #feats of data
  id_kernel <- "sp_t"   #way of generating data 
  norm      <- 0.8          #Kernel constant (for the L2 norm of the kernel that has to be <1)
  id_noise  <- "1"          #error of the FAR(1) process
  
  #grid for the FAR(1)
  s.grid <- seq(0,1, length.out=200)
  grid   <- expand.grid(t.grid, s.grid)
}

proc = feat_far_1_process(id_kernel,norm)
id_kernel   <- proc$kernel
a           <- proc$constant
name_kernel <- proc$name



##Simulate a stationary FAR(1) process according to a specific kernel
X.sample <- far_1_1D(kernel_id = id_kernel, noise_id = id_noise, n = n, t.grid = t.grid, a = a, burnin = burnin)
#center them
X.eval <- t(scale(t(X.sample), center=TRUE, scale=FALSE))
Xmean.eval <- rowMeans(X.sample)


#errors vector
{
  err.dPPC.KO_en <- numeric(N)     #PF:  Kargin-Onatski PPC
  err.dPPC.KO_rn <- numeric(N) 
  err.dPPC.KO_en2 <- numeric(N)     #PF with extra toll:  Kargin-Onatski PPC
  err.dPPC.KO_rn2 <- numeric(N) 
  err.dEK_en     <- numeric(N)     #EK:  Kokoszka Estimated Kernel
  err.dEK_rn     <- numeric(N)
  err.dEKI_en    <- numeric(N)     #EKI: Kokoszka Estimated Kernel Improved
  err.dEKI_rn    <- numeric(N)
  err.perf_en    <- numeric(N)     #EX:  exact prediction
  err.perf_rn    <- numeric(N)
  err.mean_en    <- numeric(N)     #MP:  mean prediction
  err.mean_rn    <- numeric(N)
  err.naive_en   <- numeric(N)     #NP:  naive prediction
  err.naive_rn   <- numeric(N)
}


#to plot the predicted 6 consecutive trajectories
X_true_plot <- matrix(data=0, nrow=length(t.grid), ncol=traj_to_be_plotted)
X_pred_plot <- matrix(data=0, nrow=length(t.grid), ncol=traj_to_be_plotted)



## Monte Carlo simulation ------------------------------------------------------------
{ 
  
  #Exact predictor
  exact_pred = innovation(id_kernel)
  
  pb <- progress::progress_bar$new(
    format = " MC simulation [:bar] :percent in :elapsed",
    total = N, clear = FALSE, width= 60)
  for(b in 1:N) #b=1
  {
    #train and test set (also are necessary with data non centered)
    X.train <- X.eval[,b:(N-1+b)]
    X.test <- X.eval[,N+b]
    X.train_no_cent = X.sample[,b:(N-1+b)]
    X.test_no_cent = X.sample[,N+b]
    
    
    ## Estimate Psi with different methods
    KO_algo  <- PPCKO.local::PPC_KO( X = X.train_no_cent, id_CV = id_CV_ko )
    KO_algo2 <- PPCKO.local2::PPC_KO( X = X.train_no_cent, id_CV = id_CV_ko)
    KE_algo  <- KE.local::KE( X = X.train_no_cent, id_ke = "KE")
    KEI_algo <- KE.local::KE( X = X.train_no_cent, id_ke = "KEI")
    #Psihat.dPPC.KO <- KO_algo$rho_hat   
    #Psihat.dEK     <- EKdiscretized(X=X.train,p=3)
    #Psihat.dEKI    <- EKimproved(X=X.train,p=3)
    
    
    ## Evaluate the error on the test function
    #PF
    Xhat.dPPC.KO      <- KO_algo$`One-step ahead prediction`
    err.dPPC.KO_en[b] <- En(X.test_no_cent,Xhat.dPPC.KO,t.grid)
    err.dPPC.KO_rn[b] <- Rn(X.test_no_cent,Xhat.dPPC.KO,t.grid)
    
    Xhat.dPPC.KO2      <- KO_algo2$`One-step ahead prediction`
    err.dPPC.KO_en2[b] <- En(X.test_no_cent,Xhat.dPPC.KO2,t.grid)
    err.dPPC.KO_rn2[b] <- Rn(X.test_no_cent,Xhat.dPPC.KO2,t.grid)
    
    if(b<=traj_to_be_plotted)  #in order to plot this 6 trajectories
    {
      X_true_plot[,b] = X.sample[,N+b]
      X_pred_plot[,b] = Xhat.dPPC.KO
    }
    
    
    #EK
    #Xhat.dEK      <- Psihat.dEK %*% X.train[,N] + Xmean.eval
    Xhat.dEK      <- KE_algo$`One step ahead prediction`
    err.dEK_en[b] <- En(X.test_no_cent,Xhat.dEK,t.grid)
    err.dEK_rn[b] <- Rn(X.test_no_cent,Xhat.dEK,t.grid)
    
    #EKI
    #Xhat.dEKI   <- Psihat.dEKI %*% X.train[,N] + Xmean.eval
    Xhat.dEKI      <- KEI_algo$`One step ahead prediction`
    err.dEKI_en[b] <- En(X.test_no_cent,Xhat.dEKI,t.grid)
    err.dEKI_rn[b] <- Rn(X.test_no_cent,Xhat.dEKI,t.grid)
    
    #EX
    Xhat.perf   <- exact_pred(y=X.sample[,(N-1+b)], t.grid=t.grid, a=a)
    err.perf_en[b] <- En(X.test,Xhat.perf,t.grid)
    err.perf_rn[b] <- Rn(X.test,Xhat.perf,t.grid)
    
    #MP (data centered: the prediction is done with their mean, that is 0)
    err.mean_en[b]  <- En(X.test,rep(0,length(t.grid)),t.grid)
    err.mean_rn[b]  <- Rn(X.test,rep(0,length(t.grid)),t.grid)
    
    #NP (prediction is done using always the last observation)
    err.naive_en[b] <- En(X.test,X.eval[,n],t.grid)
    err.naive_rn[b] <- Rn(X.test,X.eval[,n],t.grid)
    
    pb$tick()
  }
}

###################
## BoxPlot of En ##
###################
err_en <- c(err.naive_en, err.perf_en, err.mean_en, err.dEK_en, err.dEKI_en, err.dPPC.KO_en, err.dPPC.KO_en2)
method <- rep(c("naive","perfect", "mean", "EK", "EKI", "PPC-KO", "PPC-KO2"), each=N)
En <- data.frame(method, err_en)
method_order<- c("naive", "perfect", "mean", "EK", "EKI", "PPC-KO", "PPC-KO2")
En.box <- En %>% mutate(method=factor(x=method, levels=method_order))

##grouped boxplot
quartz()
pgplot <- ggplot(En.box, aes(x=method, y=err_en, fill=method)) + 
  geom_boxplot() + ggtitle(name_kernel)
pgplot <- pgplot +
  #scale_y_continuous(limits=c(0,0.1)) +
  theme_bw() + 
  labs(x="", y="En", fill = "Prediction method") +
  #labs(x="", y=TeX(r'($\frac{1}{N} \; \sum_{j=1}^N (f_{t+1, j}^b - \hat{f}_{t+1,j}^b)^2$)'), fill="Prediction method") +
  theme(plot.title = element_text(face="bold", hjust=0.5, size=22),
        axis.text.x = element_text(size=22),
        axis.title.x = element_text(size=22),
        axis.text.y = element_text(size=22),
        axis.title.y = element_text(size=22),
        legend.title = element_text(size=22),
        legend.text = element_text(size=22),
        legend.position="bottom",
        legend.direction = "horizontal") +
  guides(fill=guide_legend(nrow=1, byrow=TRUE))
pgplot + 
  theme(legend.position="none")



###################
## BoxPlot of Rn ##
###################
err_rn <- c(err.naive_rn, err.perf_rn, err.mean_rn, err.dEK_rn, err.dEKI_rn, err.dPPC.KO_rn, err.dPPC.KO_rn2)
method <- rep(c("naive","perfect", "mean", "EK", "EKI", "PPC-KO", "PPC-KO2"), each=N)
Rn <- data.frame(method, err_rn)
method_order<- c("naive", "perfect", "mean", "EK", "EKI", "PPC-KO", "PPC-KO2")
Rn.box <- En %>% mutate(method=factor(x=method, levels=method_order))

##grouped boxplot
quartz()
pgplot <- ggplot(Rn.box, aes(x=method, y=err_rn, fill=method)) + 
  geom_boxplot()  + ggtitle(name_kernel)
pgplot <- pgplot +
  #scale_y_continuous(limits=c(0,0.1)) +
  theme_bw() + 
  labs(x="", y="Rn", fill = "Prediction method") +
  #labs(x="", y=TeX(r'($\frac{1}{N} \; \sum_{j=1}^N (f_{t+1, j}^b - \hat{f}_{t+1,j}^b)^2$)'), fill="Prediction method") +
  theme(plot.title = element_text(face="bold", hjust=0.5, size=22),
        axis.text.x = element_text(size=22),
        axis.title.x = element_text(size=22),
        axis.text.y = element_text(size=22),
        axis.title.y = element_text(size=22),
        legend.title = element_text(size=22),
        legend.text = element_text(size=22),
        legend.position="bottom",
        legend.direction = "horizontal") +
  guides(fill=guide_legend(nrow=1, byrow=TRUE))
pgplot + 
  theme(legend.position="none")




quartz()
par(mfrow=c(3,2))
for(i in 1:traj_to_be_plotted)
{
  plot(x=t.grid,y=X_true_plot[,i]-mean(X_true_plot[,i]),type='l',col='black',xlab="t",ylab = "Data", main=name_kernel)
  points(x=t.grid,y=X_pred_plot[,i]-mean(X_pred_plot[,i]),type='l',col='blue')
  legend("topright", legend = c("True","Predicted"), fill = c('black','blue'), bty = "n", cex = 0.9)
}


#mean and sd of simulations errors
#mean and standard error of the Ens
mean(err.dPPC.KO_en)
sqrt(var(err.dPPC.KO_en)/length(err.dPPC.KO_en))
mean(err.dPPC.KO_en2)
sqrt(var(err.dPPC.KO_en2)/length(err.dPPC.KO_en2))
mean(err.dEK_en)
sqrt(var(err.dEK_en)/length(err.dEK_en))
mean(err.dEKI_en)
sqrt(var(err.dEKI_en)/length(err.dEKI_en))
mean(err.perf_en)
sqrt(var(err.perf_en)/length(err.perf_en))
mean(err.mean_en)
sqrt(var(err.mean_en)/length(err.mean_en))
mean(err.naive_en)
sqrt(var(err.naive_en)/length(err.naive_en))



#mean and standard error of the Rns
mean(err.dPPC.KO_rn)
sqrt(var(err.dPPC.KO_rn)/length(err.dPPC.KO_rn))
mean(err.dPPC.KO_rn2)
sqrt(var(err.dPPC.KO_rn2)/length(err.dPPC.KO_rn2))
mean(err.dEK_rn)
sqrt(var(err.dEK_rn)/length(err.dEK_rn))
mean(err.dEKI_rn)
sqrt(var(err.dEKI_rn)/length(err.dEKI_rn))
mean(err.perf_rn)
sqrt(var(err.perf_rn)/length(err.perf_rn))
mean(err.mean_rn)
sqrt(var(err.mean_rn)/length(err.mean_rn))
mean(err.naive_rn)
sqrt(var(err.naive_rn)/length(err.naive_rn))
