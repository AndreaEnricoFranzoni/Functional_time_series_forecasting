##################################################################
################### COMPARISON BETWEEN DIFFERENT CV ##############
##################################################################
##
## tests for compare CV on both regularization parameter alpha and
## number of PPCs retained k vs CV only on alpha and number of PPCs
## that has at least some explanatory power
##
## For all the eights types of data generation taken into account,
## simulations are performed: rolling window, KO used to predict next
## time instants, and then prediction accuracy on the next time instant
## is evaluated through MSE and MAE estimators. 
## KO ALGOS: - CV on {alpha,k}
##           - CV on alpha and PPCs that retain 75% of explanatory power
##           - CV on alpha and PPCs that retain 85% of explanatory power
##           - CV on alpha and PPCs that retain 95% of explanatory power


rm(list=ls())
graphics.off()
cat("\014")

#change here 
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series"

# if you want to use already saved results: TRUE; if you want to run simulations: FALSE
use_saved_res = TRUE
# il you want to run simulations and then also saving the results: TRUE; if don't want to save them: FALSE
saves_new_res = FALSE

# to load the functions to generate the data
source(paste0(dir_w,"/Test_domain1D/Artificial_data/utils/far_1_1d.R"))
# to open the graphic window according to your operative system
source(paste0(dir_w,"/Test_domain1D/Artificial_data/utils/graphic_window.R"))

#to directly load the obtained results
if(use_saved_res){
  load(paste0(dir_w,"/Test_domain1D/Artificial_data/results/CV_comparison/CV_comp_res.Rdata"))
}else{
  ## ----- Data parameters -----
  {
    left_ex            <- 0                                           #left extreme of the domain of the functional data
    right_ex           <- 1                                           #right extreme of the domain of the functional data
    dim_grid           <- 200                                         #number of discrete evaluations of the functional statistical units
    n                  <- 100                                         #time instants of the functional time series
    burnin             <- 50                                          #burnin iterations for FAR(1)
    N                  <- n - burnin                                  #instants that will actually be taken into account  
    t.grid             <- seq(left_ex,right_ex, length.out=dim_grid)  #grid for the discrete evaluation of the statistical units
  }
  
  
  ## ----- PPCKO parameters -----
  {
    alpha         <- 0.1                                #regularization parameter
    k             <- 0                                  #if a number of PPCs has to be retained
    alpha_vec     <- c(1e-4,1e-3,1e-2,1e-1,1,1e1,1e2)   #alpha values for CV on regularization parameter
    k_vec         <- NULL                               #k values for CV on number of PPCs
    toll          <- 1e-4                               #toll for stopping adding others PPCs during CV
    disc_ev       <- t.grid                             #discrete evaluations of the functional domain (has to be the same as t.grid)
    left_extreme  <- left_ex                            #left extreme of the domain of the functional data (has to be the same as left_ex)
    right_extreme <- right_ex                           #right extreme of the domain of the functional data (has to be the same as right_ex)
    err_ret       <- 0                                  #if CV errors have to be retained
    id_rem_nan    <- NULL
    
    # algos used
    cv1 = "CV"
    cv2 = "CV_alpha"
  }
  
  
  ## ----- errors: estimation of norm L2 errors (en) and norm L1 errors (rn) -----
  {
    #gaussian
    err_KO_gau_0_5_Sim_CV_en <- numeric(N)
    err_KO_gau_0_5_Sim_CV_rn <- numeric(N)
    err_KO_gau_0_5_Sim_CV_alpha_thres_0_75_en <- numeric(N)
    err_KO_gau_0_5_Sim_CV_alpha_thres_0_75_rn <- numeric(N)
    err_KO_gau_0_5_Sim_CV_alpha_thres_0_85_en <- numeric(N)
    err_KO_gau_0_5_Sim_CV_alpha_thres_0_85_rn <- numeric(N)
    err_KO_gau_0_5_Sim_CV_alpha_thres_0_95_en <- numeric(N)
    err_KO_gau_0_5_Sim_CV_alpha_thres_0_95_rn <- numeric(N)
    
    err_KO_gau_0_8_Sim_CV_en <- numeric(N)
    err_KO_gau_0_8_Sim_CV_rn <- numeric(N)
    err_KO_gau_0_8_Sim_CV_alpha_thres_0_75_en <- numeric(N)
    err_KO_gau_0_8_Sim_CV_alpha_thres_0_75_rn <- numeric(N)
    err_KO_gau_0_8_Sim_CV_alpha_thres_0_85_en <- numeric(N)
    err_KO_gau_0_8_Sim_CV_alpha_thres_0_85_rn <- numeric(N)
    err_KO_gau_0_8_Sim_CV_alpha_thres_0_95_en <- numeric(N)
    err_KO_gau_0_8_Sim_CV_alpha_thres_0_95_rn <- numeric(N)
    
    #identity
    err_KO_id_0_5_Sim_CV_en <- numeric(N)
    err_KO_id_0_5_Sim_CV_rn <- numeric(N)
    err_KO_id_0_5_Sim_CV_alpha_thres_0_75_en <- numeric(N)
    err_KO_id_0_5_Sim_CV_alpha_thres_0_75_rn <- numeric(N)
    err_KO_id_0_5_Sim_CV_alpha_thres_0_85_en <- numeric(N)
    err_KO_id_0_5_Sim_CV_alpha_thres_0_85_rn <- numeric(N)
    err_KO_id_0_5_Sim_CV_alpha_thres_0_95_en <- numeric(N)
    err_KO_id_0_5_Sim_CV_alpha_thres_0_95_rn <- numeric(N)
    
    err_KO_id_0_8_Sim_CV_en <- numeric(N)
    err_KO_id_0_8_Sim_CV_rn <- numeric(N)
    err_KO_id_0_8_Sim_CV_alpha_thres_0_75_en <- numeric(N)
    err_KO_id_0_8_Sim_CV_alpha_thres_0_75_rn <- numeric(N)
    err_KO_id_0_8_Sim_CV_alpha_thres_0_85_en <- numeric(N)
    err_KO_id_0_8_Sim_CV_alpha_thres_0_85_rn <- numeric(N)
    err_KO_id_0_8_Sim_CV_alpha_thres_0_95_en <- numeric(N)
    err_KO_id_0_8_Sim_CV_alpha_thres_0_95_rn <- numeric(N)
    
    #sp t
    err_KO_spt_0_5_Sim_CV_en <- numeric(N)
    err_KO_spt_0_5_Sim_CV_rn <- numeric(N)
    err_KO_spt_0_5_Sim_CV_alpha_thres_0_75_en <- numeric(N)
    err_KO_spt_0_5_Sim_CV_alpha_thres_0_75_rn <- numeric(N)
    err_KO_spt_0_5_Sim_CV_alpha_thres_0_85_en <- numeric(N)
    err_KO_spt_0_5_Sim_CV_alpha_thres_0_85_rn <- numeric(N)
    err_KO_spt_0_5_Sim_CV_alpha_thres_0_95_en <- numeric(N)
    err_KO_spt_0_5_Sim_CV_alpha_thres_0_95_rn <- numeric(N)
    
    err_KO_spt_0_8_Sim_CV_en <- numeric(N)
    err_KO_spt_0_8_Sim_CV_rn <- numeric(N)
    err_KO_spt_0_8_Sim_CV_alpha_thres_0_75_en <- numeric(N)
    err_KO_spt_0_8_Sim_CV_alpha_thres_0_75_rn <- numeric(N)
    err_KO_spt_0_8_Sim_CV_alpha_thres_0_85_en <- numeric(N)
    err_KO_spt_0_8_Sim_CV_alpha_thres_0_85_rn <- numeric(N)
    err_KO_spt_0_8_Sim_CV_alpha_thres_0_95_en <- numeric(N)
    err_KO_spt_0_8_Sim_CV_alpha_thres_0_95_rn <- numeric(N)
    
    
    #sp s
    err_KO_sps_0_5_Sim_CV_en <- numeric(N)
    err_KO_sps_0_5_Sim_CV_rn <- numeric(N)
    err_KO_sps_0_5_Sim_CV_alpha_thres_0_75_en <- numeric(N)
    err_KO_sps_0_5_Sim_CV_alpha_thres_0_75_rn <- numeric(N)
    err_KO_sps_0_5_Sim_CV_alpha_thres_0_85_en <- numeric(N)
    err_KO_sps_0_5_Sim_CV_alpha_thres_0_85_rn <- numeric(N)
    err_KO_sps_0_5_Sim_CV_alpha_thres_0_95_en <- numeric(N)
    err_KO_sps_0_5_Sim_CV_alpha_thres_0_95_rn <- numeric(N)
    
    err_KO_sps_0_8_Sim_CV_en <- numeric(N)
    err_KO_sps_0_8_Sim_CV_rn <- numeric(N)
    err_KO_sps_0_8_Sim_CV_alpha_thres_0_75_en <- numeric(N)
    err_KO_sps_0_8_Sim_CV_alpha_thres_0_75_rn <- numeric(N)
    err_KO_sps_0_8_Sim_CV_alpha_thres_0_85_en <- numeric(N)
    err_KO_sps_0_8_Sim_CV_alpha_thres_0_85_rn <- numeric(N)
    err_KO_sps_0_8_Sim_CV_alpha_thres_0_95_en <- numeric(N)
    err_KO_sps_0_8_Sim_CV_alpha_thres_0_95_rn <- numeric(N)
  }
  
  
  ## ----- data generation -----
  {
    id_noise  <- "1"          #error of the FAR(1) process ("1", "2" or "3")
    
    #grid for the kernel for generating FAR(1)
    s.grid <- seq(0,1, length.out=dim_grid)
    grid   <- expand.grid(t.grid, s.grid)
    
    # gaussian kernel with norm 0.5
    proc_gaus_0_5 = feat_far_1_process("gaussian",0.5)
    id_kernel_gaus_0_5   <- proc_gaus_0_5$kernel
    a_gaus_0_5           <- proc_gaus_0_5$constant
    name_kernel_gaus_0_5 <- proc_gaus_0_5$name
    X_gaus_0_5 <- far_1_1D(kernel_id = id_kernel_gaus_0_5, noise_id = id_noise, n = n, t.grid = t.grid, a = a_gaus_0_5, burnin = burnin)
    
    # gaussian kernel with norm 0.8
    proc_gaus_0_8 = feat_far_1_process("gaussian",0.8)
    id_kernel_gaus_0_8   <- proc_gaus_0_8$kernel
    a_gaus_0_8           <- proc_gaus_0_8$constant
    name_kernel_gaus_0_8 <- proc_gaus_0_8$name
    X_gaus_0_8 <- far_1_1D(kernel_id = id_kernel_gaus_0_8, noise_id = id_noise, n = n, t.grid = t.grid, a = a_gaus_0_8, burnin = burnin)
    
    
    # identity kernel with norm 0.5
    proc_id_0_5 = feat_far_1_process("identity",0.5)
    id_kernel_id_0_5   <- proc_id_0_5$kernel
    a_id_0_5           <- proc_id_0_5$constant
    name_kernel_id_0_5 <- proc_id_0_5$name
    X_id_0_5 <- far_1_1D(kernel_id = id_kernel_id_0_5, noise_id = id_noise, n = n, t.grid = t.grid, a = a_id_0_5, burnin = burnin)
    
    # identity kernel with norm 0.8
    proc_id_0_8 = feat_far_1_process("identity",0.8)
    id_kernel_id_0_8   <- proc_id_0_8$kernel
    a_id_0_8           <- proc_id_0_8$constant
    name_kernel_id_0_8 <- proc_id_0_8$name
    X_id_0_8 <- far_1_1D(kernel_id = id_kernel_id_0_8, noise_id = id_noise, n = n, t.grid = t.grid, a = a_id_0_8, burnin = burnin)
    
    
    # sloping plane t kernel with norm 0.5
    proc_sp_t_0_5 = feat_far_1_process("sp_t",0.5)
    id_kernel_sp_t_0_5   <- proc_sp_t_0_5$kernel
    a_sp_t_0_5           <- proc_sp_t_0_5$constant
    name_kernel_sp_t_0_5 <- proc_sp_t_0_5$name
    X_sp_t_0_5 <- far_1_1D(kernel_id = id_kernel_sp_t_0_5, noise_id = id_noise, n = n, t.grid = t.grid, a = a_sp_t_0_5, burnin = burnin)
    
    # sloping plane t kernel with norm 0.8
    proc_sp_t_0_8 = feat_far_1_process("sp_t",0.8)
    id_kernel_sp_t_0_8   <- proc_sp_t_0_8$kernel
    a_sp_t_0_8           <- proc_sp_t_0_8$constant
    name_kernel_sp_t_0_8 <- proc_sp_t_0_8$name
    X_sp_t_0_8 <- far_1_1D(kernel_id = id_kernel_sp_t_0_8, noise_id = id_noise, n = n, t.grid = t.grid, a = a_sp_t_0_8, burnin = burnin)
    
    
    # sloping plane s kernel with norm 0.5
    proc_sp_s_0_5 = feat_far_1_process("sp_s",0.5)
    id_kernel_sp_s_0_5   <- proc_sp_s_0_5$kernel
    a_sp_s_0_5           <- proc_sp_s_0_5$constant
    name_kernel_sp_s_0_5 <- proc_sp_s_0_5$name
    X_sp_s_0_5 <- far_1_1D(kernel_id = id_kernel_sp_s_0_5, noise_id = id_noise, n = n, t.grid = t.grid, a = a_sp_s_0_5, burnin = burnin)
    
    # sloping plane s kernel with norm 0.8
    proc_sp_s_0_8 = feat_far_1_process("sp_s",0.8)
    id_kernel_sp_s_0_8   <- proc_sp_s_0_8$kernel
    a_sp_s_0_8           <- proc_sp_s_0_8$constant
    name_kernel_sp_s_0_8 <- proc_sp_s_0_8$name
    X_sp_s_0_8 <- far_1_1D(kernel_id = id_kernel_sp_s_0_8, noise_id = id_noise, n = n, t.grid = t.grid, a = a_sp_s_0_8, burnin = burnin)
  }
  
  
  
  
  ## ----- MC simulations (could take a while) -----
  
  pb <- progress::progress_bar$new(
    format = " MC simulation [:bar] :percent in :elapsed",
    total = N, clear = FALSE, width= 60)
  for(b in 1:N){
    
    # ----- gaussian kernel with norm 0.5 -----
    X_train_gau_0_5 = X_gaus_0_5[,b:(N-1+b)]
    X_test_gau_0_5 = X_gaus_0_5[,N+b]
    
    KO_cv  <- PPCKO::PPC_KO( X = X_train_gau_0_5, id_CV = cv1 , alpha_vec=alpha_vec)
    KO_alpha_0_75 = PPCKO::PPC_KO( X = X_train_gau_0_5, id_CV = cv2, threshold_ppc = 0.75, alpha_vec=alpha_vec)
    KO_alpha_0_85 = PPCKO::PPC_KO( X = X_train_gau_0_5, id_CV = cv2, threshold_ppc = 0.85, alpha_vec=alpha_vec)
    KO_alpha_0_95 = PPCKO::PPC_KO( X = X_train_gau_0_5, id_CV = cv2, threshold_ppc = 0.95, alpha_vec=alpha_vec)
    
    pred_cv = KO_cv$`One-step ahead prediction`
    pred_cv_alpha_0_75 = KO_alpha_0_75$`One-step ahead prediction`
    pred_cv_alpha_0_85 = KO_alpha_0_85$`One-step ahead prediction`
    pred_cv_alpha_0_95 = KO_alpha_0_95$`One-step ahead prediction`
    
    err_KO_gau_0_5_Sim_CV_en[b] <- MLmetrics::MSE(pred_cv,X_test_gau_0_5)
    err_KO_gau_0_5_Sim_CV_rn[b] <- MLmetrics::MAE(pred_cv,X_test_gau_0_5)
    err_KO_gau_0_5_Sim_CV_alpha_thres_0_75_en[b] <- MLmetrics::MSE(pred_cv_alpha_0_75,X_test_gau_0_5)
    err_KO_gau_0_5_Sim_CV_alpha_thres_0_75_rn[b] <- MLmetrics::MAE(pred_cv_alpha_0_75,X_test_gau_0_5)
    err_KO_gau_0_5_Sim_CV_alpha_thres_0_85_en[b] <- MLmetrics::MSE(pred_cv_alpha_0_85,X_test_gau_0_5)
    err_KO_gau_0_5_Sim_CV_alpha_thres_0_85_rn[b] <- MLmetrics::MAE(pred_cv_alpha_0_85,X_test_gau_0_5)
    err_KO_gau_0_5_Sim_CV_alpha_thres_0_95_en[b] <- MLmetrics::MSE(pred_cv_alpha_0_95,X_test_gau_0_5)
    err_KO_gau_0_5_Sim_CV_alpha_thres_0_95_rn[b] <- MLmetrics::MAE(pred_cv_alpha_0_95,X_test_gau_0_5)
    
    
    
    # ----- gaussian kernel with norm 0.8 -----
    X_train_gau_0_8 = X_gaus_0_8[,b:(N-1+b)]
    X_test_gau_0_8 = X_gaus_0_8[,N+b]
    
    KO_cv  <- PPCKO::PPC_KO( X = X_train_gau_0_8, id_CV = cv1 , alpha_vec=alpha_vec)
    KO_alpha_0_75 = PPCKO::PPC_KO( X = X_train_gau_0_8, id_CV = cv2, threshold_ppc = 0.75, alpha_vec=alpha_vec)
    KO_alpha_0_85 = PPCKO::PPC_KO( X = X_train_gau_0_8, id_CV = cv2, threshold_ppc = 0.85, alpha_vec=alpha_vec)
    KO_alpha_0_95 = PPCKO::PPC_KO( X = X_train_gau_0_8, id_CV = cv2, threshold_ppc = 0.95, alpha_vec=alpha_vec)
    
    pred_cv = KO_cv$`One-step ahead prediction`
    pred_cv_alpha_0_75 = KO_alpha_0_75$`One-step ahead prediction`
    pred_cv_alpha_0_85 = KO_alpha_0_85$`One-step ahead prediction`
    pred_cv_alpha_0_95 = KO_alpha_0_95$`One-step ahead prediction`
    
    err_KO_gau_0_8_Sim_CV_en[b] <- MLmetrics::MSE(pred_cv,X_test_gau_0_8)
    err_KO_gau_0_8_Sim_CV_rn[b] <- MLmetrics::MAE(pred_cv,X_test_gau_0_8)
    err_KO_gau_0_8_Sim_CV_alpha_thres_0_75_en[b] <- MLmetrics::MSE(pred_cv_alpha_0_75,X_test_gau_0_8)
    err_KO_gau_0_8_Sim_CV_alpha_thres_0_75_rn[b] <- MLmetrics::MAE(pred_cv_alpha_0_75,X_test_gau_0_8)
    err_KO_gau_0_8_Sim_CV_alpha_thres_0_85_en[b] <- MLmetrics::MSE(pred_cv_alpha_0_85,X_test_gau_0_8)
    err_KO_gau_0_8_Sim_CV_alpha_thres_0_85_rn[b] <- MLmetrics::MAE(pred_cv_alpha_0_85,X_test_gau_0_8)
    err_KO_gau_0_8_Sim_CV_alpha_thres_0_95_en[b] <- MLmetrics::MSE(pred_cv_alpha_0_95,X_test_gau_0_8)
    err_KO_gau_0_8_Sim_CV_alpha_thres_0_95_rn[b] <- MLmetrics::MAE(pred_cv_alpha_0_95,X_test_gau_0_8)
    
    
    
    
    # ----- identiy kernel with norm 0.5 -----
    X_train_id_0_5 = X_id_0_5[,b:(N-1+b)]
    X_test_id_0_5 = X_id_0_5[,N+b]
    
    KO_cv  <- PPCKO::PPC_KO( X = X_train_id_0_5, id_CV = cv1, alpha_vec=alpha_vec )
    KO_alpha_0_75 = PPCKO::PPC_KO( X = X_train_id_0_5, id_CV = cv2, threshold_ppc = 0.75, alpha_vec=alpha_vec)
    KO_alpha_0_85 = PPCKO::PPC_KO( X = X_train_id_0_5, id_CV = cv2, threshold_ppc = 0.85, alpha_vec=alpha_vec)
    KO_alpha_0_95 = PPCKO::PPC_KO( X = X_train_id_0_5, id_CV = cv2, threshold_ppc = 0.95, alpha_vec=alpha_vec)
    
    pred_cv = KO_cv$`One-step ahead prediction`
    pred_cv_alpha_0_75 = KO_alpha_0_75$`One-step ahead prediction`
    pred_cv_alpha_0_85 = KO_alpha_0_85$`One-step ahead prediction`
    pred_cv_alpha_0_95 = KO_alpha_0_95$`One-step ahead prediction`
    
    err_KO_id_0_5_Sim_CV_en[b] <- MLmetrics::MSE(pred_cv,X_test_id_0_5)
    err_KO_id_0_5_Sim_CV_rn[b] <- MLmetrics::MAE(pred_cv,X_test_id_0_5)
    err_KO_id_0_5_Sim_CV_alpha_thres_0_75_en[b] <- MLmetrics::MSE(pred_cv_alpha_0_75,X_test_id_0_5)
    err_KO_id_0_5_Sim_CV_alpha_thres_0_75_rn[b] <- MLmetrics::MAE(pred_cv_alpha_0_75,X_test_id_0_5)
    err_KO_id_0_5_Sim_CV_alpha_thres_0_85_en[b] <- MLmetrics::MSE(pred_cv_alpha_0_85,X_test_id_0_5)
    err_KO_id_0_5_Sim_CV_alpha_thres_0_85_rn[b] <- MLmetrics::MAE(pred_cv_alpha_0_85,X_test_id_0_5)
    err_KO_id_0_5_Sim_CV_alpha_thres_0_95_en[b] <- MLmetrics::MSE(pred_cv_alpha_0_95,X_test_id_0_5)
    err_KO_id_0_5_Sim_CV_alpha_thres_0_95_rn[b] <- MLmetrics::MAE(pred_cv_alpha_0_95,X_test_gau_0_5)
    
    
    
    # ----- identity kernel with norm 0.8 -----
    X_train_id_0_8 = X_id_0_8[,b:(N-1+b)]
    X_test_id_0_8 = X_id_0_8[,N+b]
    
    KO_cv  <- PPCKO::PPC_KO( X = X_train_id_0_8, id_CV = cv1 , alpha_vec=alpha_vec)
    KO_alpha_0_75 = PPCKO::PPC_KO( X = X_train_id_0_8, id_CV = cv2, threshold_ppc = 0.75, alpha_vec=alpha_vec)
    KO_alpha_0_85 = PPCKO::PPC_KO( X = X_train_id_0_8, id_CV = cv2, threshold_ppc = 0.85, alpha_vec=alpha_vec)
    KO_alpha_0_95 = PPCKO::PPC_KO( X = X_train_id_0_8, id_CV = cv2, threshold_ppc = 0.95, alpha_vec=alpha_vec)
    
    pred_cv = KO_cv$`One-step ahead prediction`
    pred_cv_alpha_0_75 = KO_alpha_0_75$`One-step ahead prediction`
    pred_cv_alpha_0_85 = KO_alpha_0_85$`One-step ahead prediction`
    pred_cv_alpha_0_95 = KO_alpha_0_95$`One-step ahead prediction`
    
    err_KO_id_0_8_Sim_CV_en[b] <- MLmetrics::MSE(pred_cv,X_test_id_0_8)
    err_KO_id_0_8_Sim_CV_rn[b] <- MLmetrics::MAE(pred_cv,X_test_id_0_8)
    err_KO_id_0_8_Sim_CV_alpha_thres_0_75_en[b] <- MLmetrics::MSE(pred_cv_alpha_0_75,X_test_id_0_8)
    err_KO_id_0_8_Sim_CV_alpha_thres_0_75_rn[b] <- MLmetrics::MAE(pred_cv_alpha_0_75,X_test_id_0_8)
    err_KO_id_0_8_Sim_CV_alpha_thres_0_85_en[b] <- MLmetrics::MSE(pred_cv_alpha_0_85,X_test_id_0_8)
    err_KO_id_0_8_Sim_CV_alpha_thres_0_85_rn[b] <- MLmetrics::MAE(pred_cv_alpha_0_85,X_test_id_0_8)
    err_KO_id_0_8_Sim_CV_alpha_thres_0_95_en[b] <- MLmetrics::MSE(pred_cv_alpha_0_95,X_test_id_0_8)
    err_KO_id_0_8_Sim_CV_alpha_thres_0_95_rn[b] <- MLmetrics::MAE(pred_cv_alpha_0_95,X_test_id_0_8)
    
    
    
    # ----- sloping plane t kernel with norm 0.5 -----
    X_train_spt_0_5 = X_sp_t_0_5[,b:(N-1+b)]
    X_test_spt_0_5 = X_sp_t_0_5[,N+b]
    
    KO_cv  <- PPCKO::PPC_KO( X = X_train_spt_0_5, id_CV = cv1 , alpha_vec=alpha_vec)
    KO_alpha_0_75 = PPCKO::PPC_KO( X = X_train_spt_0_5, id_CV = cv2, threshold_ppc = 0.75, alpha_vec=alpha_vec)
    KO_alpha_0_85 = PPCKO::PPC_KO( X = X_train_spt_0_5, id_CV = cv2, threshold_ppc = 0.85, alpha_vec=alpha_vec)
    KO_alpha_0_95 = PPCKO::PPC_KO( X = X_train_spt_0_5, id_CV = cv2, threshold_ppc = 0.95, alpha_vec=alpha_vec)
    
    pred_cv = KO_cv$`One-step ahead prediction`
    pred_cv_alpha_0_75 = KO_alpha_0_75$`One-step ahead prediction`
    pred_cv_alpha_0_85 = KO_alpha_0_85$`One-step ahead prediction`
    pred_cv_alpha_0_95 = KO_alpha_0_95$`One-step ahead prediction`
    
    err_KO_spt_0_5_Sim_CV_en[b] <- MLmetrics::MSE(pred_cv,X_test_spt_0_5)
    err_KO_spt_0_5_Sim_CV_rn[b] <- MLmetrics::MAE(pred_cv,X_test_spt_0_5)
    err_KO_spt_0_5_Sim_CV_alpha_thres_0_75_en[b] <- MLmetrics::MSE(pred_cv_alpha_0_75,X_test_spt_0_5)
    err_KO_spt_0_5_Sim_CV_alpha_thres_0_75_rn[b] <- MLmetrics::MAE(pred_cv_alpha_0_75,X_test_spt_0_5)
    err_KO_spt_0_5_Sim_CV_alpha_thres_0_85_en[b] <- MLmetrics::MSE(pred_cv_alpha_0_85,X_test_spt_0_5)
    err_KO_spt_0_5_Sim_CV_alpha_thres_0_85_rn[b] <- MLmetrics::MAE(pred_cv_alpha_0_85,X_test_spt_0_5)
    err_KO_spt_0_5_Sim_CV_alpha_thres_0_95_en[b] <- MLmetrics::MSE(pred_cv_alpha_0_95,X_test_spt_0_5)
    err_KO_spt_0_5_Sim_CV_alpha_thres_0_95_rn[b] <- MLmetrics::MAE(pred_cv_alpha_0_95,X_test_spt_0_5)
    
    
    
    # ----- sloping plane t kernel with norm 0.8 -----
    X_train_spt_0_8 = X_sp_t_0_8[,b:(N-1+b)]
    X_test_spt_0_8 = X_sp_t_0_8[,N+b]
    
    KO_cv  <- PPCKO::PPC_KO( X = X_train_spt_0_8, id_CV = cv1, alpha_vec=alpha_vec )
    KO_alpha_0_75 = PPCKO::PPC_KO( X = X_train_spt_0_8, id_CV = cv2, threshold_ppc = 0.75, alpha_vec=alpha_vec)
    KO_alpha_0_85 = PPCKO::PPC_KO( X = X_train_spt_0_8, id_CV = cv2, threshold_ppc = 0.85, alpha_vec=alpha_vec)
    KO_alpha_0_95 = PPCKO::PPC_KO( X = X_train_spt_0_8, id_CV = cv2, threshold_ppc = 0.95, alpha_vec=alpha_vec)
    
    pred_cv = KO_cv$`One-step ahead prediction`
    pred_cv_alpha_0_75 = KO_alpha_0_75$`One-step ahead prediction`
    pred_cv_alpha_0_85 = KO_alpha_0_85$`One-step ahead prediction`
    pred_cv_alpha_0_95 = KO_alpha_0_95$`One-step ahead prediction`
    
    err_KO_spt_0_8_Sim_CV_en[b] <- MLmetrics::MSE(pred_cv,X_test_spt_0_8)
    err_KO_spt_0_8_Sim_CV_rn[b] <- MLmetrics::MAE(pred_cv,X_test_spt_0_8)
    err_KO_spt_0_8_Sim_CV_alpha_thres_0_75_en[b] <- MLmetrics::MSE(pred_cv_alpha_0_75,X_test_spt_0_8)
    err_KO_spt_0_8_Sim_CV_alpha_thres_0_75_rn[b] <- MLmetrics::MAE(pred_cv_alpha_0_75,X_test_spt_0_8)
    err_KO_spt_0_8_Sim_CV_alpha_thres_0_85_en[b] <- MLmetrics::MSE(pred_cv_alpha_0_85,X_test_spt_0_8)
    err_KO_spt_0_8_Sim_CV_alpha_thres_0_85_rn[b] <- MLmetrics::MAE(pred_cv_alpha_0_85,X_test_spt_0_8)
    err_KO_spt_0_8_Sim_CV_alpha_thres_0_95_en[b] <- MLmetrics::MSE(pred_cv_alpha_0_95,X_test_spt_0_8)
    err_KO_spt_0_8_Sim_CV_alpha_thres_0_95_rn[b] <- MLmetrics::MAE(pred_cv_alpha_0_95,X_test_spt_0_8)
    
    
    
    # ----- sloping plane s kernel with norm 0.5 -----
    X_train_sps_0_5 = X_sp_s_0_5[,b:(N-1+b)]
    X_test_sps_0_5 = X_sp_s_0_5[,N+b]
    
    KO_cv  <- PPCKO::PPC_KO( X = X_train_sps_0_5, id_CV = cv1, alpha_vec=alpha_vec )
    KO_alpha_0_75 = PPCKO::PPC_KO( X = X_train_sps_0_5, id_CV = cv2, threshold_ppc = 0.75, alpha_vec=alpha_vec)
    KO_alpha_0_85 = PPCKO::PPC_KO( X = X_train_sps_0_5, id_CV = cv2, threshold_ppc = 0.85, alpha_vec=alpha_vec)
    KO_alpha_0_95 = PPCKO::PPC_KO( X = X_train_sps_0_5, id_CV = cv2, threshold_ppc = 0.95, alpha_vec=alpha_vec)
    
    pred_cv = KO_cv$`One-step ahead prediction`
    pred_cv_alpha_0_75 = KO_alpha_0_75$`One-step ahead prediction`
    pred_cv_alpha_0_85 = KO_alpha_0_85$`One-step ahead prediction`
    pred_cv_alpha_0_95 = KO_alpha_0_95$`One-step ahead prediction`
    
    err_KO_sps_0_5_Sim_CV_en[b] <- MLmetrics::MSE(pred_cv,X_test_sps_0_5)
    err_KO_sps_0_5_Sim_CV_rn[b] <- MLmetrics::MAE(pred_cv,X_test_sps_0_5)
    err_KO_sps_0_5_Sim_CV_alpha_thres_0_75_en[b] <- MLmetrics::MSE(pred_cv_alpha_0_75,X_test_sps_0_5)
    err_KO_sps_0_5_Sim_CV_alpha_thres_0_75_rn[b] <- MLmetrics::MAE(pred_cv_alpha_0_75,X_test_sps_0_5)
    err_KO_sps_0_5_Sim_CV_alpha_thres_0_85_en[b] <- MLmetrics::MSE(pred_cv_alpha_0_85,X_test_sps_0_5)
    err_KO_sps_0_5_Sim_CV_alpha_thres_0_85_rn[b] <- MLmetrics::MAE(pred_cv_alpha_0_85,X_test_sps_0_5)
    err_KO_sps_0_5_Sim_CV_alpha_thres_0_95_en[b] <- MLmetrics::MSE(pred_cv_alpha_0_95,X_test_sps_0_5)
    err_KO_sps_0_5_Sim_CV_alpha_thres_0_95_rn[b] <- MLmetrics::MAE(pred_cv_alpha_0_95,X_test_sps_0_5)
    
    
    
    # ----- sloping plane s kernel with norm 0.8 -----
    X_train_sps_0_8 = X_sp_s_0_8[,b:(N-1+b)]
    X_test_sps_0_8 = X_sp_s_0_8[,N+b]
    
    KO_cv  <- PPCKO::PPC_KO( X = X_train_sps_0_8, id_CV = cv1, alpha_vec=alpha_vec )
    KO_alpha_0_75 = PPCKO::PPC_KO( X = X_train_sps_0_8, id_CV = cv2, threshold_ppc = 0.75, alpha_vec=alpha_vec)
    KO_alpha_0_85 = PPCKO::PPC_KO( X = X_train_sps_0_8, id_CV = cv2, threshold_ppc = 0.85, alpha_vec=alpha_vec)
    KO_alpha_0_95 = PPCKO::PPC_KO( X = X_train_sps_0_8, id_CV = cv2, threshold_ppc = 0.95, alpha_vec=alpha_vec)
    
    pred_cv = KO_cv$`One-step ahead prediction`
    pred_cv_alpha_0_75 = KO_alpha_0_75$`One-step ahead prediction`
    pred_cv_alpha_0_85 = KO_alpha_0_85$`One-step ahead prediction`
    pred_cv_alpha_0_95 = KO_alpha_0_95$`One-step ahead prediction`
    
    err_KO_sps_0_8_Sim_CV_en[b] <- MLmetrics::MSE(pred_cv,X_test_sps_0_8)
    err_KO_sps_0_8_Sim_CV_rn[b] <- MLmetrics::MAE(pred_cv,X_test_sps_0_8)
    err_KO_sps_0_8_Sim_CV_alpha_thres_0_75_en[b] <- MLmetrics::MSE(pred_cv_alpha_0_75,X_test_sps_0_8)
    err_KO_sps_0_8_Sim_CV_alpha_thres_0_75_rn[b] <- MLmetrics::MAE(pred_cv_alpha_0_75,X_test_sps_0_8)
    err_KO_sps_0_8_Sim_CV_alpha_thres_0_85_en[b] <- MLmetrics::MSE(pred_cv_alpha_0_85,X_test_sps_0_8)
    err_KO_sps_0_8_Sim_CV_alpha_thres_0_85_rn[b] <- MLmetrics::MAE(pred_cv_alpha_0_85,X_test_sps_0_8)
    err_KO_sps_0_8_Sim_CV_alpha_thres_0_95_en[b] <- MLmetrics::MSE(pred_cv_alpha_0_95,X_test_sps_0_8)
    err_KO_sps_0_8_Sim_CV_alpha_thres_0_95_rn[b] <- MLmetrics::MAE(pred_cv_alpha_0_95,X_test_sps_0_8)
    
    
    pb$tick()
  }
  
  
  ## ----- Storing results -----
  {
    gauss_0_5_data = list(CV =                list(En = err_KO_gau_0_5_Sim_CV_en,                  Rn = err_KO_gau_0_5_Sim_CV_rn),
                          CV_alpha_0_75_pow = list(En = err_KO_gau_0_5_Sim_CV_alpha_thres_0_75_en, Rn = err_KO_gau_0_5_Sim_CV_alpha_thres_0_75_rn),
                          CV_alpha_0_85_pow = list(En = err_KO_gau_0_5_Sim_CV_alpha_thres_0_85_en, Rn = err_KO_gau_0_5_Sim_CV_alpha_thres_0_85_rn),
                          CV_alpha_0_95_pow = list(En = err_KO_gau_0_5_Sim_CV_alpha_thres_0_95_en, Rn = err_KO_gau_0_5_Sim_CV_alpha_thres_0_95_rn))
    
    gauss_0_8_data = list(CV =                list(En = err_KO_gau_0_8_Sim_CV_en,                  Rn = err_KO_gau_0_8_Sim_CV_rn),
                          CV_alpha_0_75_pow = list(En = err_KO_gau_0_8_Sim_CV_alpha_thres_0_75_en, Rn = err_KO_gau_0_8_Sim_CV_alpha_thres_0_75_rn),
                          CV_alpha_0_85_pow = list(En = err_KO_gau_0_8_Sim_CV_alpha_thres_0_85_en, Rn = err_KO_gau_0_8_Sim_CV_alpha_thres_0_85_rn),
                          CV_alpha_0_95_pow = list(En = err_KO_gau_0_8_Sim_CV_alpha_thres_0_95_en, Rn = err_KO_gau_0_8_Sim_CV_alpha_thres_0_95_rn))
    
    id_0_5_data = list(CV =                list(En = err_KO_id_0_5_Sim_CV_en,                  Rn = err_KO_id_0_5_Sim_CV_rn),
                       CV_alpha_0_75_pow = list(En = err_KO_id_0_5_Sim_CV_alpha_thres_0_75_en, Rn = err_KO_id_0_5_Sim_CV_alpha_thres_0_75_rn),
                       CV_alpha_0_85_pow = list(En = err_KO_id_0_5_Sim_CV_alpha_thres_0_85_en, Rn = err_KO_id_0_5_Sim_CV_alpha_thres_0_85_rn),
                       CV_alpha_0_95_pow = list(En = err_KO_id_0_5_Sim_CV_alpha_thres_0_95_en, Rn = err_KO_id_0_5_Sim_CV_alpha_thres_0_95_rn))
    
    id_0_8_data = list(CV =                list(En = err_KO_id_0_8_Sim_CV_en,                  Rn = err_KO_id_0_8_Sim_CV_rn),
                       CV_alpha_0_75_pow = list(En = err_KO_id_0_8_Sim_CV_alpha_thres_0_75_en, Rn = err_KO_id_0_8_Sim_CV_alpha_thres_0_75_rn),
                       CV_alpha_0_85_pow = list(En = err_KO_id_0_8_Sim_CV_alpha_thres_0_85_en, Rn = err_KO_id_0_8_Sim_CV_alpha_thres_0_85_rn),
                       CV_alpha_0_95_pow = list(En = err_KO_id_0_8_Sim_CV_alpha_thres_0_95_en, Rn = err_KO_id_0_8_Sim_CV_alpha_thres_0_95_rn))
    
    spt_0_5_data = list(CV =                list(En = err_KO_spt_0_5_Sim_CV_en,                  Rn = err_KO_spt_0_5_Sim_CV_rn),
                        CV_alpha_0_75_pow = list(En = err_KO_spt_0_5_Sim_CV_alpha_thres_0_75_en, Rn = err_KO_spt_0_5_Sim_CV_alpha_thres_0_75_rn),
                        CV_alpha_0_85_pow = list(En = err_KO_spt_0_5_Sim_CV_alpha_thres_0_85_en, Rn = err_KO_spt_0_5_Sim_CV_alpha_thres_0_85_rn),
                        CV_alpha_0_95_pow = list(En = err_KO_spt_0_5_Sim_CV_alpha_thres_0_95_en, Rn = err_KO_spt_0_5_Sim_CV_alpha_thres_0_95_rn))
    
    spt_0_8_data = list(CV =                list(En = err_KO_spt_0_8_Sim_CV_en,                  Rn = err_KO_spt_0_8_Sim_CV_rn),
                        CV_alpha_0_75_pow = list(En = err_KO_spt_0_8_Sim_CV_alpha_thres_0_75_en, Rn = err_KO_spt_0_8_Sim_CV_alpha_thres_0_75_rn),
                        CV_alpha_0_85_pow = list(En = err_KO_spt_0_8_Sim_CV_alpha_thres_0_85_en, Rn = err_KO_spt_0_8_Sim_CV_alpha_thres_0_85_rn),
                        CV_alpha_0_95_pow = list(En = err_KO_spt_0_8_Sim_CV_alpha_thres_0_95_en, Rn = err_KO_spt_0_8_Sim_CV_alpha_thres_0_95_rn))
    
    sps_0_5_data = list(CV =                list(En = err_KO_sps_0_5_Sim_CV_en,                  Rn = err_KO_sps_0_5_Sim_CV_rn),
                        CV_alpha_0_75_pow = list(En = err_KO_sps_0_5_Sim_CV_alpha_thres_0_75_en, Rn = err_KO_sps_0_5_Sim_CV_alpha_thres_0_75_rn),
                        CV_alpha_0_85_pow = list(En = err_KO_sps_0_5_Sim_CV_alpha_thres_0_85_en, Rn = err_KO_sps_0_5_Sim_CV_alpha_thres_0_85_rn),
                        CV_alpha_0_95_pow = list(En = err_KO_sps_0_5_Sim_CV_alpha_thres_0_95_en, Rn = err_KO_sps_0_5_Sim_CV_alpha_thres_0_95_rn))
    
    sps_0_8_data = list(CV =                list(En = err_KO_sps_0_8_Sim_CV_en,                  Rn = err_KO_sps_0_8_Sim_CV_rn),
                        CV_alpha_0_75_pow = list(En = err_KO_sps_0_8_Sim_CV_alpha_thres_0_75_en, Rn = err_KO_sps_0_8_Sim_CV_alpha_thres_0_75_rn),
                        CV_alpha_0_85_pow = list(En = err_KO_sps_0_8_Sim_CV_alpha_thres_0_85_en, Rn = err_KO_sps_0_8_Sim_CV_alpha_thres_0_85_rn),
                        CV_alpha_0_95_pow = list(En = err_KO_sps_0_8_Sim_CV_alpha_thres_0_95_en, Rn = err_KO_sps_0_8_Sim_CV_alpha_thres_0_95_rn))
    
    res_comp_CV = list( gau_0_5 = gauss_0_5_data,
                        gau_0_8 = gauss_0_8_data,
                        id_0_5  = id_0_5_data,
                        id_0_8  = id_0_8_data,
                        spt_0_5 = spt_0_5_data,
                        spt_0_8 = spt_0_8_data,
                        sps_0_5 = sps_0_5_data,
                        sps_0_8 = sps_0_8_data)
  }
}

# to eventually save the new results
if(saves_new_res){
  file_saving_path = paste0(dir_w,"/Test_domain1D/Artificial_data/results/CV_comparison/CV_comp_res.Rdata")
  save(res_comp_CV, file = file_saving_path)
}


## ----- Showing results -----
N = length(res_comp_CV$gau_0_5$CV$En)

## ----- Boxplots errors -----
{
  ## ----- boxplot errors gaussian kernel with norm 0.5 -----
  
  # En boxplot
  err_en <- c(res_comp_CV$gau_0_5$CV$En,res_comp_CV$gau_0_5$CV_alpha_0_75_pow$En,res_comp_CV$gau_0_5$CV_alpha_0_85_pow$En,res_comp_CV$gau_0_5$CV_alpha_0_95_pow$En)
  method <- rep(c("CV","Exp. Pow. 0.75","Exp. Pow. 0.85","Exp. Pow. 0.95"), each=N)
  En <- data.frame(method, err_en)
  method_order<- c("CV","Exp. Pow. 0.75","Exp. Pow. 0.85","Exp. Pow. 0.95")
  En.box <- En %>% mutate(method=factor(x=method, levels=method_order))
  
  pgplot <- ggplot(En.box, aes(x=method, y=err_en, fill=method)) + 
    geom_boxplot() + ggtitle("Gaussian Kernel, norm 0.5")
  pgplot <- pgplot +
    theme_bw() + 
    labs(x="", y="En", fill = "KO") +
    theme(plot.title = element_text(face="bold", hjust=0.5, size=22),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=22),
          axis.title.y = element_text(size=22))
  pgplot + theme(legend.position="none")
  
  open_window()
  print(pgplot)
  
  if(saves_new_res){
    title = "En_Gaussian_Kernel_norm_0_5"
    ggsave(filename = paste0(title,".pdf"),
           plot = pgplot,
           device = NULL,
           path = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series/Test_domain1D/Artificial_data/results/CV_comparison",
           scale = 1,
           width = 14,
           height = 10,
           dpi = 300)
  }
  
  # Rn boxplot
  err_rn <- c(res_comp_CV$gau_0_5$CV$Rn,res_comp_CV$gau_0_5$CV_alpha_0_75_pow$Rn,res_comp_CV$gau_0_5$CV_alpha_0_85_pow$Rn,res_comp_CV$gau_0_5$CV_alpha_0_95_pow$Rn)
  method <- rep(c("CV","Exp. Pow. 0.75","Exp. Pow. 0.85","Exp. Pow. 0.95"), each=N)
  Rn <- data.frame(method, err_rn)
  method_order<- c("CV","Exp. Pow. 0.75","Exp. Pow. 0.85","Exp. Pow. 0.95")
  Rn.box <- Rn %>% mutate(method=factor(x=method, levels=method_order))
  
  pgplot <- ggplot(Rn.box, aes(x=method, y=err_rn, fill=method)) + 
    geom_boxplot()  + ggtitle("Gaussian Kernel, norm 0.5")
  pgplot <- pgplot +
    theme_bw() + 
    labs(x="", y="Rn", fill = "KO") +
    theme(plot.title = element_text(face="bold", hjust=0.5, size=22),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=22),
          axis.title.y = element_text(size=22))
  pgplot + theme(legend.position="none")
  
  open_window()
  print(pgplot)
  
  if(saves_new_res){
    title = "Rn_Gaussian_Kernel_norm_0_5"
    ggsave(filename = paste0(title,".pdf"),
           plot = pgplot,
           device = NULL,
           path = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series/Test_domain1D/Artificial_data/results/CV_comparison",
           scale = 1,
           width = 14,
           height = 10,
           dpi = 300)
  }
  
  
  ## ----- boxplot errors gaussian kernel with norm 0.8 -----
  
  # En boxplot
  err_en <- c(res_comp_CV$gau_0_8$CV$En,res_comp_CV$gau_0_8$CV_alpha_0_75_pow$En,res_comp_CV$gau_0_8$CV_alpha_0_85_pow$En,res_comp_CV$gau_0_8$CV_alpha_0_95_pow$En)
  method <- rep(c("CV","Exp. Pow. 0.75","Exp. Pow. 0.85","Exp. Pow. 0.95"), each=N)
  En <- data.frame(method, err_en)
  method_order<- c("CV","Exp. Pow. 0.75","Exp. Pow. 0.85","Exp. Pow. 0.95")
  En.box <- En %>% mutate(method=factor(x=method, levels=method_order))
  
  pgplot <- ggplot(En.box, aes(x=method, y=err_en, fill=method)) + 
    geom_boxplot() + ggtitle("Gaussian Kernel, norm 0.8")
  pgplot <- pgplot +
    theme_bw() + 
    labs(x="", y="En", fill = "KO") +
    theme(plot.title = element_text(face="bold", hjust=0.5, size=22),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=22),
          axis.title.y = element_text(size=22))
  pgplot + theme(legend.position="none")
  
  open_window()
  print(pgplot)
  
  if(saves_new_res){
    title = "En_Gaussian_Kernel_norm_0_8"
    ggsave(filename = paste0(title,".pdf"),
           plot = pgplot,
           device = NULL,
           path = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series/Test_domain1D/Artificial_data/results/CV_comparison",
           scale = 1,
           width = 14,
           height = 10,
           dpi = 300)
  }
  
  # Rn boxplot
  err_rn <- c(res_comp_CV$gau_0_8$CV$Rn,res_comp_CV$gau_0_8$CV_alpha_0_75_pow$Rn,res_comp_CV$gau_0_8$CV_alpha_0_85_pow$Rn,res_comp_CV$gau_0_8$CV_alpha_0_95_pow$Rn)
  method <- rep(c("CV","Exp. Pow. 0.75","Exp. Pow. 0.85","Exp. Pow. 0.95"), each=N)
  Rn <- data.frame(method, err_rn)
  method_order<- c("CV","Exp. Pow. 0.75","Exp. Pow. 0.85","Exp. Pow. 0.95")
  Rn.box <- Rn %>% mutate(method=factor(x=method, levels=method_order))
  
  pgplot <- ggplot(Rn.box, aes(x=method, y=err_rn, fill=method)) + 
    geom_boxplot()  + ggtitle("Gaussian Kernel, norm 0.8")
  pgplot <- pgplot +
    theme_bw() + 
    labs(x="", y="Rn", fill = "KO") +
    theme(plot.title = element_text(face="bold", hjust=0.5, size=22),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=22),
          axis.title.y = element_text(size=22))
  pgplot + theme(legend.position="none")
  
  open_window()
  print(pgplot)
  
  if(saves_new_res){
    title = "Rn_Gaussian_Kernel_norm_0_8"
    ggsave(filename = paste0(title,".pdf"),
           plot = pgplot,
           device = NULL,
           path = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series/Test_domain1D/Artificial_data/results/CV_comparison",
           scale = 1,
           width = 14,
           height = 10,
           dpi = 300)
  }
  ## ----- boxplot errors identity kernel with norm 0.5 -----
  
  # En boxplot
  err_en <- c(res_comp_CV$id_0_5$CV$En,res_comp_CV$id_0_5$CV_alpha_0_75_pow$En,res_comp_CV$id_0_5$CV_alpha_0_85_pow$En,res_comp_CV$id_0_5$CV_alpha_0_95_pow$En)
  method <- rep(c("CV","Exp. Pow. 0.75","Exp. Pow. 0.85","Exp. Pow. 0.95"), each=N)
  En <- data.frame(method, err_en)
  method_order<- c("CV","Exp. Pow. 0.75","Exp. Pow. 0.85","Exp. Pow. 0.95")
  En.box <- En %>% mutate(method=factor(x=method, levels=method_order))
  
  pgplot <- ggplot(En.box, aes(x=method, y=err_en, fill=method)) + 
    geom_boxplot() + ggtitle("Identity Kernel, norm 0.5")
  pgplot <- pgplot +
    theme_bw() + 
    labs(x="", y="En", fill = "KO") +
    theme(plot.title = element_text(face="bold", hjust=0.5, size=22),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=22),
          axis.title.y = element_text(size=22))
  pgplot + theme(legend.position="none")
  
  open_window()
  print(pgplot)
  
  if(saves_new_res){
    title = "En_Identity_Kernel_norm_0_5"
    ggsave(filename = paste0(title,".pdf"),
           plot = pgplot,
           device = NULL,
           path = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series/Test_domain1D/Artificial_data/results/CV_comparison",
           scale = 1,
           width = 14,
           height = 10,
           dpi = 300)
  }
  
  # Rn boxplot
  err_rn <- c(res_comp_CV$id_0_5$CV$Rn,res_comp_CV$id_0_5$CV_alpha_0_75_pow$Rn,res_comp_CV$id_0_5$CV_alpha_0_85_pow$Rn,res_comp_CV$id_0_5$CV_alpha_0_95_pow$Rn)
  method <- rep(c("CV","Exp. Pow. 0.75","Exp. Pow. 0.85","Exp. Pow. 0.95"), each=N)
  Rn <- data.frame(method, err_rn)
  method_order<- c("CV","Exp. Pow. 0.75","Exp. Pow. 0.85","Exp. Pow. 0.95")
  Rn.box <- Rn %>% mutate(method=factor(x=method, levels=method_order))
  
  pgplot <- ggplot(Rn.box, aes(x=method, y=err_rn, fill=method)) + 
    geom_boxplot()  + ggtitle("Identity Kernel, norm 0.5")
  pgplot <- pgplot +
    theme_bw() + 
    labs(x="", y="Rn", fill = "KO") +
    theme(plot.title = element_text(face="bold", hjust=0.5, size=22),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=22),
          axis.title.y = element_text(size=22))
  pgplot + theme(legend.position="none")
  
  open_window()
  print(pgplot)
  
  if(saves_new_res){
    title = "Rn_Identity_Kernel_norm_0_5"
    ggsave(filename = paste0(title,".pdf"),
           plot = pgplot,
           device = NULL,
           path = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series/Test_domain1D/Artificial_data/results/CV_comparison",
           scale = 1,
           width = 14,
           height = 10,
           dpi = 300)
  }
  
  
  
  ## ----- boxplot errors identity kernel with norm 0.8 -----
  
  # En boxplot
  err_en <- c(res_comp_CV$id_0_8$CV$En,res_comp_CV$id_0_8$CV_alpha_0_75_pow$En,res_comp_CV$id_0_8$CV_alpha_0_85_pow$En,res_comp_CV$id_0_8$CV_alpha_0_95_pow$En)
  method <- rep(c("CV","Exp. Pow. 0.75","Exp. Pow. 0.85","Exp. Pow. 0.95"), each=N)
  En <- data.frame(method, err_en)
  method_order<- c("CV","Exp. Pow. 0.75","Exp. Pow. 0.85","Exp. Pow. 0.95")
  En.box <- En %>% mutate(method=factor(x=method, levels=method_order))
  
  pgplot <- ggplot(En.box, aes(x=method, y=err_en, fill=method)) + 
    geom_boxplot() + ggtitle("Identity Kernel, norm 0.8")
  pgplot <- pgplot +
    theme_bw() + 
    labs(x="", y="En", fill = "KO") +
    theme(plot.title = element_text(face="bold", hjust=0.5, size=22),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=22),
          axis.title.y = element_text(size=22))
  pgplot + theme(legend.position="none")
  
  open_window()
  print(pgplot)
  
  if(saves_new_res){
    title = "En_Identity_Kernel_norm_0_8"
    ggsave(filename = paste0(title,".pdf"),
           plot = pgplot,
           device = NULL,
           path = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series/Test_domain1D/Artificial_data/results/CV_comparison",
           scale = 1,
           width = 14,
           height = 10,
           dpi = 300)
  }
  
  # Rn boxplot
  err_rn <- c(res_comp_CV$id_0_8$CV$Rn,res_comp_CV$id_0_8$CV_alpha_0_75_pow$Rn,res_comp_CV$id_0_8$CV_alpha_0_85_pow$Rn,res_comp_CV$id_0_8$CV_alpha_0_95_pow$Rn)
  method <- rep(c("CV","Exp. Pow. 0.75","Exp. Pow. 0.85","Exp. Pow. 0.95"), each=N)
  Rn <- data.frame(method, err_rn)
  method_order<- c("CV","Exp. Pow. 0.75","Exp. Pow. 0.85","Exp. Pow. 0.95")
  Rn.box <- Rn %>% mutate(method=factor(x=method, levels=method_order))
  
  pgplot <- ggplot(Rn.box, aes(x=method, y=err_rn, fill=method)) + 
    geom_boxplot()  + ggtitle("Identity Kernel, norm 0.8")
  pgplot <- pgplot +
    theme_bw() + 
    labs(x="", y="Rn", fill = "KO") +
    theme(plot.title = element_text(face="bold", hjust=0.5, size=22),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=22),
          axis.title.y = element_text(size=22))
  pgplot + theme(legend.position="none")
  
  open_window()
  print(pgplot)
  
  if(saves_new_res){
    title = "Rn_Identity_Kernel_norm_0_8"
    ggsave(filename = paste0(title,".pdf"),
           plot = pgplot,
           device = NULL,
           path = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series/Test_domain1D/Artificial_data/results/CV_comparison",
           scale = 1,
           width = 14,
           height = 10,
           dpi = 300)
  }
  ## ----- boxplot errors sloping plane t kernel with norm 0.5 -----
  
  # En boxplot
  err_en <- c(res_comp_CV$spt_0_5$CV$En,res_comp_CV$spt_0_5$CV_alpha_0_75_pow$En,res_comp_CV$spt_0_5$CV_alpha_0_85_pow$En,res_comp_CV$spt_0_5$CV_alpha_0_95_pow$En)
  method <- rep(c("CV","Exp. Pow. 0.75","Exp. Pow. 0.85","Exp. Pow. 0.95"), each=N)
  En <- data.frame(method, err_en)
  method_order<- c("CV","Exp. Pow. 0.75","Exp. Pow. 0.85","Exp. Pow. 0.95")
  En.box <- En %>% mutate(method=factor(x=method, levels=method_order))
  
  pgplot <- ggplot(En.box, aes(x=method, y=err_en, fill=method)) + 
    geom_boxplot() + ggtitle("Sloping plane t Kernel, norm 0.5")
  pgplot <- pgplot +
    theme_bw() + 
    labs(x="", y="En", fill = "KO") +
    theme(plot.title = element_text(face="bold", hjust=0.5, size=22),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=22),
          axis.title.y = element_text(size=22))
  pgplot + theme(legend.position="none")
  
  open_window()
  print(pgplot)
  
  if(saves_new_res){
    title = "En_Sp_t_Kernel_norm_0_5"
    ggsave(filename = paste0(title,".pdf"),
           plot = pgplot,
           device = NULL,
           path = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series/Test_domain1D/Artificial_data/results/CV_comparison",
           scale = 1,
           width = 14,
           height = 10,
           dpi = 300)
  }
  
  # Rn boxplot
  err_rn <- c(res_comp_CV$spt_0_5$CV$Rn,res_comp_CV$spt_0_5$CV_alpha_0_75_pow$Rn,res_comp_CV$spt_0_5$CV_alpha_0_85_pow$Rn,res_comp_CV$spt_0_5$CV_alpha_0_95_pow$Rn)
  method <- rep(c("CV","Exp. Pow. 0.75","Exp. Pow. 0.85","Exp. Pow. 0.95"), each=N)
  Rn <- data.frame(method, err_rn)
  method_order<- c("CV","Exp. Pow. 0.75","Exp. Pow. 0.85","Exp. Pow. 0.95")
  Rn.box <- Rn %>% mutate(method=factor(x=method, levels=method_order))
  
  pgplot <- ggplot(Rn.box, aes(x=method, y=err_rn, fill=method)) + 
    geom_boxplot()  + ggtitle("Sloping plane t Kernel, norm 0.5")
  pgplot <- pgplot +
    theme_bw() + 
    labs(x="", y="Rn", fill = "KO") +
    theme(plot.title = element_text(face="bold", hjust=0.5, size=22),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=22),
          axis.title.y = element_text(size=22))
  pgplot + theme(legend.position="none")
  
  open_window()
  print(pgplot)
  
  if(saves_new_res){
    title = "Rn_Sp_t_Kernel_norm_0_5"
    ggsave(filename = paste0(title,".pdf"),
           plot = pgplot,
           device = NULL,
           path = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series/Test_domain1D/Artificial_data/results/CV_comparison",
           scale = 1,
           width = 14,
           height = 10,
           dpi = 300)
  }
  ## ----- boxplot errors sloping plane t kernel with norm 0.8 -----
  
  # En boxplot
  err_en <- c(res_comp_CV$spt_0_8$CV$En,res_comp_CV$spt_0_8$CV_alpha_0_75_pow$En,res_comp_CV$spt_0_8$CV_alpha_0_85_pow$En,res_comp_CV$spt_0_8$CV_alpha_0_95_pow$En)
  method <- rep(c("CV","Exp. Pow. 0.75","Exp. Pow. 0.85","Exp. Pow. 0.95"), each=N)
  En <- data.frame(method, err_en)
  method_order<- c("CV","Exp. Pow. 0.75","Exp. Pow. 0.85","Exp. Pow. 0.95")
  En.box <- En %>% mutate(method=factor(x=method, levels=method_order))
  
  pgplot <- ggplot(En.box, aes(x=method, y=err_en, fill=method)) + 
    geom_boxplot() + ggtitle("Sloping plane t Kernel, norm 0.8")
  pgplot <- pgplot +
    theme_bw() + 
    labs(x="", y="En", fill = "KO") +
    theme(plot.title = element_text(face="bold", hjust=0.5, size=22),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=22),
          axis.title.y = element_text(size=22))
  pgplot + theme(legend.position="none")
  
  open_window()
  print(pgplot)
  
  if(saves_new_res){
    title = "En_Sp_t_Kernel_norm_0_8"
    ggsave(filename = paste0(title,".pdf"),
           plot = pgplot,
           device = NULL,
           path = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series/Test_domain1D/Artificial_data/results/CV_comparison",
           scale = 1,
           width = 14,
           height = 10,
           dpi = 300)
  }
  
  # Rn boxplot
  err_rn <- c(res_comp_CV$spt_0_8$CV$Rn,res_comp_CV$spt_0_8$CV_alpha_0_75_pow$Rn,res_comp_CV$spt_0_8$CV_alpha_0_85_pow$Rn,res_comp_CV$spt_0_8$CV_alpha_0_95_pow$Rn)
  method <- rep(c("CV","Exp. Pow. 0.75","Exp. Pow. 0.85","Exp. Pow. 0.95"), each=N)
  Rn <- data.frame(method, err_rn)
  method_order<- c("CV","Exp. Pow. 0.75","Exp. Pow. 0.85","Exp. Pow. 0.95")
  Rn.box <- Rn %>% mutate(method=factor(x=method, levels=method_order))
  
  pgplot <- ggplot(Rn.box, aes(x=method, y=err_rn, fill=method)) + 
    geom_boxplot()  + ggtitle("Sloping plane t Kernel, norm 0.8")
  pgplot <- pgplot +
    theme_bw() + 
    labs(x="", y="Rn", fill = "KO") +
    theme(plot.title = element_text(face="bold", hjust=0.5, size=22),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=22),
          axis.title.y = element_text(size=22))
  pgplot + theme(legend.position="none")
  
  open_window()
  print(pgplot)
  
  if(saves_new_res){
    title = "Rn_Sp_t_Kernel_norm_0_8"
    ggsave(filename = paste0(title,".pdf"),
           plot = pgplot,
           device = NULL,
           path = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series/Test_domain1D/Artificial_data/results/CV_comparison",
           scale = 1,
           width = 14,
           height = 10,
           dpi = 300)
  }
  ## ----- boxplot errors sloping plane s kernel with norm 0.5 -----
  
  # En boxplot
  err_en <- c(res_comp_CV$sps_0_5$CV$En,res_comp_CV$sps_0_5$CV_alpha_0_75_pow$En,res_comp_CV$sps_0_5$CV_alpha_0_85_pow$En,res_comp_CV$sps_0_5$CV_alpha_0_95_pow$En)
  method <- rep(c("CV","Exp. Pow. 0.75","Exp. Pow. 0.85","Exp. Pow. 0.95"), each=N)
  En <- data.frame(method, err_en)
  method_order<- c("CV","Exp. Pow. 0.75","Exp. Pow. 0.85","Exp. Pow. 0.95")
  En.box <- En %>% mutate(method=factor(x=method, levels=method_order))
  
  pgplot <- ggplot(En.box, aes(x=method, y=err_en, fill=method)) + 
    geom_boxplot() + ggtitle("Sloping plane s Kernel, norm 0.5")
  pgplot <- pgplot +
    theme_bw() + 
    labs(x="", y="En", fill = "KO") +
    theme(plot.title = element_text(face="bold", hjust=0.5, size=22),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=22),
          axis.title.y = element_text(size=22))
  pgplot + theme(legend.position="none")
  
  open_window()
  print(pgplot)
  
  if(saves_new_res){
    title = "En_Sp_s_Kernel_norm_0_5"
    ggsave(filename = paste0(title,".pdf"),
           plot = pgplot,
           device = NULL,
           path = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series/Test_domain1D/Artificial_data/results/CV_comparison",
           scale = 1,
           width = 14,
           height = 10,
           dpi = 300)
  }
  
  # Rn boxplot
  err_rn <- c(res_comp_CV$sps_0_5$CV$Rn,res_comp_CV$sps_0_5$CV_alpha_0_75_pow$Rn,res_comp_CV$sps_0_5$CV_alpha_0_85_pow$Rn,res_comp_CV$sps_0_5$CV_alpha_0_95_pow$Rn)
  method <- rep(c("CV","Exp. Pow. 0.75","Exp. Pow. 0.85","Exp. Pow. 0.95"), each=N)
  Rn <- data.frame(method, err_rn)
  method_order<- c("CV","Exp. Pow. 0.75","Exp. Pow. 0.85","Exp. Pow. 0.95")
  Rn.box <- Rn %>% mutate(method=factor(x=method, levels=method_order))
  
  pgplot <- ggplot(Rn.box, aes(x=method, y=err_rn, fill=method)) + 
    geom_boxplot()  + ggtitle("Sloping plane s Kernel, norm 0.5")
  pgplot <- pgplot +
    theme_bw() + 
    labs(x="", y="Rn", fill = "KO") +
    theme(plot.title = element_text(face="bold", hjust=0.5, size=22),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=22),
          axis.title.y = element_text(size=22))
  pgplot + theme(legend.position="none")
  
  open_window()
  print(pgplot)
  
  if(saves_new_res){
    title = "Rn_Sp_s_Kernel_norm_0_5"
    ggsave(filename = paste0(title,".pdf"),
           plot = pgplot,
           device = NULL,
           path = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series/Test_domain1D/Artificial_data/results/CV_comparison",
           scale = 1,
           width = 14,
           height = 10,
           dpi = 300)
  }
  ## ----- boxplot errors sloping plane t kernel with norm 0.8 -----
  
  # En boxplot
  err_en <- c(res_comp_CV$sps_0_8$CV$En,res_comp_CV$sps_0_8$CV_alpha_0_75_pow$En,res_comp_CV$sps_0_8$CV_alpha_0_85_pow$En,res_comp_CV$sps_0_8$CV_alpha_0_95_pow$En)
  method <- rep(c("CV","Exp. Pow. 0.75","Exp. Pow. 0.85","Exp. Pow. 0.95"), each=N)
  En <- data.frame(method, err_en)
  method_order<- c("CV","Exp. Pow. 0.75","Exp. Pow. 0.85","Exp. Pow. 0.95")
  En.box <- En %>% mutate(method=factor(x=method, levels=method_order))
  
  pgplot <- ggplot(En.box, aes(x=method, y=err_en, fill=method)) + 
    geom_boxplot() + ggtitle("Sloping plane s Kernel, norm 0.8")
  pgplot <- pgplot +
    theme_bw() + 
    labs(x="", y="En", fill = "KO") +
    theme(plot.title = element_text(face="bold", hjust=0.5, size=22),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=22),
          axis.title.y = element_text(size=22))
  pgplot + theme(legend.position="none")
  
  open_window()
  print(pgplot)
  
  if(saves_new_res){
    title = "En_Sp_s_Kernel_norm_0_8"
    ggsave(filename = paste0(title,".pdf"),
           plot = pgplot,
           device = NULL,
           path = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series/Test_domain1D/Artificial_data/results/CV_comparison",
           scale = 1,
           width = 14,
           height = 10,
           dpi = 300)
  }
  
  # Rn boxplot
  err_rn <- c(res_comp_CV$sps_0_8$CV$Rn,res_comp_CV$sps_0_8$CV_alpha_0_75_pow$Rn,res_comp_CV$sps_0_8$CV_alpha_0_85_pow$Rn,res_comp_CV$sps_0_8$CV_alpha_0_95_pow$Rn)
  method <- rep(c("CV","Exp. Pow. 0.75","Exp. Pow. 0.85","Exp. Pow. 0.95"), each=N)
  Rn <- data.frame(method, err_rn)
  method_order<- c("CV","Exp. Pow. 0.75","Exp. Pow. 0.85","Exp. Pow. 0.95")
  Rn.box <- Rn %>% mutate(method=factor(x=method, levels=method_order))
  
  pgplot <- ggplot(Rn.box, aes(x=method, y=err_rn, fill=method)) + 
    geom_boxplot()  + ggtitle("Sloping plane s Kernel, norm 0.8")
  pgplot <- pgplot +
    theme_bw() + 
    labs(x="", y="Rn", fill = "KO") +
    theme(plot.title = element_text(face="bold", hjust=0.5, size=22),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=22),
          axis.title.y = element_text(size=22))
  pgplot + theme(legend.position="none")
  
  open_window()
  print(pgplot)
  
  if(saves_new_res){
    title = "Rn_Sp_s_Kernel_norm_0_8"
    ggsave(filename = paste0(title,".pdf"),
           plot = pgplot,
           device = NULL,
           path = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series/Test_domain1D/Artificial_data/results/CV_comparison",
           scale = 1,
           width = 14,
           height = 10,
           dpi = 300)
  }
}

## ----- Summary statistics -----
{
  ## ----- gaussian kernel with norm 0.5 -----
  # En
  summary(res_comp_CV$gau_0_5$CV$En)
  mean(res_comp_CV$gau_0_5$CV$En)
  sd(res_comp_CV$gau_0_5$CV$En)
  summary(res_comp_CV$gau_0_5$CV_alpha_0_75_pow$En)
  mean(res_comp_CV$gau_0_5$CV_alpha_0_75_pow$En)
  sd(res_comp_CV$gau_0_5$CV_alpha_0_75_pow$En)
  summary(res_comp_CV$gau_0_5$CV_alpha_0_85_pow$En)
  mean(res_comp_CV$gau_0_5$CV_alpha_0_85_pow$En)
  sd(res_comp_CV$gau_0_5$CV_alpha_0_85_pow$En)
  summary(res_comp_CV$gau_0_5$CV_alpha_0_95_pow$En)
  mean(res_comp_CV$gau_0_5$CV_alpha_0_95_pow$En)
  sd(res_comp_CV$gau_0_5$CV_alpha_0_95_pow$En)
  # Rn
  summary(res_comp_CV$gau_0_5$CV$Rn)
  mean(res_comp_CV$gau_0_5$CV$Rn)
  sd(res_comp_CV$gau_0_5$CV$Rn)
  summary(res_comp_CV$gau_0_5$CV_alpha_0_75_pow$Rn)
  mean(res_comp_CV$gau_0_5$CV_alpha_0_75_pow$Rn)
  sd(res_comp_CV$gau_0_5$CV_alpha_0_75_pow$Rn)
  summary(res_comp_CV$gau_0_5$CV_alpha_0_85_pow$Rn)
  mean(res_comp_CV$gau_0_5$CV_alpha_0_85_pow$Rn)
  sd(res_comp_CV$gau_0_5$CV_alpha_0_85_pow$Rn)
  summary(res_comp_CV$gau_0_5$CV_alpha_0_95_pow$Rn)
  mean(res_comp_CV$gau_0_5$CV_alpha_0_95_pow$Rn)
  sd(res_comp_CV$gau_0_5$CV_alpha_0_95_pow$Rn)
  
  ## ----- gaussian kernel with norm 0.8 -----
  # En
  summary(res_comp_CV$gau_0_8$CV$En)
  mean(res_comp_CV$gau_0_8$CV$En)
  sd(res_comp_CV$gau_0_8$CV$En)
  summary(res_comp_CV$gau_0_8$CV_alpha_0_75_pow$En)
  mean(res_comp_CV$gau_0_8$CV_alpha_0_75_pow$En)
  sd(res_comp_CV$gau_0_8$CV_alpha_0_75_pow$En)
  summary(res_comp_CV$gau_0_8$CV_alpha_0_85_pow$En)
  mean(res_comp_CV$gau_0_8$CV_alpha_0_85_pow$En)
  sd(res_comp_CV$gau_0_8$CV_alpha_0_85_pow$En)
  summary(res_comp_CV$gau_0_8$CV_alpha_0_95_pow$En)
  mean(res_comp_CV$gau_0_8$CV_alpha_0_95_pow$En)
  sd(res_comp_CV$gau_0_8$CV_alpha_0_95_pow$En)
  # Rn
  summary(res_comp_CV$gau_0_8$CV$Rn)
  mean(res_comp_CV$gau_0_8$CV$Rn)
  sd(res_comp_CV$gau_0_8$CV$Rn)
  summary(res_comp_CV$gau_0_8$CV_alpha_0_75_pow$Rn)
  mean(res_comp_CV$gau_0_8$CV_alpha_0_75_pow$Rn)
  sd(res_comp_CV$gau_0_8$CV_alpha_0_75_pow$Rn)
  summary(res_comp_CV$gau_0_8$CV_alpha_0_85_pow$Rn)
  mean(res_comp_CV$gau_0_8$CV_alpha_0_85_pow$Rn)
  sd(res_comp_CV$gau_0_8$CV_alpha_0_85_pow$Rn)
  summary(res_comp_CV$gau_0_8$CV_alpha_0_95_pow$Rn)
  mean(res_comp_CV$gau_0_8$CV_alpha_0_95_pow$Rn)
  sd(res_comp_CV$gau_0_8$CV_alpha_0_95_pow$Rn)
  
  ## ----- identity kernel with norm 0.5 -----
  # En
  summary(res_comp_CV$id_0_5$CV$En)
  mean(res_comp_CV$id_0_5$CV$En)
  sd(res_comp_CV$id_0_5$CV$En)
  summary(res_comp_CV$id_0_5$CV_alpha_0_75_pow$En)
  mean(res_comp_CV$id_0_5$CV_alpha_0_75_pow$En)
  sd(res_comp_CV$id_0_5$CV_alpha_0_75_pow$En)
  summary(res_comp_CV$id_0_5$CV_alpha_0_85_pow$En)
  mean(res_comp_CV$id_0_5$CV_alpha_0_85_pow$En)
  sd(res_comp_CV$id_0_5$CV_alpha_0_85_pow$En)
  summary(res_comp_CV$id_0_5$CV_alpha_0_95_pow$En)
  mean(res_comp_CV$id_0_5$CV_alpha_0_95_pow$En)
  sd(res_comp_CV$id_0_5$CV_alpha_0_95_pow$En)
  # Rn
  summary(res_comp_CV$id_0_5$CV$Rn)
  mean(res_comp_CV$id_0_5$CV$Rn)
  sd(res_comp_CV$id_0_5$CV$Rn)
  summary(res_comp_CV$id_0_5$CV_alpha_0_75_pow$Rn)
  mean(res_comp_CV$id_0_5$CV_alpha_0_75_pow$Rn)
  sd(res_comp_CV$id_0_5$CV_alpha_0_75_pow$Rn)
  summary(res_comp_CV$id_0_5$CV_alpha_0_85_pow$Rn)
  mean(res_comp_CV$id_0_5$CV_alpha_0_85_pow$Rn)
  sd(res_comp_CV$id_0_5$CV_alpha_0_85_pow$Rn)
  summary(res_comp_CV$id_0_5$CV_alpha_0_95_pow$Rn)
  mean(res_comp_CV$id_0_5$CV_alpha_0_95_pow$Rn)
  sd(res_comp_CV$id_0_5$CV_alpha_0_95_pow$Rn)
  
  ## ----- identity kernel with norm 0.8 -----
  # En
  summary(res_comp_CV$id_0_8$CV$En)
  mean(res_comp_CV$id_0_8$CV$En)
  sd(res_comp_CV$id_0_8$CV$En)
  summary(res_comp_CV$id_0_8$CV_alpha_0_75_pow$En)
  mean(res_comp_CV$id_0_8$CV_alpha_0_75_pow$En)
  sd(res_comp_CV$id_0_8$CV_alpha_0_75_pow$En)
  summary(res_comp_CV$id_0_8$CV_alpha_0_85_pow$En)
  mean(res_comp_CV$id_0_8$CV_alpha_0_85_pow$En)
  sd(res_comp_CV$id_0_8$CV_alpha_0_85_pow$En)
  summary(res_comp_CV$id_0_8$CV_alpha_0_95_pow$En)
  mean(res_comp_CV$id_0_8$CV_alpha_0_95_pow$En)
  sd(res_comp_CV$id_0_8$CV_alpha_0_95_pow$En)
  # Rn
  summary(res_comp_CV$id_0_8$CV$Rn)
  mean(res_comp_CV$id_0_8$CV$Rn)
  sd(res_comp_CV$id_0_8$CV$Rn)
  summary(res_comp_CV$id_0_8$CV_alpha_0_75_pow$Rn)
  mean(res_comp_CV$id_0_8$CV_alpha_0_75_pow$Rn)
  sd(res_comp_CV$id_0_8$CV_alpha_0_75_pow$Rn)
  summary(res_comp_CV$id_0_8$CV_alpha_0_85_pow$Rn)
  mean(res_comp_CV$id_0_8$CV_alpha_0_85_pow$Rn)
  sd(res_comp_CV$id_0_8$CV_alpha_0_85_pow$Rn)
  summary(res_comp_CV$id_0_8$CV_alpha_0_95_pow$Rn)
  mean(res_comp_CV$id_0_8$CV_alpha_0_95_pow$Rn)
  sd(res_comp_CV$id_0_8$CV_alpha_0_95_pow$Rn)
  ## ----- sloping plane t kernel with norm 0.5 -----
  # En
  summary(res_comp_CV$spt_0_5$CV$En)
  mean(res_comp_CV$spt_0_5$CV$En)
  sd(res_comp_CV$spt_0_5$CV$En)
  summary(res_comp_CV$spt_0_5$CV_alpha_0_75_pow$En)
  mean(res_comp_CV$spt_0_5$CV_alpha_0_75_pow$En)
  sd(res_comp_CV$spt_0_5$CV_alpha_0_75_pow$En)
  summary(res_comp_CV$spt_0_5$CV_alpha_0_85_pow$En)
  mean(res_comp_CV$spt_0_5$CV_alpha_0_85_pow$En)
  sd(res_comp_CV$spt_0_5$CV_alpha_0_85_pow$En)
  summary(res_comp_CV$spt_0_5$CV_alpha_0_95_pow$En)
  mean(res_comp_CV$spt_0_5$CV_alpha_0_95_pow$En)
  sd(res_comp_CV$spt_0_5$CV_alpha_0_95_pow$En)
  # Rn
  summary(res_comp_CV$spt_0_5$CV$Rn)
  mean(res_comp_CV$spt_0_5$CV$Rn)
  sd(res_comp_CV$spt_0_5$CV$Rn)
  summary(res_comp_CV$spt_0_5$CV_alpha_0_75_pow$Rn)
  mean(res_comp_CV$spt_0_5$CV_alpha_0_75_pow$Rn)
  sd(res_comp_CV$spt_0_5$CV_alpha_0_75_pow$Rn)
  summary(res_comp_CV$spt_0_5$CV_alpha_0_85_pow$Rn)
  mean(res_comp_CV$spt_0_5$CV_alpha_0_85_pow$Rn)
  sd(res_comp_CV$spt_0_5$CV_alpha_0_85_pow$Rn)
  summary(res_comp_CV$spt_0_5$CV_alpha_0_95_pow$Rn)
  mean(res_comp_CV$spt_0_5$CV_alpha_0_95_pow$Rn)
  sd(res_comp_CV$spt_0_5$CV_alpha_0_95_pow$Rn)
  
  ## ----- sloping plane t kernel with norm 0.8 -----
  # En
  summary(res_comp_CV$spt_0_8$CV$En)
  mean(res_comp_CV$spt_0_8$CV$En)
  sd(res_comp_CV$spt_0_8$CV$En)
  summary(res_comp_CV$spt_0_8$CV_alpha_0_75_pow$En)
  mean(res_comp_CV$spt_0_8$CV_alpha_0_75_pow$En)
  sd(res_comp_CV$spt_0_8$CV_alpha_0_75_pow$En)
  summary(res_comp_CV$spt_0_8$CV_alpha_0_85_pow$En)
  mean(res_comp_CV$spt_0_8$CV_alpha_0_85_pow$En)
  sd(res_comp_CV$spt_0_8$CV_alpha_0_85_pow$En)
  summary(res_comp_CV$spt_0_8$CV_alpha_0_95_pow$En)
  mean(res_comp_CV$spt_0_8$CV_alpha_0_95_pow$En)
  sd(res_comp_CV$spt_0_8$CV_alpha_0_95_pow$En)
  # Rn
  summary(res_comp_CV$spt_0_8$CV$Rn)
  mean(res_comp_CV$spt_0_8$CV$Rn)
  sd(res_comp_CV$spt_0_8$CV$Rn)
  summary(res_comp_CV$spt_0_8$CV_alpha_0_75_pow$Rn)
  mean(res_comp_CV$spt_0_8$CV_alpha_0_75_pow$Rn)
  sd(res_comp_CV$spt_0_8$CV_alpha_0_75_pow$Rn)
  summary(res_comp_CV$spt_0_8$CV_alpha_0_85_pow$Rn)
  mean(res_comp_CV$spt_0_8$CV_alpha_0_85_pow$Rn)
  sd(res_comp_CV$spt_0_8$CV_alpha_0_85_pow$Rn)
  summary(res_comp_CV$spt_0_8$CV_alpha_0_95_pow$Rn)
  mean(res_comp_CV$spt_0_8$CV_alpha_0_95_pow$Rn)
  sd(res_comp_CV$spt_0_8$CV_alpha_0_95_pow$Rn)
  ## ----- sloping plane s kernel with norm 0.5 -----
  # En
  summary(res_comp_CV$sps_0_5$CV$En)
  mean(res_comp_CV$sps_0_5$CV$En)
  sd(res_comp_CV$sps_0_5$CV$En)
  summary(res_comp_CV$sps_0_5$CV_alpha_0_75_pow$En)
  mean(res_comp_CV$sps_0_5$CV_alpha_0_75_pow$En)
  sd(res_comp_CV$sps_0_5$CV_alpha_0_75_pow$En)
  summary(res_comp_CV$sps_0_5$CV_alpha_0_85_pow$En)
  mean(res_comp_CV$sps_0_5$CV_alpha_0_85_pow$En)
  sd(res_comp_CV$sps_0_5$CV_alpha_0_85_pow$En)
  summary(res_comp_CV$sps_0_5$CV_alpha_0_95_pow$En)
  mean(res_comp_CV$sps_0_5$CV_alpha_0_95_pow$En)
  sd(res_comp_CV$sps_0_5$CV_alpha_0_95_pow$En)
  # Rn
  summary(res_comp_CV$sps_0_5$CV$Rn)
  mean(res_comp_CV$sps_0_5$CV$Rn)
  sd(res_comp_CV$sps_0_5$CV$Rn)
  summary(res_comp_CV$sps_0_5$CV_alpha_0_75_pow$Rn)
  mean(res_comp_CV$sps_0_5$CV_alpha_0_75_pow$Rn)
  sd(res_comp_CV$sps_0_5$CV_alpha_0_75_pow$Rn)
  summary(res_comp_CV$sps_0_5$CV_alpha_0_85_pow$Rn)
  mean(res_comp_CV$sps_0_5$CV_alpha_0_85_pow$Rn)
  sd(res_comp_CV$sps_0_5$CV_alpha_0_85_pow$Rn)
  summary(res_comp_CV$sps_0_5$CV_alpha_0_95_pow$Rn)
  mean(res_comp_CV$sps_0_5$CV_alpha_0_95_pow$Rn)
  sd(res_comp_CV$sps_0_5$CV_alpha_0_95_pow$Rn)
  
  ## ----- sloping plane s kernel with norm 0.8 -----
  # En
  summary(res_comp_CV$sps_0_8$CV$En)
  mean(res_comp_CV$sps_0_8$CV$En)
  sd(res_comp_CV$sps_0_8$CV$En)
  summary(res_comp_CV$sps_0_8$CV_alpha_0_75_pow$En)
  mean(res_comp_CV$sps_0_8$CV_alpha_0_75_pow$En)
  sd(res_comp_CV$sps_0_8$CV_alpha_0_75_pow$En)
  summary(res_comp_CV$sps_0_8$CV_alpha_0_85_pow$En)
  mean(res_comp_CV$sps_0_8$CV_alpha_0_85_pow$En)
  sd(res_comp_CV$sps_0_8$CV_alpha_0_85_pow$En)
  summary(res_comp_CV$sps_0_8$CV_alpha_0_95_pow$En)
  mean(res_comp_CV$sps_0_8$CV_alpha_0_95_pow$En)
  sd(res_comp_CV$sps_0_8$CV_alpha_0_95_pow$En)
  # Rn
  summary(res_comp_CV$sps_0_8$CV$Rn)
  mean(res_comp_CV$sps_0_8$CV$Rn)
  sd(res_comp_CV$sps_0_8$CV$Rn)
  summary(res_comp_CV$sps_0_8$CV_alpha_0_75_pow$Rn)
  mean(res_comp_CV$sps_0_8$CV_alpha_0_75_pow$Rn)
  sd(res_comp_CV$sps_0_8$CV_alpha_0_75_pow$Rn)
  summary(res_comp_CV$sps_0_8$CV_alpha_0_85_pow$Rn)
  mean(res_comp_CV$sps_0_8$CV_alpha_0_85_pow$Rn)
  sd(res_comp_CV$sps_0_8$CV_alpha_0_85_pow$Rn)
  summary(res_comp_CV$sps_0_8$CV_alpha_0_95_pow$Rn)
  mean(res_comp_CV$sps_0_8$CV_alpha_0_95_pow$Rn)
  sd(res_comp_CV$sps_0_8$CV_alpha_0_95_pow$Rn)
}
