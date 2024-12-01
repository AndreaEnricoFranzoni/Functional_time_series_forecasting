rm(list=ls())
graphics.off()
cat("\014")

library(PPCKO)
data("data_2d")




#PPCKO parameters
{
  id_CV            <- "NoCV"
  alpha            <- 0.1
  k                <- 0 
  threshold_ppc    <- 0.95
  alpha_vec        <- c(1e-4,1e-3,1e-2,1e-1,1,1e1,1e2)
  k_vec            <- 1:20
  toll             <- 1e-4
  disc_ev_x1       <- NULL
  disc_ev_x2       <- NULL
  left_extreme_x1  <- 0
  right_extreme_x1 <- 1
  left_extreme_x2  <- 0
  right_extreme_x2 <- 1
  num_disc_ev_x1   <- 10
  num_disc_ev_x2   <- 10
  err_ret          <- 0
  id_rem_nan       <- NULL
  
  x1.grid = seq(0,1,length.out=num_disc_ev_x1)
  x2.grid = seq(0,1,length.out=num_disc_ev_x2)
}





data_wrapped = data_2d_wrapper_from_list(data_2d)

test2d_hp = KO_check_hps_2d( X = data_wrapped, 
                             dim_x1 = num_disc_ev_x1, 
                             dim_x2 = num_disc_ev_x2)


test2d = PPC_KO_2d(X = data_wrapped,
                           id_CV = id_CV,
                           alpha = alpha,
                           k = k,
                           threshold_ppc = threshold_ppc,
                           alpha_vec = NULL,
                           k_vec = k_vec,
                           toll = toll,
                           disc_ev_x1 = x1.grid,
                           num_disc_ev_x1 = num_disc_ev_x1,
                           disc_ev_x2 = x2.grid,
                           num_disc_ev_x2 = num_disc_ev_x2,
                           left_extreme_x1 = left_extreme_x1,
                           right_extreme_x1 = right_extreme_x1,
                           left_extreme_x2 = left_extreme_x2,
                           right_extreme_x2 = right_extreme_x2,
                           err_ret = 0)


KO_show_results_2d(test2d,test2d_hp)
