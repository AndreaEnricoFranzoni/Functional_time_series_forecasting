rm(list=ls())
graphics.off()
cat("\014")

set.seed(23032000)

#change here
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series"
load(paste0(dir_w,"/Test_domain2D/RealWorld_data/BS.Rdata"))


Xt_piece1 = list()
Xt_piece2 = list()
Xt_piece3 = list()
Xt_piece4 = list()
Xt_piece5 = list()
Xt_piece6 = list()
Xt_piece7 = list()
Xt_piece8 = list()


for (i in 1:(length(Xt)-1)) {

  Xt_piece1 = c(Xt_piece1,list(Xt[[i]][1:30,1:28]))
  Xt_piece2 = c(Xt_piece2,list(Xt[[i]][1:30,29:56]))
  Xt_piece3 = c(Xt_piece3,list(Xt[[i]][31:60,1:28]))
  Xt_piece4 = c(Xt_piece4,list(Xt[[i]][31:60,29:56]))
  Xt_piece5 = c(Xt_piece5,list(Xt[[i]][61:90,1:28]))
  Xt_piece6 = c(Xt_piece6,list(Xt[[i]][61:90,29:56]))
  Xt_piece7 = c(Xt_piece7,list(Xt[[i]][91:120,1:28]))
  Xt_piece8 = c(Xt_piece8,list(Xt[[i]][91:120,29:56]))
}


alpha_vec = c(1e-4,1e-3,1e-2,1e-1,1,1e1,1e2)
{
   
  temp1 = PPCKO::data_2d_wrapper_from_list(Xt_piece1)
  res1  = PPCKO::PPC_KO_2d(X = temp1,
                           id_CV = "CV",
                           num_disc_ev_x1 = 30,
                           num_disc_ev_x2 = 28)
  
  temp2 = PPCKO::data_2d_wrapper_from_list(Xt_piece2)
  res2  = PPCKO::PPC_KO_2d(X = temp2,
                           id_CV = "CV",
                           num_disc_ev_x1 = 30,
                           num_disc_ev_x2 = 28)
  
  temp3 = PPCKO::data_2d_wrapper_from_list(Xt_piece3)
  res3  = PPCKO::PPC_KO_2d(X = temp3,
                           id_CV = "CV",
                           num_disc_ev_x1 = 30,
                           num_disc_ev_x2 = 28)
  
  temp4 = PPCKO::data_2d_wrapper_from_list(Xt_piece4)
  res4  = PPCKO::PPC_KO_2d(X = temp4,
                           id_CV = "CV",
                           num_disc_ev_x1 = 30,
                           num_disc_ev_x2 = 28)
  
  temp5 = PPCKO::data_2d_wrapper_from_list(Xt_piece5)
  res5  = PPCKO::PPC_KO_2d(X = temp5,
                           id_CV = "CV",
                           num_disc_ev_x1 = 30,
                           num_disc_ev_x2 = 28)
  
  temp6 = PPCKO::data_2d_wrapper_from_list(Xt_piece6)
  res6  = PPCKO::PPC_KO_2d(X = temp6,
                           id_CV = "CV",
                           num_disc_ev_x1 = 30,
                           num_disc_ev_x2 = 28)
  
  temp7 = PPCKO::data_2d_wrapper_from_list(Xt_piece7)
  res7  = PPCKO::PPC_KO_2d(X = temp7,
                           id_CV = "CV",
                           num_disc_ev_x1 = 30,
                           num_disc_ev_x2 = 28)
  
  temp8 = PPCKO::data_2d_wrapper_from_list(Xt_piece8)
  res8  = PPCKO::PPC_KO_2d(X = temp8,
                           id_CV = "CV",
                           num_disc_ev_x1 = 30,
                           num_disc_ev_x2 = 28)
}



  