rm(list=ls())
graphics.off()
cat("\014")


##################################################################################################
#### Graphs with logarithmic time for using PPCKO no-cv comparing imposing k with ex_solver,  ####
####  selecting k with exp pow criterion, imposing k with gep_solver                          ####
##################################################################################################


#put here the path of the local copy of the directory
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"

#to eventually store results
save_res = TRUE
format_fl = ".jpg"


#load results to be analyzed
load(paste0(dir_w,"/Test_CompTime/results/times_NoCV_k_imp.Rdata"))
load(paste0(dir_w,"/Test_CompTime/results/times_NoCV_k_imp_gen.Rdata"))
load(paste0(dir_w,"/Test_CompTime/results/times_NoCV_k_no_imp.Rdata"))

#path to store the resulting graphs
path_store_res = paste0(dir_w,"/Test_CompTime//results/ex_vs_gep")

#packages for graphs
library(ggplot2)
library(tidyr)

#transform times in log times
log_time <- data.frame(
  Point = as.numeric(c(100, 400, 900, 1600, 2500, 10000)), 
  A = c(log2(times_NoCV_k_imp[1,4]), 
        log2(times_NoCV_k_imp[2,4]), 
        log2(times_NoCV_k_imp[3,4]), 
        log2(times_NoCV_k_imp[4,4]), 
        log2(times_NoCV_k_imp[5,4]), 
        log2(times_NoCV_k_imp[6,4])),                   
  B = c(log2(times_NoCV_k_no_imp[1,4]), 
        log2(times_NoCV_k_no_imp[2,4]), 
        log2(times_NoCV_k_no_imp[3,4]), 
        log2(times_NoCV_k_no_imp[4,4]), 
        log2(times_NoCV_k_no_imp[5,4]), 
        log2(times_NoCV_k_no_imp[6,4])),                  
  C = c(log2(times_NoCV_k_imp_gen[1,4]), 
        log2(times_NoCV_k_imp_gen[2,4]), 
        log2(times_NoCV_k_imp_gen[3,4]), 
        log2(times_NoCV_k_imp_gen[4,4]), 
        log2(times_NoCV_k_imp_gen[5,4]), 
        log2(times_NoCV_k_imp_gen[6,4]))                     
)

#prepare data for plotting
data_long <- pivot_longer(log_time, cols = c(A, B, C), names_to = "Solver", values_to = "Value")

data_long <- data_long %>%
  mutate(Point = as.numeric(Point),  
         Solver = as.factor(Solver))  

plt=ggplot(data_long, aes(x = Point, y = Value, fill = Solver)) +
           geom_bar(stat = "identity", position = "dodge") +
           geom_line(aes(color = Solver, group = Solver), size = 1) +  
           geom_point(aes(color = Solver), size = 2) +
           scale_x_continuous(breaks = sort(unique(data_long$Point)))+
           scale_fill_manual(values = c("A" = "purple", "B" = "lightblue", "C" = "orange"),
                             labels = c("A" = "Exact solver, k imposed", 
                                        "B" = "Exact solver, k via explanatory power", 
                                        "C" = "GEP solver, k imposed")) + 
          scale_color_manual(values = c("A" = "purple", "B" = "lightblue", "C" = "orange"),
                             labels = c("A" = "Exact solver, k imposed", 
                                        "B" = "Exact solver, k via explanatory power", 
                                        "C" = "GEP solver, k imposed")) + 
          labs(title = "PPCKO, no cv",
               x = "Grid size",
               y = "Logarithmic time [log2(s)]") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


#save the graph
ggsave(filename = paste0("ex_vs_gep",format_fl),
       plot = plt,
       device = NULL,
       path = path_store_res,
       scale = 1,
       width = 14,
       height = 10,
       dpi = 300)
