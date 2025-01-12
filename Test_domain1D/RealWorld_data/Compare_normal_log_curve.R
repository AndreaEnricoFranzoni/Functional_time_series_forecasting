rm(list=ls())
graphics.off()
cat("\014")
set.seed(23032000)

################################################################ 
##  Simple script for comparing normal and log curve visually ## 
################################################################ 



#put here the path of the local copy of the directory
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series_forecasting"


# ----- data -----
load(paste0(dir_w,"/Test_domain1D/RealWorld_data/utils/data/MGS_cg_260419_310120_data.Rdata"))

{
  x_grid_dim         <- 401
  x_grid             <- MGS_cg_260419_310120_data$x_axis[1:x_grid_dim]
  left_extreme       <- min(x_grid)
  right_extreme      <- max(x_grid)
  
  tot_time_instants  <- length(MGS_cg_260419_310120_data$y_axis)
  offers_dataset     <- matrix(data = NA, nrow = x_grid_dim, ncol = tot_time_instants)
  demands_dataset    <- matrix(data = NA, nrow = x_grid_dim, ncol = tot_time_instants)
  
  for (i in 1:tot_time_instants) {
    offers_dataset[,i]  <-  (MGS_cg_260419_310120_data$y_axis[[i]][[2]][1:x_grid_dim])
    demands_dataset[,i] <-  (MGS_cg_260419_310120_data$y_axis[[i]][[3]][1:x_grid_dim])
  }
  
  first_prediction <- 98
}




day = 281


data <- data.frame(
  x = x_grid,               
  y = c(MGS_cg_260419_310120_data$y_axis[[day]][[2]][1:x_grid_dim],
        log(MGS_cg_260419_310120_data$y_axis[[day]][[2]][1:x_grid_dim]),
        MGS_cg_260419_310120_data$y_axis[[day]][[3]][1:x_grid_dim],
        log(MGS_cg_260419_310120_data$y_axis[[day]][[3]][1:x_grid_dim])),      
  Curve = rep(c("Original offer price", 
                 "Log transformed offer price",
                 "Original demand price", 
                 "Log transformed demand price"), each = length(x_grid)) 
)

plt=ggplot(data, aes(x = x, y = y, color = Curve)) +
  geom_line(size = 1) +
  labs(
    title = "Comparison normal and log-transformed curve",
    x = "Quantity [MWh]",
    y = "Price [Euro/MWh]",
    color = "Curve"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("grey", "lightblue", "black", "blue"))

quartz()
print(plt)



data <- data.frame(
  x = x_grid,               
  y = c(
        log(MGS_cg_260419_310120_data$y_axis[[day]][[2]][1:x_grid_dim]),

        log(MGS_cg_260419_310120_data$y_axis[[day]][[3]][1:x_grid_dim])),      
  Curve = rep(c(
                "Log transformed offer price",
                "Log transformed demand price"), each = length(x_grid)) 
)


plt=ggplot(data, aes(x = x, y = y, color = Curve)) +
  geom_line(size = 1) +
  labs(
    title = "Log-transformed curve",
    x = "Quantity [MWh]",
    y = "log(Price) [log(Euro/MWh)]",
    color = "Curve"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("grey", "lightblue"))

quartz()
print(plt)
