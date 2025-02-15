## ----- Data parameters -----
{
  left_ex            <- 0                                           #left extreme of the domain of the functional data
  right_ex           <- 1                                           #right extreme of the domain of the functional data
  dim_grid           <- 200                                         #number of discrete evaluations of the functional statistical units
  n                  <- 300                                         #time instants of the functional time series
  burnin             <- 50                                          #burnin iterations for FAR(1)
  N                  <- n - burnin                                  #instants that will actually be taken into account  
  t.grid             <- seq(left_ex,right_ex, length.out=dim_grid)  #grid for the discrete evaluation of the statistical units
}