**Analysis of computational performances of `PPCKO`**

-   *TestCompTimeNoCV.R*: Computational time of PPCKO no-cv version, for different values of number of FTS evaluations and time instants: comparing imposing k with ex_solver, selecting k with exp pow criterion, imposing k with gep_solver
-   *Analysis_ex_vs_gep.R*: Plotting the logarithmic times comparing, for different values of number of FTS evaluations and time instants, PPCKO no-cv version, imposing k with ex_solver, selecting k with exp pow criterion, imposing k with gep_solver
-   *TestCompTimeCV.R*: Computational time of PPCKO cv version, for sizes of alpha input space and FTS time instants, comparing parallel and sequential versions, for different numbers of threads, for ex_solver vs gep_solver
-   *Analysis_par_vs_seq.R*: Plotting computational time of PPCKO cv version, for sizes of alpha input space and FTS time instants: one plot, for each thread, for each pair of tested alpha input space size and FTS time instants, comparing ex_solver and gep_solver; for both ex_solver and gep_solver, plotting, for each time instant, the time wrt the alpha input space size, and viceversa
-   Folder *utils*: utilities to generate a 1d FAR(1)
-   Folder *results*: containing the results
