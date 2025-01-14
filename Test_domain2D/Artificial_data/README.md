# **Test on synthetically generated surfaces FTS**

Data are generated as in [Ajroldi, Diquigiovanni, Fontana and Vantini](#ref-ajroldi) (see main READ.ME). One process is generated, with 20x20 elements for each surface (domain $[0,1]\times [0,1]$), 100 time instants, 20 burn-in iterations. The first b instants are used as available data, b+1 as test set: evaluate the prediction loss as L2 norm estimate ($E_n$) and L1 norm ($R_n$). $b \in \{50, \dots, 99\}$. Evaluate mean and standard deviation of both $E_n$ and $R_n$, other than their boxplots. 

-   `simulation_predictions.R`: predictors used: PPC (Principal Predictive Components) with cv, input space of $\alpha \in \{10^{-10}, 10^{-9},\dots,10^{10},10^{11}\}$, of $k \in \{1,2,\dots,200\}$, EK (Estimated Kernel) and EKI (Estimated Kernel Improved) with cv, input space of number of directions $\{2,3,4,5,6\}$, MP (prediction with mean), NP (prediction with last available instant) and EX (prediction using process generating the data). In the end, also PPC with gep_solver, cv with same input space as PPC. Prediction error computation

-   `comparison_predictors.R`: evaluating $E_n$ and $R_n$ of the forecasts, their mean, sd, boxplots

-   `Analysis_PPC.R`: collecting the best $\alpha$, the best $k$ and the retained explanatory power of the PPC predictor, investigating their distributions, thorugh barcharts and boxplots

-   `check_alphas.R`: checking why for some PPC predictions $\alpha$ was $10^{10}$ (in the end, problem of bad conditioning of the covariance: unlucky case): with a new seed: everything fixed. DOing a cv on $\alpha$, k via exp pow criterion to double check since all of them bad for exp pow retained

-   `comparison_solvers.R`: comparing the PPC prediction obtained with ex_solver and gep_solver




Results:

-   `results`: contains the results

Helpers:

-   `utils`: contains functions to generate surfaces FAR(1) and doing prediction with the process generating data, to perform KE and KEI predictions on surfaces FTS ( all taken from Ajroldi N.)
