**Test on synthetically generated curves FTS**

Data are generated as in [Didericksen, Kokoszka and Zhang](#ref-kokoskza) (see main READ.ME). 8 different processes are generated (4 different kernels, each one with 2 different norms magnitude), with 100 time instants, 50 burn-in iterations, 200 discrete evaluations of the curve. For each one of them: use the first b instants as available data, b+1 as test set: evaluate the prediction loss as L2 norm estimate ($E_n$) and L1 norm ($R_n$). $b \in \{50, \dots, 99\}$. Evaluate mean and standard deviation of both $E_n$ and $R_n$, other than their boxplots. Predictions are done using:

-   `Prediction_PPC.R`: PPC with cv, input space of $\alpha \in \{10^{-10}, 10^{-9},\dots,10^{9},10^{10}\}$, of $k \in \{1,2,\dots,200\}$

-   `Prediction_KE.R`: Estimated Kernel predictor. Input space of number of directions $\{2,3,4,5,6\}$

-   `Prediction_KEI.R`: Estimated Kernel Improved predictor. Input space of number of directions $\{2,3,4,5,6\}$

-   `Prediction_MP.R`: using the mean as predictor

-   `Prediction_NP.R`: using the last time instant as predictor

-   `Prediction_EX.R`: using the process generating the data for retrieving the next instant

-   `Prediction_PPC_gen.R`: PPC with cv, input space of $\alpha \in \{10^{-10}, 10^{-9},\dots,10^{9},10^{10}\}$, of $k \in \{1,2,\dots,200\}$, using gep_solver

Then, the comparisons:

-   `comparison_predictors.R`: evaluating mean and sd of $E_n$ and $R_n$ of the predictions, for each predictor, other than their boxplots

-   `comparison_solvers.R`: comparing the predictions using ex_solver and gep_solver: same results

-   `analysis_valid_err_PPC_cv_alpha.R`: for each process mean of the validation errors for each element of the $\alpha$ input space, selecting $k$ with cv and with explanatory power criterion

-   `comparison_valid_errors_alpha.R`: plotting then $\alpha$ validation errors dynamic

-   `Analysis_PPC.R`: collecting the best $\alpha$, the best $k$ and the retained explanatory power of the PPC predictor, investigating their distributions, thorugh barcharts and boxplots

Results:

-   `results`: contains the results

Helpers:

-   `utils`: contains the parameters of the data, other than functions to generate curves FAR(1) and to perform KE and KEI predictions (courtesy of Ajroldi N.)
