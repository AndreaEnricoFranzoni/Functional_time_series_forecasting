**Test on real-world curves FTS**

Data comes from [MGS](https://www.mercatoelettrico.org/en/).
Data consists, for days from the 26th of April of 2019 up to the 31st of January of 2020, in gas offers (monotonic increasing) and demands (monotonic decreasing) prices (euro/MWh) in functions of the quantity traded (MWh). The final prices and the final quantity traded is their intersection.
Forecasting of 184 days, from the 1st of August of 2019 up to the 31st of January of 2020, using the data from the first available instant as trainig set. EX predictor is substituted by a concurrent function-on-function model proposed by [Diquigiovanni, Fontana and Vantini](#ref-diquigiovanni) (see main READ.ME), that exploits more infomration than PPC (also the actual price).
The same experiment are then repeated using the log curves, in order to flat the curve steep behavior in the initial domain part.
Evaluate mean and standard deviation of both $E_n$ and $R_n$, other than their boxplots. Predictions, for both curves are done using:

-   *Prediction_PPC.R*: PPC with cv, input space of $\alpha \in \{10^{-10}, 10^{-9},\dots,10^{10},10^{11}\}$, of $k \in \{1,2,\dots,200\}$

-   *Prediction_KE.R*: Estimated Kernel and Estimated Kernel Improved predictor. Input space of number of directions $\{1,...,10\}$

-   *Prediction_MP.R*: using the mean as predictor

-   *Prediction_NP.R*: using the last time instant as predictor

-   *Prediction_CC.R*: using the concurrent predictor

-   *Prediction_PPC_gen.R*: PPC with cv, input space of $\alpha \in \{10^{-10}, 10^{-9},\dots,10^{10},10^{11}\}$, of $k \in \{1,2,\dots,200\}$, using gep_solver

-   *Prediction_PPC_log.R*: PPC with cv, input space of $\alpha \in \{10^{-10}, 10^{-9},\dots,10^{10},10^{11}\}$, of $k \in \{1,2,\dots,200\}$, on log-curves

-   *Prediction_KE_log.R*: Estimated Kernel and Estimated Kernel Improved predictor. Input space of number of directions $\{1,...,10\}$, on log-curves

-   *Prediction_MP_log.R*: using the mean as predictor, on log-curves

-   *Prediction_NP_log.R*: using the last time instant as predictor, on log-curves

-   *Prediction_CC_log.R*: using the concurrent predictor, on log-curves

Then, the comparisons, for both curves:

-   *Comparison_predictors.R*: evaluating mean and sd of $E_n$ and $R_n$ of the predictions, for each predictor, other than their boxplots

-   *Comparison_predictions.R*: plotting, for each of the 184 days predicted, the predictions, for each predictors

-   *Comparison_solvers.R*: comparing the predictions using ex_solver and gep_solver: same results

-   *Analysis_PPC.R*: collecting the best $\alpha$, the best $k$ and the retained explanatory power of the PPC predictor, investigating their distributions, thorugh barcharts and boxplots

-   *prediction_errors.R*: computing $E_n$ and $R_n$ of the predictions made, for ecery predictor

-   °Next_instant_prediction_PPC.R*: forecasting with PPC of the next available day. Analyzing forecasting and PPCs

-   *Comparison_predictors_log.R*: evaluating mean and sd of $E_n$ and $R_n$ of the predictions, for each predictor, other than their boxplots, on log-curves

-   *Comparison_predictions_log.R*: plotting, for each of the 184 days predicted, the predictions, for each predictors, on log-curves

-   *Analysis_PPC_log.R*: collecting the best $\alpha$, the best $k$ and the retained explanatory power of the PPC predictor, investigating their distributions, thorugh barcharts and boxplots, on log-curves

-   *prediction_errors_log.R*: computing $E_n$ and $R_n$ of the predictions made, for ecery predictor, on log curves

-   °Next_instant_prediction_PPC_log.R*: forecasting with PPC of the next available day. Analyzing forecasting and PPCs, on log-curves

-   *Comparison_final_quantity_price.R*: evaluating interesection between PPC predicted curves, and evaluating it wrt the real intersection with a bagplot of the error

Results:

-   *results*: contains the results, in the corresponding folders as indicated in the script for the computations

Helpers:

-   *utils*: contains the data (MGS_cg_260419_310120_data.Rdata), functions to perform KE and KEI predictions (taken from Ajroldi N.) and the code (subfolder *CC_code*) for CC predictor (taken from Diquigiovanni J.) 
-   *Compare_normal_log_curve.R*: script to visually compare curves and their log transformation