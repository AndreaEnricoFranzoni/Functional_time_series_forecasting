# **Test on real-world surfaces FTS (Black Sea sea level)**

Data comes from [C3S](https://climate.copernicus.eu).
Data consists, for days from the 1st of January of 2014 up to the 3rd of June of 2020, in Black Sea (BS) sea level with respect to a reference value (mean sea surface over 20 years) in functions of the geographic position.
Forecasting of 1000 days, from the 10th of April of 2017 up to the 5th of January of 2020, using the previous 100 days as trainig set. EX predictor is substituted by a concurrent one, that basically exploits a pointwise scalar FAR(1).
The forecasting is computed for original time series, onedifferentiation time series and two differentiation time series. Forecasting has been performed on two different zones: north-west (mouth zone), near rivers mouth (no pointiwise stationarity) and south-west (center zone) (pointwise stationarity). Predictions, for both zones, for all time series, are done using:

-   `prediction_PPC.R`: PPC (Principal Predictive Components) with cv, input space of $\alpha \in \{0.00001,0.0001,0.001,0.01,0.1,1,10\}$

-   `prediction_PPC_gen.R`: PPC (Principal Predictive Components) with cv, input space of $\alpha \in \{0.00001,0.0001,0.001,0.01,0.1,1,10\}$, using gep_solver

-   `prediction_ppc_exp_pow.R`: PPC (Principal Predictive Components) with cv, input space of $\alpha \in \{0.00001,0.0001,0.001,0.01,0.1,1,10\}$, retaining $k$ not with cv but imposing 0.9 retained explanatory power to the predictor

-   `prediction_KE.R`: KE (Kernel Estimated) with cv, input space of componentns $\{2,3,4,5,6\}$

-   `prediction_KEI.R`: KEI (Kernel Estimated Improved) with cv, input space of componentns $\{2,3,4,5,6\}$

-   `prediction_MP.R`: MP: predicting with mean

-   `prediction_NP.R`: NP: predicting with last instant

-   `prediction_CC.R`: predicting with FAR(1) concurrent pointwise


And then, for both zones, for all time series:

-   `prediction_errors.R`: computing $E_n$ and $R_n$

-   `comparison_predictors.R`: evaluating mean, sd and boxplots of both $E_n$ and $R_n$

-   `analysis_PPC.R`: collecting the best $\alpha$, the best $k$ and the retained explanatory power of the PPC predictor, investigating their distributions, thorugh barcharts and boxplots

-   `analysis_PPC_exp_pow.R`: collecting the best $\alpha$, the best $k$ and the retained explanatory power of the PPC predictor, investigating their distributions, thorugh barcharts and boxplots, if $k$ selected via explanatory power criterion

-   `comparison_PPC_algo.R`: evaluating mean, sd and boxplots of both $E_n$ and $R_n$ of PPC using cv for $k$ and PPC using exp pow for $k$

-   `comparison_solvers.R`: comparing the PPC prediction obtained with ex_solver and gep_solver


Helpers:

-   `data_generation.R`: generating the surface FTS for the two zones

-   `stationarity_test.R`: pointwise ADF p-value for original data, the two zones and all the types of ts (original, one diff, two diffs)

-   `data`: contains the data

-   `results`: contains the results as described here

-   `utils`: functions to perform 2d EK, EKI (with cv) and CC (courtesy of Ajroldi N.)
