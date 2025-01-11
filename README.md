# Functional Time Series Forecasting
The repo contains tests conducted on Functional Time Series (FTS) forecasting.

Particular focus has been put on [Kargin-Onatski algorithm](https://core.ac.uk/download/pdf/82625156.pdf) algorithm, and its implementation in the package `PPCKO`, [here](https://github.com/AndreaEnricoFranzoni/PPCforAutoregressiveOperator). Tests for its predictive and computational performances have been performed. 

Specifically, PPC predictor has been compared to other predictors. Forecasting has been performed for curves and surfaces FTS domain data; data have been generate synthetically but also come from real-world scenarios. We resume the contents of the folders here, in each one of the folders other readme files describe their content more deeply. The tests results, reported and commented in ['Principal Predictive Components for estimating an autoregressive operator'](#ref-pacsrep), are stored in the results folder.

In each script: 

  - `dir_w` contains the path of the local copy of the repo. Change it accordingly.
  - `save_res` flag: if TRUE, results obtained by running the tests will be saved in the folders for the results if test run. If FALSE, no.

> ❗️ **N.B.:** the folder with the **PACS** flag contains the tests reported in the report for the Advanced Programming for Scientific Computing (a.k.a. PACS) at Politecnico di Milano, a.y. 2023/2024.

> ❗️ **WIP:** `PPCKO` applied to synthetic data for which PPC KO assumptions definitely fail and Campi Flegrei dataset forecasting.


# Installation

Run the scripts `Install.R` and `Requirements.R` to install `PPCKO` and all the other packages needed for the tests.

If problems arise installing `PPCKO`, check [here](https://github.com/AndreaEnricoFranzoni/PPCforAutoregressiveOperator).



# Time series of curves
Folder `Test_domain1D`: **PACS**. Forecasting of curves FTS.

- Subfolder `Artificial_data`: `PPCKO` is used to forecast synthetic curves FTS, as in [Didericksen, Kokoszka and Zhang](#ref-kokoskza). Comparison with other predictors, analysis of PPCKO features.

- Subfolder `RealWorld_data`: `PPCKO` is used to forecast gas prices from [MGS](https://www.mercatoelettrico.org/en/), as in [Diquigiovanni, Fontana and Vantini](#ref-diquigiovanni). Comparison with other predictors, analysis of PPCKO features.


# Time series of surfaces
Folder `Test_domain2D`: **PACS**. Forecasting of surfaces FTS.

- Subfolder `Artificial_data`: `PPCKO` is used to forecast synthetic surfaces FTS, as in [Ajroldi, Diquigiovanni, Fontana and Vantini](ref-ajroldi). Comparison with other predictors, analysis of PPCKO features.

- Subfolder `RealWorld_data`: `PPCKO` is used to forecast Black Sea sea-level altitude from from Copernicus Climate Change Service [C3S](https://climate.copernicus.eu), as suggested by [Ajroldi, Diquigiovanni, Fontana and Vantini](ref-ajroldi). Comparison with other predictors, analysis of PPCKO features.


# Computational performance
Folder `Test_CompTime`: **PACS**. 

Tests for `PPCKO` computational performances, comparing no-cv version vs cv-version, ex_solver vs gep_solver, parallel vs sequential.


# Bibliography 
1. <a id="ref-PPCKO"></a> **Kargin, V., Onatski, A.**, `Curve forecasting by functional autoregression`, *Journal of Multivariate Analysis*, 99, 2508-2526, 2008, https://www.sciencedirect.com/science/article/pii/S0047259X08000961

2.  <a id="ref-kokoskza"></a> **Didericksen D., Kokoszka P., Zhang X.**, `Empirical properties of forecasts with the functional autoregressive model`, *Comput Stat*, 27, 285-298, 2021, https://doi.org/10.1007/s00180-011-0256-2

3.  <a id="ref-ajroldi"></a> **Ajroldi N., Diquigiovanni J., Fontana  M., Vantini S.**, `Conformal prediction bands for two-dimensional functional time series`, *Computational Statistics and Data Analysis*, 187, 2023, https://arxiv.org/abs/2207.13656

4.  <a id="ref-diquigiovanni"></a> **Diquigiovanni J, Fontana M., Vantini S.**, `Distribution-Free Prediction Bands for Multivariate Functional Time Series: an Application to the Italian Gas Market`, *MOX-Report No. 45/2021*, 2021, https://arxiv.org/abs/2107.00527

5.  <a id="ref-dati_sat"></a> **Bernardi M. S., Africa P. C., de Falco C., Formaggia L., Menafoglio A., Vantini S.**, `On the Use of Interferometric Synthetic Aperture Radar Data for Monitoring and Forecasting Natural Hazards`, *Math Geosci*, 53, 1781–1812, 2021, https://doi.org/10.1007/s11004-021-09948-8

6.  <a id="ref-pacsrep"></a> **Franzoni A. E., Vantini S., Menafoglio A., Bortolotti T.**, `Principal Predictive Components for estimating an autoregressive operator`, 2025

7. <a id="ref-PPCKO_package"></a> **Franzoni A. E., Vantini S., Menafoglio A., Bortolotti T.**, `PPCKO: Principal Predictive Components for Estimating an Autoregressive Operator`, *R package version 1.0*, 2024, https://github.com/AndreaEnricoFranzoni/PPCforAutoregressiveOperator
