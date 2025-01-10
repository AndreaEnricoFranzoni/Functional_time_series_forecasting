# Functional Time Series Forecasting
The repo contains tests conducted on Functional Time Series (FTS) forecasting.

Particular focus has been put on [Kargin-Onatski algorithm](https://core.ac.uk/download/pdf/82625156.pdf) algorithm, and its implementation in the package `PPCKO`, [here](https://github.com/AndreaEnricoFranzoni/PPCforAutoregressiveOperator). Tests for its predictive and computational performances have been performed. 

Specifically, the PPC KO predictor has been compared to other predictors. Forecasting has been performed for curves and surfaces FTS domain data; data have been generate synthetically but also come from real-world scenarios.

> ❗️ **N.B.:** the folder with the **PACS** flag contains the tests reported in the report for the Advanced Programming for Scientific Computing (a.k.a. PACS) at Politecnico di Milano, a.y. 2023/2024.

> ❗️ **WIP:**  PPC KO applied to synthetic data for which PPC KO assumptions definitely fail and Campi Flegrei dataset forecasting.


# Installation
`dir_w` contains the path of the local copy of the repo. Change it accordingly.

Each script contains `save_res` flag: if TRUE, obtained results will be saved in the folders for the results if test run. If FALSE, no.

Run the scripts `Install.R` and `Requirements.R` to install `PPCKO` package and all the other ones needed for the tests. If problems arise installing `PPCKO`, check [here](https://github.com/AndreaEnricoFranzoni/PPCforAutoregressiveOperator).



# Time series of curves
Folder `Test_domain1D`: **PACS**. 

- Subfolder `Artificial_data`: reproduction of the Didericksen, Kokoszka and Zhang [experiment](https://ideas.repec.org/cgi-bin/refs.cgi). Comparison of different predictors for forecasting synthetic time series of curves.

- Subfolder `RealWorld_data`: same comparison on data from [MGS](https://www.mercatoelettrico.org/en/).


# Time series of surfaces
Folder `Test_domain2D`: **PACS**. 

- Subfolder `Artificial_data`: comparison of different predictors for forecasting synthetic time series of surfaces, generated as in [Ajroldi et Al.](https://arxiv.org/abs/2207.13656).

- Subfolder `RealWorld_data`: PPC KO is used to make predictions on Black Sea sea-level altitude. Forecasting of time serie, delta-1 lag, delta-2 lag for reconstructing the prediction.


# Computational performance
Folder `Test_CompTime`: **PACS**. 

Tests for computational time, no-cv version vs cv-version, ex_solver vs gep_solver, parallel vs sequential.
