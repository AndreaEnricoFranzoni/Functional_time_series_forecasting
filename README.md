# Functional Time Series Forecasting
The repo contains tests conducted on functional time series forecasting, 

Particular focus has been put on [Kargin-Onatski algorithm](https://core.ac.uk/download/pdf/82625156.pdf) algorithm, and its implementation in the package `PPCKO`, [here](https://github.com/AndreaEnricoFranzoni/PPCforAutoregressiveOperator). Tests for its predictive power and its performance have been performed. 

Specifically, the PPC KO predictor has been compared to other predictors. Forecasting has been performed for unidmensional and bidimensional domain data, synthetic and real-world.

> ❗️ **N.B.:** the folder with the **PACS** flag contains the tests reported in the report for the Advanced Programming for Scientific Computing (a.k.a. PACS) at Politecnico di Milano, a.y. 2023/2024.

> ❗️ **WIP:**  PPC KO applied to synthetic data for which PPC KO assumptions definitely fail and Campi Flegrei dataset forecasting.


# Installation
Run the script `Install.R`. Eventually check `Requirements.R` packages installation. In all the scripts, `dir_w` contains the path of the local copy of the repo.

If problems arise installing `PPCKO`, check [here](https://github.com/AndreaEnricoFranzoni/PPCforAutoregressiveOperator).

# Tutorials
Folder `Tutorial`: **PACS**.
Look at `tutorial_1d.R` and `tutorial_2d.R`, for an usage of `PPCKO` for time series of, respectively, curves and surfaces forecasting.


# Computational performance
Folder `Test_CompTime`: **PACS**. 

Tests for computational time, no-cv version vs cv-version, parallel vs sequential.


# Time series of curves
Folder `Test_domain1D`: **PACS**. 

- Subfolder `Artificial_data`: reproduction of the Didericksen, Kokoszka and Zhang [experiment](https://ideas.repec.org/cgi-bin/refs.cgi). Comparison of different predictors for forecasting synthetic time series of curves.

- Subfolder `RealWorld_data`: same comparison on data from [MGS](https://www.mercatoelettrico.org/en/).


# Time series of surfaces
Folder `Test_domain2D`: **PACS**. 

- Subfolder `Artificial_data`: comparison of different predictors for forecasting synthetic time series of surfaces, generated as in [Ajroldi et Al.](https://arxiv.org/abs/2207.13656).

- Subfolder `RealWorld_data`: PPC KO is used to make predictions on Black Sea sea-level altitude. Forecasting of time serie, delta-1 lag, delta-2 lag for reconstructing the prediction.
