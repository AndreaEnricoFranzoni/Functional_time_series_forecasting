# Forecasting of Time Series of Functional Data
The repo contains tests on forecasting of functional time series. 

Attention has been paid to [Kargin-Onatski algorithm](https://core.ac.uk/download/pdf/82625156.pdf) algorithm, and its implementation in the package `PPCKO`, reference [here](https://github.com/AndreaEnricoFranzoni/PPCforAutoregressiveOperator). Tests for its predictive power and its performance have been performed. 

Specifically, the PPC KO predictor has been compared to other predictors. Forecasting has been performed for unidmensional and bidimensional domain data, synthetic and real-world.

> ❗️ **N.B.:** the folder with the **PACS** flag contains the tests reported in the report for the Advanced Programming for Scientific Computing (a.k.a. PACS) at Politecnico di Milano, a.y. 2023/2024.

> ❗️ **N.B.:** the package `KePredictor` contains [here](https://github.com/AndreaEnricoFranzoni/KePredictor) a very simple, trivial, not efficient implementation of the Kernel Estimate (KE) and Kernel Estimate Improved (KEI) predictor, performing cv as in `PPCKO`, to compare coherently their predictive performances.



# Installation
Run the script `Install.R`. If problems arises in installing `PPCKO` and `KePredictor`, could be helpful looking , respectively, [here](https://github.com/AndreaEnricoFranzoni/PPCforAutoregressiveOperator) and [here](https://github.com/AndreaEnricoFranzoni/KePredictor).


# Tutorials
In the folders `Test_domain1D` and `Test_domain2D`, it is possible to find `tutorial_1d` and `tutorial_2d` for `PPCKO`.


# Computational performance
Folder `Test_CompTime`: **PACS**. 

Tests can be found, for the computational time for no-cv version and cv-version, parallel vs sequential.


# Time series of curves
Folder `Test_domain1D`: **PACS**. 

- Subfolder `Artificial_data`: reproduction of the Didericksen, Kokoszka and Zhang [experiment](https://ideas.repec.org/cgi-bin/refs.cgi) is conducted, for demostrating the goodness of PPC KO. 

- Subfolder `RealWorld_data`: the same comparison is conducted on data from [MGS](https://www.mercatoelettrico.org/en/), to make PPC KO facing a more challenging scenario of not-ad-hoc data.


# Time series of surfaces
Folder `Test_domain2D`: **PACS**. 

- Subfolder `Artificial_data`: comparison of predictors on 2D domain synthetic data, built as in [Ajroldi et Al.](https://arxiv.org/abs/2207.13656).

- Subfolder `RealWorld_data`: PPC KO is used to make predictions on Black Sea sea-level altitude. Forecasting of time serie, delta-1 lag, delta-2 lag for reconstructing the prediction.

# WIP
- Creating time series that do not satisfy PPC KO assumptions, to see how it behaves
- Applying PPC KO to "Campi Flegrei" data, as for BS data.

