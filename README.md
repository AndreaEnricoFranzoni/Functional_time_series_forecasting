# Forecasting of Time Series of Functional Data
The repo contains tests on forecasting of functional time series. 

Attention has been paid to [Kargin-Onatski algorithm](https://core.ac.uk/download/pdf/82625156.pdf) algorithm, and its implementation in the package `PPCKO`, reference [here](https://github.com/AndreaEnricoFranzoni/PPCforAutoregressiveOperator). Tests for its predictive power and its performance have been performed. 

Specifically, the PPC KO predictor has been compared to other predictors. Forecasting has been performed for unidmensional and bidimensional domain data, synthetic and real-world.

> ❗️ **N.B.:** the folder with the **PACS** flag contains the tests reported in the report for the Advanced Programming for Scientific Computing (a.k.a. PACS) at Politecnico di Milano, a.y. 2023/2024.

> ❗️ **N.B.:** the package `KePredictor` contains [here](https://github.com/AndreaEnricoFranzoni/KePredictor) a very simple, trivial, not efficient implementation of the Kernel Estimate (KE) and Kernel Estimate Improved (KEI) predictor, performing cv as in `PPCKO`, to compare coherently their predictive performances.



# Installation
Run the script `Install.R`. If problems arises in installing `PPCKO` and `KePredictor`, could be helpful looking , respectively, [here](https://github.com/AndreaEnricoFranzoni/PPCforAutoregressiveOperator) and [here](https://github.com/AndreaEnricoFranzoni/KePredictor).


# Tutorials
In the folders


# Unidimensional domain data: built-in data
Script `Ex_1d.R`. 

Reconstruction of [Didericksen, Kokoszka & Zhang](https://www.semanticscholar.org/paper/Empirical-properties-of-forecasts-with-the-model-Didericksen-Kokoszka/c1fae9f292c2b42beffe4e4146a2bf9ca005f060) experiment: comparison between different algorithm to compute one-step ahead prediction of functional time series (KO, Estimated Kernel (KE), Estimated Kernel Improved (KEI), Naive Predictor (NP), Mean Predictor (MP) and Exact Predictor(EP)) on ad-hoc built in data. Functional AutoRegressive process of order 1 (FAP(1)) are created using different kernels (Gaussian, Identinty, Sloping Plane t, Sloping Plane s) and noises as described in the paper.


# Unidimensional domain data: real-world data

# Bidimensional domain data: built-in data

# Bidimensional domain data: real-world data
