# Forecasting of Time Series of Functional Data
The repo contains tests for the goodness of one-step ahead prediction of functional time series (with unidimensional and bidimensional domain) using the [Kargin-Onatski algorithm](https://core.ac.uk/download/pdf/82625156.pdf), implemented [here](https://github.com/AndreaEnricoFranzoni/PPCforAutoregressiveOperator).


# Installation
After having pulled the repo, run the script `settings.R`, after having set `dir_w` to your working directory (in this way every needed package, function and/or dataset will be available).


# Toy example for unidimensional domain data
Script `Ex_1d.R`. Reconstruction of [Didericksen, Kokoszka & Zhang](https://www.semanticscholar.org/paper/Empirical-properties-of-forecasts-with-the-model-Didericksen-Kokoszka/c1fae9f292c2b42beffe4e4146a2bf9ca005f060) experiment: comparison between different algorithm to compute one-step ahead prediction of functional time series (KO, Estimated Kernel (KE), Estimated Kernel Improved (KEI), Naive Predictor (NP), Mean Predictor (MP) and Exact Predictor(EP)) on ad-hoc built in data. Functional AutoRegressive process of order 1 (FAP(1)) are created using different kernels (Gaussian, Identinty, Sloping Plane t, Sloping Plane s) and noises as described in the paper.


# Real example for unidimensional domain data

# Toy example for bidimensional domain data

# Real example for bidimensional domain data
