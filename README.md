# Impact of human behaviour and social determinants on the adherence to healthcare interventions in the post-acute COVID-19 society

This Github repository contains the code used to extend the existing social contact data of the COVID-19 pandemic to 28 EU/EEA Member States over the whole time period from 2020-2022 (also between data collections). Using the available CoMix data for Austria, Belgium, Croatia, Denmark, Estonia, Finland, France, Greece, Hungary, Italy, Lithuania, Netherlands, Poland, Portugal, Slovakia, Slovenia, Spain and Switzerland, social contact patterns observed during the pandemic were linked to pre-pandemic social contact patterns through a time-varying effect model(Level 1). The estimated changes in social contact patterns were then related to the implementation of Non-Pharmaceutical interventions (NPIs) for which data are available during the whole COVID pandemic period. Through the NPIs out of sample predictions of the change in contacts were generated and consequently the average number of contacts. For countries without CoMix contact data; Czech Republic, Bulgaria, Cyprus, Ireland, Latvia, Luxembourg, Malta, Romania, Sweden, Iceland, Germany the average number of contacts between age groups was generated using the out of sample predictions from the countries with CoMix data based on clustering and NPI similarity(Level 2).


## Code

The R folder contains all the scripts and functions. The following files/folders are available.

[level_1](https://github.com/EU-ECDC/COVID19SocialPatterns/tree/main/R/level_1) contains all the scripts and functions used for Level 1 of the modelling framework:

* [main_level1.R](https://github.com/EU-ECDC/COVID19SocialPatterns/blob/main/R/level_1/main_level1.R) runs the time-varying effect model (TVEM) for each country with CoMix data using Stan’s No-U-Turn sampler variant of Hamiltonian Monte Carlo and saves outputs and generated figures of the estimated coefficient functions and posterior predictive checks.

* [contact_data.R](https://github.com/EU-ECDC/COVID19SocialPatterns/blob/main/R/level_1/contact_data.R) contains functions to load the POLYMOD and CoMix data and modify the data into a form suitable for Stan.

* [fit_level1.R](https://github.com/EU-ECDC/COVID19SocialPatterns/blob/main/R/level_1/fit_level1.R) contains functions to fit a model defined in the Stan modeling language and return the fitted result as an instance of stanfit and load fitted model results. 

* [figures_level1.R](https://github.com/EU-ECDC/COVID19SocialPatterns/blob/main/R/level_1/figures_level1.R) contains functions to provide a comprehensive set of diagnostic plots to assess the convergence and quality of the fitted model, to visualise the estimated coefficient functions and the predicted number of contacts between age groups.

[level_2](https://github.com/EU-ECDC/COVID19SocialPatterns/tree/main/R/level_2) contains all the scripts and functions used for Level 2 of the modelling framework:

* [main_level2.R](https://github.com/EU-ECDC/COVID19SocialPatterns/blob/main/R/level_2/main_level2.R) generates plots of the Multiple correspondence analysis on the NPIs, visual representations of the estimated change in contacts and the first 5 factor dimensions of the MCA on all the NPIs and runs the multivariate regression analysis for each CoMix country relating the estimated changes in social contact patterns to the implementation of NPIs. Generates plots of the predicted change in contacts for each country with CoMix data and the generated number of contacts between age groups both for countries with and without CoMix data.

* [NPI_data.R](https://github.com/EU-ECDC/COVID19SocialPatterns/blob/main/R/level_2/NPI_data.R) function to load the NPI data

* [mca.R](https://github.com/EU-ECDC/COVID19SocialPatterns/blob/main/R/level_2/mca.R) contains functions to perform the MCA analysis on NPI data for a given country and generate a combined plot to visualize the results of the MCA(scree plot and variable contribution plots for the first three dimensions)

* [comix_waves_dates.R](https://github.com/EU-ECDC/COVID19SocialPatterns/blob/main/R/level_2/comix_waves_dates.R) contains a function to extract relevant date information to align CoMix data (recorded per survey wave) with NPI data (recorded with daily granularity)

* [regression_data.R](https://github.com/EU-ECDC/COVID19SocialPatterns/blob/main/R/level_2/regression_data.R) contains functions to extract the TVEM posterior estimates of the coefficient functions(Level 1) and their credible intervals from a fitted Stan model for a specific country and percentile, translate the estimates from CoMix survey waves time into real time using linear interpolation between waves, to perform the MCA analysis on NPI data and modify the TVEM posterior estimates and NPI data for the regression, into a form suitable for Stan.

* [reg_dep_indep.R](https://github.com/EU-ECDC/COVID19SocialPatterns/blob/main/R/level_2/reg_dep_indep.R) contains a function to visualize the relationship between estimated changes in contacts (represented by TVEM posterior estimates of the coefficient functions) and the underlying factors identified in NPI data (through MCA) in a single plot.

* [noComix_C.R](https://github.com/EU-ECDC/COVID19SocialPatterns/blob/main/R/level_2/noComix_C.R)

* [figures_level2.R](https://github.com/EU-ECDC/COVID19SocialPatterns/blob/main/R/level_2/figures_level2.R)


[stan_models](https://github.com/EU-ECDC/COVID19SocialPatterns/tree/main/stan_models) contains all the [Stan](https://mc-stan.org/) model statements used:

* [m1_penalised_test.stan](https://github.com/EU-ECDC/COVID19SocialPatterns/blob/main/stan_models/m1_penalised_test.stan) TVEM

* [m2_penalised.stan](https://github.com/EU-ECDC/COVID19SocialPatterns/blob/main/stan_models/m2_penalised.stan) TVEM with pair specific random effects

* [MVR_out_of_sample.stan](https://github.com/EU-ECDC/COVID19SocialPatterns/blob/main/stan_models/MVR_out_of_sample.stan) contains the multivariate regression model

[data](https://github.com/EU-ECDC/COVID19SocialPatterns/tree/main/data) contains input data.

