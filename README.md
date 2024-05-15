# Impact of human behaviour and social determinants on the adherence to healthcare interventions in the post-acute COVID-19 society

This Github repository contains the code used to extend the existing social contact data of the COVID-19 pandemic to 28 EU/EEA Member States over the whole time period from 2020-2022 (also between data collections). Using the available CoMix data for Austria, Belgium, Croatia, Denmark, Estonia, Finland, France, Greece, Hungary, Italy, Lithuania, Netherlands, Poland, Portugal, Slovakia, Slovenia, Spain and Switzerland, social contact patterns observed during the pandemic were linked to pre-pandemic social contact patterns through a time-varying effect model(Level 1). The estimated changes in social contact patterns were then related to the implementation of NPIs for which data are available during the whole COVID pandemic period. Through the NPIs out of sample predictions of the change in contacts were generated and consequently the average number of contacts. For countries without CoMix contact data; Czech Republic, Bulgaria, Cyprus, Ireland, Latvia, Luxembourg, Malta, Romania, Sweden, Iceland, Germany the average number of contacts between age groups was generated using the out of sample predictions from the countries with CoMix data based on clustering and NPI similarity(Level 2).


## Code

The R folder contains all the scripts and functions. The following files/folders are available.

[level_1](https://github.com/EU-ECDC/COVID19SocialPatterns/tree/main/R/level_1) contains all the scripts and functions used for Level 1 of the modelling framework:

* [main_level1.R](https://github.com/EU-ECDC/COVID19SocialPatterns/blob/main/R/level_1/main_level1.R) runs the time-varying effect model (TVEM) for all countries with CoMix data using Stan’s No-U-Turn sampler variant of Hamiltonian Monte Carlo, saves the outputs and generates .

* [contact_data.R](https://github.com/EU-ECDC/COVID19SocialPatterns/blob/main/R/level_1/contact_data.R) contains a function which modifies the data into a form suitable for Stan for each country.

* [fit_level1.R](https://github.com/EU-ECDC/COVID19SocialPatterns/blob/main/R/level_1/fit_level1.R)

* [figures_level1.R](https://github.com/EU-ECDC/COVID19SocialPatterns/blob/main/R/level_1/figures_level1.R)

[level_2](https://github.com/EU-ECDC/COVID19SocialPatterns/tree/main/R/level_2) contains all the scripts and functions used for Level 2 of the modelling framework:

* [main_level2.R](https://github.com/EU-ECDC/COVID19SocialPatterns/blob/main/R/level_2/main_level2.R) runs the the multivariate regression analysis for each country using Stan’s No-U-Turn sampler variant of Hamiltonian Monte Carlo.

* [NPI_data.R](https://github.com/EU-ECDC/COVID19SocialPatterns/blob/main/R/level_2/NPI_data.R)

* [mca.R](https://github.com/EU-ECDC/COVID19SocialPatterns/blob/main/R/level_2/mca.R)

* [comix_waves_dates.R](https://github.com/EU-ECDC/COVID19SocialPatterns/blob/main/R/level_2/comix_waves_dates.R)

* [reg_dep_indep.R](https://github.com/EU-ECDC/COVID19SocialPatterns/blob/main/R/level_2/reg_dep_indep.R)

* [regression_data.R](https://github.com/EU-ECDC/COVID19SocialPatterns/blob/main/R/level_2/regression_data.R) modifies the TVEM posterior estimates and NPI data for the regression, into a form suitable for Stan.

* [noComix_C.R](https://github.com/EU-ECDC/COVID19SocialPatterns/blob/main/R/level_2/noComix_C.R)

* [figures_level2.R](https://github.com/EU-ECDC/COVID19SocialPatterns/blob/main/R/level_2/figures_level2.R)


[stan_models](https://github.com/EU-ECDC/COVID19SocialPatterns/tree/main/stan_models) contains all the [Stan](https://mc-stan.org/) model statements used:

* [m1_penalised_test.stan](https://github.com/EU-ECDC/COVID19SocialPatterns/blob/main/stan_models/m1_penalised_test.stan)

* [m2_penalised.stan](https://github.com/EU-ECDC/COVID19SocialPatterns/blob/main/stan_models/m2_penalised.stan) TVEM with pair specific random effects

* [MVR_out_of_sample.stan](https://github.com/EU-ECDC/COVID19SocialPatterns/blob/main/stan_models/MVR_out_of_sample.stan) contains the multivariate regression model

[data](https://github.com/EU-ECDC/COVID19SocialPatterns/tree/main/data) contains input data.

