# Impact of human behaviour and social determinants on the adherence to healthcare interventions in the post-acute COVID-19 society

This Github repository contains the code used to extend the existing social contact data of the COVID-19 pandemic to 28 EU/EEA Member States over the whole time period from 2020-2022 (also between data collections). Using the available CoMix data for Austria, Belgium, Croatia, Denmark, Estonia, Finland, France, Greece, Hungary, Italy, Lithuania, Netherlands, Poland, Portugal, Slovakia, Slovenia, Spain and Switzerland, social contact patterns observed during the pandemic were linked to pre-pandemic social contact patterns through a time-varying effect model(Level 1). The estimated changes in social contact patterns were then related to the implementation of Non-Pharmaceutical interventions (NPIs) for which data are available during the whole COVID pandemic period. Through the NPIs out of sample predictions of the change in contacts were generated and consequently the average number of contacts. For countries without CoMix contact data; Czech Republic, Bulgaria, Cyprus, Ireland, Latvia, Luxembourg, Malta, Romania, Sweden, Iceland, Germany the average number of contacts between age groups was generated using the out of sample predictions from the countries with CoMix data based on clustering and NPI similarity(Level 2).

### Relate pre-pandemic contact patterns to pandemic contact patterns for EU/EEA Member States with data for both time periods

* Consider 4 age groups: [0,18), [18,45), [45,65) and 65+
* $\mathbf{Y_{ijw}}$: average number of contacts for individuals in age group $j$ with individuals of age group $i$ at time $w$, where $w$ denotes the waves of the CoMix survey.
  $\mathbf{Z_{ij}^k}$: pre-pandemic average number of contacts for individuals in age group $j$ with individuals of age group $i$ at location $k$ where $k$ corresponds to home, work, school and other places.
* \begin{align}
  \begin{split}
  Y_{ijw} & \sim \mathcal{N}(\mu_{ijt},\sigma^2)\\
  \mu_{ijw} & = \eta_{ij}+\beta_1(w)Z^{home}_{ij}+\beta_2(w)Z^{work}_{ij}+\beta_3(w)Z^{school}_{ij}+\beta_4(w)Z^{other}_{ij}\qquad\textbf{[Level 1]}
  \end{split}
  \end{align}
  where $\eta_{ij}$ denotes pair specific random effects, $\beta_p(w)$ ($p=1,2,3,4$) are \textbf{smooth coefficient functions of time} $w$ (time-varying effect model (TVEM), Lajot et al. (2023)).

* \begin{equation}
        \beta_p(w)=\sum_{j=1}^{K}\alpha_{pj}B_j(w)
        \end{equation}
        where $K$ is the number of B-spline basis functions, $a_{pj}$ are the coefficients associated with each basis function and $B_j(w)$ is a B-spline basis function.

* Random-walk prior on the coefficients $a_j$ to reduce the risk of a prior choice of a large number of knots resulting in overfitting
  \begin{equation}
  a_{p1} \sim \mathcal{N}(0,1)
  \end{equation}
   \begin{equation}
   a_{pj} \sim \mathcal{N}(a_{p,j-1},\tau_p)
   \end{equation}
   
### Fill in gaps in countries with pandemic data with incomplete time coverage by relating changes in social contact patterns to non-pharmaceutical interventions

* $\widehat{\beta_p(w)}$: time-varying expected change in the contacts between two age groups during the pandemic resulting from a change of one contact between those age groups at different locations before the pandemic (Level 1).
* NPIs related to physical distancing measures are highly correlated $\rightarrow$ Multiple correspondence analysis (MCA) - use first 5 dimensions.
* \begin{align}
   \begin{split}
    \hat{\mathbf{\beta_t}} & \sim \mathrm{MVN}(\mathbf{m_t},\mathbf{\Sigma})\\
    \mathbf{m_t} & = \mathbf{\delta_0}+\mathbf{\delta_1} Dim1_t+\mathbf{\delta_2} Dim3_t+\mathbf{\delta_3} Dim3_t+\mathbf{\delta_4} Dim4_t+\mathbf{\delta_5} Dim5_t\qquad\textbf{[Level 2]}
    \end{split}
   \end{align}
  where $\hat{\mathbf{\beta_t}}=(\hat{\beta_{1t}}, \hat{\beta_{2t}}, \hat{\beta_{3t}}, \hat{\beta_{4t}})$.
* Assumption: adopted measures had the same effect on the change in contacts across time $\rightarrow$ use the estimated $\delta_q$ to generate out-of-sample predictions of the change in the number of contacts between age groups $\mathbf{\beta_t}$ in sampling points where there are available measurements of NPIs, but no contact data.

### Infer pandemic contact patterns for EU/EEA Member States with pre-pandemic data (but not pandemic data)

* Rely on estimates for the countries with CoMix data
* For every target country without pandemic data (no CoMix country) assign a “neighbouring” country (CoMix country) with pandemic data.
* Summarise NPIs related to physical distancing by the first 5 factor dimensions of an MCA for each country
* Compute the Euclidean distances between the factor dimensions matrices for all possible pairs of countries using the Frobenius norm and perform hierarchical clustering based on these distances
* Identify a CoMix country within the same cluster as the target country and confirm that they also belong in the same cluster based on their socio-demographic characteristics according to Prem et al.(2017)
* Assume that countries in the same cluster responded in the same way to the implemented NPIs under the physical distancing category, as expressed by their first 5 factor dimensions of the MCA on the NPIs.
* Sample the vector of changes in contacts for different locations for a target country without CoMix $\mathbf{\beta_t^{nc}}=(\beta_{1t}^{nc}, \beta_{2t}^{nc}, \beta_{3t}^{nc}, \beta_{4t}^{nc})$, from a multivariate normal with mean $\hat{\mathbf{\delta_0}}^c+\hat{\mathbf{\delta_1}}^c Dim1_t^{nc}+\hat{\mathbf{\delta_2}}^c Dim2_t^{nc}+\hat{\mathbf{\delta_3}}^c Dim3_t^{nc}+\hat{\mathbf{\delta_4}}^c Dim4_t^{nc}+\hat{\mathbf{\delta_5}}^c Dim5_t^{nc}$ and covariance matrix $\hat{\boldsymbol{\Sigma}}^c$. The estimated marginal effects of the factor dimensions for a CoMix country are denoted by $\hat{\mathbf{\delta_q}}^c$, $q=0,1,2,3,4,5$, $\hat{\boldsymbol{\Sigma}}^c$ is the estimated covariance matrix of a CoMix country and $Dim1_t^{nc}, Dim2_t^{nc}, Dim3_t^{nc}, Dim4_t^{nc}, Dim5_t^{nc}$ represent the first five factor dimensions of an MCA on all the NPIs under the physical distancing category for the target country without CoMix data.
* Given the generated changes in contacts, transform them into the pandemic contacts considering a linear combination of the pre-pandemic contacts in different locations (home, work, school and other places) as follows:
  \begin{equation}
    C_{ijw}^{nc}  = \beta_1^{nc}(w)Z^{home}_{ij}+\beta_2^{nc}(w)Z^{work}_{ij}+\beta_3^{nc}(w)Z^{school}_{ij}+\beta_4^{nc}(w)Z^{other}_{ij}
  \end{equation}
  where $Z_{ij}^k$ denotes the pre-pandemic average number of contacts for individuals in age group $j$ with individuals of age group $i$ at location $k$ where $k$ corresponds to home, work, school and other places.

## Code Structure

### Main Script
  
* [main.R](R/main.R) - The main script that runs the entire pipeline in three steps:
 1. Creates an EU map with contact data coverage visualization and generates POLYMOD and CoMix heatmaps for each country
 2. Runs the Level 1 Time-Varying Effect Models (TVEM) analysis for each country with CoMix data using Stan’s No-U-Turn sampler variant of Hamiltonian Monte Carlo. Saves outputs and generated figures of the estimated coefficient functions and posterior predictive checks.
 3. Runs the Level 2 Multivariate Regression (MVR) analysis and performs clustering/matching for non-CoMix countries

The R folder contains all the scripts and functions. The following files/folders are available.

### Level 1: Time-Varying Effect Model

[level_1](R/level_1) contains all the scripts and functions used for Level 1 of the modelling framework:

* [contact_data_coverage.R](R/level_1/contact_data_coverage.R) - Functions to visualize contact data coverage:
  * `create_eu_eea_contact_map()` - Generates a map of EU/EEA countries colored by data availability
  * `POLYMOD_heatmaps()` - Creates heatmap visualizations of POLYMOD contact matrices
  * `CoMix_heatmaps_real_time()` - Creates heatmaps of CoMix contact matrices over time
  
* [contact_data.R](R/level_1/contact_data.R) - Functions to load and process POLYMOD and CoMix data and modify the data into a form suitable for Stan, including:
  * `polymod_setup()` - Creates country-specific contact matrices by aggregating raw contact data
  * `contact_data()` - Loads POLYMOD and CoMix contact data for a specific country
  * `data_to_stan_list()` - Converts contact data to a suitable format for Stan
  
* [fit_level1.R](R/level_1/fit_level1.R) - Functions for fitting a model defined in the Stan modeling language and loading fitted model results:
  * `nuts_fit()` - Fits a Stan model using the NUTS sampler
  * `load_fit()` - Loads a previously fitted model from an RData file
  
* [figures_level1.R](R/level_1/figures_level1.R) - Functions for generating diagnostic plots and visualizations:
  * `diagnostics()` - Generates a comprehensive set of diagnostic plots to assess the convergence and quality of the fitted model
  * `fitted_beta()` - Creates plots of the estimated coefficient functions over time
  * `fitted_contacts()` - Visualizes the predicted number of contacts between age groups over time
  
* [main_level1.R](R/level_1/main_level1.R) - The script that implements Level 1 of the modeling framework, fitting TVEMs to estimate the temporal changes in social contact patterns

### Level 2: Multivariate Regression Analysis 

[level_2](R/level_2) contains all the scripts and functions used for Level 2 of the modelling framework:

* [NPI_data.R](R/level_2/NPI_data.R) - Function to load Non-Pharmaceutical Interventions (NPI) data for each country

* [comix_waves_dates.R](R/level_2/comix_waves_dates.R) - Function to extract and process extract relevant date information to align CoMix data (recorded per survey wave) with NPI data (recorded with daily granularity)

* [regression_data.R](R/level_2/regression_data.R) - Functions for preparing data for regression modeling:
  * `fitted_beta_coefficients()` - Extracts TVEM posterior estimates of the coefficient functions(Level 1) and their credible intervals from a fitted Stan model for a specific country and percentile and uses linear interpolation between waves to translate the estimates from CoMix survey waves time into real time
  * `fitted_interc()` - Extracts intercept values
  * `mca_analysis()` - Performs Multiple Correspondence Analysis on NPI data
  * `mca()` - Helper function to extract MCA object for a country
  * `mca_factors()` - Extracts MCA factors for regression analysis
  * `percentage_var()` - Calculates percentage of variance explained by MCA dimensions
  * `MCA_plot()` - Creates visualization plots for MCA results including scree plot and variable contribution plots for the first three dimensions
  * `estBeta_mcaNPI()` - Visualizes the relationship between estimated changes in contacts (represented by TVEM posterior estimates of the coefficient functions) and the underlying factors identified in NPI data (through MCA) in a single plot
  * `reg_data_to_stan_list()` - Prepares the TVEM posterior estimates and NPI data for the regression, into a form suitable for Stan

* [fit_level2.R](R/level_2/fit_level2.R) - Functions for fitting multivariate regression models:
  * `nuts_reg()` - Fits a multivariate regression model using Stan
  * `load_reg_fit()` - Loads a previously fitted regression model

* [figures_level2.R](R/level_2/figures_level2.R) - Functions for creating visualizations of Level 2 results:
  * `diagnostics()` - Function that generates diagnostic plots to assess the convergence and quality of the multivariate regression model
  * `fitted_median_beta()` - Visualizes predicted beta coefficients
  * `pred_CM()` - Predicts and plots contact matrices based on the posterior median from level 1
  * `pred_CM_full()` - Creates plots with prediction uncertainty using all percentiles
  
* [save_matrices.R](R/level_2/save_matrices.R) - Functions for saving and visualizing contact matrices:
  * `save_contact_matrices()` - Saves predicted contact matrices to CSV files
  * `create_contact_matrix_heatmaps()` - Creates heatmap visualizations of contact matrices

* [clustering.R](R/level_2/clustering.R) - Functions for clustering countries based on NPI patterns:
  * `find_common_dates()` - Identifies common date ranges across countries
  * `align_mca_factors()` - Aligns MCA factors to common dates for comparison
  * `cluster_countries()` - Performs hierarchical clustering using multiple methods

* [matching.R](R/level_2/matching.R) - Functions for matching non-CoMix countries to similar CoMix countries:
  * `match_countries()` - Matches countries based on NPI pattern similarity
  * `visualize_matches()` - Creates visualizations of country matching results

* [noComix_C.R](R/level_2/noComix_C.R) - Functions for generating contact matrices for non-CoMix countries:
  * `generate_CM_predictions()` - Generates the average number of contacts between age groups in a country without CoMix data based on the estimated changes in contacts from a country with CoMix data and NPI data of the country without CoMix data. Saves generated contact matrices to CSV files.
  * `plot_CM_from_csv()` - Creates visualizations of predicted contact patterns

* [main_level2.R](R/level_2/main_level2.R) - The script that implements Level 2 of the modeling framework, fitting multivariate regression models to link NPIs to the time-varying contact coefficients. Generates plots of the predicted change in contacts for each country with CoMix data and the generated number of contacts between age groups both for countries with and without CoMix data.

### Stan Models

[stan_models](stan_models/) contains all the [Stan](https://mc-stan.org/) model statements used:

* [m1_penalised_test.stan](stan_models/m1_penalised_test.stan) time-varying effect model

* [m2_penalised.stan](stan_models/m2_penalised.stan) time-varying effect model with pair specific random effects

* [MVR_out_of_sample.stan](stan_models/MVR_out_of_sample.stan) multivariate regression model

### Data

The [data](data/) directory contains the input data organized into subdirectories:
  
* [POLYMOD/](data/POLYMOD/) - Pre-pandemic contact matrices
* [CoMix/](data/CoMix/) - Pandemic contact survey data
* [comix_dates/](data/comix_dates/) - Date information for CoMix survey waves
* [NPI/](data/NPI/) - Non-Pharmaceutical Intervention data

## Usage

To run the complete analysis pipeline, execute the main script:
  
```R
source("R/main.R")
```
This will:

1. Generate data coverage visualizations and contact matrix heatmaps
2. Run the Level 1 TVEM analysis to estimate changes in contact patterns
3. Run the Level 2 MVR analysis to relate estimated changes to NPIs
4. Perform clustering and matching to extend predictions to non-CoMix countries

Individual components can also be run separately by sourcing the respective scripts.

## Authors
* Anastasia Chatzilena: <a.chatzilena@bristol.ac.uk>
* Ewan Colman: <ewan.colman@bristol.ac.uk>
* Nikolaos Demiris: <nikos@aueb.gr>
* Ellen Brooks-Pollock: <ellen.brooks-pollock@bristol.ac.uk>
* Leon Danon: <l.danon@bristol.ac.uk>