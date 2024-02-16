# DenSpread_public
Code to accompany the paper "Human movement and environmental barriers shape the emergence of dengue”
This repository contains data, code and results from the paper. Users looking to refit the models will need to also download the environmental and human movement covariate matrices from the following Figshare repository: 10.6084/m9.figshare.22047905 

## Higher level folder structure

### Data folder
Contains cleaned and thresholded dengue datasets for Mexico and Brazil. See cited sources for original datasets. This folder also contains several intermediate datasets and population data that is used at various stages of the analysis to adjust covariates or extract summaries from the datasets

### Phylogenetic trees folder
Contains sky grid files for reconstructing the phylogenetic trees in Figure 4 (historical spread of dengue in Brazil)

### Predictions folder
Contains final model predictions used to plot each figure in the manuscript

### PaperCode folder
All code used in the analysis. Subdivided into the following folders:

#### Investigation and prep
Code used for cleaning the original data (C01 and C02), projecting covariate values into the future (F01), identifying candidate sources of dengue spread from occurrence data in Brazil (I01), Malaria Atlas Project (MAP) climate data processing (S01), MAP friction surface processing (S02) and processing of all movement variables (S03). Each of these scripts process the original data which can be accessed using the sources cited in the paper. These steps can be skipped by instead using the processed matrices in the fig share repository (10.6084/m9.figshare.22047905)

#### Model_fitting
Contains one script to process the dengue data into a list of invaded municipalities each year (01_Fit_Eval_data_prep.R, can be skipped by using “Spread_datasets.RData” and “Spread_datasets_Mexico.RData” directly), one script to fit the temporal survival models for both countries (02_Incidence_model.R), then two separate Markdown scripts that implement the geographic spread model fitting for Brazil (03_summarizeXGBBra.Rmd) and Mexico (04_summarizeXGBMex.Rmd). A separate Functions folder contains a variety of specific functions called during the spread model fitting.

#### Plots
Containes scripts to create the main figures of the paper
