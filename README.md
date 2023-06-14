# README

This repository contains the code and dta used in the analyses presented in the paper ###

## Data Processing

The `Data Processing` folder contains files about processing data from GISAID and CSSE. 

Submitted sequences of human conronavirus from 15 Aug 2020 to 1 Jun 2021 as downloaded the 2 Jun 2021 are stored in "XXX". As collection date are missing, we impute them from distribution of delays to give the `GISAID.csv` file. The code to do so is written in `ReadDataGISAID-CSSE.R`

Data of Covid cases are retrieved from data repository hosted by the Center for Systems Science and Engineering at Johns Hopkins University (CSSE) and contained in `DataCSSE.csv`.

## IDM

The `IDM` concerns the International Dissemination Model. The code is provided in the `IDM.R` and `IDM_functions.R` files. To get the analysis of the paper, they must be used with the input data stored in `Input_IDM.RData` and `delays.RData` files.
The main output of the model are stored in `res_baseline.RData`, `sum_baseline.RData` and `sumcount_baseline.RData`.

## ATMA



