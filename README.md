# README

This repository contains the code and dta used in the analyses presented in the paper ###

## Data Procession

The `Data Processing` folder contains files about processing data from GISAID and CSSE. 

Submitted sequences of human conronavirus from 15 Aug 2020 to 1 Jun 2021 as downloaded the 2 Jun 2021 are stored in "XXX". As collection date are missing, we impute them from distribution of delays to give the `GISAID.csv` file. The code to do so is written in `ReadDataGISAID-CSSE.R`

Data of Covid cases are retrieved from data repository hosted by the Center for Systems Science and Engineering at Johns Hopkins University (CSSE) and contained in `DataCSSE.csv`.



