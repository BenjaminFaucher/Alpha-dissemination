# README

This repository contains the code and data used in the analyses presented in the paper ###.


## Data Processing

The data on Covid cases is retrieved from the data repository hosted by the Center for Systems Science and Engineering at Johns Hopkins University (CSSE) and is contained in the `DataCSSE.csv` file.

The Data Processing folder contains files related to processing data from GISAID and CSSE.

The submitted sequences of human coronaviruses from August 15, 2020, to June 1, 2021, as downloaded on June 2, 2021, are stored in the "XXX" file. Since the collection dates are missing, we impute them using the distribution of delays to generate the `GISAID.csv` file. The code to perform this imputation is written in the `ReadDataGISAID-CSSE.R` file. This code also computes the screening fraction and the number of Covid cases.


## IDM

The `IDM` folder pertains to the International Dissemination Model. The code for this model is provided in the `IDM.R` and `IDM_functions.R` files. To replicate the analyses in the paper, these files must be used with the input data stored in the `Input_IDM.RData` and `delays.RData` files. The main outputs of the model are stored in the `res_baseline.RData`, `sum_baseline.RData`, and `sumcount_baseline.RData` files.

The code needs the R package importFromUK (3.2), stored in `importFromUK_3.2.tar.gz`


## AM A

The files required for the Autochthonous Model A (AMA) are stored in the `AMA` folder. The code for the model can be found in the `AMA.R` file, with associated functions in the `AMA_functions.R`, `sim_epidemic_R.cpp`, and `sim_epidemic_R2.cpp` files. The algorithm takes as input the weekly reproduction rate estimates stored in the `weekly_R_estimate.csv` file, as well as the daily rates of importation estimated by the IDM, which are stored in the `lambda.firstlo.csv`, `lambda.firstmed.csv`, and `lambda.firstup.csv` files.

## AM B

To have more information on AM B code, please follow the link https://github.com/EPIcx-lab/COVID-19/tree/master/Adherence_and_sustainability

