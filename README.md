# README

This repository contains the code and data used in the analyses presented in the paper Drivers and impact of the early silent invasion of SARS-CoV-2 Alpha Benjamin Faucher, Chiara E. Sabbatini, Peter Czuppon, Moritz U.G. Kraemer, Philippe Lemey, Vittoria Colizza, Francois Blanquart, Pierre-Yves Boëlle, Chiara Poletto

## Data Processing
The findings of this study are based on metadata associated with a total of 1,735,675 sequences available on GISAID and submitted between 15 Aug 2020 and 1 Jun 2021 included and downloaded on 2 Jun 2021 via gisaid.org (GISAID: EPI_SET_230724tv). To view the contributors of each sequence associated with the metadata we used, visit https://doi.org/10.55876/gis8.230724tv. Some collection date are missing.

We use daily number of COVID-19 cases by country from the COVID-19 data repository hosted by the Center for Systems Science and Engineering at Johns Hopkins University (CSSE).

The `readDataGISAID-CSSE.R` file contains the code that is used to retrieved the two table from GISAID and CSSE to obtain :
- A table of the metadata from GISAID, where the missing collection date are imputed from distribution of delays.
- A table of COVID-19 cases from brut CSSE table
- A table of sequencing coverage per country and per day
- The matrix of delay computed by country and by date of collection (used for days before 18 Dec 2020)

The `Collection_submission.R` file is used to compute the first submission date and the collection date of Alpha variant associated for each country, computed from GISAID metadata.

The `delays.R` file computes the matrix of delays used for days after 18 Dec 2020, aggregated over all countries outside of UK, using a sliding window of three days.

Due to data privacy policy, we do not provide input and output data for this code.

## IDM

The `IDM` folder pertains to the International Dissemination Model. The code for this model is provided in the `IDM.R` and `IDM_functions.R` files. To replicate the analyses in the paper, these files must be used with the input data stored in the `Inputs.RData`.
The `Inputs.RData` contains :
- The matrix of delay computed by country and by date of collection (used for days before 18 Dec 2020), computed with `readDataGISAID-CSSE.R`.
- The matrix of delay aggregated over all countries outside of UK, using a sliding window of three days (used for days after 18 Dec 2020), computed with `delays.R`.
- `first.VOC.fake` contains an example data of first submission date and the collection date computed from `Collection_submission.R`. Due to data privacy policy, we provide here a dummy file.
- `screening.fake` contains the table of sequencing coverage per country and per day computed by `readDataGISAID-CSSE.R`. Due to data privacy policy, we provide here a dummy file.
- `travel.london.fake` contains data about international travel flux.Due to data privacy policy, we provide here a dummy file.
- `epid.UK.fake`,`r.VOC.smooth.fake` and `R.VOC.smooth.fake` are files that are not used anymore in the current version of the code but that need to be provided. We use dummy files.

The code needs the R package importFromUK (3.2), stored in `importFromUK_3.2.tar.gz`


## AM A

The files required for the Autochthonous Model A (AMA) are stored in the `AMA` folder. The code for the model can be found in the `AMA.R` file, with associated functions in the `AMA_functions.R`, `sim_epidemic_R.cpp`, and `sim_epidemic_R2.cpp` files. 
The algorithm takes as input :
- the weekly reproduction rate estimates stored in the `weekly_R_estimate.csv` file.
- the daily rates of importation computed by the IDM, which are stored in the `lambda.firstlo.csv`, `lambda.firstmed.csv`, and `lambda.firstup.csv` files. We provide dummy files here.


## AM B

To have more information on AM B code, please follow the link https://github.com/EPIcx-lab/COVID-19/tree/master/Adherence_and_sustainability

