The code for the Autochthonous Model A (AMA) is contained in AMA.R, with associated functions in AMA_functions.R, sim_epidemic_R.cpp, and sim_epidemic_R2.cpp.

The algorithm takes the following inputs:

- Weekly reproduction rate estimates stored in the weekly_R_estimate.csv file. The reproduction ratio is computed from mortality data published by European Surveillance System (TESSy) retrived from https://www.ecdc.europa.eu/en/publications-data/data-national-14-day-notification-rate-covid-19
- Daily rates of importation stored in lambda.firstlo.csv, lambda.firstmed.csv, and lambda.firstup.csv, output of IDM. 
The output_AMA.csv file contains the output data used for Fig. 4 of the main paper (i.e. date of first introductions and seeding time).
