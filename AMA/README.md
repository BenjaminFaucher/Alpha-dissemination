The required files for the Autochthonous Model A (AMA) are stored in the AMA folder. The model's code can be found in the AMA.R file, with associated functions in the AMA_functions.R, sim_epidemic_R.cpp, and sim_epidemic_R2.cpp files.

The algorithm takes the following inputs:

- Weekly reproduction rate estimates stored in the weekly_R_estimate.csv file.
- Daily rates of importation computed by the International Dissemination Model (IDM), which are stored in the lambda.firstlo.csv, lambda.firstmed.csv, and lambda.firstup.csv files. These files are obtained from IDM.
The output_AMA.csv file contains the key outputs used for figure 4 in the main paper, which includes the date of first introductions and seeding time.
