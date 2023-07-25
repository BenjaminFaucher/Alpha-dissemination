The files required for the Autochthonous Model A (AMA) are stored in the `AMA` folder. The code for the model can be found in the `AMA.R` file, with associated functions in the `AMA_functions.R`, `sim_epidemic_R.cpp`, and `sim_epidemic_R2.cpp` files. 
The algorithm takes as input :
- the weekly reproduction rate estimates stored in the `weekly_R_estimate.csv` file.
- the daily rates of importation computed by the IDM, which are stored in the `lambda.firstlo.csv`, `lambda.firstmed.csv`, and `lambda.firstup.csv` files. They are obtain from IDM.

The `output_AMA.csv` file contains the key outputs used for fig 4 in the main paper : date of first introductions and seeding time.
