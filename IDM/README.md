Source codes for the International Dissemination Model are provided in the `IDM.R` and `IDM_functions.R` files. 

These codes require the following input data obtained as output of the Data Processing scripts:
- The matrix of delay computed by country and by date of collection (used before 18 Dec 2020).
- The matrix of delay for Alpha aggregated over all countries outside of the UK (used after 18 Dec 2020).
- First submission date and associated collection for each country.
- Table of sequencing coverage per country and per day.

Notes:

Data about international travel are not publicly available owing to stringent licensing agreements. A dummy travel matrix is stored in inputs2.RData to make it possible to run the code.

The code needs the R package importFromUK (3.2), stored in `importFromUK_3.2.tar.gz`

The `output.csv` file contains the key output of the model: estimations of first collection date, first submission date, and introduction dates plotted in Fig. 2 and Fig. 3 of the main paper.
