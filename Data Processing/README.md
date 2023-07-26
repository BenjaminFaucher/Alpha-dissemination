
The input_preparation.R file contains the code used to process GISAID and CSSE data and generate the following tables:

- A table of metadata from GISAID, where the missing collection dates are imputed using the distribution of delays.
- A table of COVID-19 cases from CSSE data.
- A table of sequencing coverage by country and day.
- The matrix of delays computed by country and by the date of collection (used for days before 18 Dec 2020).
- The first submission date and the associated collection date of an Alpha sequence for each country.
- The matrix of delays used for days after 18 Dec 2020, specific for Alpha and aggregated over all countries outside of the UK

All output data are stored on an RData file to be used as input for the IDM code.
