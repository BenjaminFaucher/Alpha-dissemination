
The readDataGISAID-CSSE.R file contains the code used to retrieve two tables from GISAID and CSSE, to obtain the following:

A table of metadata from GISAID, where the missing collection dates are imputed using the distribution of delays.
A table of COVID-19 cases from the raw CSSE table.
A table of sequencing coverage per country and per day.
The matrix of delays computed by country and by the date of collection (used for days before 18 Dec 2020).
The Collection_submission.R file is used to compute the first submission date and the collection date of the Alpha variant associated with each country. This information is computed from GISAID metadata.

The delays.R file computes the matrix of delays used for days after 18 Dec 2020, aggregated over all countries outside of the UK. It uses a sliding window of three days for the computation.
