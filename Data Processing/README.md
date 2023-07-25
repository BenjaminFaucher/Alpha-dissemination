
The `readDataGISAID-CSSE.R` file contains the code that is used to retrieved the two table from GISAID and CSSE to obtain :

A table of the metadata from GISAID, where the missing collection date are imputed from distribution of delays.
A table of COVID-19 cases from brut CSSE table
A table of sequencing coverage per country and per day
The matrix of delay computed by country and by date of collection (used for days before 18 Dec 2020)
The `Collection_submission.R` file is used to compute the first submission date and the collection date of Alpha variant associated for each country, computed from GISAID metadata.

The `delays.R` file computes the matrix of delays used for days after 18 Dec 2020, aggregated over all countries outside of UK, using a sliding window of three days.
