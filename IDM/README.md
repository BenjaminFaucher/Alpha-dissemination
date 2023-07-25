The code for to run the IDM is provided in the `IDM.R` and `IDM_functions.R` files. To replicate the analyses in the paper, these files must be used with some
inputs data :
- The matrix of delay computed by country and by date of collection (used for days before 18 Dec 2020), computed with `readDataGISAID-CSSE.R`.
- The matrix of delay aggregated over all countries outside of UK, using a sliding window of three days (used for days after 18 Dec 2020), computed with `delays.R`.
- Data of first submission date and the collection date computed from `Collection_submission.R`.
- Table of sequencing coverage per country and per day computed by `readDataGISAID-CSSE.R`.
- Data about international travel flux from IATA.Due to data privacy policy, we provide here a dummy file `travel.london.fake`.
- `epid.UK.fake`,`r.VOC.smooth.fake` and `R.VOC.smooth.fake` are files that are not used anymore in the current version of the code but that need to be provided. We use dummy files.

The code needs the R package importFromUK (3.2), stored in `importFromUK_3.2.tar.gz`

The `output.csv` file contains the key output of the model : estimations of first collection date, first submission date and introduction date used in Fig. 2 and Fig. 3 of the main paper.
