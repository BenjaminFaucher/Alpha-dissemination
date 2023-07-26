This repository contains the code and data used in the analyses presented in the paper "Drivers and Impact of the Early Silent Invasion of SARS-CoV-2 Alpha" by Benjamin Faucher, Chiara E. Sabbatini, Peter Czuppon, Moritz U.G. Kraemer, Philippe Lemey, Vittoria Colizza, Francois Blanquart, Pierre-Yves Boëlle, and Chiara Poletto.

The findings of this study are based on metadata associated with a total of 1,735,675 sequences available on GISAID and submitted between 15 Aug 2020 and 1 Jun 2021. The data was downloaded on 2 Jun 2021 via gisaid.org (GISAID: EPI_SET_230724tv). To view the contributors of each sequence associated with the metadata we used, please visit https://doi.org/10.55876/gis8.230724tv. Note that some collection dates are missing.

We used daily numbers of COVID-19 cases by country from the COVID-19 data repository hosted by the Center for Systems Science and Engineering at Johns Hopkins University (CSSE).

Proprietary airline data, which are commercially available from OAG and IATA databases, were also used in this study.

This repository is divided into three parts:

The Data Processing folder contains the code to process the aforementioned data and compute inputs for the IDM (International Dissemination Model). The code produce a RData file that must be used in IDM code.

The IDM folder pertains to the International Dissemination Model. The code produces three csv files that must be used to run AMA code.

The AMA folder contains the code to run the AMA model (Adherence and Sustainability).

For more information on the AM B code, please follow the link: https://github.com/EPIcx-lab/COVID-19/tree/master/Adherence_and_sustainability
