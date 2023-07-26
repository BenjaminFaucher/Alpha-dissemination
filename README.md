This repository contains the code used in the analyses presented in the manuscript: 

**Drivers and Impact of the Early Silent Invasion of SARS-CoV-2 Alpha**

by Benjamin Faucher, Chiara E. Sabbatini, Peter Czuppon, Moritz U.G. Kraemer, Philippe Lemey, Vittoria Colizza, Francois Blanquart, Pierre-Yves Boëlle, and Chiara Poletto.

**The analysis is based on the following data:**

Metadata associated with a total of 1,735,675 sequences available on GISAID and submitted between 15 Aug 2020 and 1 Jun 2021. The data were downloaded on 2 Jun 2021 via gisaid.org (GISAID: EPI_SET_230724tv). To view the contributors of each sequence associated with the metadata we used visit https://doi.org/10.55876/gis8.230724tv. 

Daily numbers of COVID-19 cases by country obtained from the COVID-19 data repository hosted by the Center for Systems Science and Engineering at Johns Hopkins University (CSSE) (https://doi.org/10.1016/S1473-3099(20)30120-1).

Proprietary airline data, which are commercially available from OAG and IATA databases (https://www.iata.org/).

**The analysis consists of three main parts:**

Processing raw data and creating input files for the International Dissemination Model (IDM). Source codes are available in the Data Processing folder. The code produces an RData file to be input in the IDM code.

International Dissemination Model, provided in the IDM folder. The output of the code include three csv files that must be used to run AMA code.

The autochthonous transmission model A and B. Source codes for the autochthonous transmission model A are provided in the AMA folder. The autochthonous transmission model B was published before (see https://github.com/EPIcx-lab/COVID-19/tree/master/Adherence_and_sustainability)
