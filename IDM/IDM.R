################################
library("importFromUK")
source("IDM_functions.R")
packageVersion("importFromUK") # should be 3.2
################################
# Import input data
load("Inputs.RData")
load("Inputs2.RData")

date.ref = as.Date("2020-08-15")
date.thr = as.Date("2020-12-31")
inherits = FALSE
T0.max = 38
T0.start = 32
Tc2.LD = 83
Tc3.LD = 126

#load("smallStart.RData")
################################

# Make data
data = make.data.dynamic(first.VOC=first.VOC.fake,screen=screening.fake,travel=travel.london.fake,epidUK=epid.UK.fake,dt.start.epid = as.Date("2020-10-01"),
                         dt.stop.epid = as.Date("2020-12-31"),date.thr = as.Date("2020-12-31"),pop.uk = 36e6, 
                         R=R.VOC.smooth.fake,r=r.VOC.smooth.fake,R.increase=0.5, K=0.5, K.UK=0.25,s.increase=1,dt.s.increase = as.Date("2020-12-18"),
                         incub = 5,delai.sub1 = delaysbefore$delai.1,delai.sub2 = delaysafter$delai.1,date.change=as.Date("2020-12-18"),
                         ahead=7,f_before_T0=1.0)

################################

# Run MCMC
res= MCMC.import.cpp(param = c("r","T0","r2","s"),ll="pois",llEpid = NULL,
                     init=list(r=0.11,T0=T0.start,k=1, r2=0.11,Tcp2=Tc2.LD,r3=0.1,Tcp3=Tc3.LD,s=1.1),
                     data = data,R = 2000,sd = list(r=0.02,T0=8,k=0.01,Tcp2=8,r2=0.2,Tcp3=4,r3=0.01,s=1.1))

################################

# compute distributions and sum up results
sum = summary.MCMC.import(res,data)
sum.count = summary.countries(sum,data)

write.csv2(sum$distr$lambda.first$lo,"lambda.firstlo.csv")
write.csv2(sum$distr$lambda.first$med,"lambda.firstmed.csv")
write.csv2(sum$distr$lambda.first$up,"lambda.firstup.csv")

################################





