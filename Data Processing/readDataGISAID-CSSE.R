
#######################
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggforce)
#######################
# read in files
# GISAID
data.GISAID <- read.delim("metadata_2021_06_02.tsv",encoding = "UTF-8")
# CSSE
data.CSSE = read.csv2("dataCSSE.csv")
################

#####################
# PROCESSING GISAID
#####################
metadata.orig2=data.GISAID
names.orig2= names(metadata.orig2)
# filter : betacoronavirus + Human + Submission date
metadata.orig2 = metadata.orig2[metadata.orig2$Type=="betacoronavirus",]
metadata.orig2 = metadata.orig2[metadata.orig2$Host=="Human",]
metadata.orig2$Submission.date=as.Date(metadata.orig2$Submission.date)
metadata.orig2=metadata.orig2[!is.na(metadata.orig2$Submission.date),]
#recode country names - for consistency with CSSE - and get subregions
metadata.orig2 = metadata.orig2 %>% separate(Location, c("Continent","country","region"), sep="/")
metadata.orig2$country = trimws(metadata.orig2$country)
metadata.orig2$region = trimws(metadata.orig2$region)
# recode some coutry names - for consistency with CSSE
metadata.orig2$country[metadata.orig2$country=="USA"] = "United States of America"
metadata.orig2$country[metadata.orig2$country=="Russia"] = "Russian Federation"
metadata.orig2$country[metadata.orig2$country=="Taiwan"] = "Chinese Taipei"
metadata.orig2$country[metadata.orig2$country=="mongolia"] = "Mongolia"
metadata.orig2$country[metadata.orig2$country=="morocco"] = "Morocco"
metadata.orig2$country[metadata.orig2$country=="Saint Vincent and the Grenadines"] = "Saint Vincent and Grenadines"
metadata.orig2$country[metadata.orig2$country=="North Macedonia"] = "Macedonia"
metadata.orig2$country[metadata.orig2$country=="Timor-Leste"] = "Timor"

names(metadata.orig2)[1]="strain"

metadata.orig2$date = metadata.orig2$Collection.date
metadata.orig2$date_submitted = metadata.orig2$Submission.date

#keep only required cols 
metadata.orig2 = metadata.orig2[,c("Collection.date","country","Pango.lineage","Submission.date")]
#
# MANUAL CURATION of IMPOSSIBLE DATES
#
#one in Czech 2016-> 2021
metadata.orig2[metadata.orig2$Collection.date=="2016-04-16" & !is.na(metadata.orig2$Collection.date),"Collection.date"] = "2021-04-16"
# one date in norway 2002 -> 2020
metadata.orig2[metadata.orig2$Collection.date=="2002-05-08" & !is.na(metadata.orig2$Collection.date),"Collection.date"] = "2020-05-08"
# fix dates in Norway - 1904 origin; dates in 1905 are 2021
metadata.orig2[metadata.orig2$country=="Norway" & substr(metadata.orig2$Collection.date,1,4)=="1905","Collection.date"] = 
  paste0("2021",substr(metadata.orig2[metadata.orig2$country=="Norway" & substr(metadata.orig2$Collection.date,1,4)=="1905","Collection.date"],5,10))


#########################
# PROCESSING 
#######################
metadata=metadata.orig2

metadata$date.col = as.Date(metadata$Collection.date,format = "%Y-%m-%d")
metadata$date.sub = as.Date(metadata$Submission.date,format = "%Y-%m-%d")
metadata$delai=as.numeric(metadata$date.sub - metadata$date.col)
metadata$imputed = as.numeric(is.na(as.Date(metadata$Collection.date,format = "%Y-%m-%d")))

rownames(metadata) = NULL
#efface les date.col des impossibles, a imputer...(26 valeurs en Norvege)
metadata[metadata$date.col > metadata$date.sub & !is.na(metadata$date.col),"date.col"] = NA


# compute date.col.min (useful if date.col rounded to month)
metadata$date.col.min=as.Date(paste0(metadata$Collection.date,"-01"),format="%Y-%m-%d")
metadata$date.col.min = as.Date(paste0(format(metadata$date.col.min,"%Y-%m"),"-01"),format="%Y-%m-%d")
# erase is date.col.min after date.sub
metadata[metadata$date.col.min > metadata$date.sub & !is.na(metadata$date.col.min),"date.col.min"] = NA

# if collection date rounded to year
metadata$date.col.min[metadata$Collection.date=="2021"] = as.Date("2021-01-01")
metadata$date.col.min[metadata$Collection.date=="2020"] = as.Date("2020-01-01")

# code end of the month
# try 31
metadata$date.col.max=as.Date(paste0(metadata$Collection.date,"-31"),format="%Y-%m-%d")
metadata$date.col.max = as.Date(paste0(format(metadata$date.col.max,"%Y-%m"),"-31"),format="%Y-%m-%d")
date.col.max.unk = is.na(metadata$date.col.max) # keep indices
#try 30
metadata$date.col.max[date.col.max.unk & !is.na(metadata$Collection.date)]=
  as.Date(paste0(metadata$Collection.date[date.col.max.unk & !is.na(metadata$Collection.date)],"-30"),format="%Y-%m-%d")
metadata$date.col.max[date.col.max.unk] = 
  as.Date(paste0(format(metadata$date.col.max[date.col.max.unk],"%Y-%m"),"-30"),format="%Y-%m-%d")
# try 29
date.col.max.unk = is.na(metadata$date.col.max) # keep indices
metadata$date.col.max[is.na(metadata$date.col.max) & !is.na(metadata$Collection.date)] = 
  as.Date(paste0(metadata$Collection.date[is.na(metadata$date.col.max) & !is.na(metadata$Collection.date)],"-29"),"%Y-%m-%d")
metadata$date.col.max[date.col.max.unk] = as.Date(paste0(format(metadata$date.col.max[date.col.max.unk],"%Y-%m"),"-29"),format="%Y-%m-%d")
#try 28
date.col.max.unk = is.na(metadata$date.col.max) # keep indices
metadata$date.col.max[is.na(metadata$date.col.max) & !is.na(metadata$Collection.date)] = 
  as.Date(paste0(metadata$Collection.date[is.na(metadata$date.col.max) & !is.na(metadata$Collection.date)],"-28"),"%Y-%m-%d")
metadata$date.col.max[date.col.max.unk] = as.Date(paste0(format(metadata$date.col.max[date.col.max.unk],"%Y-%m"),"-28"),format="%Y-%m-%d")

# date.col.max before date.submission
metadata$date.col.max[!is.na(metadata$date.col.max) & metadata$date.col.max >metadata$date.sub] = 
  metadata$date.sub[!is.na(metadata$date.col.max) & metadata$date.col.max >metadata$date.sub] 
metadata$date.col.max[is.na(metadata$date.col.max)]=metadata$date.sub[is.na(metadata$date.col.max)]

metadata$delai=as.numeric(metadata$date.sub - metadata$date.col)

metadata$alpha = as.numeric(metadata$Pango.lineage=="B.1.1.7")

# 
metadata.before = metadata
metadata=metadata[!is.na(metadata$date.col.min),]

print(paste(sum(is.na(as.Date(metadata$date.col))), " missing collection dates"))

#
# imputation according to country, between date.col.min and date.col.max and VOC alpha
#
impute.dates <- function(metadata, imp="imputed", delta=7) {
  #delta is window for close submission date
for (co in sort(unique(metadata$country))) {
  nb=0
  mis.col = which(metadata[,imp]==1 & metadata$country == co)
  if (length(mis.col)>0) {
    print(paste("processing ", co, " ",length(mis.col), " missing"))
    #get sample of country lines
    tmp = metadata[metadata$country == co & !is.na(metadata$date.col) & !is.na(metadata$delai) ,]
  if (length(mis.col)>1)  { mis.col=sample(mis.col)} # melange
    for (x in mis.col) {
        # match on close submission date (<7 jours) and approx date of collection if present and alpha/not alpha
        tmp2 = tmp[tmp$date.col.min >= metadata$date.col.min[x] & 
                     tmp$date.col.max <= metadata$date.col.max[x] & 
                     abs(tmp$date.sub-metadata$date.sub[x])<delta &
                     tmp$alpha == metadata$alpha[x],]
        if (dim(tmp2)[1]>0) {
          metadata$date.col[x] = sample(tmp2$date.col,size = 1)
          metadata$delai[x] = metadata$date.sub[x] - metadata$date.col[x]
          nb=nb+1;
        } else if (metadata$alpha[x]==1) { # si pas de match sur alpha on impute en tout
          tmp2 = tmp[tmp$date.col.min >= metadata$date.col.min[x] & 
                       tmp$date.col.max <= metadata$date.col.max[x] & 
                       abs(tmp$date.sub-metadata$date.sub[x])<delta ,]
          if (dim(tmp2)[1]>0) {
            metadata$date.col[x] = sample(tmp2$date.col,size = 1)
            metadata$delai[x] = metadata$date.sub[x] - metadata$date.col[x]
            nb=nb+1;
          }
        }
      }
    }
  if (nb>0) print(paste(co, "-> ",nb," imputations"))
}
  metadata
  }

# first round with +/- 7 days
metadata.imp = impute.dates(metadata)
metadata.imp$delai=as.numeric(metadata.imp$date.sub - metadata.imp$date.col)
print(paste(sum(is.na(as.Date(metadata.imp$date.col)))," missing collection dates"))

#second round with +/- 21 days
metadata.imp2 = metadata.imp
metadata.imp2$imputed2 = as.numeric(is.na(metadata.imp2$date.col))
metadata.imp2 = impute.dates(metadata.imp2, "imputed2", 15)
print(paste(sum(is.na(as.Date(metadata.imp2$date.col)))," missing collection dates"))

metadata=metadata.imp2

#
# if not imputed, but date.min.col and date.max.col exists, sample at random
#
x = (metadata$date.col.max[is.na(metadata$date.col)&!is.na(metadata$date.col.min)]-
       metadata$date.col.min[is.na(metadata$date.col)&!is.na(metadata$date.col.min)]+1)
x=as.numeric(ceiling(runif(length(x))*x))
metadata$date.col[is.na(metadata$date.col)&!is.na(metadata$date.col.min)] = 
  as.Date(paste0(format(metadata[is.na(metadata$date.col)&!is.na(metadata$date.col.min),"date.col.min"],"%Y-%m"),"-",x),format="%Y-%m-%d")
metadata$delai=as.numeric(metadata$date.sub - metadata$date.col)
print(paste(sum(is.na(as.Date(metadata$date.col))),"missing collection dates"))

#
# now we don't have data to bound imputation, use reported lags - sample +/- 8 days for submission
#
for (co in unique(metadata$country)) {
  nb=0
  mis.col = which(is.na(metadata$date.col) & metadata$country == co)
  if (length(mis.col)>0) {
    print(paste("processing ", co, " ",length(mis.col), " missing"))
    #get sample submitted less than 8 days around
    tmp = metadata[metadata$country == co & !is.na(metadata$date.col) & !is.na(metadata$delai),]
    for (x in mis.col) {
      tmp2 = tmp[abs(tmp$date_submitted -metadata$date_submitted[x]) <8,] 
      if (dim(tmp2)[1]>0) {
        metadata$date.col[x] = metadata$date_submitted[x] - sample(tmp2$delai,size = 1)
        nb=nb+1
      }
    }
  }
  if (nb>0) print(paste(co, "-> ",nb," imputations"))
}

metadata$delai=as.numeric(metadata$date.sub - metadata$date.col)
print(paste(sum(is.na(as.Date(metadata$date.col))),"missing collection dates"))

#
# now even looser, impute from submission +/-21 days around
#
for (co in unique(metadata$country)) {
  nb=0
  mis.col = which(is.na(metadata$date.col) & metadata$country == co)
  if (length(mis.col)>0) {
    print(paste("processing ", co, " ",length(mis.col), " missing"))
    #get sample
    tmp = metadata[metadata$country == co & !is.na(metadata$date.col) & !is.na(metadata$delai),]
    for (x in mis.col) {
      tmp2 = tmp[abs(as.numeric(tmp$date_submitted -metadata$date_submitted[x])) <21,] 
      if (dim(tmp2)[1]>0) {
        metadata$date.col[x] = metadata$date_submitted[x] - sample(tmp2$delai,size = 1)
        nb=nb+1
      }
    }
  }
  if (nb>0) print(paste(co, "-> ",nb," imputations"))
}

# stop imputation 
# failed to impute : 
print(paste("overall missing collection dates: ",sum(is.na(as.Date(metadata$date.col)))))
print(table(metadata$country, is.na(as.Date(metadata$date.col))))
print("removing records with unimputed collection dates")
metadata=metadata[!is.na(metadata$date.col),]

write.csv(metadata,file = "./output/GISAID.csv")


############################
#
# VOC in GISAID
# 
###############################
# get VOC : first submission of a B.1.1.7
VOC = metadata[metadata$Pango.lineage == "B.1.1.7",]
VOC.orig=metadata.orig2[metadata.orig2$Pango.lineage == "B.1.1.7",]
VOC = VOC[order(VOC$country,VOC$date.sub,VOC$date.col),]
VOC.orig=VOC.orig[order(VOC.orig$country,VOC.orig$Submission.date,VOC.orig$Collection.date),]

first.VOC.submitted = data.frame(VOC %>% group_by(country) %>% arrange(date.sub,imputed,date.col) %>% filter(row_number(date.sub) == 1))
first.VOC.submitted = first.VOC.submitted[,c("country","date.col","date.sub","imputed")]
first.VOC.submitted$country = as.character(first.VOC.submitted$country) 
first.VOC.submitted=first.VOC.submitted[order(first.VOC.submitted$date.sub),]

#first.VOC.ever
#
VOC.orig$date.col = as.Date(VOC.orig$Collection.date)
VOC.orig.noNA = VOC.orig[!is.na(VOC.orig$Collection.date),]
VOC.orig.noNA=VOC.orig.noNA[order(VOC.orig.noNA$country,VOC.orig.noNA$Collection.date),]

first.VOC.collected = data.frame(VOC.orig.noNA %>% group_by(country) %>% filter(row_number(date.col) == 1))
first.VOC.collected = first.VOC.collected[,c("country","date.col","Submission.date")]
first.VOC.collected$country = as.character(first.VOC.collected$country) 

################################
#
# compute number of sequences per date of collection per country, cumulated over the past 1,3,5 15,30 days
#
################################

get.nb.sequences <- function(metadata,date.sub.max=NULL) {
  if (!is.null(date.sub.max)) {
    # Use all data with imputation
    metadata = metadata[metadata$date.sub <= date.sub.max,]
  }
# nb sequences by date of collection - all dates
  metadata.nb=data.frame(ftable(metadata$country,metadata$date.col))
  names(metadata.nb)=c("country","date","nb.seq")
  metadata.nb$date = as.Date(metadata.nb$date)

  metadata.nb = merge(metadata.nb, merge(data.frame(date=seq(as.Date("2020-07-01"),as.Date("2021-03-01"),1)),
                                       data.frame(country=unique(metadata.nb$country))),by=c("date","country"),all.y=T)
  metadata.nb$nb.seq[is.na(metadata.nb$nb.seq)] = 0
  metadata.nb = metadata.nb[order(metadata.nb$country, metadata.nb$date),]
  # compute cumulated sum by country
  require(dplyr)
  metadata.nb = metadata.nb %>% group_by(country) %>% arrange(date) %>% mutate(csum.nb.seq=cumsum(nb.seq))

  metadata.nb2 = metadata.nb
  metadata.nb2$date = metadata.nb2$date + 30
  metadata.nb2 = merge(metadata.nb,metadata.nb2,by=c("country","date"))
  metadata.nb2$delta.seq = metadata.nb2$csum.nb.seq.x -metadata.nb2$csum.nb.seq.y

  data.gisaid.30=metadata.nb2[metadata.nb2$date>=as.Date("2020-08-01"),c("country","date","delta.seq")]
  data.gisaid.30$country=as.character(data.gisaid.30$country)

  metadata.nb2 = metadata.nb
  metadata.nb2$date = metadata.nb2$date + 15
  metadata.nb2 = merge(metadata.nb,metadata.nb2,by=c("country","date"))
  metadata.nb2$delta.seq = metadata.nb2$csum.nb.seq.x -metadata.nb2$csum.nb.seq.y

  data.gisaid.15=metadata.nb2[metadata.nb2$date>=as.Date("2020-08-01"),c("country","date","delta.seq")]
  data.gisaid.15$country=as.character(data.gisaid.15$country)

  metadata.nb2 = metadata.nb
  metadata.nb2$date = metadata.nb2$date + 7
  metadata.nb2 = merge(metadata.nb,metadata.nb2,by=c("country","date"))
  metadata.nb2$delta.seq = metadata.nb2$csum.nb.seq.x -metadata.nb2$csum.nb.seq.y

  data.gisaid.7=metadata.nb2[metadata.nb2$date>=as.Date("2020-08-01"),c("country","date","delta.seq")]
  data.gisaid.7$country=as.character(data.gisaid.7$country)


  metadata.nb2 = metadata.nb
  metadata.nb2$date = metadata.nb2$date + 3
  metadata.nb2 = merge(metadata.nb,metadata.nb2,by=c("country","date"))
  metadata.nb2$delta.seq = metadata.nb2$csum.nb.seq.x -metadata.nb2$csum.nb.seq.y

  data.gisaid.3=metadata.nb2[metadata.nb2$date>=as.Date("2020-08-01"),c("country","date","delta.seq")]
  data.gisaid.3$country=as.character(data.gisaid.3$country)


  metadata.nb2 = metadata.nb
  metadata.nb2$date = metadata.nb2$date + 1
  metadata.nb2 = merge(metadata.nb,metadata.nb2,by=c("country","date"))
  metadata.nb2$delta.seq = metadata.nb2$csum.nb.seq.x -metadata.nb2$csum.nb.seq.y

  data.gisaid.1=metadata.nb2[metadata.nb2$date>=as.Date("2020-08-01"),c("country","date","delta.seq")]
  data.gisaid.1$country=as.character(data.gisaid.1$country)


  return(list(data.gisaid.30=data.gisaid.30, data.gisaid.15=data.gisaid.15, data.gisaid.7=data.gisaid.7, data.gisaid.3=data.gisaid.3,data.gisaid.1=data.gisaid.1))
}

sequences.all.col.imp = get.nb.sequences(metadata)

sequences.all.col.sub31 = get.nb.sequences(metadata, date.sub.max = as.Date("2020-12-31"))

# these contain total number of sequences over period of times 
plot(sequences.all.col.imp$data.gisaid.30[sequences.all.col.imp$data.gisaid.30$country=="France",c("date","delta.seq")],t='l')
lines(sequences.all.col.sub31$data.gisaid.30[sequences.all.col.sub31$data.gisaid.30$country=="France",c("date")],
      sequences.all.col.sub31$data.gisaid.30[sequences.all.col.sub31$data.gisaid.30$country=="France",c("delta.seq")])

##########################
#
# PROCESSING CSSE
#
##########################

nCountry=dim(data.CSSE)[1]
nDates=dim(data.CSSE)[2]-1
names(data.CSSE)[1]="country"
tmp.c=as.character(data.CSSE$country)
tmp=as.matrix(data.CSSE[,-1])
#
# MANUAL CURATION - ECUADOR has shift, then goes back to initial
tmp[109,94:229] = tmp[109,94:229] - 8300
# 

# reorder data in countries where cumulated number of cases is not strictly increasing
#apply reordering to all countries
for (i in 1:nCountry) {
  tmp[i,] = sort(tmp[i,])
}

data.CSSE = cbind(tmp.c,as.data.frame(tmp))

data.CSSE = gather(data.CSSE,key = "date",value="csum.cases",-1)
data.CSSE$date=rep(seq(as.Date("2020-01-22"),as.Date("2020-01-22")+nDates-1,1),each=nCountry)
data.CSSE$country = as.character(data.CSSE$tmp.c)
data.CSSE$tmp.c = NULL
data.CSSE = data.CSSE[order(data.CSSE$country,data.CSSE$date),]
data.CSSE = aggregate(data.CSSE[,"csum.cases",drop=F], by=list(country=data.CSSE$country,date=data.CSSE$date),sum)
data.CSSE$country = as.character(data.CSSE$country)

#
# country names - homogeneize with GISAID
#
data.CSSE$country[data.CSSE$country=="US"]= "United States of America" 
data.CSSE$country[data.CSSE$country=="Korea, South"] = "South Korea"
data.CSSE$country[data.CSSE$country=="St Martin"] = "Saint Martin"
data.CSSE$country[data.CSSE$country=="Burma"] = "Myanmar"
data.CSSE$country[data.CSSE$country=="US"] = "United States of America"
data.CSSE$country[data.CSSE$country=="South korea"] = "South Korea"
data.CSSE$country[data.CSSE$country=="Taiwan"] = "Chinese Taipei"
data.CSSE$country[data.CSSE$country=="Taiwan*"] = "Chinese Taipei"
data.CSSE$country[data.CSSE$country=="Burma"] = "Myanmar"
data.CSSE$country[data.CSSE$country=="St Martin"] = "Saint Martin"
data.CSSE$country[data.CSSE$country=="North Macedonia"] = "Macedonia"
data.CSSE$country[data.CSSE$country=="Timor-Leste"] = "Timor"
data.CSSE$country[data.CSSE$country=="Czechia"] = "Czech Republic"
data.CSSE$country[data.CSSE$country=="Russia"] = "Russian Federation"
data.CSSE$country[data.CSSE$country=="Saint Vincent and the Grenadines"] = "Saint Vincent and Grenadines"
data.CSSE$country[data.CSSE$country=="Democratic Republic of Congo"] = "Democratic Republic of the Congo"
data.CSSE$country[data.CSSE$country=="West Bank and Gaza"]="Palestine"
data.CSSE$country[data.CSSE$country=="Congo (Kinshasa)"]="Democratic Republic of the Congo"
data.CSSE$country[data.CSSE$country=="Congo (Brazzaville)"]="Republic of the Congo"
data.CSSE$country[data.CSSE$country=="Guinea-Bissau"]="Guinea Bissau"


max.cases = data.CSSE[data.CSSE$date=="2020-12-31",]

# 
# get cumulated number of cases over periods of diffeent size
#

get.nb.cases <- function(data.CSSE) {

  data.CSSE2 = data.CSSE
  data.CSSE2$date = data.CSSE2$date+30
  data.CSSE.30 = merge(data.CSSE,data.CSSE2,by=c("country","date"))
  data.CSSE.30$delta.cases = data.CSSE.30$csum.cases.x -data.CSSE.30$csum.cases.y
  data.CSSE.30 = data.CSSE.30[,c("country","date","delta.cases")]
  data.CSSE.30=data.CSSE.30[data.CSSE.30$date>=as.Date("2020-08-01"),]
  data.CSSE.30$country=as.character(data.CSSE.30$country)

  data.CSSE2 = data.CSSE
  data.CSSE2$date = data.CSSE2$date+15
  data.CSSE.15 = merge(data.CSSE,data.CSSE2,by=c("country","date"))
  data.CSSE.15$delta.cases = data.CSSE.15$csum.cases.x -data.CSSE.15$csum.cases.y
  data.CSSE.15 = data.CSSE.15[,c("country","date","delta.cases")]
  data.CSSE.15=data.CSSE.15[data.CSSE.15$date>=as.Date("2020-08-01"),]
  data.CSSE.15$country=as.character(data.CSSE.15$country)

  data.CSSE2 = data.CSSE
  data.CSSE2$date = data.CSSE2$date+7
  data.CSSE.7 = merge(data.CSSE,data.CSSE2,by=c("country","date"))
  data.CSSE.7$delta.cases = data.CSSE.7$csum.cases.x -data.CSSE.7$csum.cases.y
  data.CSSE.7 = data.CSSE.7[,c("country","date","delta.cases")]
  data.CSSE.7=data.CSSE.7[data.CSSE.7$date>=as.Date("2020-08-01"),]
  data.CSSE.7$country=as.character(data.CSSE.7$country)

    data.CSSE2 = data.CSSE
  data.CSSE2$date = data.CSSE2$date+3
  data.CSSE.3 = merge(data.CSSE,data.CSSE2,by=c("country","date"))
  data.CSSE.3$delta.cases = data.CSSE.3$csum.cases.x -data.CSSE.3$csum.cases.y
  data.CSSE.3 = data.CSSE.3[,c("country","date","delta.cases")]
  data.CSSE.3=data.CSSE.3[data.CSSE.3$date>=as.Date("2020-08-01"),]
  data.CSSE.3$country=as.character(data.CSSE.3$country)

  data.CSSE2 = data.CSSE
  data.CSSE2$date = data.CSSE2$date+1
  data.CSSE.1 = merge(data.CSSE,data.CSSE2,by=c("country","date"))
  data.CSSE.1$delta.cases = data.CSSE.1$csum.cases.x -data.CSSE.1$csum.cases.y
  data.CSSE.1 = data.CSSE.1[,c("country","date","delta.cases")]
  data.CSSE.1=data.CSSE.1[data.CSSE.1$date>=as.Date("2020-08-01"),]
  data.CSSE.1$country=as.character(data.CSSE.1$country)

  CSSE.all = list(data.CSSE.30=data.CSSE.30, data.CSSE.15=data.CSSE.15, data.CSSE.7=data.CSSE.7,  data.CSSE.3=data.CSSE.3 ,  data.CSSE.1=data.CSSE.1 )
  CSSE.all
}
# these are cumulated cases 

CSSE.all=get.nb.cases(data.CSSE)

write.csv(CSSE.all$data.CSSE.1,"./output/CSSE.csv")

# plot cases 
# different lookback
plot(CSSE.all$data.CSSE.30[CSSE.all$data.CSSE.30$country=="France",c("date")],
     CSSE.all$data.CSSE.30[CSSE.all$data.CSSE.30$country=="France",c("delta.cases")],t='l')
lines(CSSE.all$data.CSSE.15[CSSE.all$data.CSSE.15$country=="France",c("date")],
      CSSE.all$data.CSSE.15[CSSE.all$data.CSSE.15$country=="France",c("delta.cases")])
lines(CSSE.all$data.CSSE.7[CSSE.all$data.CSSE.7$country=="France",c("date")],
      CSSE.all$data.CSSE.7[CSSE.all$data.CSSE.7$country=="France",c("delta.cases")])
lines(CSSE.all$data.CSSE.1[CSSE.all$data.CSSE.1$country=="France",c("date")],
      CSSE.all$data.CSSE.1[CSSE.all$data.CSSE.1$country=="France",c("delta.cases")])


# check COUNTRY names between CSSE and GISAID
# may need some MANUAL CURATION if some were forgotten / 
unique(CSSE.all$data.CSSE.30$country[!(CSSE.all$data.CSSE.30$country %in% CSSE.all$data.gisaid.30$country)])
unique(CSSE.all$data.gisaid.30$country[!(CSSE.all$data.gisaid.30$country %in% CSSE.all$data.CSSE.30$country)])

# compute sequencing fraction
# we take cumulated data from GISAID and CSSE over same window, then compute fraction
# add 0.5 cases to denominator to avoid division/0
compute.seq.fraction <- function(list.screening, list.CSSE, date.end=as.Date("2021-01-31"), date.start=as.Date("2020-08-15")) {
  #compute sequencing fraction from date.start to date.end
##JOIN
data.all.30 = merge(list.screening$data.gisaid.30, list.CSSE$data.CSSE.30, all.y=T, by=c("country","date"))
data.all.15 = merge(list.screening$data.gisaid.15, list.CSSE$data.CSSE.15, all.y=T, by=c("country","date"))
data.all.7  = merge(list.screening$data.gisaid.7,  list.CSSE$data.CSSE.7,  all.y=T, by=c("country","date"))
data.all.3  = merge(list.screening$data.gisaid.3,  list.CSSE$data.CSSE.3,  all.y=T, by=c("country","date"))
data.all.1  = merge(list.screening$data.gisaid.1,  list.CSSE$data.CSSE.1,  all.y=T, by=c("country","date"))

data.all.30 = data.all.30[data.all.30$date>= date.start,]
data.all.15 = data.all.15[data.all.15$date>= date.start,]
data.all.7 = data.all.7[data.all.7$date>= date.start,]
data.all.3 = data.all.3[data.all.3$date>= date.start,]
data.all.1 = data.all.1[data.all.1$date>= date.start,]

data.all.30$delta.seq[is.na(data.all.30$delta.seq)]=0
data.all.15$delta.seq[is.na(data.all.15$delta.seq)]=0
data.all.7$delta.seq[is.na(data.all.7$delta.seq)]=0
data.all.3$delta.seq[is.na(data.all.3$delta.seq)]=0
data.all.1$delta.seq[is.na(data.all.1$delta.seq)]=0

data.all.30$s = sapply((data.all.30$delta.seq) / (0.5+data.all.30$delta.cases),max,0)
data.all.15$s = sapply((data.all.15$delta.seq) / (0.5+data.all.15$delta.cases),max,0)
data.all.7$s = sapply((data.all.7$delta.seq) / (0.5+data.all.7$delta.cases),max,0)
data.all.3$s = sapply((data.all.3$delta.seq) / (0.5+data.all.3$delta.cases),max,0)
data.all.1$s = sapply((data.all.1$delta.seq) / (0.5+data.all.1$delta.cases),max,0)

data.all.30 = data.all.30[data.all.30$date <= date.end,]
data.all.15 = data.all.15[data.all.15$date <= date.end,]
data.all.7 = data.all.7[data.all.7$date <= date.end,]
data.all.3 = data.all.3[data.all.3$date <= date.end,]
data.all.1 = data.all.1[data.all.1$date <= date.end,]

list.country.noSeq.30 = aggregate(data.all.30[data.all.30$date>=date.start,"s"], by=list(data.all.30$country[data.all.30$date>=date.start]), mean)
names(list.country.noSeq.30) = c("country","s.avg")
list.country.noSeq.30 = list.country.noSeq.30$country[list.country.noSeq.30$s.avg==0.0]

list.country.noSeq.15 = aggregate(data.all.15[data.all.15$date>=date.start,"s"], by=list(data.all.15$country[data.all.15$date>=date.start]), mean)
names(list.country.noSeq.15) = c("country","s.avg")
list.country.noSeq.15 = list.country.noSeq.15$country[list.country.noSeq.15$s.avg==0.0]

list.country.noSeq.7 = aggregate(data.all.7[data.all.7$date>=date.start,"s"], by=list(data.all.7$country[data.all.7$date>=date.start]), mean)
names(list.country.noSeq.7) = c("country","s.avg")
list.country.noSeq.7 = list.country.noSeq.7$country[list.country.noSeq.7$s.avg==0.0]

list.country.noSeq.3 = aggregate(data.all.3[data.all.3$date>=date.start,"s"], by=list(data.all.3$country[data.all.3$date>=date.start]), mean)
names(list.country.noSeq.3) = c("country","s.avg")
list.country.noSeq.3 = list.country.noSeq.3$country[list.country.noSeq.3$s.avg==0.0]

list.country.noSeq.1 = aggregate(data.all.1[data.all.1$date>=date.start,"s"], by=list(data.all.1$country[data.all.1$date>=date.start]), mean)
names(list.country.noSeq.1) = c("country","s.avg")
list.country.noSeq.1 = list.country.noSeq.1$country[list.country.noSeq.1$s.avg==0.0]

# delete those provided no sequences 
data.all.30 = data.all.30[!(data.all.30$country %in% list.country.noSeq.30), ]
data.all.15 = data.all.15[!(data.all.15$country %in% list.country.noSeq.15), ]
data.all.7 = data.all.7[!(data.all.7$country %in% list.country.noSeq.7), ]
data.all.3 = data.all.3[!(data.all.3$country %in% list.country.noSeq.3), ]
data.all.1 = data.all.1[!(data.all.1$country %in% list.country.noSeq.1), ]

# rename day columns with day of the year counting from 1st january 2020
first.index=as.numeric(format(date.start,"%j"))

screening.30 = data.frame(data.all.30[,c("country","date","s")] %>% pivot_wider(names_from=date,values_from=s))
rownames(screening.30)=screening.30$country
screening.30 = screening.30[,-1]
screening.30$country=NULL
screening.30 = as.matrix(screening.30)
colnames(screening.30) = paste0("s.",seq(first.index,length.out=dim(screening.30)[2],by=1))

screening.15 = data.frame(data.all.15[,c("country","date","s")] %>% pivot_wider(names_from=date,values_from=s))
rownames(screening.15)=screening.15$country
screening.15$country=NULL
screening.15 = as.matrix(screening.15)
colnames(screening.15) = paste0("s.",seq(first.index,length.out=dim(screening.15)[2],by=1))

screening.7 = data.frame(data.all.7[,c("country","date","s")] %>% pivot_wider(names_from=date,values_from=s))
rownames(screening.7)=screening.7$country
screening.7$country=NULL
screening.7 = as.matrix(screening.7)
colnames(screening.7) = paste0("s.",seq(first.index,length.out=dim(screening.7)[2],by=1))

screening.3 = data.frame(data.all.3[,c("country","date","s")] %>% pivot_wider(names_from=date,values_from=s))
rownames(screening.3)=screening.3$country
screening.3$country=NULL
screening.3 = as.matrix(screening.3)
colnames(screening.3) = paste0("s.",seq(first.index,length.out=dim(screening.3)[2],by=1))

screening.1 = data.frame(data.all.1[,c("country","date","s")] %>% pivot_wider(names_from=date,values_from=s))
rownames(screening.1)=screening.1$country
screening.1$country=NULL
screening.1 = as.matrix(screening.1)
colnames(screening.1) = paste0("s.",seq(first.index,length.out=dim(screening.1)[2],by=1))

return(list(screening.30=screening.30, screening.15=screening.15, screening.7=screening.7,
            screening.3=screening.3,screening.1=screening.1,
            date.start=date.start, date.end=date.end))

}

screening.all.sub31 = compute.seq.fraction(sequences.all.col.sub31, CSSE.all,date.end=as.Date("2020-12-31"))

plot(seq(screening.all.sub31$date.start, screening.all.sub31$date.end,1),screening.all.sub31$screening.30["France",],t='l')

pdf("SequencingEffort-allSeq-20211201.pdf")
for (p in 1 : (150/25)) {
  print(ggplot() + 
          geom_step(data=data.all.30[data.all.30$imput==1,], aes(x=date,y=s,col=imput,linetype="30")) + 
          geom_step(data=data.all.30[data.all.30$imput==0,], aes(x=date,y=s,col=imput,linetype="30")) + 
          geom_step(data=data.all.15[data.all.15$imput==1,], aes(x=date,y=s,col=imput,linetype="15")) + 
          geom_step(data=data.all.15[data.all.15$imput==0,], aes(x=date,y=s,col=imput,linetype="15")) + 
          geom_step(data=data.all.7[data.all.7$imput==1,], aes(x=date,y=s,col=imput,linetype="7")) + 
          geom_step(data=data.all.7[data.all.7$imput==0,], aes(x=date,y=s,col=imput,linetype="7")) +
          facet_wrap_paginate(~country,ncol = 5,nrow = 5,page = p) + 
          geom_hline(yintercept = 1,linetype=1) + theme_bw() + xlab("Date") + 
          ylab("percentage sequenced") + scale_y_log10())
}
dev.off()

##############################
#
# PROCESSING TIME COLLECTION to SUBMISSION
#
##############################
# this is by country and by date 

compute.delai <- function(metadata, date.start=as.Date("2020-08-01"), date.end=as.Date("2021-01-31")) {
  require(dplyr)
  require(tidyr)
  metadata.forSub = metadata[ metadata$date.col>=date.start,]
  metadata.forSub$nb=0
  distr.col.sub=aggregate(metadata.forSub[,"nb"],list(country=metadata.forSub$country, date.col=metadata.forSub$date.col, date.sub=metadata.forSub$date.sub),length)
  tmp=data.frame(country=unique(distr.col.sub$country))
  tmp=merge(tmp,data.frame(date.col=seq(date.start,date.end,1)))
  # need to go to June 
  tmp=merge(tmp,data.frame(date.sub=seq(date.start,as.Date("2021-06-01"),1)))
  tmp = tmp[tmp$date.sub >= tmp$date.col,]
  
  distr.col.sub=merge(distr.col.sub,tmp,by=c("country","date.col","date.sub"),all.y=T)
  distr.col.sub$delai=as.numeric(distr.col.sub$date.sub-distr.col.sub$date.col)
  names(distr.col.sub)[4]="nb"
  distr.col.sub$nb[is.na(distr.col.sub$nb)]=0
  distr.col.sub = distr.col.sub[order(distr.col.sub$country,distr.col.sub$date.col,distr.col.sub$date.sub),]  

  make.matrix <- function(delai) {
    delai.sub.by.country = list()
    i=1
    for (co in  unique(delai$country)) {
      tmp = as.matrix(delai[delai$country==co,-(1:2)])
      rownames(tmp) = colnames(tmp)[1:dim(tmp)[1]]
      tmp[is.na(tmp)]=0
      delai.sub.by.country[[i]]= tmp
      names(delai.sub.by.country)[i]=co
      i=i+1
    }
    delai.sub.by.country
  }
  
    # 
  distr.col.sub$nb.cum = distr.col.sub$nb
  #1 day
  delai = data.frame(distr.col.sub[,c("country","date.col","date.sub","nb.cum")] %>% pivot_wider(names_from=date.sub,values_from=nb.cum))
  delai[,-(1:2)] = delai[,-(1:2)]/apply(delai[,-(1:2)],1,sum,na.rm=T)
  d.start = format(date.start,"%j")
  names(delai)[3:(dim(delai)[2])]=paste0("d.",seq(d.start,length.out=dim(delai)[2]-2))
  delai.sub.by.country.1 = make.matrix(delai)
  print("1")
  #2 day
  for (d in 1:1) {
    tmp=distr.col.sub
    tmp$date.col = tmp$date.col+d
    distr.col.sub=merge(distr.col.sub, setNames(tmp[,c("country","date.col","delai","nb")],c("country","date.col","delai","nb2")),
                        by=c("country","date.col","delai"),all.x=T)
    distr.col.sub$nb2[is.na(distr.col.sub$nb2)]=0
    distr.col.sub$nb.cum = distr.col.sub$nb.cum + distr.col.sub$nb2
    distr.col.sub$nb2=NULL
    print(d)
  }
  delai = data.frame(distr.col.sub[,c("country","date.col","date.sub","nb.cum")] %>% pivot_wider(names_from=date.sub,values_from=nb.cum))
  delai[,-(1:2)] = delai[,-(1:2)]/apply(delai[,-(1:2)],1,sum,na.rm=T)
  names(delai)[3:(dim(delai)[2])]=paste0("d.",as.numeric(format(seq(date.start,date.start+dim(delai)[2]-3,1),"%j")))
  delai.sub.by.country.2 = make.matrix(delai)
  print("2")
  for (d in 2:2) {
    tmp=distr.col.sub
    tmp$date.col = tmp$date.col+d
    distr.col.sub=merge(distr.col.sub, setNames(tmp[,c("country","date.col","delai","nb")],c("country","date.col","delai","nb2")),
                        by=c("country","date.col","delai"),all.x=T)
    distr.col.sub$nb2[is.na(distr.col.sub$nb2)]=0
    distr.col.sub$nb.cum = distr.col.sub$nb.cum + distr.col.sub$nb2
    distr.col.sub$nb2=NULL
    print(d)
  }
  delai = data.frame(distr.col.sub[,c("country","date.col","date.sub","nb.cum")] %>% pivot_wider(names_from=date.sub,values_from=nb.cum))
  delai[,-(1:2)] = delai[,-(1:2)]/apply(delai[,-(1:2)],1,sum)
  names(delai)[3:(dim(delai)[2])]=paste0("d.",as.numeric(format(seq(date.start,date.start+dim(delai)[2]-3,1),"%j")))
  delai.sub.by.country.3 = make.matrix(delai)
  print("3")
  
  delai.sub.by.country=list( delai.3=delai.sub.by.country.3,delai.2=delai.sub.by.country.2, delai.1=delai.sub.by.country.1)
  return(delai.sub.by.country)
  
  
  #7 day
  for (d in 3:6) {
    tmp=distr.col.sub
    tmp$date.col = tmp$date.col+d
    distr.col.sub=merge(distr.col.sub, setNames(tmp[,c("country","date.col","delai","nb")],c("country","date.col","delai","nb2")),
                        by=c("country","date.col","delai"),all.x=T)
    distr.col.sub$nb2[is.na(distr.col.sub$nb2)]=0
    distr.col.sub$nb.cum = distr.col.sub$nb.cum + distr.col.sub$nb2
    distr.col.sub$nb2=NULL
    print(d)
  }
  delai = data.frame(distr.col.sub[,c("country","date.col","date.sub","nb.cum")] %>% pivot_wider(names_from=date.sub,values_from=nb.cum))
  delai[,-(1:2)] = delai[,-(1:2)]/apply(delai[,-(1:2)],1,sum)
  names(delai)[3:(dim(delai)[2])]=paste0("d.",as.numeric(format(seq(date.start,date.start+dim(delai)[2]-3,1),"%j")))
  delai.sub.by.country.7 = make.matrix(delai)
  
  #15 day
  for (d in 7:14) {
    tmp=distr.col.sub
    tmp$date.col = tmp$date.col+d
    distr.col.sub=merge(distr.col.sub, setNames(tmp[,c("country","date.col","delai","nb")],c("country","date.col","delai","nb2")),
                        by=c("country","date.col","delai"),all.x=T)
    distr.col.sub$nb2[is.na(distr.col.sub$nb2)]=0
    distr.col.sub$nb.cum = distr.col.sub$nb.cum + distr.col.sub$nb2
    distr.col.sub$nb2=NULL
    print(d)
  }
  delai = data.frame(distr.col.sub[,c("country","date.col","date.sub","nb.cum")] %>% pivot_wider(names_from=date.sub,values_from=nb.cum))
  delai[,-(1:2)] = delai[,-(1:2)]/apply(delai[,-(1:2)],1,sum)
  names(delai)[3:(dim(delai)[2])]=paste0("d.",as.numeric(format(seq(date.start,date.start+dim(delai)[2]-3,1),"%j")))
  delai.sub.by.country.15 = make.matrix(delai)
  
  #30 day
  for (d in 15:29) {
    tmp=distr.col.sub
    tmp$date.col = tmp$date.col+d
    distr.col.sub=merge(distr.col.sub, setNames(tmp[,c("country","date.col","delai","nb")],c("country","date.col","delai","nb2")),
                        by=c("country","date.col","delai"),all.x=T)
    distr.col.sub$nb2[is.na(distr.col.sub$nb2)]=0
    distr.col.sub$nb.cum = distr.col.sub$nb.cum + distr.col.sub$nb2
    distr.col.sub$nb2=NULL
    print(d)
  }
  delai = data.frame(distr.col.sub[,c("country","date.col","date.sub","nb.cum")] %>% pivot_wider(names_from=date.sub,values_from=nb.cum))
  delai[,-(1:2)] = delai[,-(1:2)]/apply(delai[,-(1:2)],1,sum)
  names(delai)[3:(dim(delai)[2])]=paste0("d.",as.numeric(format(seq(date.start,date.start+dim(delai)[2]-3,1),"%j")))
  delai.sub.by.country.30 = make.matrix(delai)

  delai.sub.by.country=list(delai.30=delai.sub.by.country.30, delai.15=delai.sub.by.country.15, delai.7=delai.sub.by.country.7, delai.3=delai.sub.by.country.3, delai.1=delai.sub.by.country.1)
  delai.sub.by.country
}

delai.sub.by.country.1to3=compute.delai(metadata)

delai.sub.by.country.VOC=compute.delai(metadata[metadata$Pango.lineage=="B.1.1.7",])

