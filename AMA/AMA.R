###################################
rm(list = ls())
library(Rcpp)
source("~/ownCloud/coronavirus/variant_importation/stochastic_simulations_functions.R")
###################################

######### INPUT FILES ##############

# Files with distribution of first importation
lf <- c("lambda.firstlo.csv", "lambda.firstmed.csv", "lambda.firstup.csv")
stopifnot(length(lf)==3)

# load weekly R estimates:
b <- read.csv("~/ownCloud/coronavirus/variant_importation/weekly_R_estimate.csv") # WEEKLY R ESTIMATE
b$year <- as.numeric(substr(b$year_week, 1, 4))
b$week <- NA
b$week[b$year==2020] <- as.numeric(substr(b$year_week[b$year == 2020], 6, 7))
b$week[b$year==2021] <- 53 + as.numeric(substr(b$year_week[b$year == 2021], 6, 7))
b$week_shifted <- b$week - 3

b$country[b$country=="United States"] <- "United.States.of.America"
list_o_countries <- c("Denmark", "France", "Italy", "Germany", "Portugal", "Switzerland", "United.States.of.America")
stopifnot(all(list_o_countries %in% b$country))

last_date <- "2021-01-31" # run simulation until 31 Jan
surviving_date <- "2020-12-31" # date taken as reference for survival
delta_date <- as.numeric(as.Date(last_date)) - as.numeric(as.Date(surviving_date))


######### RUN MODEL ##############

for(myfile in lf){
  
  a <- read.csv(myfile) # NUMBER OF IMPORTED PER DESTINATION
  a$X <- NULL # remove first column
  names(a)[names(a)=="day"] <- "date"
  a$date <- as.Date(a$date, format = "%Y-%m-%d") # this format may need to be adapted

  
  # add other dates if last line is 2021-01-07
  n <- nrow(a)
  stopifnot(a[n, "date"] == "2021-01-07")
  last_lines <- data.frame(date = seq.Date(from = as.Date("2021-01-08"), to = as.Date(last_date), by = 1))
  for(cc in names(a)[2:ncol(a)]){
    last_lines[, cc] <- 0
  }
  a <- rbind(a, last_lines)
  a$week <- as.numeric(format(as.Date(a$date), "%W")) + 1
  a$year <- as.numeric(format(as.Date(a$date), "%Y"))
  a$week[a$year == 2021] <- a$week[a$year == 2021] + 52
  
  # replace 'commas' in numbers with dots
  first_country <- "Australia"
  last_country <- "United.States.of.America"
  for(col_nb in which(names(a)==first_country):which(names(a)==last_country)){
    a[, col_nb] <- as.numeric(gsub(pattern = ",", replacement = ".", x = a[, col_nb]))
  }
  
  # control all the dates we have
  stopifnot(all(a$date==seq.Date(from=as.Date("2020-08-15"), to = as.Date(last_date), by = 1))) # from 11th Oct 22, this starts at 2020-08-15 instead of 2020-08-01
  
  myname <- gsub(pattern = ".csv", replacement = "", myfile)

  
  # this is the table of weeks and first days FYI
  if(F){
    week_first_day <- seq.Date(from = as.Date("2019-12-30"), to = as.Date("2021-12-27"), by = "7 day")
    weeks2020 <- 1:53; weeks2020 <- sapply(as.character(weeks2020), function(cc) if(nchar(cc) == 1) paste0("0", cc) else cc)
    weeks2021 <- 1:52; weeks2021 <- sapply(as.character(weeks2021), function(cc) if(nchar(cc) == 1) paste0("0", cc) else cc)
    names(week_first_day) <- c(paste("2020", weeks2020, sep = "-"), paste("2021", weeks2021, sep = "-"))
  }
  
  ############################ SET PARAMETERS ON/OFF    ############################
  
  mykappa <- 0.4
  transmission_advantage <- 1.6 # 30/08/2021 changed to 1.6
  dt <- 0.1
  nrep <- 500 # stochastic replicates
  
  ############################ NOW SIMULATE DEVELOPEMENT OF VARIANT IN EACH COUNTRY ############################
  
  
  for(mycountry in list_o_countries){
    
    suba <- a[c("date", mycountry, "week", "year")]
    names(suba) <- c("date", "travellers", "week", "year")
    subb <- b[b$country==mycountry, ]
    suba$R <- subb$R_deaths[match(suba$week, subb$week_shifted)]
    suba$R_epiestim <- subb$R_epiestim_deaths[match(suba$week, subb$week_shifted)]
    suba$numericdate <- as.numeric(strftime(suba$date, format = "%j"))
    suba$numericdate[suba$year == 2021] <- suba$numericdate[suba$year == 2021] + 366
    ndays <- nrow(suba)
    

    daterange <- range(suba$date)
    daterange <- c(as.numeric(strftime(daterange[1], format = "%j")), as.numeric(strftime(daterange[2], format = "%j")) + 366)
    init_day <- daterange[1]
    days <- daterange[1]:daterange[2]
    stopifnot(length(days) == nrow(suba))
    weeks <- suba$week
    Rlist <- suba$R * transmission_advantage
    Rlist_dt <- c(sapply(suba$R * transmission_advantage, function(x) rep(x, 1 / dt))); Rlist_dt <- c(Rlist_dt, Rlist_dt[length(Rlist_dt)])
    
    # check a few things:
    stopifnot(length(days) == length(Rlist))
    stopifnot(length(days) == length(weeks))
    stopifnot(length(days)/dt + 1 == length(Rlist_dt))
    
    
    all_nseeds <- rep(NA, nrep)
    proba_surviving <- rep(NA, nrep)
    all_sims <- c()
    all_dates_importation <- list()
    all_dates_importation_surviving <- list()
    all_dates_importation_two <- list() # new 05/05/2023 (dates chains of transmission giving rise to at least two cases)
    all_dates_importation_ten <- list() # new 05/05/2023 (dates chains of transmission giving rise to at least 10 cases)
    distrib_final_size <- list()
    n_importations <- c()
    
    # TODO: get:
    # - fraction autochtonous / impoorted case at each timepoint
    # use all_dates_importation to get the dates when 1 case was introduced
    for(rr in 1:nrep){
      print(rr)
      
      sim <- c()
      dates_importation <- c()
      idx_sim <- 1
      for(myday in suba$numericdate){ # for each day
        idx <- which(suba$numericdate==myday)
        nimportations <- rpois(n = 1, lambda = suba$travellers[idx]) # number of importations that week
        if(nimportations > 0){
          for(i in 1:nimportations){
            importation_day <- myday
            dates_importation <- c(dates_importation, importation_day)
            importation_day0 <- importation_day - init_day
            maxt <- ndays - importation_day0
            sim <- rbind(sim, c(rep(0, importation_day0), simulate_arbitraryRlist(Rlist = Rlist_dt[(importation_day0/dt+1):(ndays/dt+1)], kappa = mykappa, tfin = maxt)$I))
            idx_sim <- idx_sim + 1
          }
        }
      }
      nseeds <- nrow(sim)
      dates_importation_surviving <- unlist(sapply(1:nseeds, function(i) rep(dates_importation[i], sim[i, ndays + 1 - delta_date]))) # changed this for 31/12/2020 instead of 31/01/2021
      dates_importation_two <- dates_importation[which(rowSums(sim[, 1:(ndays + 1 - delta_date)]) >= 2)]
      dates_importation_ten <- dates_importation[which(rowSums(sim[, 1:(ndays + 1 - delta_date)]) >= 10)]
      
      stopifnot(length(dates_importation_surviving)==sum(sim[, ndays + 1 - delta_date])) # check we have as many dates_importation_surviving as cases at the chosen ref date

      # record sims and dates of importation
      all_nseeds[rr] <- nseeds
      all_sims <- rbind(all_sims, colSums(sim))
      all_dates_importation[[rr]] <- dates_importation
      all_dates_importation_surviving[[rr]] <- dates_importation_surviving
      all_dates_importation_two[[rr]] <- dates_importation_two
      all_dates_importation_ten[[rr]] <- dates_importation_ten
      proba_surviving[rr] <- mean(sim[, ndays + 1 - delta_date] > 0)
      distrib_final_size[[rr]] <- sim[, ndays + 1 - delta_date]
      
    }
    
    x <- daterange[1]:(daterange[2]+1)
    datenumbers <- suba$numericdate
    datelabels <- suba$date
    sub_label <- c(1, 32, 62, 93, 123, 154, 185, 213)
    
############# SAVE RESULTS #############

    save(
      list = c("suba", "subb", "all_dates_importation", "all_dates_importation_surviving", "all_dates_importation_two", "all_dates_importation_ten", "distrib_final_size", "fraction_autochtonous",
                     "all_sims", "all_sims_ci", "all_nseeds", "proba_surviving",
                     "daterange", "init_day", "Rlist", "Rlist_dt", "transmission_advantage", "weeks", "days", "ndays",
                     "gtpar", "mykappa", "dt", "maxt", "mycountry"),
         file = paste0("~/ownCloud/coronavirus/variant_importation/simuls/", mycountry, "_", myname, ".RData")
    )
  }

}




