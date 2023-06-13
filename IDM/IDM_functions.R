
prep.data.dynamic = function(VOC, date.thr, pop.uk=36E6, dt.sub=T, before=F) {
  
  # columns are expected to start at 1 with "date.ref" (date.ref -> column 1)
  
  ref.date= as.Date("2020-08-15")
  # first we count dates from 2020-08-0=15
  VOC$dt.col.lo = as.numeric(VOC$date.collection.lo - ref.date)+1
  VOC$dt.col.up = as.numeric(VOC$date.collection.up - ref.date)+1
  VOC$dt.sub = as.numeric(VOC$date.submission - ref.date)+1
  # date of first submission
  VOC$dt.1sub = as.numeric(VOC$date.1st.sub - ref.date)+1
  
  
  VOC$Ts =as.numeric(date.thr - ref.date)+1
  VOC$Ts[!is.na(VOC$date.submission) & (VOC$date.submission < date.thr) ] = 
    VOC$dt.sub[!is.na(VOC$date.submission) & (VOC$date.submission < date.thr) ]
  #those unobserved are set to 
  # then we compute indicator of censoring
  VOC$delta1=0
  # exact dates
  if (dt.sub == TRUE) { dt.cond = VOC$date.submission}
  else if (dt.sub == FALSE) { dt.cond =VOC$date.collection}
  
  # on met delta1=1 si soumission avant dt.thr
  VOC$delta1[!is.na(dt.cond) & (dt.cond <= date.thr)] = 1

  if (before == FALSE) {
  } else {
  }
  # We prob number per day
  VOC[,grep("p.",names(VOC),fixed=T)] = VOC[,grep("p.",names(VOC),fixed=T)]/pop.uk
  VOC[VOC$country=="United Kingdom",grep("p.",names(VOC),fixed=T)]=1
  
#  VOC$Ts=VOC$dt.sub
#  VOC$Ts[is.na(VOC$Ts)]=as.numeric(date.thr - ref.date)+1
#  VOC$Ts[VOC$delta1==0] = as.numeric(date.thr - ref.date)+1
  
  VOC$Ti.lo=VOC$dt.col.lo
  VOC$Ti.lo[VOC$delta1==0]=as.numeric(date.thr - ref.date)+1
  
  VOC$Ti.up=VOC$dt.col.up
  VOC$Ti.up[VOC$delta1==0]=as.numeric(date.thr - ref.date)+2

  
  VOC
}

make.data.dynamic = function(first.VOC, screen, travel, date.thr, epidUK, freq.screening=NULL, min.freq=0,
                             delai.sub1=NULL,delai.sub2=NULL,date.change=as.Date("2020-12-18"),
                             index.country=NULL,pop.uk=38e6, M=NULL,R=NULL, r=NULL, 
                             R.increase=0,K=0.5,K.UK=0.4,dt.sub=T, 
                             before=F,countries=NULL, s.increase=1, 
                             dt.s.increase=NULL, incub=4,
                             truncate=FALSE,T0.max=NULL,
                             date.ref=as.Date("2020-08-15"),
                             ahead=7, dt.start.epid=NULL, dt.stop.epid=NULL, f_before_T0=0.0) {
  
  # add ahead to make projections, keeping tKps and delai and p 
  # add epidUK and t0Epid
  
  VOC=merge(first.VOC,screen,by=0, all.y=T)
  VOC$Row.names=NULL
  VOC= merge(VOC,travel,by="country", all.x=T)
  
  # days are numbered from 2020/01/01 as "%j",  "d.XXX" - in sequecing, in delai
  index.date.start=as.numeric(format(date.ref,"%j"))
  index.date.change=as.numeric(format(date.change,"%j"))
  # weeks are numbered from 2020/01/01, as "%V", "p.XX" in travel data
  index.week.start=as.numeric(format(date.ref,"%V"))
  
  
  if (is.null(dt.start.epid)) {dt.start.epid = min(epidUK$day); print (paste("setting epid start date to", dt.start.epid))}
  if (is.null(dt.stop.epid)) {dt.stop.epid = min(max(epidUK$day),date.thr) ; print (paste("setting epid stop date to", dt.stop.epid))}
  if (dt.start.epid < min(epidUK$day)) {dt.start.epid = min(epidUK$day); print (paste("changing epid start date to", dt.start.epid))}
  if (dt.stop.epid > min(max(epidUK$day),date.thr)) {dt.stop.epid = min(max(epidUK$day),date.thr); print (paste("changing epid stop date to", dt.stop.epid))}
  epidUK = epidUK[(epidUK$day <=date.thr) &(epidUK$day>=dt.start.epid) &(epidUK$day<=dt.stop.epid),]
  t0Epid=as.numeric(format(min(epidUK$day),"%j")) - index.date.start+1
  epidUK=epidUK$VOCcases
  
  
  # delai is date.sub - date.col +1 // computed elsewhere
  
  VOC[VOC$country=="United Kingdom",grep("p.",names(VOC),fixed=T)]=0
  
  VOC = VOC[!is.na(VOC[,paste("p.",index.week.start,sep="")]),]
  
  if (!is.null(countries)) {
    VOC = VOC[VOC$country %in% countries,]
  } 
  countries=as.character(VOC$country)
  
  
  # here we compute the dates of the travel weeks.
  # data starts with the first week of August : 2020-08-03, so p.1 is for this week
  # we extend the data from August 1st then delete unnecessary columns
  
  # nombre de jours
  seq.days =seq(date.ref,date.thr+ahead,1) 
  lW= length(seq.days)-ahead
  #days weeks
  # correspondance avec les semaines pour le travel
  # this compute the week numbers
  # when goes to 1
  weeks = as.numeric(format(seq.days,"%V"))
  # if there are weeks before index.week.start, add 53 because 53 weeks in 2020
  weeks[weeks < index.week.start] = 53 + weeks[weeks < index.week.start]
  
  W=weeks
  
  if (!is.null(freq.screening)) {
    VOC = merge(VOC,freq.screening,by="country")
    VOC = VOC[VOC$freq>=min.freq,]
    countries=as.character(VOC$country)
  }
  
  
  if (!is.null(R)) {
    #extract days
    R = R[,c("country",paste("R",seq(index.date.start,length.out=lW+ahead,by=1),sep="."))]
    names(R) = c("country",paste("R",seq(1,length.out=lW+ahead,by=1),sep="."))
    VOC = merge(VOC,R, by="country",all.x=T)
    if (any(is.na(VOC$R.1))) stop("not all countries have R")
    VOC[VOC$country=="United Kingdom",grep("R.",names(VOC),fixed=T)]=0
  }
  
  if (!is.null(r)) {
    r = r[,c("country",paste("r",seq(index.date.start,length.out=lW+ahead,by=1),sep="."))]
    names(r) = c("country",paste("log.r",seq(1,length.out=lW+ahead,by=1),sep="."))
    VOC = merge(VOC,r, by="country",all.x=T)
    if (any(is.na(VOC$r.1))) stop("not all countries have r")
    VOC[VOC$country=="United Kingdom",grep("log.r.",names(VOC),fixed=T)]=0
  }
  
  if (!is.null(countries)) {
    VOC = VOC[VOC$country %in% countries,]
  }
  
  VOC.prep = prep.data.dynamic(VOC = VOC, 
                               date.thr = as.Date(date.thr,origin="1970-01-01"), 
                               pop.uk = pop.uk,
                               dt.sub = dt.sub, before=before)
  
  
  
  if (truncate==TRUE) {
    VOC.prep = VOC.prep[VOC.prep$delta1==1,]
  }
  
  if (!is.null(delai.sub1)) { 
    delai.sub1 = delai.sub1[VOC$country] 
  }
  if (!is.null(delai.sub2)) { 
    delai.sub2 = delai.sub2[VOC$country] 
  }
  
  if (!is.null(delai.sub1) && is.null(delai.sub2)) { 
    for (i in 1:length(delai.sub1)) {
      # select from date start to lW
      row.col.names = paste("d",index.date.start:(index.date.start+lW+ahead-1),sep=".")
      delai.sub1[[i]] = delai.sub1[[i]][row.col.names,row.col.names]
    }
    delai.sub = delai.sub1
  }
  
  
  if (!is.null(delai.sub1) && !is.null(delai.sub2)) {
    delai.sub = delai.sub1
    for (i in 1:length(delai.sub1)) {
      # select from date start to lW
      col.names = paste("d",index.date.start:(index.date.start+lW+ahead-1),sep=".")
      row.names.1 = paste("d",index.date.start:(index.date.change-1),sep=".")
      row.names.2 = paste("d",index.date.change:(index.date.start+lW+ahead-1),sep=".")
      delai.sub1[[i]] = delai.sub1[[i]][row.names.1,col.names]
      delai.sub2[[i]] = delai.sub2[[i]][row.names.2,col.names]
      delai.sub[[i]] = rbind(delai.sub1[[i]],delai.sub2[[i]])
    }
  }
  
  
  delai.sub = delai.sub[VOC.prep$country] 
  
  nCountry=length(VOC.prep$country)
  # verifier l'ordre des delais
  #make an array of this :
  delai.sub.array=array(NA,c(lW+ahead,lW+ahead,nCountry))
  for (i in 1:nCountry) {
    delai.sub.array[,,i] = delai.sub[[i]]
  }
  
  if (is.null(M)) {M = length(W)} 
  M = rep(1,times=M)
  
  # screening 
  s=as.matrix(VOC.prep[,paste0("s.",seq(index.date.start,length.out=lW+ahead,by=1))])
  # rename to 1:lW
  colnames(s) = paste0("s.",1:(lW+ahead))
  #  s=as.matrix(VOC.prep[,paste0("s.",1:length(W))])
  
  rownames(s)=VOC.prep$country
  
  colnames(s)=NULL
  
  s.orig=s
  #s.orig[s.orig>1]=1
  # increase testing in travellers // all but not the UK
  if (!is.null(dt.s.increase)) {
    dt.s.increase = as.numeric(dt.s.increase-date.ref)+1
    s[-which(VOC.prep$country=="United Kingdom"),dt.s.increase:length(s[1,])]=  
      s[-which(VOC.prep$country=="United Kingdom"),dt.s.increase:length(s[1,])] * s.increase
    s[s>1]=1 
  } else {
    dt.s.increase=lW+ahead
    s.increase=1
  }

  #expand p to dates
  p=VOC.prep[,paste0("p.",weeks)]
  rownames(p) = VOC.prep$country
  
  #T0.max can be a few days before first collection in the UK
  if(is.null(T0.max)) {
    T0.max=VOC.prep$Ti.lo[VOC.prep$country=="United Kingdom"]-1
  } else {
    T0.max=VOC.prep$Ti.lo[VOC.prep$country=="United Kingdom"]-T0.max
  }
  
  Kps = K*p*s
  Kps[which(VOC.prep$country=="United Kingdom"),] = K.UK/K *  Kps[which(VOC.prep$country=="United Kingdom"),]
  tKps=t(Kps)
  colnames(tKps)=rownames(s)
  
  # o use in estimating s
  Kp = K*p
  Kp[which(VOC.prep$country=="United Kingdom"),] = K.UK/K *  Kp[which(VOC.prep$country=="United Kingdom"),]
  tKp=t(Kp)
  colnames(tKp)=rownames(s)
  
  # to test
  iUK = which(VOC.prep$country=="United Kingdom")
  iFR = which(VOC.prep$country=="France")
  
  
  data=list(country = VOC.prep$country,
            nCountry=length(VOC.prep$country), 
            nWeeks=max(W),
            lW=lW,
            ahead=ahead,
            T=VOC.prep$T,
            probD=VOC.prep$prob,
            s=s.orig,
            p=p,
            tKps=tKps,
            tKp=tKp,
            W=W,
            delta1=VOC.prep$delta1,
            K=K,
            M=M,
            d=delai.sub,# delais
            incub=incub,
            dt.s.increase=dt.s.increase,
            s.increase=s.increase, 
            K=K, 
            K.UK=K.UK,
            T0.max=T0.max,
            Ts=VOC.prep$Ts,
            Ti.lo=VOC.prep$Ti.lo,
            Ti.up=VOC.prep$Ti.up,
            date.start=date.ref,
            date.thr=date.thr,
            delai.array=delai.sub.array,
            epidUK=epidUK,
            t0Epid=t0Epid,
            iUK=iUK,
            iFR=iFR,
            f_before_T0=f_before_T0
  )
  
  if (!is.null(R)) {
    # if R<1, probability of survival is 0
    # if R>1, probabuility of survival is 1-1/R
    # this is probability of having offspring when entering 
    R=as.matrix(VOC.prep[,paste0("R.",1:(lW+ahead))])
    R=R[,1:(lW+ahead)]*(1+R.increase)
    R[R<1]=0
    #R=R[-which(VOC.prep$country=="United Kingdom"),]
    colnames(R)=NULL
    data$R=R
    data$p.over.R = data$p*(1-1/data$R)
    data$p.over.R = as.matrix(data$p.over.R)
    data$p.over.R[is.infinite(data$p.over.R)]=0
    data$p.over.R[is.na(data$p.over.R)]=0
    data$p.over.R[data$p.over.R<0]=0
    colnames(data$p.over.R)=NULL
    data$p.over.R=t(data$p.over.R)
  }
  
  if (!is.null(r)) {
    #this is the log-expected multiplication of cases
    r=as.matrix(VOC.prep[,paste0("log.r.",1:(lW+ahead))])
    r=r[,1:(lW+ahead)]
    r=r + log((1+R.increase))/7
    r.preComp = array(0,dim = c((lW+ahead),(lW+ahead),data$nCountry))
    for (i in 1:data$nCountry) {
      for (j in 2:lW) {
        r.preComp[ (j:(lW+ahead))-1,j-1,i] = exp(cumsum(r[i,j:(lW+ahead)]))      
      }
      r.preComp[,,i]=t(r.preComp[,,i])
    }
    data$r=r.preComp
  }
  
  data
}


make.data.dynamic.UK =function(first.VOC, screen, travel,freq.screening, min.freq,delai.sub,
                            date.thr,index.country=NULL,pop.uk=38e6, M=NULL,R=NULL, r=NULL, 
                            R.increase=0,K.UK=0.4,dt.sub=T, before=F,countries=NULL, s.increase=1, 
                            dt.s.increase=NULL, incub=4,
                            truncate=FALSE,T0.max=NULL) {
  
  VOC=merge(first.VOC,screen,by=0, all.y=T)
  names(VOC)[2]="date.collection"
  names(VOC)[3]="date.submission"
  VOC= merge(VOC,travel,by="country", all.x=T)
  
  VOC$delai = as.numeric(date.thr - VOC$date.collection) # delai de soumission
  
  VOC[VOC$country=="United Kingdom",grep("p.",names(VOC))]=0
  VOC = VOC[!is.na(VOC$p.1),]
  
  VOC = merge(VOC,freq.screening,by="country")
  VOC = VOC[VOC$freq>min.freq,]
  
  if (!is.null(delai.sub)) { 
    VOC = merge(VOC,delai.sub,by="country") #adds mean/sd for delai
  }
  
  if (!is.null(countries)) {
    VOC = VOC[VOC$country %in% countries,]
  }
  
  if (!is.null(R)) {
    VOC = merge(VOC,R, by="country",all.x=T)
    if (any(is.na(VOC$R.1))) stop("not all countries have R")
  }
  
  if (!is.null(r)) {
    VOC = merge(VOC,r, by="country",all.x=T)
    if (any(is.na(VOC$r.1))) stop("not all countries have r")
  }
  
  if (is.null(index.country)) index.country=1:dim(VOC)[1]
  VOC=VOC[index.country,]
  W=c(1,1,1+floor(as.numeric(seq(as.Date("2020-08-03"),
                                 as.Date(date.thr,origin="1970-01-01"),1)+1-as.Date("2020-08-03"))/7))
  
  VOC.prep = prep.data.dynamic(VOC = VOC, 
                               dt.thr = as.Date(date.thr,origin="1970-01-01"), 
                               pop.uk = pop.uk,
                               dt.sub = dt.sub, before=before)
  
  if (truncate==TRUE) {
    VOC.prep = VOC.prep[VOC.prep$delta1==1,]
  }
  
  if (is.null(M)) {M = length(W)} 
  M = rep(1,times=M)
  s=as.matrix(VOC.prep[,paste0("s.",1:length(W))])
  colnames(s)=NULL

  #expand p to dates
  p.week=VOC.prep[,paste0("p.",1:max(W))]
  p=matrix(0,nrow =length(VOC.prep$country),ncol=length(W))
  
  for (j in 1:length(W)) {
    p[,j]=p.week[,W[j]]    
  }
  colnames(p)=NULL  
  
  if (!is.null(delai.sub)) {
    # delai # adds term 
    d=matrix(0,ncol =length(VOC.prep$country),nrow=length(W))
    # si observ?, dlnorm
    for (i in 1:dim(VOC.prep)[1]) {
      if (!is.na(VOC.prep$delai[i])) { # observed with a delai/ should multiply f(T[i]) P(delai<T-t)
        d[VOC.prep$T[i],i] = plnorm(VOC.prep$delai[i],meanlog = VOC.prep$mean[i],sdlog = VOC.prep$sd[i],lower.tail = T)
      } else {
        # not observed
        d[,i] = plnorm(length(W):1,meanlog = VOC.prep$mean[i],sdlog = VOC.prep$sd[i],lower.tail = F)
      }
    }
  } else {
    d=NULL
  }
  
  if(is.null(T0.max)) {
    T0.max=min(VOC.prep$T)-1
  } else {
    T0.max=min(VOC.prep$T)-T0.max
  }
  
  
  ps = p*s
  ps = ps[-which(VOC.prep$country=="United Kingdom"),]
  tps=t(ps)
  p=p[-which(VOC.prep$country=="United Kingdom"),]
  # here we split data for UK and others
  T.UK = unlist(VOC.prep$T[VOC.prep$country=="United Kingdom"])
  s.UK = unlist(VOC.prep[VOC.prep$country=="United Kingdom",paste0("s.",1:length(W))])
  VOC.prep = VOC.prep[!(VOC.prep$countr=="United Kingdom"),]
  
  data=list(country = VOC.prep$country,
            nCountry=length(VOC.prep$country), 
            nWeeks=max(W),
            lW=length(W),
            T=VOC.prep$T,
            s=s,
            p=p,
            tps=tps,
            W=W,
            delta1=VOC.prep$delta1,
            M=M,
            d=d,
            incub=incub,
            dt.s.increase=dt.s.increase,
            s.increase=s.increase, 
            K.UK=K.UK, 
            T.UK=T.UK,
            s.UK=s.UK,
            T0.max=T0.max)
  
  if (!is.null(R)) {
    R=as.matrix(VOC.prep[,paste0("R.",1:length(W))])
    R=R[,1:data$lW]*(1+R.increase)
    R[R<1]=0
    #R=R[-which(VOC.prep$country=="United Kingdom"),]
    colnames(R)=NULL
    data$R=R
    data$p.over.R = data$p/data$R
    data$p.over.R = as.matrix(data$p.over.R)
    data$p.over.R[is.infinite(data$p.over.R)]=0
    data$p.over.R[is.na(data$p.over.R)]=0
    colnames(data$p.over.R)=NULL
    data$tp.over.R=t(data$p.over.R)
  }
  
  if (!is.null(r)) {
    r=as.matrix(VOC.prep[,paste0("r.",1:length(W))])
    r=r[,1:data$lW]
    r=r + log((1+R.increase))/7
    r.preComp = array(0,dim = c(data$nCountry,data$lW,data$lW))
    for (i in 1:data$nCountry) {
      for (j in 2:data$T[i]) {
        r.preComp[i, j:data$lW,j-1] = exp(cumsum(r[i,j:data$lW]))      
      }
    }
    data$r=r.preComp
  }
  
  data
}

# this is new code for the actual first submission
calc.logL.import.1sub <- function(r, T0,k, data, r2=NA, Tc2=NA, r3=NA,Tc3=NA) {
  target=0;
  
  #target=target + dexp(r,rate = 0.1,log=T)
  
  log.exp.r.t = rep(0,data$lW)
  
  if (is.na(Tc2)) { Tc2=data$lW }
  if (is.na(Tc3)) { Tc3=data$lW }
  log.exp.r.t[T0:data$lW] = r*(seq(T0,data$lW)-T0);
  if (Tc2 < data$lW) {
    log.exp.r.t[Tc2:data$lW] = r*(Tc2-T0) + r2*(seq(Tc2,data$lW)-Tc2);
    target=target + dexp(r2,rate = 0.1,log=T)
  }
  if (Tc3 < data$lW) {
    log.exp.r.t[Tc3:data$lW] = r*(Tc2-T0) + r2*(Tc3-Tc2) + r3* (seq(Tc3,data$lW)-Tc3);
    target=target + dexp(r3,rate = 0.1,log=T)
  }
  
  exp.r.t=exp(log.exp.r.t)
  # cancel before T0
  
  if (T0>1) {exp.r.t[1:(T0-1)]=0}
  #  exp.r.t.int = (exp(r*data$M)-1)/(r*data$M)* exp.r.t 
  exp.r.t.travel = exp.r.t
  for (i in 1:(data$incub-1)) {
    exp.r.t.travel = exp.r.t.travel + c(rep(0,i),exp.r.t)[1:data$lW]
  }
   lambda=data$tKps * exp.r.t.travel
   # this is actual number of introduction which are sequenced
  
#  log.S.lambda = -apply(lambda,2,function(x) {c(0,cumsum(x))[1:length(x)]}) # decale de 1
#  f.T = lambda*exp(log.S.lambda) # this is probability of a given date
#  f.T.delai = f.T * data$d # this is P(T=t) * P(D>T-t)
  
   for (i in 1:data$nCountry) {
    ##
    lambda.t.u =  data$d[[i]] * lambda[,i] # note d is transposed [pi_1(0) pi_1(1) ...  pi_1(T) ]
    #                                                            [  0     pi_2(0) ... pi_2(T-1)]
    #                                                            [  0       0     ...  pi_T(0) ]
    # joint 
    lambda.t=apply(lambda.t.u,2,sum)
     E.N.T0.TS = sum(lambda.t[1:(data$Ts[i])])
    target = target + dpois(0,E.N.T0.TS,log=TRUE);
    
    if (data$delta1[i] == 1) {
      target = target - dpois(0,lambda.t[data$Ts[i]],log=T) # up to date Ts-1
      lC = 1.0 - dpois(0,lambda.t[data$Ts[i]]) 
      if (lC>0) {
        target = target + log (lC); # stop just before date
      } else {
        print(paste("problem with ",data$country[i], " r :", r, " T0 :",T0," r2: ",r2, " r3:",r3))
      }
      for (u in data$Ti.lo[i]:(data$Ti.up[i]-1)) {
        target = target + log(lambda.t.u[u,data$Ts[i]]) 
      }
      target = target -log(lambda.t[data$Ts[i]])
    }
    cat(data$country[i], target,"\n")
    
   }
  
  return(target)
}

# this is new code for the actual first submission with NB
calc.logL.import.1sub.nb <- function(r, T0, rho, data, r2=NA, Tc2=NA, r3=NA,Tc3=NA) {
  target=0;
  
  #a priori 
  target=target + dexp(r,rate = 0.1,log=T)
  target=target + dexp(rho,rate = 0.1,log=T)
  
  log.exp.r.t = rep(0,data$lW)
  
  if (is.na(Tc2)) { Tc2=data$lW }
  if (is.na(Tc3)) { Tc3=data$lW }
  log.exp.r.t[T0:data$lW] = r*(seq(T0,data$lW)-T0);
  if (Tc2 < data$lW) {
    log.exp.r.t[Tc2:data$lW] = r*(Tc2-T0) + r2*(seq(Tc2,data$lW)-Tc2);
    target=target + dexp(r2,rate = 0.1,log=T)
  }
  if (Tc3 < data$lW) {
    log.exp.r.t[Tc3:data$lW] = r*(Tc2-T0) + r2*(Tc3-Tc2) + r3* (seq(Tc3,data$lW)-Tc3);
    target=target + dexp(r3,rate = 0.1,log=T)
  }
  
  exp.r.t=exp(log.exp.r.t)
  # cancel before T0
  
  if (T0>1) {exp.r.t[1:(T0-1)]=0}
  #  exp.r.t.int = (exp(r*data$M)-1)/(r*data$M)* exp.r.t 
  exp.r.t.travel = exp.r.t
  for (i in 1:(data$incub-1)) {
    exp.r.t.travel = exp.r.t.travel + c(rep(0,i),exp.r.t)[1:data$lW]
  }
  
  lambda=data$tKps * exp.r.t.travel
  # this is actual number of introduction which are sequenced
  
  #now we compute country by country the expected submissions each day
#  for (i in 1:length(data$country)) {
#    lambda[,i] = lambda[,i] %*% data$d[[i]]
#  }
  print(target)
  target_old=target
  #for the NB, we compute k log(k/k+lambda(t,u)) looping over t and u  
  for (i in 1:data$nCountry) {
    ##
    lambda.t.u = lambda[,i] * data$d[[i]]
    lambda.t=apply(lambda.t.u,2,sum)
    #extract what is required
    lambda.t.u = lambda.t.u[1:data$Ts[i],1:data$Ts[i]]
    # get the lower triangular matrix
    m = lambda.t.u[upper.tri(lambda.t.u,diag=T)]
    m=m[m>0]
    if (i==27) {
      for (u in 1:data$Ts[i])
        for (t in 1:u)
          if (lambda.t.u[t,u]>0.0) print(paste(t-1,u-1,lambda.t.u[t,u]))
    }
    target = target + sum(dnbinom(0,mu = m,size = rho,log=TRUE));
    print(target)
    
    log.prob.Ts.0=0.0
    lC=0
    if (data$delta1[i] == 1) {
      m=lambda.t.u[1:data$Ts[i],data$Ts[i]]
      m=m[m>0]
      # compute probability of 0
      log.prob.Ts.0 = sum(dnbinom(0,mu=m,size=rho,log=T))
      target = target - log.prob.Ts.0
      
      lC = 1.0 - exp(log.prob.Ts.0)
      if (lC>0) {
        target = target + log (lC) 
      } else {
        print(paste("problem with ",data$country[i], " r :", r, " T0 :",T0," r2: ",r2, " r3:",r3))
      }
      target = target + log(lambda.t.u[data$Ti[i],data$Ts[i]]) - log(lambda.t[data$Ts[i]])
    }
    #print(paste(data$country[i],r,T0,r2,Tc2,r3,Tc3,target))
    print(paste(i-1,log.prob.Ts.0,lC,target,target-target_old))
    target_old=target
    
  }
  
  return(target)
}

# this is new code for the actual first submission
calc.logL.import.1sub.cond <- function(r, T0,k, data, r2=NA, Tc2=NA, r3=NA,Tc3=NA) {
  target=0;
  
  target=target + dexp(r,rate = 0.1,log=T)
  
  log.exp.r.t = rep(0,data$lW)
  
  if (is.na(Tc2)) { Tc2=data$lW }
  if (is.na(Tc3)) { Tc3=data$lW }
  log.exp.r.t[T0:data$lW] = r*(seq(T0,data$lW)-T0);
  if (Tc2 < data$lW) {
    log.exp.r.t[Tc2:data$lW] = r*(Tc2-T0) + r2*(seq(Tc2,data$lW)-Tc2);
    target=target + dexp(r2,rate = 0.1,log=T)
  }
  if (Tc3 < data$lW) {
    log.exp.r.t[Tc3:data$lW] = r*(Tc2-T0) + r2*(Tc3-Tc2) + r3* (seq(Tc3,data$lW)-Tc3);
    target=target + dexp(r3,rate = 0.1,log=T)
  }
  
  exp.r.t=exp(log.exp.r.t)
  # cancel before T0
  
  if (T0>1) {exp.r.t[1:(T0-1)]=0}
  #  exp.r.t.int = (exp(r*data$M)-1)/(r*data$M)* exp.r.t 
  exp.r.t.travel = exp.r.t
  for (i in 1:(data$incub-1)) {
    exp.r.t.travel = exp.r.t.travel + c(rep(0,i),exp.r.t)[1:data$lW]
  }
  
  lambda=data$tKps * exp.r.t.travel
  # this is actual number of introduction which are sequenced
  
  #  log.S.lambda = -apply(lambda,2,function(x) {c(0,cumsum(x))[1:length(x)]}) # decale de 1
  #  f.T = lambda*exp(log.S.lambda) # this is probability of a given date
  #  f.T.delai = f.T * data$d # this is P(T=t) * P(D>T-t)
  
  for (i in 1:data$nCountry) {
    ##
    lambda.t.u = lambda[,i] * data$d[[i]]
    # joint 
    lambda.t=apply(lambda.t.u,2,sum)
    #print(data$country[i])
    E.N.T0.TS = sum(lambda.t[1:(data$Ts[i]-1)])
    target = target + dpois(0,E.N.T0.TS,log=TRUE);
    E.N.T0.T = sum(lambda.t)
    lC = 1.0 - dpois(0,lambda.t[data$Ts[i]]) 
      if (lC>0) {
        target = target + log (lC); # stop just before date
      } else {
        print(paste("problem with ",data$country[i], " r :", r, " T0 :",T0," r2: ",r2, " r3:",r3))
      }
    target = target + log(lambda.t.u[data$Ti[i],data$Ts[i]]) - log(lambda.t[data$Ts[i]])
    #print(paste(data$country[i],r,T0,r2,Tc2,r3,Tc3,target))
    target = target - log(1.0-dpois(0,E.N.T0.T)) # conditioning event (P (TS < T))
  }
  
  return(target)
}

# this is new code for the actual first submission with NB
calc.logL.import.1sub.nb.cond <- function(r, T0, rho, data, r2=NA, Tc2=NA, r3=NA,Tc3=NA) {
  target=0;
  
  #a priori 
  target=target + dexp(r,rate = 0.1,log=T)
  target=target + dexp(rho,rate = 0.1,log=T)
  
  log.exp.r.t = rep(0,data$lW)
  
  if (is.na(Tc2)) { Tc2=data$lW }
  if (is.na(Tc3)) { Tc3=data$lW }
  log.exp.r.t[T0:data$lW] = r*(seq(T0,data$lW)-T0);
  if (Tc2 < data$lW) {
    log.exp.r.t[Tc2:data$lW] = r*(Tc2-T0) + r2*(seq(Tc2,data$lW)-Tc2);
    target=target + dexp(r2,rate = 0.1,log=T)
  }
  if (Tc3 < data$lW) {
    log.exp.r.t[Tc3:data$lW] = r*(Tc2-T0) + r2*(Tc3-Tc2) + r3* (seq(Tc3,data$lW)-Tc3);
    target=target + dexp(r3,rate = 0.1,log=T)
  }
  
  exp.r.t=exp(log.exp.r.t)
  # cancel before T0
  
  if (T0>1) {exp.r.t[1:(T0-1)]=0}
  #  exp.r.t.int = (exp(r*data$M)-1)/(r*data$M)* exp.r.t 
  exp.r.t.travel = exp.r.t
  for (i in 1:(data$incub-1)) {
    exp.r.t.travel = exp.r.t.travel + c(rep(0,i),exp.r.t)[1:data$lW]
  }
  
  lambda=data$tKps * exp.r.t.travel
  # this is actual number of introduction which are sequenced
  
  #now we compute country by country the expected submissions each day
  #  for (i in 1:length(data$country)) {
  #    lambda[,i] = lambda[,i] %*% data$d[[i]]
  #  }
  
  #for the NB, we compute k log(k/k+lambda(t,u)) looping over t and u  
  for (i in 1:data$nCountry) {
    ##
    lambda.t.u = lambda[,i] * data$d[[i]]
    lambda.t=apply(lambda.t.u,2,sum)
    # this is the conditional part P(Ts <= T)
    m = lambda.t.u[upper.tri(lambda.t.u)]
    m = m[m>0]
    target = target - log(1.0-exp(sum(dnbinom(0,mu = m,size = rho,log=TRUE))))
    # then probability up to Ts
    lambda.t.u.x = lambda.t.u[1:(data$Ts[i]-1),1:(data$Ts[i]-1)]
    # get the lower triangular matrix
    m = lambda.t.u.x[upper.tri(lambda.t.u.x)]
    m=m[m>0]
    target = target + sum(dnbinom(0,mu = m,size = rho,log=TRUE));

    m=lambda.t.u[1:data$Ts[i],data$Ts[i]]
    m=m[m>0]
      # compute probability of 0
    log.prob.Ts.0 = sum(dnbinom(0,mu=m,size=rho,log=T))
      
    lC = 1.0 - exp(log.prob.Ts.0)
    if (lC>0) {
        target = target + log (lC) 
      } else {
        print(paste("problem with ",data$country[i], " r :", r, " T0 :",T0," r2: ",r2, " r3:",r3))
      }
    target = target + log(lambda.t.u[data$Ti[i],data$Ts[i]]) - log(lambda.t[data$Ts[i]])
      # print(paste(data$country[i],r,T0,r2,Tc2,r3,Tc3,target))
  }
  
  return(target)
}


calc.logL.import.auto <- function(r, T0,data, r2=NA, Tc2=NA, r3=NA,Tc3=NA) {
  target=0;
  
  target=target + dexp(r,rate = 0.1,log=T)
  
  log.exp.r.t = rep(0,data$lW)
  
  if (is.na(Tc2)) { Tc2=data$lW }
  if (is.na(Tc3)) { Tc3=data$lW }
  log.exp.r.t[T0:data$lW] = r*(seq(T0,data$lW)-T0);
  if (Tc2 < data$lW) {
    log.exp.r.t[Tc2:data$lW] = r*(Tc2-T0) + r2*(seq(Tc2,data$lW)-Tc2);
    target=target + dexp(r2,rate = 0.1,log=T)
  }
  if (Tc3 < data$lW) {
    log.exp.r.t[Tc3:data$lW] = r*(Tc2-T0) + r2*(Tc3-Tc2) + r3* (seq(Tc3,data$lW)-Tc3);
    target=target + dexp(r3,rate = 0.1,log=T)
  }
  
  exp.r.t=exp(log.exp.r.t)
  
  exp.r.t.int = (exp(r*data$M)-1)/(r*data$M)* exp.r.t 
  exp.r.t.travel = exp.r.t.int
  for (i in 1:(data$incub-1)) {
    exp.r.t.travel = exp.r.t.travel + c(rep(0,i),exp.r.t.int)[1:data$lW]
  }
  
  
  # nul au meme endroit que predcedement
  lambda=data$tKps * exp.r.travel
  log.S.lambda = -apply(lambda,2,function(x) {c(0,cumsum(x))[1:length(x)]}) # decale de 1
  #mu
  lambda.first=data$tp.over.R * exp.r.t.travel
  log.S.first = -apply(lambda.first,2,function(x) {c(0,cumsum(x))[1:length(x)]}) # decale de 1
  f.first= lambda.first * exp(log.S.first)
  #  f.first = f.first* exp.r.t
  
  for (i in 1:data$nCountry) {
    # convolution - this part is precomputed, could be exponentiated allright
    muI = data$r[i,,] %*% f.first[,i,drop=T] 
    muI = muI * data$s[i,]
    log.Si.mu = -c(0,cumsum(muI));
    # joint 
    target= target + log.S.lambda[data$T[i],i] + log.Si.mu[data$T[i]]
    if (data$delta1[i] == 1) {
      target = target + log (lambda[data$T[i],i] + muI[data$T[i]]) 
    } else {
      target = target - lambda[data$T[i],i] - muI[data$T[i]] 
    }
    #    print(paste(i," ",target," ",log.S.lambda[i,data$T[i]] ," ", log.Si.mu[data$T[i]] ," ",lambda[i,data$T[i]]," ", mu[data$T[i]]))
  }
  target
}

calc.logL.import.auto.truncate <- function(r, T0,data) {
  target=0;
  exp.r.t = exp(r*data$M*(seq(1,data$lW)-T0));
  exp.r.t[exp.r.t<1.0]=0;
  exp.r.t.int = (exp(r*data$M)-1)/(r*data$M)* exp.r.t 
  
  # nul au meme endroit que predcedement
  lambda=data$tKps * exp.r.t.int
  log.S.lambda = -apply(data$tKps * exp.r.t.int,2,function(x) {c(0,cumsum(x))[1:length(x)]}) # decale de 1
  #mu
  log.S.first = -apply(data$tp.over.R * exp.r.t.int,2,function(x) {c(0,cumsum(x))[1:length(x)]}) # decale de 1
  f.first= data$tp.over.R * exp.r.t.int * exp(log.S.first)
  #  f.first = f.first* exp.r.t
  
  for (i in 1:data$nCountry) {
    # convolution - this part is precomputed, could be exponentiated allright
    muI = data$r[i,,] %*% f.first[,i,drop=T] 
    muI = muI * data$s[i,]
    log.Si.mu = -c(0,cumsum(muI));
    # joint 
    target= target + log.S.lambda[data$T[i],i] + log.Si.mu[data$T[i]] -log(1.0 - exp(log.S.lambda[data$lW,i]+log.Si.mu[data$lW]))
    if (data$delta1[i] == 1) {
      target = target + log (lambda[data$T[i],i] + muI[data$T[i]]) 
    } else {
      target = target - lambda[data$T[i],i] - muI[data$T[i]] 
    }
    #    print(paste(i," ",target," ",log.S.lambda[i,data$T[i]] ," ", log.Si.mu[data$T[i]] ," ",lambda[i,data$T[i]]," ", mu[data$T[i]]))
  }
  target=target + dexp(r,rate = 0.1,log=T)
  target
}

calc.logL.all.import.auto <- function(r, T0,data, r2=NA, Tc2=NA, r3=NA,Tc3=NA) {
  target=0;

  target=target + dexp(r,rate = 0.1,log=T)
  
  log.exp.r.t = rep(0,data$lW)
  
  if (is.na(Tc2)) { Tc2=data$lW }
  if (is.na(Tc3)) { Tc3=data$lW }
  log.exp.r.t[T0:data$lW] = r*(seq(T0,data$lW)-T0);
  if (Tc2 < data$lW) {
    log.exp.r.t[Tc2:data$lW] = r*(Tc2-T0) + r2*(seq(Tc2,data$lW)-Tc2);
    target=target + dexp(r2,rate = 0.1,log=T)
  }
  if (Tc3 < data$lW) {
    log.exp.r.t[Tc3:data$lW] = r*(Tc2-T0) + r2*(Tc3-Tc2) + r3* (seq(Tc3,data$lW)-Tc3);
    target=target + dexp(r3,rate = 0.1,log=T)
  }
  
  exp.r.t=exp(log.exp.r.t)
  
  exp.r.t.int = (exp(r*data$M)-1)/(r*data$M)* exp.r.t 
  exp.r.t.travel = exp.r.t.int
  for (i in 1:(data$incub-1)) {
    exp.r.t.travel = exp.r.t.travel + c(rep(0,i),exp.r.t.int)[1:data$lW]
  }  
  # nul au meme endroit que predcedement
  lambda=data$tKps * exp.r.t.travel
  log.S.lambda = -apply(lambda,2,function(x) {c(0,cumsum(x))[1:length(x)]}) # decale de 1
  #mu
  lambda.first=data$tp.over.R * exp.r.t.travel
#  log.S.first = -apply(lambda.first,2,function(x) {c(0,cumsum(x))[1:length(x)]}) # decale de 1
#  f.first= lambda.first * exp(log.S.first)
  #  f.first = f.first* exp.r.t
  
  for (i in 1:data$nCountry) {
    # convolution - this part is precomputed, could be exponentiated allright
    muI = data$r[i,,] %*% lambda.first[,i,drop=T] 
    muI = muI * data$s[i,]
    log.Si.mu = -c(0,cumsum(muI));
    # joint 
    target= target + log.S.lambda[data$T[i],i] + log.Si.mu[data$T[i]]
    if (data$delta1[i] == 1) {
      target = target + log (1-exp(-lambda[data$T[i],i] - muI[data$T[i]])) 
    } else {
      target = target - lambda[data$T[i],i] - muI[data$T[i]] 
    }
    #    print(paste(i," ",target," ",log.S.lambda[i,data$T[i]] ," ", log.Si.mu[data$T[i]] ," ",lambda[i,data$T[i]]," ", mu[data$T[i]]))
  }
  target
}

calc.logL.all.import.auto.truncate <- function(r, T0,data) {
  target=0;
  exp.r.t = exp(r*data$M*(seq(1,data$lW)-T0));
  exp.r.t[exp.r.t<1.0]=0;
  exp.r.t.int = (exp(r*data$M)-1)/(r*data$M)* exp.r.t 
  
  # nul au meme endroit que predcedement
  lambda=data$tKps * exp.r.t.int
  log.S.lambda = -apply(data$tKps * exp.r.t.int,2,function(x) {c(0,cumsum(x))[1:length(x)]}) # decale de 1
  #mu
  lambda.first=data$tp.over.R * exp.r.t.int
#  log.S.first = -apply(data$tp.over.R * exp.r.t.int,2,function(x) {c(0,cumsum(x))[1:length(x)]}) # decale de 1
#  f.first= data$tp.over.R * exp.r.t.int * exp(log.S.first)
  #  f.first = f.first* exp.r.t
  
  for (i in 1:data$nCountry) {
    # convolution - this part is precomputed, could be exponentiated allright
    muI = data$r[i,,] %*% lambda.first[,i,drop=T] 
    muI = muI * data$s[i,]
    log.Si.mu = -c(0,cumsum(muI));
    # joint 
    target= target + log.S.lambda[data$T[i],i] + log.Si.mu[data$T[i]] -log(1.0 - exp(log.S.lambda[data$lW,i]+log.Si.mu[data$lW]))
    if (data$delta1[i] == 1) {
      target = target + log (1-exp(-lambda[data$T[i],i] - muI[data$T[i]])) 
    } else {
      target = target - lambda[data$T[i],i] - muI[data$T[i]] 
    }
    #    print(paste(i," ",target," ",log.S.lambda[i,data$T[i]] ," ", log.Si.mu[data$T[i]] ," ",lambda[i,data$T[i]]," ", mu[data$T[i]]))
  }
  target=target + dexp(r,rate = 0.1,log=T)
  target
}



MCMC.import <- function(param, init,data,sd=NULL,R=100, ll="full") {
  require(TruncatedNormal)
  
 if (ll=="1.sub"){
    calc.logL = calc.logL.import.1sub
    print("computing likelihood for submission")
  } else if (ll=="1.sub.cond"){
    calc.logL = calc.logL.import.1sub.cond
    print("computing likelihood for submission")
  } else if (ll=="1.sub.nb"){
    calc.logL = calc.logL.import.1sub.nb
    print("computing likelihood NB for submission")
  }
  
  # printout
  interv = floor(R/10)
  # results
  res=matrix(rep(NA,(length(init)+1)*R),ncol = length(init)+1)
  colnames(res)=c(names(init),"lp__")
  accept=matrix(rep(NA,length(init)*R),ncol = length(init))
  colnames(accept)=names(init)
  
  lW = length(data$W)
  # param : parameters for optimisation; list
  r.cur=init$r
  col.r=which(colnames(res)=="r")
  do.r=0
  if("r" %in% param) {do.r=1; print("optimizing r")} else {print(paste("r fixed at ",r.cur))}
  
  T0.cur=init$T0
  col.T0=which(colnames(res)=="T0")
  do.T0=0
  if("T0" %in% param) {
    do.T0=1; 
    print(paste0("optimizing T0 between ", format(data$date.start) ,"and ",format(data$date.start+data$T0.max)))} else {print(paste("T0 fixed at ",T0.cur))
  }
  
  k.cur = init$k
  col.k = which(colnames(res)=="k")
  do.k=0
  if ("k" %in% param){
    do.k=1
    print("optimizing k")} 
  else {
    print(paste("k not used"))
    if (ll == "1.sub.nb") {stop("NB model requires k parameter")}
  }

    # Tcp.cur=init$Tcp
  # col.Tcp=which(colnames(res)=="Tcp")
  # do.Tcp=0
  # if("Tcp" %in% param) {
  #   do.Tcp=1; 
  #   print(paste("optimizing Tcp between ", as.Date("2020-08-01")+80, " and ",as.Date("2020-08-01")+lW-15))
  # } else {
  #   Tcp.cur=NA
  #   print(paste("Tcp not optimized. set to ",Tcp.cur))
  # }

  Tcp2.cur=init$Tcp2
  r2.cur=init$r2
  col.r2=which(colnames(res)=="r2")
  col.Tcp2=which(colnames(res)=="Tcp2")
  do.r2=0
    if("r2" %in% param) {
      do.r2=1; 
      print("optimizing r2"); 
      r2.cur=init$r2; 
      Tcp2.cur=init$Tcp2;
      print(paste0("set Tcp2 to ",Tcp2.cur));
    } else {
      r2.cur=NA
      Tcp2.cur=NA
      print(paste("r2 not used. Set to ",r2.cur))
    }
  
  if (ll == "LD") {
    phi.cur=r2.cur/r.cur
    print(paste("set phi.cur for LD to",phi.cur))
  }

  Tcp3.cur=init$Tcp3
  r3.cur=init$r3
  col.r3=which(colnames(res)=="r3")
  col.Tcp3=which(colnames(res)=="Tcp3")
  do.r3=0
  if("r3" %in% param) {
    if (do.r2 == 0) stop("cannot optimize r3 without r2")
    do.r3=1; 
    print("optimizing r3"); 
    r3.cur=init$r3; 
    Tcp3.cur=init$Tcp3;
    print(paste0("reset Tcp3 to ",Tcp3.cur));
  } else if (ll == "LD") {
    r3.cur=r.cur
    Tcp3.cur=init$Tcp3;
    print(paste0("LD - set Tcp3 to ",Tcp3.cur));
  } else {
    r3.cur=NA
    Tcp3.cur=NA
    print(paste("r3 not used. Set to ",r3.cur))
  }
  
  col.lp=which(colnames(res)=="lp__")
  
  # init M
  M.cur=data$M
  if (do.r2) {M.cur[-(1:Tcp2.cur)]=r2.cur/r.cur}
  if (do.r3) {M.cur[-(1:Tcp3.cur)]=r3.cur/r.cur}
  if (ll=="LD") {
    M.cur[-(1:Tcp2.cur)]=phi.cur
    M.cur[-(1:Tcp3.cur)]=1
  }
  print(paste("M ",M.cur))
  data$M=M.cur
  #init logL
  logL.cur=calc.logL(r.cur,T0.cur,k.cur,data, r2.cur,Tcp2.cur, r3.cur,Tcp3.cur)
  
  for (i in (1:R)) {
    if (!(i%%interv)) print(paste("Sampling ",floor(i/R*100),"%"))
    #r
    res[i,col.r]=r.cur
    accept[i,col.r]=0
  if(ll=="LD") {
    res[i,col.r3]=r.cur
    res[i,col.Tcp3]=Tcp3.cur
  }
    if (do.r) {
      r.new = rtnorm(1,mu = r.cur,sd = sd$r,lb = 0,ub = 10)
      
      log.P.old.new = dnorm(r.cur,mean = r.new,sd=sd$r,log=T) - 
        pnorm(0,mean=r.new,sd=sd$r,lower.tail = F,log.p = T)
      log.P.new.old = dnorm(r.new,mean = r.cur,sd=sd$r,log=T) - 
        pnorm(0,mean=r.cur,sd=sd$r,lower.tail = F,log.p = T)
      if (ll=="LD") {
        r3.cur=r.new
      }
      logL.new=calc.logL(r.new,T0.cur,k.cur,data,r2.cur,Tcp2.cur,r3.cur,Tcp3.cur)

      if (log(runif(1)) < logL.new - logL.cur + log.P.old.new - log.P.new.old) {
        logL.cur=logL.new
        res[i,col.r]=r.new
        r.cur=r.new
        if (ll=="LD") {
          r3.cur=r.new
          res[i,col.r3]=r.new
        }
        accept[i,col.r]=1
      } else {
        if (ll=="LD") r3.cur=r.cur
      }
    }
    
    #k
    res[i,col.k]=k.cur
    accept[i,col.k]=0
    if (do.k) {
      k.new = rtnorm(1,mu = k.cur,sd = sd$k,lb = 0,ub = 10)
      
      log.P.old.new = dnorm(k.cur,mean = k.new,sd=sd$k,log=T) - 
        pnorm(0,mean=k.new,sd=sd$k,lower.tail = F,log.p = T)
      log.P.new.old = dnorm(k.new,mean = k.cur,sd=sd$k,log=T) - 
        pnorm(0,mean=k.cur,sd=sd$k,lower.tail = F,log.p = T)
      logL.new=calc.logL(r.cur,T0.cur,k.new, data,r2.cur,Tcp2.cur,r3.cur,Tcp3.cur)
      
      if (log(runif(1)) < logL.new - logL.cur + log.P.old.new - log.P.new.old) {
        logL.cur=logL.new
        res[i,col.k]=k.new
        k.cur=k.new
        accept[i,col.k]=1
      } 
    }
    

        # T0
    res[i,col.T0]=T0.cur
    accept[i,col.T0]=0
    if (do.T0) {
      T0.new.candidate = c(T0.cur -(1:sd$T0), T0.cur + 1:sd$T0) 
      T0.new.candidate = T0.new.candidate[T0.new.candidate>=1 & T0.new.candidate <data$T0.max] 
      T0.new = sample(T0.new.candidate,1)
      T0.cur.candidate = c(T0.new -(1:sd$T0), T0.new + 1:sd$T0) 
      T0.cur.candidate = T0.cur.candidate[T0.cur.candidate>=1 & T0.cur.candidate <data$T0.max] 
      
      P.old.new = 1/length(T0.cur.candidate)
      P.new.old = 1/length(T0.new.candidate)
      
      logL.new=calc.logL(r.cur,T0.new,k.cur,data,r2.cur,Tcp2.cur,r3.cur,Tcp3.cur)
      
      if (log(runif(1)) < logL.new +log(P.old.new)- logL.cur - log(P.new.old)) {
        logL.cur=logL.new
        res[i,col.T0]=T0.new
        T0.cur=T0.new
        accept[i,col.T0]=1
      } 
    }
    
    res[i,col.r2]=r2.cur
    res[i,col.Tcp2]=Tcp2.cur
    accept[i,col.r2]=0
      
    if (do.r2) {
        M.cur=data$M

        if (ll=="LD") {
          phi.cur=r2.cur/r.cur
          phi.new = runif(1,min = max(0,phi.cur-sd$phi), max = min(phi.cur+sd$phi,1))
          length2.new = min(phi.cur+sd$phi,1)-max(0,phi.cur-sd$phi)
          length2.cur = min(phi.new+sd$phi,1)-max(0,phi.new-sd$phi)
          log.P.old.new = log(1/length(length2.cur))
          log.P.new.old = log(1/length(length2.new))
          
          M.new=rep(1,lW)
          M.new[-(1:Tcp2.cur)]=phi.new
          M.new[-(1:Tcp3.cur)]=1
        
          r2.new=phi.new*r.cur
          data$M=M.new
        } else {  
          r2.new = rtnorm(1,mu=r2.cur,sd=sd$r2,lb=-1,ub=10,method="fast")
          
          log.P.old.new = dnorm(r2.cur,mean = r2.new,sd=sd$r2,log=T) - 
            pnorm(-1,mean=r2.new,sd=sd$r2,lower.tail = F,log.p = T)
          log.P.new.old = dnorm(r2.new,mean = r2.cur,sd=sd$r2,log=T) - 
            pnorm(-1,mean=r2.cur,sd=sd$r2,lower.tail = F,log.p = T)
          
          M.new=rep(1,lW)
          M.new[-(1:Tcp2.cur)]=r2.new/r.cur
          if (do.r3 | ll=="LD") {
            M.new[-(1:Tcp3.cur)]=1
          }
          data$M=M.new
        }
        
        logL.new=calc.logL(r.cur,T0.cur,k.cur,data,r2.new,Tcp2.cur,r3.cur,Tcp3.cur)
        
        if (log(runif(1)) < logL.new +log.P.old.new- logL.cur - log.P.new.old) {
          logL.cur=logL.new
          res[i,col.r2]=r2.new
          r2.cur=r2.new
          accept[i,col.r2]=1
        } else {
          data$M=M.cur
        }
      }
    
    
    res[i,col.r3]=r3.cur
    res[i,col.Tcp3]=Tcp3.cur
    accept[i,col.r3]=0
    
    if (do.r3) {
      M.cur=data$M
      
      r3.new = rtnorm(1,mu=r3.cur,sd=sd$r3,lb=0,ub=10,method="fast")
      
      log.P.old.new = dnorm(r3.cur,mean = r3.new,sd=sd$r3,log=T) - 
        pnorm(0,mean=r3.new,sd=sd$r3,lower.tail = F,log.p = T)
      log.P.new.old = dnorm(r3.new,mean = r3.cur,sd=sd$r3,log=T) - 
        pnorm(0,mean=r3.cur,sd=sd$r3,lower.tail = F,log.p = T)
      
      M.new=rep(1,lW)
      M.new[-(1:Tcp2.cur)]=r2.cur/r.cur
      M.new[-(1:Tcp3.cur)]=r3.new/r.cur
      data$M=M.new
      
      logL.new=calc.logL(r.cur,T0.cur,k.cur,data,r2.cur,Tcp2.cur,r3.new,Tcp3.cur)
      
      if (log(runif(1)) < logL.new +log.P.old.new- logL.cur - log.P.new.old) {
        logL.cur=logL.new
        res[i,col.r3]=r3.new
        r3.cur=r3.new
        accept[i,col.r3]=1
      } else {
        data$M=M.cur
      }
    }
    
    # #Tcp
    # res[i,col.Tcp]=Tcp.cur
    # accept[i,col.Tcp]=0
    # 
    # if(do.Tcp) {
    #   Tcp.new.candidate = c(Tcp.cur -(1:sd$Tcp), Tcp.cur + 1:sd$Tcp) 
    #   Tcp.new.candidate = Tcp.new.candidate[Tcp.new.candidate>=81 & Tcp.new.candidate <lW-15] 
    #   Tcp.new = sample(Tcp.new.candidate,1)
    #   Tcp.cur.candidate = c(Tcp.new -(1:sd$Tcp), Tcp.new + 1:sd$Tcp) 
    #   Tcp.cur.candidate = Tcp.cur.candidate[Tcp.cur.candidate>=81 & Tcp.cur.candidate <lW-15] 
    #   
    #   M.cur = data$M
    #   M.new=rep(1,lW)
    #   M.new[-(1:Tcp.new)]=r2.cur/r.cur
    #   data$M=M.new
    #   
    #   logL.new=calc.logL(r.cur,T0.cur,data,r2.cur,Tcp.new)
    #   
    #   P.old.new = 1/length(Tcp.cur.candidate)
    #   P.new.old = 1/length(Tcp.new.candidate)
    #   
    #   if (log(runif(1)) < logL.new +log(P.old.new)- logL.cur - log(P.new.old)) {
    #     logL.cur=logL.new
    #     res[i,col.Tcp]=Tcp.new
    #     Tcp.cur=Tcp.new
    #     accept[i,col.Tcp]=1
    #   } else {
    #     data$M=M.cur
    #   }   
    # }
    # lp
    res[i,col.lp]=logL.cur
#    print(res[i,])
    
   }
  
  list(res=res,accept=apply(accept,2,sum,na.rm=T)/R)
}

MCMC.import.auto <- function(param, init,data,sd=NULL,R=100, ll="full") {
  require(TruncatedNormal)
  
  # choose likelihood
  if (ll == "full" | ll == "LD") {
    calc.logL=calc.logL.all.import.auto
    print("computing full likelihood")
  } else if (ll=="truncate"){
    calc.logL=calc.logL.import.auto.truncate
    print("computing likelihood for truncated sample")
  } else if (ll == "melange") {
    calc.logL=calc.logL.import.auto.melange
    print("computing likelihood for before sample")
  }
  
  # printout
  interv = floor(R/10)
  # results
  res=matrix(rep(NA,(length(init)+1)*R),ncol = length(init)+1)
  colnames(res)=c(names(init),"lp__")
  accept=matrix(rep(NA,length(init)*R),ncol = length(init))
  colnames(accept)=names(init)
  
  lW = length(data$W)
  # param : parameters for optimisation; list
  r.cur=init$r
  col.r=which(colnames(res)=="r")
  do.r=0
  if("r" %in% param) {do.r=1; print("optimizing r")} else {print(paste("r fixed at ",r.cur))}
  
  T0.cur=init$T0
  col.T0=which(colnames(res)=="T0")
  do.T0=0
  if("T0" %in% param) {do.T0=1; print("optimizing T0")} else {print(paste("T0 fixed at ",T0.cur))}
  
  # Tcp.cur=init$Tcp
  # col.Tcp=which(colnames(res)=="Tcp")
  # do.Tcp=0
  # if("Tcp" %in% param) {
  #   do.Tcp=1; 
  #   print(paste("optimizing Tcp between ", as.Date("2020-08-01")+80, " and ",as.Date("2020-08-01")+lW-15))
  # } else {
  #   Tcp.cur=NA
  #   print(paste("Tcp not optimized. set to ",Tcp.cur))
  # }
  
  Tcp2.cur=init$Tcp2
  r2.cur=init$r2
  col.r2=which(colnames(res)=="r2")
  col.Tcp2=which(colnames(res)=="Tcp2")
  do.r2=0
  if("r2" %in% param) {
    do.r2=1; 
    print("optimizing r2"); 
    r2.cur=init$r2; 
    Tcp2.cur=init$Tcp2;
    print(paste0("set Tcp2 to ",Tcp2.cur));
  } else {
    r2.cur=NA
    Tcp2.cur=NA
    print(paste("r2 not used. Set to ",r2.cur))
  }
  
  if (ll == "LD") {
    phi.cur=r2.cur/r.cur
    print(paste("set phi.cur for LD to",phi.cur))
  }
  
  Tcp3.cur=init$Tcp3
  r3.cur=init$r3
  col.r3=which(colnames(res)=="r3")
  col.Tcp3=which(colnames(res)=="Tcp3")
  do.r3=0
  if("r3" %in% param) {
    if (do.r2 == 0) stop("cannot optimize r3 without r2")
    do.r3=1; 
    print("optimizing r3"); 
    r3.cur=init$r3; 
    Tcp3.cur=init$Tcp3;
    print(paste0("reset Tcp3 to ",Tcp3.cur));
  } else if (ll == "LD") {
    r3.cur=r.cur
    Tcp3.cur=init$Tcp3;
    print(paste0("LD - set Tcp3 to ",Tcp3.cur));
  } else {
    r3.cur=NA
    Tcp3.cur=NA
    print(paste("r3 not used. Set to ",r3.cur))
  }
  
  col.lp=which(colnames(res)=="lp__")
  
  # init M
  M.cur=data$M
  if (do.r2) {M.cur[-(1:Tcp2.cur)]=r2.cur/r.cur}
  if (do.r3) {M.cur[-(1:Tcp3.cur)]=r3.cur/r.cur}
  if (ll=="LD") {
    M.cur[-(1:Tcp2.cur)]=phi.cur
    M.cur[-(1:Tcp3.cur)]=1
  }
  print(paste("M ",M.cur))
  data$M=M.cur
  #init logL
  logL.cur=calc.logL(r.cur,T0.cur,data, r2.cur,Tcp2.cur, r3.cur,Tcp3.cur)
  
  for (i in (1:R)) {
    if (!(i%%interv)) print(paste("Sampling ",floor(i/R*100),"%"))
    #r
#    print(paste("r",r.cur,"T0",T0.cur,"r2",r2.cur,"lp",logL.cur))
    res[i,col.r]=r.cur
    accept[i,col.r]=0
    if(ll=="LD") {
      res[i,col.r3]=r.cur
      res[i,col.Tcp3]=Tcp3.cur
    }
    if (do.r) {
      r.new = rtnorm(1,mu = r.cur,sd = sd$r,lb = 0,ub = 10)
      
      log.P.old.new = dnorm(r.cur,mean = r.new,sd=sd$r,log=T) - 
        pnorm(0,mean=r.new,sd=sd$r,lower.tail = F,log.p = T)
      log.P.new.old = dnorm(r.new,mean = r.cur,sd=sd$r,log=T) - 
        pnorm(0,mean=r.cur,sd=sd$r,lower.tail = F,log.p = T)
      if (ll=="LD") {
        r3.cur=r.new
      }
      logL.new=calc.logL(r.new,T0.cur,data,r2.cur,Tcp2.cur,r3.cur,Tcp3.cur)
      
      if (log(runif(1)) < logL.new - logL.cur + log.P.old.new - log.P.new.old) {
        logL.cur=logL.new
        res[i,col.r]=r.new
        r.cur=r.new
        if (ll=="LD") {
          r3.cur=r.new
          res[i,col.r3]=r.new
        }
        accept[i,col.r]=1
      } else {
        if (ll=="LD") r3.cur=r.cur
      }
    }
    
    # T0
    res[i,col.T0]=T0.cur
    accept[i,col.T0]=0
    if (do.T0) {
      T0.new.candidate = c(T0.cur -(1:sd$T0), T0.cur + 1:sd$T0) 
      T0.new.candidate = T0.new.candidate[T0.new.candidate>=1 & T0.new.candidate <52] 
      T0.new = sample(T0.new.candidate,1)
      T0.cur.candidate = c(T0.new -(1:sd$T0), T0.new + 1:sd$T0) 
      T0.cur.candidate = T0.cur.candidate[T0.cur.candidate>=1 & T0.cur.candidate <52] 
      
      P.old.new = 1/length(T0.cur.candidate)
      P.new.old = 1/length(T0.new.candidate)
      
      logL.new=calc.logL(r.cur,T0.new,data,r2.cur,Tcp2.cur,r3.cur,Tcp3.cur)
      
      if (log(runif(1)) < logL.new +log(P.old.new)- logL.cur - log(P.new.old)) {
        logL.cur=logL.new
        res[i,col.T0]=T0.new
        T0.cur=T0.new
        accept[i,col.T0]=1
      } 
    }
    
    res[i,col.r2]=r2.cur
    res[i,col.Tcp2]=Tcp2.cur
    accept[i,col.r2]=0
    
    if (do.r2) {
      M.cur=data$M
      
      if (ll=="LD") {
        phi.cur=r2.cur/r.cur
        phi.new = runif(1,min = max(0,phi.cur-sd$phi), max = min(phi.cur+sd$phi,1))
        length2.new = min(phi.cur+sd$phi,1)-max(0,phi.cur-sd$phi)
        length2.cur = min(phi.new+sd$phi,1)-max(0,phi.new-sd$phi)
        log.P.old.new = log(1/length(length2.cur))
        log.P.new.old = log(1/length(length2.new))
        
        M.new=rep(1,lW)
        M.new[-(1:Tcp2.cur)]=phi.new
        M.new[-(1:Tcp3.cur)]=1
        
        r2.new=phi.new*r.cur
        data$M=M.new
      } else {  
        r2.new = rtnorm(1,mu=r2.cur,sd=sd$r2,lb=-1,ub=10,method="fast")
        
        log.P.old.new = dnorm(r2.cur,mean = r2.new,sd=sd$r2,log=T) - 
          pnorm(-1,mean=r2.new,sd=sd$r2,lower.tail = F,log.p = T)
        log.P.new.old = dnorm(r2.new,mean = r2.cur,sd=sd$r2,log=T) - 
          pnorm(-1,mean=r2.cur,sd=sd$r2,lower.tail = F,log.p = T)
        
        M.new=rep(1,lW)
        M.new[-(1:Tcp2.cur)]=r2.new/r.cur
        if (do.r3 | ll=="LD") {
          M.new[-(1:Tcp3.cur)]=1
        }
        data$M=M.new
      }
      
      logL.new=calc.logL(r.cur,T0.cur,data,r2.new,Tcp2.cur,r3.cur,Tcp3.cur)
      
      if (log(runif(1)) < logL.new +log.P.old.new- logL.cur - log.P.new.old) {
        logL.cur=logL.new
        res[i,col.r2]=r2.new
        r2.cur=r2.new
        accept[i,col.r2]=1
      } else {
        data$M=M.cur
      }
    }
    
    
    res[i,col.r3]=r3.cur
    res[i,col.Tcp3]=Tcp3.cur
    accept[i,col.r3]=0
    
    if (do.r3) {
      M.cur=data$M
      
      r3.new = rtnorm(1,mu=r3.cur,sd=sd$r3,lb=0,ub=10,method="fast")
      
      log.P.old.new = dnorm(r3.cur,mean = r3.new,sd=sd$r3,log=T) - 
        pnorm(0,mean=r3.new,sd=sd$r3,lower.tail = F,log.p = T)
      log.P.new.old = dnorm(r3.new,mean = r3.cur,sd=sd$r3,log=T) - 
        pnorm(0,mean=r3.cur,sd=sd$r3,lower.tail = F,log.p = T)
      
      M.new=rep(1,lW)
      M.new[-(1:Tcp2.cur)]=r2.cur/r.cur
      M.new[-(1:Tcp3.cur)]=r3.new/r.cur
      data$M=M.new
      
      logL.new=calc.logL(r.cur,T0.cur,data,r2.cur,Tcp2.cur,r3.new,Tcp3.cur)
      
      if (log(runif(1)) < logL.new +log.P.old.new- logL.cur - log.P.new.old) {
        logL.cur=logL.new
        res[i,col.r3]=r3.new
        r3.cur=r3.new
        accept[i,col.r3]=1
      } else {
        data$M=M.cur
      }
    }
    
    # #Tcp
    # res[i,col.Tcp]=Tcp.cur
    # accept[i,col.Tcp]=0
    # 
    # if(do.Tcp) {
    #   Tcp.new.candidate = c(Tcp.cur -(1:sd$Tcp), Tcp.cur + 1:sd$Tcp) 
    #   Tcp.new.candidate = Tcp.new.candidate[Tcp.new.candidate>=81 & Tcp.new.candidate <lW-15] 
    #   Tcp.new = sample(Tcp.new.candidate,1)
    #   Tcp.cur.candidate = c(Tcp.new -(1:sd$Tcp), Tcp.new + 1:sd$Tcp) 
    #   Tcp.cur.candidate = Tcp.cur.candidate[Tcp.cur.candidate>=81 & Tcp.cur.candidate <lW-15] 
    #   
    #   M.cur = data$M
    #   M.new=rep(1,lW)
    #   M.new[-(1:Tcp.new)]=r2.cur/r.cur
    #   data$M=M.new
    #   
    #   logL.new=calc.logL(r.cur,T0.cur,data,r2.cur,Tcp.new)
    #   
    #   P.old.new = 1/length(Tcp.cur.candidate)
    #   P.new.old = 1/length(Tcp.new.candidate)
    #   
    #   if (log(runif(1)) < logL.new +log(P.old.new)- logL.cur - log(P.new.old)) {
    #     logL.cur=logL.new
    #     res[i,col.Tcp]=Tcp.new
    #     Tcp.cur=Tcp.new
    #     accept[i,col.Tcp]=1
    #   } else {
    #     data$M=M.cur
    #   }   
    # }
    # lp
    res[i,col.lp]=logL.cur
    #    print(res[i,])
    
  }
  
  list(res=res,accept=apply(accept,2,sum,na.rm=T)/R)
}

calc.F.country.import <- function(res,data) {
  
  lW=data$lW  
  nCountry=data$nCountry  
  nWeeks=data$nWeeks
  
  col.r=which(colnames(res)=="r")
  col.T0=which(colnames(res)=="T0")
  col.r2=which(colnames(res)=="r2")
  col.Tcp2=which(colnames(res)=="Tcp2")
  col.r3=which(colnames(res)=="r3")
  col.Tcp3=which(colnames(res)=="Tcp3")
  
  if (is.na(res[1,col.r2])) res[,col.r2]=res[,col.r]
  if (is.na(res[1,col.Tcp2])) res[,col.Tcp2]=data$lW-5
  if (is.na(res[1,col.r3])) res[,col.r3]=res[,col.r2]
  if (is.na(res[1,col.Tcp3])) res[,col.Tcp3]=data$lW-3
  
  col.lp=which(colnames(res)=="lp__")
  
  R = dim(res)[1] # nombre de simulations
  
  inc.lo = matrix(NA,nrow = 1,ncol = data$lW)
  inc.med = matrix(NA,nrow = 1,ncol = data$lW)
  inc.up = matrix(NA,nrow = 1,ncol = data$lW)  
  inc.mean = matrix(NA,nrow = 1,ncol = data$lW)  
  
  inc.lo.norm = matrix(NA,nrow = 1,ncol = data$lW)
  inc.med.norm = matrix(NA,nrow = 1,ncol = data$lW)
  inc.up.norm = matrix(NA,nrow = 1,ncol = data$lW)  
  inc.mean.norm = matrix(NA,nrow = 1,ncol = data$lW)  
  
  FSeq.lo = matrix(NA,nrow = nCountry,ncol = data$lW)
  FSeq.med = matrix(NA,nrow = nCountry,ncol = data$lW)
  FSeq.up = matrix(NA,nrow = nCountry,ncol = data$lW)  
  FSeq.mean = matrix(NA,nrow = nCountry,ncol = data$lW)  
  
  FSub.lo = matrix(NA,nrow = nCountry,ncol = data$lW)
  FSub.med = matrix(NA,nrow = nCountry,ncol = data$lW)
  FSub.up = matrix(NA,nrow = nCountry,ncol = data$lW)  
  FSub.mean = matrix(NA,nrow = nCountry,ncol = data$lW)  
  
  Ffirst.lo = matrix(NA,nrow = nCountry,ncol = data$lW)
  Ffirst.med = matrix(NA,nrow = nCountry,ncol = data$lW)
  Ffirst.up = matrix(NA,nrow = nCountry,ncol = data$lW)  
  Ffirst.mean = matrix(NA,nrow = nCountry,ncol = data$lW)  
  
  Lfirst.lo = matrix(NA,nrow = nCountry,ncol = data$lW)
  Lfirst.med = matrix(NA,nrow = nCountry,ncol = data$lW)
  Lfirst.up = matrix(NA,nrow = nCountry,ncol = data$lW)  
  Lfirst.mean = matrix(NA,nrow = nCountry,ncol = data$lW)  
  
  
  Fall = matrix(0,ncol = R,nrow = data$lW)
  
  res.ctry.withS = NULL
  res.ctry.noS = NULL
  
  times = matrix(rep(1:data$lW,each=R),ncol = data$lW, byrow = F)
  
  exp.r.t = t(apply(res,1,function(x,lW,col.r,col.T0,col.r2,col.Tcp2,col.r3,col.Tcp3) {
    exp.r.t=rep(0,data$lW);
    exp.r.t[x[col.T0]:data$lW] = exp(x[col.r]*(seq(x[col.T0],lW)-x[col.T0]));
    exp.r.t[x[col.Tcp2]:data$lW] = exp(x[col.r]*(x[col.Tcp2]-x[col.T0]) + x[col.r2]*(seq(x[col.Tcp2],lW)-x[col.Tcp2]));
    exp.r.t[x[col.Tcp3]:data$lW] = exp(x[col.r]*(x[col.Tcp2]-x[col.T0]) + x[col.r2]*(x[col.Tcp3]-x[col.Tcp2])  + x[col.r3]*(seq(x[col.Tcp3],lW)-x[col.Tcp3]));
    exp.r.t
  },lW=data$lW,col.r=col.r,col.T0=col.T0,col.r2=col.r2,col.Tcp2=col.Tcp2,col.r3=col.r3,col.Tcp3=col.Tcp3))
  
  big.M = t(apply(res,1,
                  function(x,lW,col.r,col.T0,col.r2,col.Tcp2,col.r3,col.Tcp3){
                    dd=rep(1,lW);
                    dd[-(1:x[col.Tcp2])]=x[col.r2]/x[col.r];
                    dd[-(1:x[col.Tcp3])]=x[col.r3]/x[col.r];
                    dd
                  }, 
                  lW=data$lW,col.r=col.r,col.T0=col.T0,
                  col.r2=col.r2,col.Tcp2=col.Tcp2,
                  col.r3=col.r3,col.Tcp3=col.Tcp3)
  )
  
  #  exp.r.t.int = (exp(big.M * res[,col.r])-1)/(big.M * res[,col.r]) * exp.r.t 
  # exp.r.t.travel = exp.r.t.int
  exp.r.t.travel = exp.r.t
  
  for (j in 1:(data$incub-1)) {
    #    exp.r.t.travel = exp.r.t.travel + cbind(matrix(rep(0,R*j),ncol=j),exp.r.t.int)[,1:data$lW]
    exp.r.t.travel = exp.r.t.travel + cbind(matrix(rep(0,R*j),ncol=j),exp.r.t)[,1:data$lW]
  }
  
  exp.r.t.travel = t(exp.r.t.travel)
  
  inc.lo = apply(exp.r.t,2,quantile, 0.025)
  inc.med = apply(exp.r.t,2,quantile, 0.5)
  inc.up = apply(exp.r.t,2,quantile, 0.975)
  inc.mean = apply(exp.r.t,2,mean)
  
  exp.r.t.norm = exp.r.t / apply(exp.r.t,1,sum)
  inc.lo.norm = apply(exp.r.t.norm,2,quantile, 0.025)
  inc.med.norm = apply(exp.r.t.norm,2,quantile, 0.5)
  inc.up.norm = apply(exp.r.t.norm,2,quantile, 0.975)
  inc.mean.norm = apply(exp.r.t.norm,2,mean)
  
  for (i in 1:nCountry) {
    lambda.ps = exp.r.t.travel * data$tKps[,i] # all sequenced
    lambda.first = exp.r.t.travel * unlist(data$p[i,]) # all travelled
    lambda.t.sub = lambda.ps # all submitted
    # lambda.t.sub is sum lambda[u] pi_u[u,t]
    # this is average number of cases submitted on day t
    for (j in 1:R) {lambda.t.sub[,j] = apply(lambda.ps[,j] * data$d[[i]] ,2,sum)}
    
    F.lambda.sub = 1.0-exp(-apply(lambda.t.sub,2,cumsum)) # decale de 1
    F.lambda.withS = 1.0-exp(-apply(lambda.ps,2,cumsum)) # decale de 1
    F.lambda.first = 1.0-exp(-apply(lambda.first,2,cumsum)) # decale de 1

    FSeq.lo[i,] = apply(F.lambda.withS,1,quantile, 0.025)
    FSeq.med[i,] = apply(F.lambda.withS,1,quantile, 0.5)
    FSeq.up[i,] = apply(F.lambda.withS,1,quantile, 0.975)
    FSeq.mean[i,] = apply(F.lambda.withS,1,mean)
    
    FSub.lo[i,] = apply(F.lambda.sub,1,quantile, 0.025)
    FSub.med[i,] = apply(F.lambda.sub,1,quantile, 0.5)
    FSub.up[i,] = apply(F.lambda.sub,1,quantile, 0.975)
    FSub.mean[i,] = apply(F.lambda.sub,1,mean)
    
    Ffirst.lo[i,] = apply(F.lambda.first,1,quantile, 0.025)
    Ffirst.med[i,] = apply(F.lambda.first,1,quantile, 0.5)
    Ffirst.up[i,] = apply(F.lambda.first,1,quantile, 0.975)
    Ffirst.mean[i,] = apply(F.lambda.first,1,mean)
    
    Lfirst.lo[i,] = apply(lambda.first,1,quantile, 0.025)
    Lfirst.med[i,] = apply(lambda.first,1,quantile, 0.5)
    Lfirst.up[i,] = apply(lambda.first,1,quantile, 0.975)
    Lfirst.mean[i,] = apply(lambda.first,1,mean)
    
    Fall = Fall + F.lambda.withS
    
  }
  
  Fall.lo = apply(Fall,1,quantile, 0.025)
  Fall.med = apply(Fall,1,quantile, 0.5)
  Fall.up = apply(Fall,1,quantile, 0.975)
  Fall.mean = apply(Fall,1,mean)
  
  
  list(country=data$country, 
       incUK=list(lo=inc.lo,med=inc.med,up=inc.up,mean=inc.mean),
       incUK.norm=list(lo=inc.lo.norm,med=inc.med.norm,up=inc.up.norm,mean=inc.mean.norm),
       submitted=list(lo=FSub.lo,med=FSub.med,up=FSub.up,mean=FSub.mean),
       collected=list(lo=FSeq.lo,med=FSeq.med,up=FSeq.up,mean=FSeq.mean),
       first=list(lo=Ffirst.lo,med=Ffirst.med,up=Ffirst.up,mean=Ffirst.mean),
       all=list(lo=Fall.lo,med=Fall.med,up=Fall.up,mean=Fall.mean),
       lambda.first=list(lo=Lfirst.lo,med=Lfirst.med,up=Lfirst.up,mean=Lfirst.mean))
}

calc.F.country.import.auto <- function(res,data) {
  
  lW=data$lW  
  nCountry=data$nCountry  
  nWeeks=data$nWeeks
  
  col.r=which(colnames(res)=="r")
  col.T0=which(colnames(res)=="T0")
  if ("r2" %in% colnames(res)) {
    col.r2=which(colnames(res)=="r2")
    res[,col.r2] = res[,col.r2]/res[,col.r]
    colnames(res)[col.r2]="phi"
  }
  col.phi=which(colnames(res)=="phi")
  col.Tcp=which(colnames(res)=="Tcp")
  col.lp=which(colnames(res)=="lp__")
  
  R = dim(res)[1] # nombre de simulations
  
  FnoS.lo = matrix(NA,nrow = nCountry,ncol = lW)
  FnoS.med = matrix(NA,nrow = nCountry,ncol = lW)
  FnoS.up = matrix(NA,nrow = nCountry,ncol = lW)  
  FnoS.mean = matrix(NA,nrow = nCountry,ncol = lW)  
  
  Ffirst.lo = matrix(NA,nrow = nCountry,ncol = lW)
  Ffirst.med = matrix(NA,nrow = nCountry,ncol = lW)
  Ffirst.up = matrix(NA,nrow = nCountry,ncol = lW)  
  Ffirst.mean = matrix(NA,nrow = nCountry,ncol = lW)  
  
  FlambdawithS.lo = matrix(NA,nrow = nCountry,ncol = lW)
  FlambdawithS.med = matrix(NA,nrow = nCountry,ncol = lW)
  FlambdawithS.up = matrix(NA,nrow = nCountry,ncol = lW)  
  FlambdawithS.mean = matrix(NA,nrow = nCountry,ncol = lW)  
  
  FlambdanoS.lo = matrix(NA,nrow = nCountry,ncol = lW)
  FlambdanoS.med = matrix(NA,nrow = nCountry,ncol = lW)
  FlambdanoS.up = matrix(NA,nrow = nCountry,ncol = lW)  
  FlambdanoS.mean = matrix(NA,nrow = nCountry,ncol = lW)  

  FmuwithS.lo = matrix(NA,nrow = nCountry,ncol = lW)
  FmuwithS.med = matrix(NA,nrow = nCountry,ncol = lW)
  FmuwithS.up = matrix(NA,nrow = nCountry,ncol = lW)  
  FmuwithS.mean = matrix(NA,nrow = nCountry,ncol = lW)  
  
  FmunoS.lo = matrix(NA,nrow = nCountry,ncol = lW)
  FmunoS.med = matrix(NA,nrow = nCountry,ncol = lW)
  FmunoS.up = matrix(NA,nrow = nCountry,ncol = lW)  
  FmunoS.mean = matrix(NA,nrow = nCountry,ncol = lW)  
  
  FwithS.lo = matrix(NA,nrow = nCountry,ncol = lW)
  FwithS.med = matrix(NA,nrow = nCountry,ncol = lW)
  FwithS.up = matrix(NA,nrow = nCountry,ncol = lW)  
  FwithS.mean = matrix(NA,nrow = nCountry,ncol = lW)  
  
  lambda.lo = matrix(NA,nrow = nCountry,ncol = lW)
  lambda.med = matrix(NA,nrow = nCountry,ncol = lW)
  lambda.up = matrix(NA,nrow = nCountry,ncol = lW)  
  lambda.mean = matrix(NA,nrow = nCountry,ncol = lW)  
  
  mu.lo = matrix(NA,nrow = nCountry,ncol = lW)
  mu.med = matrix(NA,nrow = nCountry,ncol = lW)
  mu.up = matrix(NA,nrow = nCountry,ncol = lW)  
  mu.mean = matrix(NA,nrow = nCountry,ncol = lW)  
  
  first.lo = matrix(NA,nrow = nCountry,ncol = lW)
  first.med = matrix(NA,nrow = nCountry,ncol = lW)
  first.up = matrix(NA,nrow = nCountry,ncol = lW)  
  first.mean = matrix(NA,nrow = nCountry,ncol = lW)  
  
  pct.import.med=rep(NA,nCountry)
  pct.import.lo=rep(NA,nCountry)
  pct.import.up=rep(NA,nCountry)
    
  
  res.ctry.withS = NULL
  res.ctry.noS = NULL
  
  times = matrix(rep(1:data$lW,each=R),ncol = data$lW, byrow = F)
  
  exp.r.t = t(apply(res,1,function(x,lW,col.r,col.T0,col.r2,col.Tcp2,col.r3,col.Tcp3) {
    exp.r.t=rep(0,data$lW);
    exp.r.t[x[col.T0]:data$lW] = exp(x[col.r]*(seq(x[col.T0],lW)-x[col.T0]));
    exp.r.t[x[col.Tcp2]:data$lW] = exp(x[col.r]*(x[col.Tcp2]-x[col.T0]) + x[col.r2]*(seq(x[col.Tcp2],lW)-x[col.Tcp2]));
    exp.r.t[x[col.Tcp3]:data$lW] = exp(x[col.r]*(x[col.Tcp2]-x[col.T0]) + x[col.r2]*(x[col.Tcp3]-x[col.Tcp2])  + x[col.r3]*(seq(x[col.Tcp3],lW)-x[col.Tcp3]));
    exp.r.t
  },lW=data$lW,col.r=col.r,col.T0=col.T0,col.r2=col.r2,col.Tcp2=col.Tcp2,col.r3=col.r3,col.Tcp3=col.Tcp3))
  
  big.M = t(apply(res,1,
                  function(x,lW,col.r,col.T0,col.r2,col.Tcp2,col.r3,col.Tcp3){
                    dd=rep(1,lW);
                    dd[-(1:x[col.Tcp2])]=x[col.r2]/x[col.r];
                    dd[-(1:x[col.Tcp3])]=x[col.r3]/x[col.r];
                    dd
                  }, 
                  lW=data$lW,col.r=col.r,col.T0=col.T0,col.r2=col.r2,col.Tcp2=col.Tcp2,col.r3=col.r3,col.Tcp3=col.Tcp3))
  
  exp.r.t.int = (exp(big.M * res[,col.r])-1)/(big.M * res[,col.r]) * exp.r.t 
  
  exp.r.t.travel = exp.r.t.int
  for (j in 1:(data$incub-1)) {
    exp.r.t.travel = exp.r.t.travel + cbind(matrix(rep(0,R*j),ncol=j),exp.r.t.int)[,1:data$lW]
  }
  
  exp.r.t.travel = t(exp.r.t.travel)
  
  
  for (i in 1:nCountry) {
    lambda.withS=exp.r.t.travel * data$tKps[,i]
    lambda.noS = data$K * exp.r.t.travel * data$p[i,]
    
    lambda.first = exp.r.t.travel * data$tp.over.R[,i]
    F.first = 1.0 - exp(-apply(lambda.first,2,cumsum))
    log.S.first = -apply(lambda.first ,2, function(x) {c(0,cumsum(x))[1:length(x)]}) # decale de 1
    f.first= lambda.first * exp(log.S.first)

    mu.noS = data$K * data$r[i,,] %*% f.first 
    mu.withS = mu.noS * data$s[i,]
    
    F.withS = 1-exp(-apply(mu.withS+lambda.withS,2,cumsum))
    F.noS = 1-exp(-apply(mu.noS+lambda.noS,2,cumsum))
    
    F.mu.withS = apply(mu.withS * exp(-apply(mu.withS+lambda.withS,2,function(x) {c(0,cumsum(x))[1:length(x)]})),2,cumsum)
    F.lambda.withS = apply(lambda.withS * exp(-apply(mu.withS+lambda.withS,2,function(x) {c(0,cumsum(x))[1:length(x)]})),2,cumsum)

    F.mu.noS = apply(mu.noS * exp(-apply(mu.noS+lambda.noS,2,function(x) {c(0,cumsum(x))[1:length(x)]})),2,cumsum)
    F.lambda.noS = apply(lambda.noS * exp(-apply(mu.noS+lambda.noS,2,function(x) {c(0,cumsum(x))[1:length(x)]})),2,cumsum)
    
    pct.import= apply((lambda.withS/(lambda.withS+mu.withS))[-1,] * apply(F.withS,2,diff),2,sum,na.rm=T)/F.withS[data$lW]

    pct.import.lo[i] = quantile(pct.import,0.025,na.rm=T)
    pct.import.med[i] = quantile(pct.import,0.5,na.rm=T)
    pct.import.up[i] = quantile(pct.import,0.975,na.rm=T)
    
    FnoS.lo[i,] = apply(F.noS,1,quantile, 0.025)
    FnoS.med[i,] = apply(F.noS,1,quantile, 0.5)
    FnoS.up[i,] = apply(F.noS,1,quantile, 0.975)
    FnoS.mean[i,] = apply(F.noS,1,mean)
    
    FlambdawithS.lo[i,] = apply(F.lambda.withS,1,quantile, 0.025)
    FlambdawithS.med[i,] = apply(F.lambda.withS,1,quantile, 0.5)
    FlambdawithS.up[i,] = apply(F.lambda.withS,1,quantile, 0.975)
    FlambdawithS.mean[i,] = apply(F.lambda.withS,1,mean)
    
    FlambdanoS.lo[i,] = apply(F.lambda.noS,1,quantile, 0.025)
    FlambdanoS.med[i,] = apply(F.lambda.noS,1,quantile, 0.5)
    FlambdanoS.up[i,] = apply(F.lambda.noS,1,quantile, 0.975)
    FlambdanoS.mean[i,] = apply(F.lambda.noS,1,mean)
    
    Ffirst.lo[i,] = apply(F.first,1,quantile, 0.025)
    Ffirst.med[i,] = apply(F.first,1,quantile, 0.5)
    Ffirst.up[i,] = apply(F.first,1,quantile, 0.975)
    Ffirst.mean[i,] = apply(F.first,1,mean)
    
    FmuwithS.lo[i,] = apply(F.mu.withS,1,quantile, 0.025)
    FmuwithS.med[i,] = apply(F.mu.withS,1,quantile, 0.5)
    FmuwithS.up[i,] = apply(F.mu.withS,1,quantile, 0.975)
    FmuwithS.mean[i,] = apply(F.mu.withS,1,mean)

    FmunoS.lo[i,] = apply(F.mu.noS,1,quantile, 0.025)
    FmunoS.med[i,] = apply(F.mu.noS,1,quantile, 0.5)
    FmunoS.up[i,] = apply(F.mu.noS,1,quantile, 0.975)
    FmunoS.mean[i,] = apply(F.mu.noS,1,mean)
    
    FwithS.lo[i,] = apply(F.withS,1,quantile, 0.025)
    FwithS.med[i,] = apply(F.withS,1,quantile, 0.5)
    FwithS.up[i,] = apply(F.withS,1,quantile, 0.975)
    FwithS.mean[i,] = apply(F.withS,1,mean)
    
  }
  
  list(country=data$country, noS=list(lo=FnoS.lo,med=FnoS.med,up=FnoS.up,mean=FnoS.mean),
       lambdaS=list(lo=FlambdawithS.lo,med=FlambdawithS.med,up=FlambdawithS.up,mean=FlambdawithS.mean),
       muS=list(lo=FmuwithS.lo,med=FmuwithS.med,up=FmuwithS.up,mean=FmuwithS.mean),
       lambdanoS=list(lo=FlambdanoS.lo,med=FlambdanoS.med,up=FlambdanoS.up,mean=FlambdanoS.mean),
       munoS=list(lo=FmunoS.lo,med=FmunoS.med,up=FmunoS.up,mean=FmunoS.mean),
       first=list(lo=Ffirst.lo,med=Ffirst.med,up=Ffirst.up,mean=Ffirst.mean),
       withS=list(lo=FwithS.lo,med=FwithS.med,up=FwithS.up,mean=FwithS.mean),
       import=list(lo=pct.import.lo,med=pct.import.med, up=pct.import.up))
  
}

calc.F.country.import.all.auto <- function(res,data) {
  
  lW=data$lW  
  nCountry=data$nCountry  
  nWeeks=data$nWeeks
  
  col.r=which(colnames(res)=="r")
  col.T0=which(colnames(res)=="T0")
  if ("r2" %in% colnames(res)) {
    col.r2=which(colnames(res)=="r2")
    res[,col.r2] = res[,col.r2]/res[,col.r]
    colnames(res)[col.r2]="phi"
  }
  col.phi=which(colnames(res)=="phi")
  col.Tcp=which(colnames(res)=="Tcp")
  col.lp=which(colnames(res)=="lp__")
  
  R = dim(res)[1] # nombre de simulations
  
  FnoS.lo = matrix(NA,nrow = nCountry,ncol = lW)
  FnoS.med = matrix(NA,nrow = nCountry,ncol = lW)
  FnoS.up = matrix(NA,nrow = nCountry,ncol = lW)  
  FnoS.mean = matrix(NA,nrow = nCountry,ncol = lW)  
  
  Ffirst.lo = matrix(NA,nrow = nCountry,ncol = lW)
  Ffirst.med = matrix(NA,nrow = nCountry,ncol = lW)
  Ffirst.up = matrix(NA,nrow = nCountry,ncol = lW)  
  Ffirst.mean = matrix(NA,nrow = nCountry,ncol = lW)  
  
  FlambdawithS.lo = matrix(NA,nrow = nCountry,ncol = lW)
  FlambdawithS.med = matrix(NA,nrow = nCountry,ncol = lW)
  FlambdawithS.up = matrix(NA,nrow = nCountry,ncol = lW)  
  FlambdawithS.mean = matrix(NA,nrow = nCountry,ncol = lW)  
  
  FlambdanoS.lo = matrix(NA,nrow = nCountry,ncol = lW)
  FlambdanoS.med = matrix(NA,nrow = nCountry,ncol = lW)
  FlambdanoS.up = matrix(NA,nrow = nCountry,ncol = lW)  
  FlambdanoS.mean = matrix(NA,nrow = nCountry,ncol = lW)  
  
  FmuwithS.lo = matrix(NA,nrow = nCountry,ncol = lW)
  FmuwithS.med = matrix(NA,nrow = nCountry,ncol = lW)
  FmuwithS.up = matrix(NA,nrow = nCountry,ncol = lW)  
  FmuwithS.mean = matrix(NA,nrow = nCountry,ncol = lW)  
  
  FmunoS.lo = matrix(NA,nrow = nCountry,ncol = lW)
  FmunoS.med = matrix(NA,nrow = nCountry,ncol = lW)
  FmunoS.up = matrix(NA,nrow = nCountry,ncol = lW)  
  FmunoS.mean = matrix(NA,nrow = nCountry,ncol = lW)  
  
  FwithS.lo = matrix(NA,nrow = nCountry,ncol = lW)
  FwithS.med = matrix(NA,nrow = nCountry,ncol = lW)
  FwithS.up = matrix(NA,nrow = nCountry,ncol = lW)  
  FwithS.mean = matrix(NA,nrow = nCountry,ncol = lW)  
  
  lambda.lo = matrix(NA,nrow = nCountry,ncol = lW)
  lambda.med = matrix(NA,nrow = nCountry,ncol = lW)
  lambda.up = matrix(NA,nrow = nCountry,ncol = lW)  
  lambda.mean = matrix(NA,nrow = nCountry,ncol = lW)  
  
  mu.lo = matrix(NA,nrow = nCountry,ncol = lW)
  mu.med = matrix(NA,nrow = nCountry,ncol = lW)
  mu.up = matrix(NA,nrow = nCountry,ncol = lW)  
  mu.mean = matrix(NA,nrow = nCountry,ncol = lW)  
  
  first.lo = matrix(NA,nrow = nCountry,ncol = lW)
  first.med = matrix(NA,nrow = nCountry,ncol = lW)
  first.up = matrix(NA,nrow = nCountry,ncol = lW)  
  first.mean = matrix(NA,nrow = nCountry,ncol = lW)  
  
  pct.import.med=rep(NA,nCountry)
  pct.import.lo=rep(NA,nCountry)
  pct.import.up=rep(NA,nCountry)
  
  
  res.ctry.withS = NULL
  res.ctry.noS = NULL
  
  times = matrix(rep(1:lW,each=R),ncol = lW, byrow = F)
  
  big.M = t(apply(res[,c(col.phi,col.Tcp)],1,
                  function(x,lW) {
                    dd=rep(1,lW);
                    dd[-(1:x[2])]=x[1];
                    dd}, 
                  lW=lW))
  
  exp.r.t = exp(big.M* (res[,col.r] * (times-res[,col.T0])));
  exp.r.t[exp.r.t<1.0]=0;
  exp.r.t.int = (exp(big.M * res[,col.r])-1)/(big.M * res[,col.r]) * exp.r.t 
  exp.r.t.int = t(exp.r.t.int)
  
  for (i in 1:nCountry) {
    lambda.withS=exp.r.t.int * data$tKps[,i]
    lambda.noS = data$K * exp.r.t.int * data$p[i,]
    
    lambda.first = exp.r.t.int * data$tp.over.R[,i]
    F.first = 1.0 - exp(-apply(lambda.first,2,cumsum))
    log.S.first = -apply(lambda.first ,2, function(x) {c(0,cumsum(x))[1:length(x)]}) # decale de 1
    f.first= lambda.first * exp(log.S.first)
    
    mu.noS = data$K * data$r[i,,] %*% lambda.first 
    mu.withS = mu.noS * data$s[i,]
    
    F.withS = 1-exp(-apply(mu.withS+lambda.withS,2,cumsum))
    F.noS = 1-exp(-apply(mu.noS+lambda.noS,2,cumsum))
    
    F.mu.withS = apply(mu.withS * exp(-apply(mu.withS+lambda.withS,2,function(x) {c(0,cumsum(x))[1:length(x)]})),2,cumsum)
    F.lambda.withS = apply(lambda.withS * exp(-apply(mu.withS+lambda.withS,2,function(x) {c(0,cumsum(x))[1:length(x)]})),2,cumsum)
    
    F.mu.noS = apply(mu.noS * exp(-apply(mu.noS+lambda.noS,2,function(x) {c(0,cumsum(x))[1:length(x)]})),2,cumsum)
    F.lambda.noS = apply(lambda.noS * exp(-apply(mu.noS+lambda.noS,2,function(x) {c(0,cumsum(x))[1:length(x)]})),2,cumsum)
    
    pct.import= apply((lambda.withS/(lambda.withS+mu.withS))[-1,] * apply(F.withS,2,diff),2,sum,na.rm=T)/F.withS[data$lW]
    
    pct.import.lo[i] = quantile(pct.import,0.025,na.rm=T)
    pct.import.med[i] = quantile(pct.import,0.5,na.rm=T)
    pct.import.up[i] = quantile(pct.import,0.975,na.rm=T)
    
    FnoS.lo[i,] = apply(F.noS,1,quantile, 0.025)
    FnoS.med[i,] = apply(F.noS,1,quantile, 0.5)
    FnoS.up[i,] = apply(F.noS,1,quantile, 0.975)
    FnoS.mean[i,] = apply(F.noS,1,mean)
    
    FlambdawithS.lo[i,] = apply(F.lambda.withS,1,quantile, 0.025)
    FlambdawithS.med[i,] = apply(F.lambda.withS,1,quantile, 0.5)
    FlambdawithS.up[i,] = apply(F.lambda.withS,1,quantile, 0.975)
    FlambdawithS.mean[i,] = apply(F.lambda.withS,1,mean)
    
    FlambdanoS.lo[i,] = apply(F.lambda.noS,1,quantile, 0.025)
    FlambdanoS.med[i,] = apply(F.lambda.noS,1,quantile, 0.5)
    FlambdanoS.up[i,] = apply(F.lambda.noS,1,quantile, 0.975)
    FlambdanoS.mean[i,] = apply(F.lambda.noS,1,mean)
    
    Ffirst.lo[i,] = apply(F.first,1,quantile, 0.025)
    Ffirst.med[i,] = apply(F.first,1,quantile, 0.5)
    Ffirst.up[i,] = apply(F.first,1,quantile, 0.975)
    Ffirst.mean[i,] = apply(F.first,1,mean)
    
    FmuwithS.lo[i,] = apply(F.mu.withS,1,quantile, 0.025)
    FmuwithS.med[i,] = apply(F.mu.withS,1,quantile, 0.5)
    FmuwithS.up[i,] = apply(F.mu.withS,1,quantile, 0.975)
    FmuwithS.mean[i,] = apply(F.mu.withS,1,mean)
    
    FmunoS.lo[i,] = apply(F.mu.noS,1,quantile, 0.025)
    FmunoS.med[i,] = apply(F.mu.noS,1,quantile, 0.5)
    FmunoS.up[i,] = apply(F.mu.noS,1,quantile, 0.975)
    FmunoS.mean[i,] = apply(F.mu.noS,1,mean)
    
    FwithS.lo[i,] = apply(F.withS,1,quantile, 0.025)
    FwithS.med[i,] = apply(F.withS,1,quantile, 0.5)
    FwithS.up[i,] = apply(F.withS,1,quantile, 0.975)
    FwithS.mean[i,] = apply(F.withS,1,mean)
    
  }
  
  list(country=data$country, noS=list(lo=FnoS.lo,med=FnoS.med,up=FnoS.up,mean=FnoS.mean),
       lambdaS=list(lo=FlambdawithS.lo,med=FlambdawithS.med,up=FlambdawithS.up,mean=FlambdawithS.mean),
       muS=list(lo=FmuwithS.lo,med=FmuwithS.med,up=FmuwithS.up,mean=FmuwithS.mean),
       lambdanoS=list(lo=FlambdanoS.lo,med=FlambdanoS.med,up=FlambdanoS.up,mean=FlambdanoS.mean),
       munoS=list(lo=FmunoS.lo,med=FmunoS.med,up=FmunoS.up,mean=FmunoS.mean),
       first=list(lo=Ffirst.lo,med=Ffirst.med,up=Ffirst.up,mean=Ffirst.mean),
       withS=list(lo=FwithS.lo,med=FwithS.med,up=FwithS.up,mean=FwithS.mean),
       import=list(lo=pct.import.lo,med=pct.import.med, up=pct.import.up))
  
}

make.distr.import.auto.country <- function (F, country) {
  lW = dim(F$noS$mean)[2]
  idx=which(F$country==country)  
  distr= data.frame(date=rep(as.Date("2020-08-01")-1 + 1:lW,
                             times=7))
  
  distr$med = c(F$noS$med[idx,], 
                F$withS$med[idx,],
                F$first$med[idx,],
                F$lambdanoS$med[idx,],
                F$munoS$med[idx,],
                F$lambdaS$med[idx,],
                F$muS$med[idx,])
  
  distr$type = rep(c("all-noS", "all-S","Introduction","imp-noS","auto-noS","imp-S","auto-S"),
                   each=lW)
  distr$seq = rep(c("th","obs","th","th","th","obs","obs"),each=lW)
  
  distr$lo = c(F$noS$lo[idx,],
               F$withS$lo[idx,],
               F$first$lo[idx,],
               F$lambdanoS$lo[idx,],
               F$munoS$lo[idx,],
               F$lambdaS$lo[idx,],
               F$muS$lo[idx,])
  
  distr$up = c(F$noS$up[idx,],
               F$withS$up[idx,],
               F$first$up[idx,],
               F$lambdanoS$up[idx,],
               F$munoS$up[idx,],
               F$lambdaS$up[idx,],
               F$muS$up[idx,])

  distr$country=country
  distr
}


summary.MCMC.import = function(res,data,burnin=1/2,sample=2000) {
  #ahead is lead time for prediction
  require(importFromUK)
  require(survival)
  #res may be a list of several results
  if(is.null(res$res)) {
    # res may contain a coda mcmc.list
    if (!is.null(res$mcmc)) {
    # make a big sample
      require(nlist)
      df.res = collapse_chains(res$mcmc)[[1]]
      df.res=df.res[,c("r","T0","r2","Tcp2","r3","Tcp3","k","s","logL","logLEpid","logPrior","lp__")]
    } else {
    # is a list : rbind columns
      df.res=NULL
      nChains=length(res)
      for (i in 1:length(res)) {
        df.res=rbind(df.res,res[[i]]$res)
      }
    }
    R = dim(df.res)[1]
  } else {
    R = dim(res$res)[1]
    df.res = res$res
  }
  print(paste("using ",R," iterations"))
  
  if (all(df.res[,"r"] == df.res[,"r2"])) {dim_r=1}
  else if (all(df.res[,"r2"] == df.res[,"r3"])) {dim_r=2}
  else {dim_r=3}
  #this calls F_country
  if (packageVersion("importFromUK")=="3.0" | packageVersion("importFromUK")=="3.1"  | packageVersion("importFromUK")=="3.2") {
    computeAutoEpid=1
    if (is.null(data$p.over.R) | is.null(data$r)) computeAutoEpid=0
    Fdistr = calc_F_country(df.res,R,data$lW,data$ahead,data$nCountry,dim_r,data$incub, 
                            data$Ts, data$delta1,
                          as.matrix(data$p), 
                          data$tKp, data$s, data$dt.s.increase, data$iUK,
                          data$delai.array, 
                          data$p.over.R, data$r,
                          computeAutoEpid, data$f_before_T0)
  } else {
    Fdistr = calc_F_country(df.res,R,data$lW,data$ahead,data$nCountry,dim_r,data$incub, data$delta1,
                            as.matrix(data$p), data$tKps,
                            data$delai.array)
    
  }
  #Fdistr is lW+ahead
  df.res=as.data.frame(df.res)

  df.res$t2=log(2)/df.res$r
  df.res$t2.2=log(2)/(df.res$r2)
  df.res$t2.3=log(2)/(df.res$r3)
  
  df.res$dt.T0=data$date.start + df.res$T0
  df.res$dt.Tcp2=data$date.start + df.res$Tcp2
  df.res$dt.Tcp3=data$date.start + df.res$Tcp3
  
  df.res$model="import"
  
  Fdistr$t.emerg = Fdistr$t.emerg/sum(Fdistr$t.emerg)
  
  fitSub= data.frame(day=data$date.start-1+1:length(Fdistr$all.sub$mean), 
#                     mean=apply(Fdistr$submitted$mean,1,sum),
                      mean=Fdistr$all.sub$mean,
#                     med=apply(Fdistr$submitted$med,1,sum),
                      med=Fdistr$all.sub$med,
#                     lo=apply(Fdistr$submitted$lo,1,sum),
                      lo=Fdistr$all.sub$lo,
#                     up=apply(Fdistr$submitted$up,1,sum),
                      up=Fdistr$all.sub$up,
                      type="model",
                     value="submitted")

  fitCol= data.frame(day=data$date.start-1+1:length(Fdistr$all.col$mean), 
#                     mean=apply(Fdistr$collected$mean,1,sum),
#                     med=apply(Fdistr$collected$med,1,sum),
#                     lo=apply(Fdistr$collected$lo,1,sum),
#                     up=apply(Fdistr$collected$up,1,sum),
                     mean=Fdistr$all.col$mean,
                      med=Fdistr$all.col$med,
                      lo=Fdistr$all.col$lo,
                      up=Fdistr$all.col$up,
                     type="model",
                     value="collected")

  fitColSub= data.frame(day=data$date.start-1+1:length(Fdistr$col.sub$mean), 
                     #                     mean=apply(Fdistr$collected$mean,1,sum),
                     #                     med=apply(Fdistr$collected$med,1,sum),
                     #                     lo=apply(Fdistr$collected$lo,1,sum),
                     #                     up=apply(Fdistr$collected$up,1,sum),
                     mean=Fdistr$col.sub$mean,
                     med=Fdistr$col.sub$med,
                     lo=Fdistr$col.sub$lo,
                     up=Fdistr$col.sub$up,
                     type="model",
                     value="coll+sub")
  
  cumSubObs = summary(survfit(Surv(data$Ts, data$delta1)~1), times=(0:data$lW),extend=T)
#  cumSubObs = (ecdf(data$Ts)(1:data$lW))*data$nCountry
  
  cumSubObs = data.frame(day=data$date.start-1 + 1:(data$lW+1),
                         surv=(1-cumSubObs$surv)*data$nCountry,
                         lo=(1-cumSubObs$lower)*data$nCountry,
                         up=(1-cumSubObs$upper)*data$nCountry,
                         type="observed",
                         value="submitted")
  
  cumColObs = summary(survfit(Surv(data$Ti.lo,data$Ti.up, 3*data$delta1,type='interval')~1), times=(0:data$lW),extend=T)
#  cumColObs = (ecdf(data$Ti)(1:data$lW))*data$nCountry
  
  cumColObs = data.frame(day=data$date.start-1 + 1:(data$lW+1),
                         surv=(1-cumColObs$surv)*data$nCountry,
                         lo=(1-cumColObs$lower)*data$nCountry,
                         up=(1-cumColObs$upper)*data$nCountry,
                         type="observed",
                         value="collected")
  
  fitModel=rbind(fitSub,fitCol,fitColSub)
  fitObs = rbind(cumSubObs,cumColObs)
  Fdistr$country = data$country
  
  # compute days 2.5-97.5 for emergence
  emerg.first = data$date.start + sum(cumsum(Fdistr$t.emerg)<0.025)
  emerg.last = data$date.start + sum(cumsum(Fdistr$t.emerg)<0.975)
  
  
  return(list(res=df.res, distr=Fdistr, model=fitModel, observed=fitObs, t.emerg=c(emerg.first, emerg.last),
              metadata=res$metadata,date.compute=date()))
}

summary.MCMC.import.auto = function(res,data,burnin=1/2,sample=2000) {
  require(survival)
  
  # discard 1/2 as burnin; then obtain sample of size 2000
  R = dim(res$res)[1]
  R.mid = floor(R/2)
  thin=floor((R-R.mid)/sample)
  print(paste("using iterations ",R.mid, " to ",R," by ",thin))
  df.res = res$res[seq(R.mid,R,thin),]
  Fdistr = calc.F.country.import.auto(df.res, data)
  df.res=as.data.frame(df.res)
  
  
  df.res$t2=log(2)/df.res$r
  if ("phi" %in% names(df.res)) {
    df.res$t2.after=log(2)/(df.res$r*df.res$phi)
  } else {
    df.res$t2.after=log(2)/(df.res$r2)
  }
  df.res$dt.T0=as.Date("2020-08-01") + df.res$T0
  df.res$dt.Tcp=as.Date("2020-08-01") + df.res$Tcp
  df.res$model="import"
  
  
  fitInc= data.frame(t=as.Date("2020-08-01")-1+1:dim(Fdistr$noS$mean)[2], 
                     mean=apply(Fdistr$withS$mean,2,sum),
                     lo=apply(Fdistr$withS$lo,2,sum),
                     up=apply(Fdistr$withS$up,2,sum),
                     model="import")
  
  cumIncObs = summary(survfit(Surv(data$T, data$delta1)~1))
  
  cumIncObs = data.frame(time=as.Date("2020-08-01")-1+cumIncObs$time,
                         surv=(1-cumIncObs$surv)*cumIncObs$n.risk[1],
                         surv.lo=(1-cumIncObs$lower)*cumIncObs$n.risk[1],
                         surv.up=(1-cumIncObs$upper)*cumIncObs$n.risk[1],
                         model="observed")
  
  
  return(list(res=df.res, distr=Fdistr, fitInc=fitInc, obsInc=cumIncObs))
}

summary.MCMC.import.all.auto = function(res,data,burnin=1/2,sample=2000) {
  require(survival)
  
  # discard 1/2 as burnin; then obtain sample of size 2000
  R = dim(res$res)[1]
  R.mid = floor(R/2)
  thin=floor((R-R.mid)/sample)
  print(paste("using iterations ",R.mid, " to ",R," by ",thin))
  df.res = res$res[seq(R.mid,R,thin),]
  Fdistr = calc.F.country.import.all.auto(df.res, data)
  df.res=as.data.frame(df.res)
  
  
  df.res$t2=log(2)/df.res$r
  if ("phi" %in% names(df.res)) {
    df.res$t2.after=log(2)/(df.res$r*df.res$phi)
  } else {
    df.res$t2.after=log(2)/(df.res$r2)
  }
  df.res$dt.T0=as.Date("2020-08-01") + df.res$T0
  df.res$dt.Tcp=as.Date("2020-08-01") + df.res$Tcp
  df.res$model="import"
  
  
  fitInc= data.frame(t=as.Date("2020-08-01")-1+1:dim(Fdistr$noS$mean)[2], 
                     cumInc=apply(Fdistr$withS$mean,2,sum),
                     model="import")
  
  cumIncObs = summary(survfit(Surv(data$T, data$delta1)~1))
  
  cumIncObs = data.frame(time=as.Date("2020-08-01")-1+cumIncObs$time,
                         surv=(1-cumIncObs$surv)*cumIncObs$n.risk[1],
                         surv.lo=(1-cumIncObs$lower)*cumIncObs$n.risk[1],
                         surv.up=(1-cumIncObs$upper)*cumIncObs$n.risk[1],
                         model="observed")
  
  
  return(list(res=df.res, distr=Fdistr, fitInc=fitInc, obsInc=cumIncObs))
}

plot.summary.MCMC <- function(sum,epid.UK,log=T,scale=T,show.epid=TRUE) {
  require(ggplot2)
  require(ggpubr)
  require(gridExtra)
  
  hist.t2 = ggplot(sum$res)+geom_histogram(aes(x=t2,fill=model),alpha=0.3)+xlab("doubling time P1")+
    theme_bw() +  theme(legend.position="top")+ guides(fill = "none") 
  hist.T0 = ggplot(sum$res)+geom_histogram(aes(x=dt.T0,fill=model),alpha=0.3)+
    xlim(as.Date("2020-08-01"),as.Date("2021-01-01"))+xlab("emergence")+
    theme_bw() +  theme(legend.position="top")+ guides(fill = "none") 
  if (any(sum$res$t2.2!=sum$res$t2)) {
    hist.t2.2 = ggplot(sum$res)+geom_histogram(aes(x=t2.2,fill=model),alpha=0.3)+
      xlab("doubling time P2")+
      theme_bw() +  theme(legend.position="top")+ guides(fill = "none") 
  } else {
    hist.t2.2 = NULL
  }
  if (any(sum$res$t2.3 != sum$res$t2.2)) {
    hist.t2.3 = ggplot(sum$res)+geom_histogram(aes(x=t2.3,fill=model),alpha=0.3)+
      xlab("doubling time P3")+
      theme_bw() +  theme(legend.position="top")+ guides(fill = "none") 
  } else {
    hist.t2.3 = NULL
  }
  
  df.inc=data.frame(day=as.Date("2020-08-15")-1+1:length(sum$distr$incUK$lo),
                    lo=sum$distr$incUK$lo,
                    med=sum$distr$incUK$med,
                    up=sum$distr$incUK$up)
  
  dt.epid = seq(sum$metadata$date.start + sum$metadata$data$t0Epid, 
                sum$metadata$date.start + sum$metadata$data$t0Epid + length(sum$metadata$data$epidUK)-1,1) 

    if (scale==TRUE) {      
    # take epidemic data in data and scale 
#      df.inc$lo = df.inc$lo/sum(df.inc$med[df.inc$day>= as.Date("2020-10-01")])* sum(epid.UK$VOCcases)
#      df.inc$up = df.inc$up/sum(df.inc$med[df.inc$day>= as.Date("2020-10-01")])* sum(epid.UK$VOCcases)
    #take dates from the epidemic and plot
    # in case there are epidemic data, 
    
    df.inc$lo = df.inc$lo/sum(df.inc$med[df.inc$day %in% dt.epid])* sum(sum$metadata$data$epidUK)
    df.inc$up = df.inc$up/sum(df.inc$med[df.inc$day %in% dt.epid])* sum(sum$metadata$data$epidUK)
    #do last because used in previous
#      df.inc$med = df.inc$med/sum(df.inc$med[df.inc$day>= as.Date("2020-10-01")])* sum(epid.UK$VOCcases)
        df.inc$med = df.inc$med/sum(df.inc$med[df.inc$day %in% dt.epid])* sum(sum$metadata$data$epidUK)
  }
  
  if (log==T) {
    inc=ggplot(df.inc) + geom_ribbon(aes(x=day,ymin=lo,ymax=up),fill="red",alpha=0.2) + 
      geom_line(aes(x=day,y=med),col='red') + scale_y_log10()+ theme_bw()
  } else {
    inc=ggplot(df.inc) + geom_ribbon(aes(x=day,ymin=lo,ymax=up),fill="red",alpha=0.2) + 
      geom_line(aes(x=day,y=med),col='red') + theme_bw()
  }
  
#  if (!is.null(epid.UK)) {
   # if (scale==TRUE) {
   #   epid.UK$VOCcases = epid.UK$VOCcases /max(cumsum(epid.UK$VOCcases))
   # }
  epid.disp =data.frame(day=dt.epid, VOCcases=sum$metadata$data$epidUK)
    inc = inc + geom_point(data=epid.disp,aes(x=day,y=VOCcases))
#  }
  
  # plot of submissions
  # sum$obsInc$surv[length(sum$obsInc$surv)] = sum$obsInc$surv[length(sum$obsInc$surv)-1] 
  surv = ggplot(sum$observed)+ 
    geom_ribbon(data=sum$model,
                aes(x=day,ymin=lo,ymax=up,fill=type,linetype=value), alpha=0.3)+
    geom_step(aes(x=day, y=surv,col=type,linetype=value)) + 
    geom_line(data=sum$model, aes(x=day,y=mean,col=type,linetype=value))+
    theme_bw() +  theme(legend.position="top") + guides(fill = "none") +
    xlim(c(as.Date("2020-09-01"), as.Date("2020-12-31")))

  if (!is.na(sum$res$t2.3[1])) {
    g1 = ggarrange(hist.t2,hist.T0,hist.t2.2,hist.t2.3,nrow=2,ncol=2,labels="AUTO")
  } else if (!is.na(sum$res$t2.2[1])) {
    g1 = ggarrange(hist.t2,hist.T0,hist.t2.2,nrow=2,ncol=2,labels="AUTO")
  } else {  
    g1=ggarrange(hist.t2,hist.T0,nrow=2,ncol=2,labels="AUTO")
  }
  
  if (show.epid==TRUE) {
    g2 = ggarrange(inc,surv,nrow=2,ncol=1,labels=c("E","F"))
  } else {
    g2 = ggarrange(surv,nrow=1,ncol=1,labels=c("F"))
  }
  plot= ggarrange(g1,g2,ncol=2)
  plot
}
  
summary.countries <- function(sum, data) {
  require(ggpubr)
  require(ggstance)
  # compute expected distributions for countries based on the fit in sum
  # sum contains all data
  #countries contains data used for estimation
  date.ref=as.Date("2020-08-15")

  countries = data.frame(country=sum$distr$country,
             first.introduced.med = apply(sum$distr$first$med<0.5,2,sum) +date.ref,
             first.introduced.lo = apply(sum$distr$first$up<0.025,2,sum) +date.ref,
             first.introduced.up = 1+apply(sum$distr$first$lo<0.975,2,sum) +date.ref,
             first.collected.med = apply(sum$distr$collected$med<0.5,2,sum) +date.ref,
            first.collected.lo = apply(sum$distr$collected$up<0.025,2,sum) +date.ref,
            first.collected.up = 1+apply(sum$distr$collected$lo<0.975,2,sum) +date.ref,
            first.submited.med = apply(sum$distr$submitted$med<0.5,2,sum) +date.ref,
            first.submited.lo = apply(sum$distr$submitted$up<0.025,2,sum) +date.ref,
            first.submited.up = 1+apply(sum$distr$submitted$lo<0.975,2,sum) +date.ref,
            collected.cond.sub.up = 1+apply(sum$distr$Pcol$lo<0.975,2,sum) +date.ref,
            collected.cond.sub.med = apply(sum$distr$Pcol$med<0.5,2,sum) +date.ref,
            collected.cond.sub.lo = apply(sum$distr$Pcol$up<0.025,2,sum) +date.ref
              )
  
  countries=merge(countries,data.frame(country=data$country, Ts=data$Ts, 
                                       Ti.lo=data$Ti.lo, Ti.up=data$Ti.up, delta1=data$delta1),
                  by="country",all.x=T)
  
  countries$Ts = date.ref+countries$Ts
  countries$Ti.lo = date.ref+countries$Ti.lo
  countries$Ti.up = date.ref+countries$Ti.up-1

  
  if (!is.null(sum$distr$import$lo)) {
    countries$import.lo = sum$distr$import$lo
    countries$import.med = sum$distr$import$med
    countries$import.up = sum$distr$import$up
  }
    
  countries$Ts[countries$delta1==0]=NA
  countries$Ti[countries$delta1==0]=NA
  countries$country=factor(countries$country, levels = countries$country)
  countries
}

dotplot.countries <- function(countries, data=NULL, freq=F, what="submitted",first.VOC.collected=NULL) {
  require(ggpubr)
  require(ggstance)
  # countries is a summary from summary.countries with predicted data
  # data is a data used for fitting
  if (is.null(data)) data=countries
  
    
#    geom_point(aes(x=first.detected.med,shape="model",col="detectable"),size=1,
#               position=position_dodgev(height = -0.1)) + 
#    geom_linerange(aes(xmin=first.detected.lo,xmax=first.detected.up,col="detectable"),
#                   position=position_dodgev(height = -0.1)) 
  small.countries=countries[countries$country %in% data$country[data$delta1==1],]
  
    if (what=="submitted") {
      small.countries = small.countries[order(small.countries$Ts),]
      small.countries$country = factor(small.countries$country, levels=small.countries$country)
      detected= ggplot(small.countries,aes(y=country)) 
      detected = detected + 
        geom_point(aes(x=Ts,shape="observed",col="observed"),size=2); 
      detected = detected + geom_point(aes(x=first.submited.med,shape="model",col="detected"),size=1) + 
        geom_linerange(aes(xmin=first.submited.lo,xmax=first.submited.up,col="detected"));
    }
    if (what=="collected") {
      small.countries = small.countries[order(small.countries$Ti.up),]
      small.countries$country = factor(small.countries$country, levels=small.countries$country)
      detected= ggplot(small.countries,aes(y=country)) 
      detected = detected + 
        geom_point(aes(x=Ti.lo,shape="observed",col="observed"),size=2)+
        geom_point(aes(x=Ti.up,shape="observed",col="observed"),size=2)+
        geom_linerange(aes(xmin=Ti.lo,xmax=Ti.up,col="observed")); 
      detected = detected + geom_point(aes(x=first.collected.med,shape="model",col="detected"),size=1) + 
    geom_linerange(aes(xmin=first.collected.lo,xmax=first.collected.up,col="detected"));
    }
    if (what=="first") {
      small.countries = small.countries[order(small.countries$first.introduced.med),]
      small.countries$country = factor(small.countries$country, levels=small.countries$country)
      detected= ggplot(small.countries,aes(y=country))
    detected = detected + geom_point(aes(x=first.introduced.med,shape="model",col="first"),size=1) + 
    geom_linerange(aes(xmin=first.introduced.lo,xmax=first.introduced.up,col="first"))
    }
    detected = detected + theme_bw()+guides(shape="none",size="none") +
    xlab("Date")+ theme(legend.position="top")

        # in all, change country color if it was used in computations
    countries$used.in.computation="not observed"
    countries$used.in.computation[countries$country %in% data$country[data$delta1==1] ]="observed"
    if (what=="submited") {
      countries2 = countries
      countries2= countries2[order(countries2$first.submited.med),]
      countries2$country=factor(countries2$country,levels=countries2$country)
      all = ggplot(countries2,aes(y=country)) + 
        geom_point(aes(x=Ts,col=used.in.computation),size=2) + 
        geom_point(aes(x=first.submited.med,shape="model",col="detected"),size=1) + 
        geom_linerange(aes(xmin=first.submited.lo,xmax=first.submited.up,col="detected"))+
        theme_bw()+guides(colour="none",shape="none",size="none")+xlab("Date")+ theme(legend.position="top")
    } else  if (what=="collected") {
      countries2 = countries
      countries2= countries2[order(countries2$first.collected.med),]
      countries2$country=factor(countries2$country,levels=countries2$country)
      all = ggplot(countries2,aes(y=country)) + 
        geom_point(aes(x=Ti.up,col=used.in.computation),size=2) + 
        geom_point(aes(x=Ti.lo,col=used.in.computation),size=2) +
        geom_linerange(aes(xmin=Ti.lo,xmax=Ti.up,col=used.in.computation)) + 
        geom_point(aes(x=first.collected.med,shape="model",col="detected"),size=1) + 
        geom_linerange(aes(xmin=first.collected.lo,xmax=first.collected.up,col="detected"))+
        theme_bw()+guides(colour="none",shape="none",size="none")+xlab("Date")+ theme(legend.position="top")
    } else if (what=="first") {
      countries2 = countries
      if (!is.null(first.VOC.collected)) {
        countries2=merge(countries2,first.VOC.collected[,c("country","date.col")],by="country",all.x=T)
      }
      countries2= countries2[order(countries2$first.introduced.med),]
      countries2$country=factor(countries2$country,levels=countries2$country)
      all = ggplot(countries2,aes(y=country)) + 
#        geom_point(aes(x=T,col=used.in.computation),size=2) + 
        geom_point(aes(x=first.introduced.med,shape="model",col="first"),size=1) + 
        geom_linerange(aes(xmin=first.introduced.lo,xmax=first.introduced.up,col="first"))+
        theme_bw()+guides(colour="none",shape="none",size="none")+xlab("Date")+ theme(legend.position="top")
      if (!is.null(first.VOC.collected)) {     
        all = all + geom_point(data=countries2, aes(x=date.col,col="VOC"),size=2)
      }
    }
  if (freq==T) {
    pct=ggplot(countries[countries$delta1==1,],aes(y=country)) + 
     geom_linerange(aes(xmin=import.lo, xmax=import.up)) + 
      geom_point(aes(x=import.med,colour="pct introduced"))+
      theme_bw()+guides(shape="none",size="none")+xlab("Date")+ theme(legend.position="top")
      plot=ggarrange(pct, detected, ncol=2, labels="AUTO")
  } else {
    plot=ggarrange(all,detected, ncol=2, labels="AUTO")
  }
  plot
    
}


compute.DIC <- function(summa,data) {
  # take the values from a summary
  # take the values and remove the dexp for r,r2,r3
  mean.logV = mean(summa$res$logL)
  mean.logVEpid= mean(summa$res$logLEpid)
  
  mean.par = apply(summa$res[,c("r","T0","r2","Tcp2","r3","Tcp3","k","s")],2,mean)
  #note rho1, s on the log scale should be averaged
  mean.par=as.list(mean.par)
  mean.par$s=exp(mean(log(summa$res$s)))
  mean.par$r2 = mean.par$r * exp(mean(log(summa$res$r2/summa$res$r)))
  
  logL_cur=NA
  logPrior_cur=NA
  
  ll=summa$metadata$ll
  
  if (ll=="pois"){
    logL = logL_1sub_pois
    conditional=0
    print("computing poisson likelihood for submission")
  } else if (ll=="nb"){
    logL = logL_1sub_nb
    conditional=0
    print("computing NB likelihood for submission")
  }else if (ll=="pois.cond"){
    logL = logL_1sub_pois
    conditional=1
    type=3
    print("computing conditional poisson likelihood for submission")
  }  else if (ll=="nb.cond"){
    logL = logL_1sub_nb
    conditional=1
    type=4
    print("computing conditional NB likelihood for submission")
  } else if (ll=="nb2"){
    logL = logL_1sub_nb2
    conditional=0
    type=5
    print("computing NB likelihood type 2 for submission")
  } else if (ll=="nb2.cond"){
    logL = logL_1sub_nb2
    conditional=1
    type=6
    print("computing conditional NB likelihood type 2 for submission")
  }else if (ll=="pois.auto"){
    logL = logL_1sub_auto_pois
    conditional=0
    type=7
    print("computing pois likelihood for submission with autochtonous cases")
  }else if (ll=="pois.auto.cond"){
    logL = logL_1sub_auto_pois
    conditional=1
    type=8
    print("computing conditional pois likelihood for submission with autochtonous cases")
  } else {
    stop("unknown logLikelihood")
  }
  
  llEpid = summa$metadata$llEpid
  if (is.null(llEpid)) {
    typeEpid=0
    logLEpid=function(...) {return(0)}
    print("no likelihood for UK epid")
  } else  if (llEpid=="pois") {
    typeEpid=1    
    logLEpid=logL_epid_pois
    print("pois likelihood for UK epid")
  } else if (llEpid=="nb") {
    typeEpid=2
    logLEpid=logL1_epid_nb
    print("nb likelihood for UK epid")
  }
  
  logV.mean = logL(c(mean.par$r,mean.par$r2,mean.par$r3), 
                 floor(c(mean.par$T0,mean.par$Tcp2,mean.par$Tcp3)), 
                 mean.par$k, mean.par$s,
                 data$lW, data$ahead, data$incub, data$nCountry,
                 data$Ts, data$Ti.lo,data$Ti.up, data$delta1,
                 data$tKp, data$s, data$dt.s.increase, data$iUK,
                 data$delai.array,
                 data$p.over.R, data$r,
                 conditional)

  logV.Epid.mean = logLEpid(c(mean.par$r,mean.par$r2,mean.par$r3), 
                   floor(c(mean.par$T0,mean.par$Tcp2,mean.par$Tcp3)), 
                   mean.par$k,data$lW, data$epidUK, data$t0Epid)
  
  DIC.intro = -2*2*mean.logV + 2 * (logV.mean)
  DIC.epid = -2*2*mean.logVEpid + 2 * (logV.Epid.mean)
  DIC = -2*2*(mean.logV+mean.logVEpid) + 2 * (logV.mean+logV.Epid.mean)
  c(DIC=DIC, intro=DIC.intro,epid=DIC.epid)
  
  }


MCMC.import.cpp <- function(param, init, data, sd=NULL,R=100,thin=20,warmup=5000,ll="pois", llEpid=NULL, nchains=3) {
  # change to put ahead in the parameters of sampler
  
  npar=7;
  if (packageVersion("importFromUK")=="3.0") {
    print("init and SD must be a list with 8 parameters (r,T0,r2,Tcp2,r3,Tcp3,k,s)")
    if (length(init)!=8 | length(sd)!=8) stop()
    npar=8;
  }
  
  require(coda)
  if (ll=="pois"){
    type=1
    print("computing poisson likelihood for submission")
  } else if (ll=="nb"){
    type=2
    print("computing NB likelihood for submission")
  }else if (ll=="pois.cond"){
    type=3
    print("computing conditional poisson likelihood for submission")
  }  else if (ll=="nb.cond"){
    type=4
    print("computing conditional NB likelihood for submission")
  } else if (ll=="nb2"){
    type=5
    print("computing NB likelihood type 2 for submission")
  } else if (ll=="nb2.cond"){
    type=6
    print("computing conditional NB likelihood type 2 for submission")
  }else if (ll=="pois.auto"){
    type=7
    print("computing pois likelihood for submission with autochtonous cases")
  }else if (ll=="pois.auto.cond"){
    type=8
    print("computing conditional pois likelihood for submission with autochtonous cases")
  }else if (ll=="nb.auto"){
    type=9
    print("computing conditional NB likelihood for submission with autochtonous cases")
  } else {
    stop("unknown option")
  }
  
  
  if (packageVersion("importFromUK")=="1.0") {
    llEpid=NULL; 
  }
  
  if (is.null(llEpid)) {
    typeEpid=0
    print("no likelihood for UK epid")
  } else  if (llEpid=="pois") {
    typeEpid=1    
    print("pois likelihood for UK epid")
  } else if (llEpid=="nb") {
    typeEpid=2
    print("nb likelihood for UK epid")
  }
  
  
  require(parallel)
  require(doParallel)
  cluster <- makeCluster(nchains)
  registerDoParallel(cluster)
  
  seeds =1:nchains
  doPar=rep(0,npar)
  lW = data$lW
  
  resAll <- foreach (chain=seeds, .packages=c("importFromUK","coda")) %dopar% {
    set.seed(as.numeric(Sys.time())+chain)
    
    # param : parameters for optimisation; list
    # init 0 for all parameters
    initPar=rep(0,npar)
    initPar[1]=init$r
    if("r" %in% param) {doPar[1]=1; dim_r=1;print(paste0("chain ",chain,": optimizing r"))} else {print(paste("chain ",chain,": r fixed at ",init$r))}
    
    initPar[2]=init$T0
    if("T0" %in% param) {doPar[2]=1;   print(paste0("chain ",chain,": optimizing T0 between ", format(data$date.start) ,"and ",format(data$date.start+data$T0.max)))} else {print(paste("chain ",chain,": T0 fixed at ",T0.cur))}
    
    initPar[3]=init$r2
    if("r2" %in% param) {
      dim_r=2
      doPar[3]=1; print(paste("chain ",chain,": optimizing r2")); 
      initPar[4]=init$Tcp2;
      
      if ("T2" %in% param) {
        doPar[4]=1;
        print(paste0("chain ",chain,": optimizing T2 between ",as.Date("2020-10-15")," and ", as.Date("2020-12-15")));
      } else {
        print(paste0("chain ",chain," set Tcp2 to ",init$Tcp2));
      }
    } else {
      initPar[3]= NA
      initPar[4] = lW # set Tcp2 to lW
      print(paste0("chain ",chain,": r2 not used. Set to NA"))
    }
    
    initPar[5]=init$r3
    if("r3" %in% param) {
      doPar[5]=1; dim_r=3; print(paste0("chain ",chain,": optimizing r3")); 
      initPar[6]=init$Tcp3;
      print(paste0("chain ",chain,": set Tcp3 to ",init$Tcp3));
    } else {
      initPar[5]= NA
      initPar[6] = lW # set Tcp3 to lW
      print(paste0("chain ",chain,": r3 not used. Set to NA"))
    }
    
    initPar[7]=init$k
    if("k" %in% param) {doPar[7]=1;print(paste0("chain",chain,": optimizing k"))} else {print(paste0("chain ",chain,": k fixed at ",init$k))}
    

    if ((packageVersion("importFromUK")=="3.0") || (packageVersion("importFromUK")=="3.1") || (packageVersion("importFromUK")=="3.2"))  {
      initPar[8]=init$s
      if("s" %in% param) {doPar[8]=1;print(paste0("chain",chain,": optimizing s"))} else {print(paste0("chain ",chain,": s fixed at ",init$s))}
      sd = as.vector(c(sd$r,sd$T0,sd$r2,sd$Tcp2,sd$r3,sd$Tcp3,sd$k,sd$s))
    } else {
      sd = as.vector(c(sd$r,sd$T0,sd$r2,sd$Tcp2,sd$r3,sd$Tcp3,sd$k))
    }
    
    
    if (packageVersion("importFromUK")=="1.0") {
      
      res=sampler(type, R, thin, warmup, doPar,initPar, dim_r,
                  data$lW, data$ahead, data$incub, data$nCountry, 
                  data$Ts, data$Ti.lo, data$Ti.up, data$delta1,
                  data$tKps, data$delai.array, 
                  data$p.over.R, data$r,
                  sd, data$T0.max,chain);
      
      res$res = matrix( res$res,ncol=10)
      colnames(res$res)=c("r","T0","r2","Tcp2","r3","Tcp3","k","logL","logPrior", "lp__")
      names(res$accept)=c("r","T0","r2","Tcp2","r3","Tcp3","k")
      
    } else if (packageVersion("importFromUK") == "2.0") {
      
      res=sampler(type, R, thin, warmup, doPar,initPar, dim_r,
                  data$lW, data$ahead, data$incub, data$nCountry, 
                  data$Ts, data$Ti.lo, data$Ti.up, data$delta1,
                  data$tKps, data$delai.array, 
                  data$p.over.R, data$r,
                  typeEpid, data$epidUK, data$t0Epid,
                  sd, data$T0.max,chain);
      res$res = matrix( res$res,ncol=11)
      colnames(res$res)=c("r","T0","r2","Tcp2","r3","Tcp3","k","logL","logLEpid","logPrior", "lp__")
      names(res$accept)=c("r","T0","r2","Tcp2","r3","Tcp3","k")
      
    } else if (packageVersion("importFromUK") == "3.0") {
      # get UK index
      
      res=sampler(type, R, thin, warmup, doPar,initPar, dim_r,
                  data$lW, data$ahead, data$incub, data$nCountry, 
                  data$Ts, data$Ti.lo, data$Ti.up, data$delta1,
                  data$tKp, data$s, data$dt.s.increase, data$iUK-1,
                  data$delai.array, 
                  data$p.over.R, data$r,
                  typeEpid, data$epidUK, data$t0Epid,
                  sd, data$T0.max,chain);
      res$res = matrix( res$res,ncol=12)
      colnames(res$res)=c("r","T0","r2","Tcp2","r3","Tcp3","k","s","logL","logLEpid","logPrior", "lp__")
      names(res$accept)=c("r","T0","r2","Tcp2","r3","Tcp3","k","s")
    } else if (packageVersion("importFromUK") == "3.2") {
      # get UK index

      res=sampler(type, R, thin, warmup, doPar,initPar, dim_r,
                  data$lW, data$ahead, data$incub, data$nCountry, 
                  data$Ts, data$Ti.lo, data$Ti.up, data$delta1,
                  data$tKp, data$s, data$dt.s.increase, data$iUK-1,
                  data$delai.array, 
                  data$p.over.R, data$r, data$f_before_T0,
                  typeEpid, data$epidUK, data$t0Epid,
                  sd, data$T0.max,chain);
      res$res = matrix( res$res,ncol=12)
      colnames(res$res)=c("r","T0","r2","Tcp2","r3","Tcp3","k","s","logL","logLEpid","logPrior", "lp__")
      names(res$accept)=c("r","T0","r2","Tcp2","r3","Tcp3","k","s")
    }
    
    if (dim_r<2) res$res[,"r2"]=res$res[,"r"]
    if (dim_r<3) res$res[,"r3"]=res$res[,"r2"]
    resMCMC = mcmc(res$res)
    
    
    attr(resMCMC,"accept")=res$accept
    resMCMC
  }  
  stopCluster(cluster)
  # transformation of mcmc
  resAll <- list(mcmc=as.mcmc.list(resAll))
  resAll$metadata <- list(ll=ll, 
                          type=type, 
                          llEpid=llEpid,
                          typeEpid=typeEpid,
                          date.thr=data$date.thr, 
                          date.start=data$date.start,
                          date.compute=date(),
                          data=data)
  #resAll is a 
  return(resAll)
}


#
#d0 = data.london.sc.1.d.1.sub31


#res=sampler(5,10,1,10,c(1,1,0,0,0,0,1,0),c(0.11,30,0.11,30,0.11,30,1,1),1, d0$lW, d0$ahead, d0$incub, d0$nCountry, d0$Ts, d0$Ti.lo, d0$Ti.up, d0$delta1,d0$tKp, d0$s, d0$dt.s.increase, d0$iUK-1,d0$delai.array, d0$p.over.R, d0$r,0, d0$epidUK, d0$t0Epid,c(0.02,8,0.2,8,0.01,4,0.01,1), d0$T0.max,1)
#res2=sampler(2,10,1,10,c(1,1,0,0,0,0,1,0),c(0.11,30,0.11,30,0.11,30,1,1),1, d0$lW, d0$ahead, d0$incub, d0$nCountry, d0$Ts, d0$Ti.lo, d0$Ti.up, d0$delta1,d0$tKp, d0$s, d0$dt.s.increase, d0$iUK-1,d0$delai.array, d0$p.over.R, d0$r,0, d0$epidUK, d0$t0Epid,c(0.02,8,0.2,8,0.01,4,0.01,1), d0$T0.max,1)

