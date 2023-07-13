
myintegrate <- function(f, tmin, tmax, deltat, param, ...){ # my function to simply integrate a function with parameters "param"
  if(tmin == tmax) return(0)
  tmp <- sapply(seq(tmin, tmax, deltat), f, param = param, ...)
  n <- length(tmp)
  weights <- rep(2, n); weights[1] <- 1; weights[n] <- 1
  return(sum(0.5 * (tmax-tmin)/(n-1) * weights * tmp))
}

solve_rtoR <- function(param, r){
  f <- function(t, param, r) dgamma(t, shape = param[1], scale = param[2]) * exp(-r * t)
  return(
    1 /  myintegrate(f, tmin = 0, tmax = 40, delta = 0.1, param = param, r = r)
  )
}

solve_Rtor <- function(param, R) uniroot(f = function(r) {solve_rtoR(param, r) - R}, interval = c(-1, 1))$root

gtpar <- c(2.64, 2.46) # generation time parameter

######################################## LOAD C CODE TO SIMULATE ######################################

codefile = "~/ownCloud/coronavirus/variant_importation/sim_epidemic_R.cpp"
codefile2 = "~/ownCloud/coronavirus/variant_importation/sim_epidemic_R_2.cpp"
sourceCpp(codefile)
sourceCpp(codefile2)

######################################## TESTING THE FUNCTIONS ########################################
if(do_test <- FALSE){
  
  mykappa <- 0.4
  for(i in 1:10){
    sim <- simulate_sigmoidR(R0_pre = 2, R0_post = 2, kappa_pre = mykappa, kappa_post = mykappa, kR = 1, tR = 50)
    if(any(sim$I == -99)) warning("simulation was prematurely stopped due to high incidence")
    if(sum(sim$I > 1)){
      plot(log(sim$I), type = "o", pch = 20, main = i)
      abline(0, solve_Rtor(gtpar, 2))
    }
  }
  
  maxt <- 365
  for(i in 1:20){
    myRlist <- 1.3 + 0.5 * cos(2 * pi / 20 * seq(0., maxt, 0.1))
    sim <- simulate_arbitraryRlist(Rlist = myRlist, kappa = mykappa, tfin = maxt)
    if(any(sim$I == -99)) warning("simulation was prematurely stopped due to high incidence")
    if(sum(sim$I > 1)) plot(log(sim$I), type = "o", pch = 20, main = i)
    points(myRlist, type = "l", col = "red")
    abline(h=1, col = "red")
  }
  
  # for a simulation that worked:
  growths <- c(NA, log(sim$I)[2:366] - log(sim$I)[1:365])
  plot(growths, type = "l", xlim = c(300, 360), ylim = c(-0.05, 0.1))
  points((myRlist[seq(1,3651,10)]-1.1)/8, type = "l", col = "red") # delay between change in R and growth rate
  
}


