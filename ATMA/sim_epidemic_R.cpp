#include <Rcpp.h>                   // Rcpp

using namespace Rcpp;
// [[Rcpp::export]]

Rcpp::List simulate_sigmoidR(
    double R0_pre, 
    double R0_post,
    double kappa_pre,
    double kappa_post,
    double kR,
    double tR)
{
    
    double shape_inf = 2.64;
    double scale_inf = 2.46;

    int i, inf_time_index, I_index;
    int offspring;
    int additional_offspring;
    double R0_t;
    double inf_time;
    int tfin = 181;
    double dt = 0.1;                                            // time step in the simulation !!! WARNING: Only values of format 10^(-k) allowed

    double t = 0;                                               // time after immigration (0 = immigration day)
    int work_index = 0;                                           // work-index for later

    // pre-defining the vectors that keep track of the number of infecteds over time
    int veclength = (int)((double)(tfin)/dt) + 1;
    //printf("%d\n", veclength);

    Rcpp::NumericVector I_work(veclength);                      // number of infecteds working vector
    Rcpp::NumericVector I(tfin+1);                              // number of infecteds at a certain time (days!)

    // Initialization = 1 infected individual at time 0
    I_work[work_index] = 1; for(i = 1; i < veclength; i++) I_work[i] = 0;
    I[0] = 1; for(i = 1; i < tfin+1; i++) I[i] = 0;
    

    // Definition of kappa
    double kappa = kappa_pre;
    
    
    //////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////////
    // Stochastic simulation of the epidemic    
    //////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////////

    while (t < (double)tfin)
    {   
        // Update kappa in case of reaching the lockdown date
        if (t == tR) kappa = kappa_post;
        
        // no. of offspring of individuals added at time t
        offspring = 0;
        for (i = 1; i <= I_work[work_index]; i++)                 // if no infections added at time t, this step is skipped
        {
            // compute R0(t)
            R0_t = R0_pre + (R0_post-R0_pre)/(1. + exp (-kR * (t-tR)));      // formula provided by François
            
            // draw a random number of offspring
            additional_offspring =  R::rnbinom(kappa, kappa / (R0_t + kappa));
            offspring += additional_offspring; // size (dispersion) parameter and proba of success; mean is size * (1-prob) / prob
            //printf("%d\n", additional_offspring);
        }

        // loop over offsprings from individuals that were added at time t (if 0 offspring, this loop is skipped)
        for (i = 1; i <= offspring; i++)
        {   
            // Computing infection time of offspring
            inf_time = t + R::rgamma(shape_inf, scale_inf);
            
            // IMPORTANT THAT dt = 10^(-k)! otherwise adapt this rounding formula!
            inf_time_index = roundf(inf_time/dt);     // precise index of infection time of the offspring
            I_index = ceilf(inf_time);                // day index of infection time of the offspring
            
            if (inf_time_index < veclength)              // if infection time after end of simulation ignore this offspring
            {
                I_work[inf_time_index]++;
            }
            
            if (I_index <= tfin)                      // if infection time after end of simulation ignore this offspring
            {
                I[I_index]++;
                if(I[I_index] > 1000000){
                    i = offspring + 1;
                    t = tfin;
                    I[I_index] = -99; // indicator that incidence was too large, stop
                }
            }            
        }
        
        // update time + index 
        t += dt; // increment t by dt
        work_index++; // index of the table
        //printf("%d\n", work_index);
    }
       
    return(List::create(
        Named("I") = I
    ));
 
}






