#include <Rcpp.h>                   // Rcpp

using namespace Rcpp;
// [[Rcpp::export]]

Rcpp::List simulate_arbitraryRlist(
        NumericVector Rlist,
        double kappa,
        int tfin)
{
    
    double shape_inf = 2.64; // these shape, scale result in mean 6.5 days, sd 4 days; previously was 7.8, 0.833
    double scale_inf = 2.46;

    int i, inf_time_index, I_index;
    int offspring;
    int additional_offspring;
    double R0_t;
    double inf_time;
    //int tfin = 181;
    double dt = 0.1;                                              // time step in the simulation !!! WARNING: Only values of format 10^(-k) allowed
    if(dt != 0.1) stop("dt must be 0.1");
        
    double t = 0.;                                                 // time after importation (0 = importation day)
    int work_index = 0;                                           // work-index for later

    // pre-defining the vectors that keep track of the number of infecteds over time
    int veclength = (int)((double)(tfin)/dt) + 1;

    Rcpp::NumericVector I_work(veclength);                      // number of infecteds working vector
    Rcpp::NumericVector I(tfin+1);                                // number of infecteds at a certain time (days!)
    if(Rlist.length() != veclength) stop("error: Rlist should be of length %d\n", veclength);

    // Initialization = 1 infected individual at time 0
    I_work[work_index] = 1; for(i = 1; i < veclength; i++) I_work[i] = 0; // I_work is table of incident infections per day/dt
    I[0] = 1; for(i = 1; i < tfin+1; i++) I[i] = 0;                       // I is table of incidence per day
    
    //////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////////
    // Stochastic simulation of the epidemic    
    //////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////////

    while (t < (double)tfin)
    {   
        // printf("time: %f\n", t);

        // no. of offspring of individuals added at time t
        offspring = 0;
        
        if(I_work[work_index] > 10000000){
            printf("%f\n", I_work[work_index]);
            stop("I_work[work_index] > 10^6");
        }
        if(work_index >= veclength){
            stop("work_index >= %d\n", veclength);
        }

        for (i = 1; i <= I_work[work_index]; i++)                 // if no infections added at time t, this step is skipped
        {
            // compute R0(t)
            R0_t = Rlist[work_index];      // formula provided by François
            
            // draw a random number of offspring
            additional_offspring =  R::rnbinom(kappa, kappa / (R0_t + kappa));
            offspring += additional_offspring; // size (dispersion) parameter and proba of success; mean is size * (1-prob) / prob
            // printf("%d\n", additional_offspring);
        }

        // loop over offsprings from individuals that were added at time t (if 0 offspring, this loop is skipped)
        for (i = 1; i <= offspring; i++)
        {   
            // Computing infection time of offspring
            inf_time = t + R::rgamma(shape_inf, scale_inf);
            
            // IMPORTANT THAT dt = 10^(-k)! otherwise adapt this rounding formula!
            inf_time_index = roundf(inf_time/dt);       // precise index of infection time of the offspring
            I_index = ceilf(inf_time);                  // day index of infection time of the offspring
            
            if (inf_time_index < veclength)             // if infection time after end of simulation ignore this offspring
            {
                I_work[inf_time_index]++;               // increment I_work at the right time
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
        //printf("work index: %d\n", work_index);
    }
       
    return(List::create(
        Named("I") = I
    ));
 
}