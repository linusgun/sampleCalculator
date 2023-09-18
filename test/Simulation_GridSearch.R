# load all packages used in this vignette
library('paramtest')
library('pwr')
library('ggplot2')
library('knitr')
library('nlme')
library('lavaan')
library('dplyr')


power_of_study <- function(iter, N, pop_mean, pop_var, b1, var_b1, design_name, baseline, alpha) {
  
  library('dplyr')
  library('lme4')
  source("C:/Users/dv661427/OneDrive - Dalhousie University/Simulation Code/sim_functions.R")
  
  #iter: it's just a required parameter for grid_search function
  data=lci_simulation_cole(N, pop_mean, pop_var, b1, var_b1,design_name)
  
  #tx_info=lci_study(data,design_name,baseline,alpha)
  tx_info=lci_study_lik(data,design_name,baseline,alpha)
  
  return(tx_info)
  
}

# Function for simulation
power_estimation <- function(power_of_study, sample_sizes, b1_values, designs, pop_mean, pop_var, var_b1=1, baseline, power_threshold=0.8, alpha=0.05, iter=5000, ncpus=4) {
  
  gs_run <- grid_search(power_of_study, params=list(N=sample_sizes, b1=b1_values,design_name=designs),
                        n.iter=iter, output='data.frame', parallel='snow', ncpus=ncpus,
                        pop_mean=pop_mean, pop_var=pop_var, var_b1=var_b1, alpha=alpha, baseline=baseline)
  
  return(gs_run)
  
}

# Code for reporting the simulation | 'parallel','crossover','longitudinal' | sample_sizes=c(20,30,40,50,60,70,80), b1_values=c(-2,-1.5)  | sample_sizes=c(90,100,140,150,170,200,220), b1_values=c(-1,-0.8,-0.5)
Sys.time()
pe=power_estimation(power_of_study,designs=c('parallel'), sample_sizes=c(20,30,40,50,60), b1_values=c(-2,-1.5,-1,-0.8,-0.5), pop_mean=8.1, pop_var=1.96,iter=800,baseline=FALSE)
Sys.time()

results_table_gd=results(pe) %>%
  group_by(design_name.test, N.test,b1.test) %>%
  summarise(
    power_tx=mean(sig_tx)
  )

results_table_gd[results_table_gd$b1.test==-1,]