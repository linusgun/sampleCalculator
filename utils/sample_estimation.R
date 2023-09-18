# Ref: https://cran.r-project.org/web/packages/paramtest/vignettes/Simulating-Power.html

# load all packages used in this vignette
library('paramtest')
library('pwr')
library('ggplot2')
library('knitr')
library('nlme')
library('lavaan')
library('dplyr')
library('lme4')
library("lmerTest")
library("lmtest")


size_search <- function(b1_values, designs, pop_mean, pop_sd, b1_sd=1, baseline=TRUE, study_duration=1, msm_per_year=2, power_threshold=0.8, alpha=0.05, iter=1000, sd_err=1.27) {
  
  #source("C:/Users/dv661427/OneDrive - Dalhousie University/Simulation Code/ShinyCalculator/cfSampleCalculator/utils/sim_functions.R")
  #source("C:/Users/dv661427/OneDrive - Dalhousie University/Simulation Code/ShinyCalculator/cfSampleCalculator/utils/signif_functions.R")
  
  #source("/utils/sim_functions.R")
  #source("/utils/signif_functions.R")
  
  N=seq(10,500,by=10)

  result=data.frame()
  
  for (design_name in designs) {
    
    for (b1 in b1_values) {
    
      power_reached=FALSE
      j=1
      while (!power_reached & j<=length(N)) {
        
        sigs=data.frame()
        check=c()
        
        #if (j==1 | N[j] %% 100 ==0) {
          #print(paste("Simulating samples of ", N[j]," for: ",design_name," design and beta=",b1," ..."))
          
        #}
        
        for (i in 1:iter) {
          
          data=lci_simulation(N[j], pop_mean, pop_sd, b1, b1_sd,design_name, study_duration, msm_per_year, sd_err)
          tx_info=study_significance(data,design_name,baseline,alpha)

          sigs[i,1]=tx_info$sig_ttest
          sigs[i,2]=tx_info$sig_ttest_lme
          sigs[i,3]=tx_info$sig_likrtest2
          
          #result[nrow(result) + 1,
          #       c('design','tx','Sample','iter','pv_ttest','pv_ttest_lme','pv_likrtest2','mean_power1','mean_power2','mean_power3')] <-
          #       c(design_name, b1, N[j], i, tx_info$pvalue_ttest, tx_info$pvalue_ttest_lme, tx_info$pvalue_likrtest2, mean(sigs1),mean(sigs2),mean(sigs3))
          
          #print(paste(sigs1[i]," ",sigs2[i]," ",sigs3[i]))
          
        }
        
        check[1:3]=FALSE
        
        for (k in 1:3) {
          if (!is.na(mean(sigs[!is.na(sigs[,k]),k]))) {
            check[k]=mean(sigs[!is.na(sigs[,k]),k])>=power_threshold
          }
        }
        
        power_reached=(check[1] & check[2] & check[3])
        
        #print(paste("Done with size: ", N[j]," for: ",design_name," design and beta=",b1," ..."))
        print(paste("j: ",j,"Mean pvalues reached: ttest=",mean(sigs[!is.na(sigs[,1]),1]),", ttest lme: ", mean(sigs[!is.na(sigs[,2]),2]),"and likrtest: ",mean(sigs[!is.na(sigs[,3]),3])))
        
        result[nrow(result) + 1, c('design','tx','Sample','m_power_ttest','m_power_ttest_lme','m_power_likrtest')] <-
                                c(design_name, b1, N[j], mean(sigs[!is.na(sigs[,1]),1]),mean(sigs[!is.na(sigs[,2]),2]),mean(sigs[!is.na(sigs[,3]),3]))
        j=j+1
      
      }
     
      print(paste("Found for: ",design_name," design and beta=",b1," ..."))
       
    }
    
  }
  
  return(result)
  
}
