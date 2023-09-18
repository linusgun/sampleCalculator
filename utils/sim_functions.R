

BalanceSample<-function(bf,tx,N){
  
  # tx is the treatment vector
  while (length(tx[tx==0])>bf*N | length(tx[tx==0])<(1-bf)*N) {
    
    adj=1+as.integer(0.05*N)
    
    if (length(tx[tx==0])>bf*N) {
      z=length(tx[tx==0])
      tx[tx==0][as.integer(runif(adj,1,z))] <- 1  
    }
    if (length(tx[tx==0])<bf*N) {
      z=length(tx[tx==1])
      tx[tx==1][as.integer(runif(adj,1,z))] <- 0  
    }
    
  }
  
  return(tx)
  
}

# Simulating the LCI 

lci_simulation <- function(N, pop_mean, pop_sd, b1, b1_sd=1,design_name, study_duration=1, msm_per_year=2,sd_err=1.27) {
  
  #N:Sample size
  #pop_mean: Population mean
  #pop_sd: Population standard deviation
  #design_name: ['parallel','crossover','longitudinal']
  #For longitudinal design the value of the slope is equal to the argument b1. 
  #The argument b1_sd is not used for longitudinal simulation
  #The variable lci_2 is the target one of all designs. In longitudinal design lci_2 when time=1 is the baseline value
  
  #Calculating the parameters of the gamma
  scale_lci=(pop_sd^2)/pop_mean
  shape_lci=pop_mean/scale_lci
  
  #Simulating values and effects
  id=seq(1,N,by=1)
  
  lci_orig <- rgamma(N, shape=shape_lci, scale=scale_lci)
  
  #truncating gamma
  impossible_cases=length(lci_orig[lci_orig<5 | lci_orig>20])
  while (impossible_cases>0) {
    lci_orig[lci_orig<5 | lci_orig>20]=rgamma(impossible_cases, shape=shape_lci, scale=scale_lci)
    impossible_cases=length(lci_orig[lci_orig<5 | lci_orig>20])
  }
  
  beta1<- rnorm(N,b1,b1_sd)
  
  #Assumptions
  #the random error (accounting for measurement error, and other factors) is distributed as N(0,1.2*b1_sd)
  # Balanced sample
  
  if (design_name=='parallel') {
    msmt_error <- rnorm(N,0,sd_err)
    tx <- rbinom(N, 1, 0.5)
    
    # balancing the treated and non treated observations. 
    ## This is no problem for bigger sample, but could be unbalanced in small samples
    tx=BalanceSample(0.6,tx,N)

    lci_2 <- lci_orig + tx*beta1 + msmt_error

    df<-data.frame(id,lci_orig,tx,beta1,msmt_error,lci_2)
    
    # Adjust lci_2 less than 5 after treatment
    sel=df$lci_2<5
    z=length(df[sel,'lci_2'])
    # df[sel,'lci_2']=5+abs(rnorm(z,0,b1_sd*0.2))
    df[sel,'lci_2']=runif(z,5,6)
    
  }
  
  if (design_name=='crossover') {
    
    txi<-rbinom(N, 1, 0.5) #variable that represents: initial treatment and/or direction
    txi=BalanceSample(0.6,txi,N)
    
    tx <-  txi #direction variable 0: placebo first | 1: tx first
    
    df<-data.frame(id,lci_orig,beta1,txi,tx)
    df <- bind_rows(replicate(2, df, simplify = FALSE)) #duplicate the df to simulate both treatments for each person
    
    # setting the 2nd part of the study. Giving or removing the treatment to each patient
    df[(N+1):(2*N),'tx']=1-df[1:N,'tx']
    
    msmt_error <- rnorm(N*2,0,sd_err)
    
    df$lci_2 <- df$lci_orig + df$tx*df$beta1 + msmt_error
    
    df[df$tx!=df$txi,'lci_orig']=df[df$tx!=df$txi,'lci_orig'] + rnorm(N,0,1) #adding random noise to the lci_orig for the measurement post washout period

    
  }
  
  if (design_name=='longitudinal') {
    
    n1=as.integer(study_duration*msm_per_year)+1

    msmt_error <- rnorm(N,0,b1_sd*1.2)
    df<-data.frame(id,lci_orig,b1,msmt_error,lci_2=lci_orig)
    
    df$time=1
    df1=df
    for (i in 2:n1) {
      
      df<-rbind(df,df1)
      timeRows=seq(((i-1)*N)+1,i*N,by=1)
      
      df[timeRows,'time']=i
      df[timeRows,'b1']=b1
      df[timeRows,'msmt_error']=rnorm(N,0,sd_err)
      df[timeRows,'lci_2']=df$lci_orig[timeRows] + (b1/msm_per_year)*(i-1) + df$msmt_error[timeRows]

      
    }
    
    # ========
    
    # Adjust lci_2 less than 5 on every time
    sel=df$lci_2<5
    z=length(df[sel,'lci_2'])
    df[sel,'lci_2']=runif(z,5,6)
    
    
  }
  
  return(df)
  
}
