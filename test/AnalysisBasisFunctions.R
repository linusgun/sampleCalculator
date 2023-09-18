

source("C:/Users/dv661427/OneDrive - Dalhousie University/Simulation Code/sim_functions.R")


design='parallel'
N=40
pMean=8.1
psd=1.4
iter=1000
tx_effect=-1

par(mfrow=c(2,1),pin=c(3,1.9),mai=c(0.4, 0.4, 0.4, 0.3))

color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
buckets = seq(0,20,by=1)

for (i in 1:iter) {

  dt=lci_simulation(N=N, pop_mean=pMean, pop_sd=pVar, b1=-1, b1_sd=1,design_name=design)
  h=hist(dt$lci_orig,breaks=buckets,freq=FALSE,plot=FALSE)
  
  h_tx = hist(dt[dt$tx==1,'lci_2'],breaks=buckets,freq=FALSE,plot=FALSE)
  h_ntx = hist(dt[dt$tx==0,'lci_2'],breaks=buckets,freq=FALSE,plot=FALSE)
  
  if (i==1) {  
    plot(h$mids,h$density,type = 'l',col=c('gray'),ylim=c(0,1),xlim=c(5,20))
    ymean=h$density
    
    #adding line for lci_2
    # lines(h_tx$mids,h_tx$density,type = 'l',col=c('lightgray'))
    ymean_tx=h_tx$density
    ymean_ntx=h_ntx$density
    
  }else{
    lines(h$mids,h$density,type = 'l',col=c('gray'))
    ymean=rbind(ymean,h$density)
    
    #adding line for lci_2
    # lines(h_tx$mids,h_tx$density,type = 'l',col=c('lightgray'))
    ymean_tx=rbind(ymean_tx,h_tx$density)
    ymean_ntx=rbind(ymean_ntx,h_ntx$density)
  }
  
} 

ymean=colMeans(ymean)

lines(h$mids,ymean,type = 'o',col=c('black'))

ymean_ntx=colMeans(ymean_ntx)

lines(h$mids,ymean_ntx,type = 'o',col=c('blue'))

ymean_tx=colMeans(ymean_tx)

lines(h$mids,ymean_tx,type = 'o',col=c('red'))


# Cole's


buckets = seq(0,20,by=1)

for (i in 1:iter) {
  
  dt=lci_simulation_exact_cole(N=N, pop_mean=pMean, pop_sd=pVar, b1=-1, b1_sd=1,design_name=design)
  h=hist(dt$lci_orig,breaks=buckets,plot=FALSE)
  
  h_tx = hist(dt[dt$tx==1,'lci_2'],breaks=buckets,freq=FALSE,plot=FALSE)
  h_ntx = hist(dt[dt$tx==0,'lci_2'],breaks=buckets,freq=FALSE,plot=FALSE)
  
  if (i==1) {  
    plot(h$mids,h$density,type = 'l',col=c('gray'),ylim=c(0,1),xlim=c(5,20))
    ymean=h$density
    
    #adding line for lci_2
    # lines(h_tx$mids,h_tx$density,type = 'l',col=c('lightgray'))
    ymean_tx=h_tx$density
    ymean_ntx=h_ntx$density
    
  }else{
    lines(h$mids,h$density,type = 'l',col=c('gray'))
    ymean=rbind(ymean,h$density)
    
    #adding line for lci_2
    # lines(h_tx$mids,h_tx$density,type = 'l',col=c('lightgray'))
    ymean_tx=rbind(ymean_tx,h_tx$density)
    ymean_ntx=rbind(ymean_ntx,h_ntx$density)
  }
  
} 

ymean=colMeans(ymean)

lines(h$mids,ymean,type = 'o',col=c('black'))

ymean_ntx=colMeans(ymean_ntx)

lines(h$mids,ymean_ntx,type = 'o',col=c('blue'))

ymean_tx=colMeans(ymean_tx)

lines(h$mids,ymean_tx,type = 'o',col=c('red'))


#analizing ui and eij terms in exact Cole

# === e_ij

buckets = seq(-10,10,by=1)

for (i in 1:iter) {
  
  dt=lci_simulation_exact_cole(N=N, pop_mean=pMean, pop_sd=pVar, b1=-1, b1_sd=1,design_name=design)
  h=hist(dt$e_ij,breaks=buckets,plot=FALSE)
  
  if (i==1) {  
    plot(h$mids,h$density,type = 'l',col=c('gray'),ylim=c(0,1),xlim=c(-10,10))
    ymean=h$density
    
    
  }else{
    lines(h$mids,h$density,type = 'l',col=c('gray'))
    ymean=rbind(ymean,h$density)
    
  }
  
} 

ymean=colMeans(ymean)

lines(h$mids,ymean,type = 'l',col=c('black'))

# === u_i

buckets = seq(-10,10,by=1)

for (i in 1:iter) {
  
  dt=lci_simulation_exact_cole(N=N, pop_mean=pMean, pop_sd=pVar, b1=-1, b1_sd=1,design_name=design)
  h=hist(dt$u_i,breaks=buckets,plot=FALSE)
  
  if (i==1) {  
    plot(h$mids,h$density,type = 'l',col=c('gray'),ylim=c(0,1),xlim=c(-10,10))
    ymean=h$density
    
    
  }else{
    lines(h$mids,h$density,type = 'l',col=c('gray'))
    ymean=rbind(ymean,h$density)
    
  }
  
} 

ymean=colMeans(ymean)

lines(h$mids,ymean,type = 'l',col=c('black'))

#analizing msmt_error term in Gamma Simulation

buckets = seq(-10,10,by=1)

for (i in 1:iter) {
  
  dt=lci_simulation(N=N, pop_mean=pMean, pop_sd=pVar, b1=-1, b1_sd=1,design_name=design)
  h=hist(dt$msmt_error,breaks=buckets,plot=FALSE)
  
  if (i==1) {  
    plot(h$mids,h$density,type = 'l',col=c('gray'),ylim=c(0,1),xlim=c(-10,10))
    ymean=h$density
    
    
  }else{
    lines(h$mids,h$density,type = 'l',col=c('gray'))
    ymean=rbind(ymean,h$density)
    
  }
  
} 

ymean=colMeans(ymean)

lines(h$mids,ymean,type = 'l',col=c('black'))

#====== ANALIZING HOW BALANCED ARE THE SAMPLES


ptx_cole=c()
ptx_gamma=c()

for (i in 1:iter) {
  
  dtc=lci_simulation_exact_cole(N=N, pop_mean=pMean, pop_sd=pVar, b1=-1, b1_sd=1,design_name=design)
  dtg=lci_simulation(N=N, pop_mean=pMean, pop_sd=pVar, b1=-1, b1_sd=1,design_name=design)

  ptx_cole=append(ptx_cole,(dim(dtc[dtc['tx']==1,])[1]/N))
  ptx_gamma=append(ptx_gamma,(dim(dtg[dtg['tx']==1,])[1]/N))
  # print(N)  
  # print(dim(dtc[dtc['tx']==1,])[1]) 

  # if ((dim(dtc[dtc['tx']==1,])[1]/N)>1) {
  #   
  #   print(dim(dtc[dtc['tx']==1,])[1])
  #   faildt=dtc
  #   
  # }
  
  # print(dim(dtg[dtg['tx']==1,])[1])  
} 

boxplot(ptx_cole,ptx_gamma,names=c("Cole", "Gamma"),ylim=c(0,1.5))


dtc=lci_simulation_exact_cole(N=N, pop_mean=pMean, pop_sd=pVar, b1=-1, b1_sd=1,design_name=design)

