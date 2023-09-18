
# ======== Simulating the LCI in EXACTLY the same way Cole did ============

lci_simulation_exact_cole <- function(N, pop_mean, pop_sd, b1, b1_sd=1,design_name, study_duration=1, msm_per_year=2) {
                                     
  if (design_name=='parallel') {
    
    #preschool simulation <6yo - adjusted for baseline
    # to keep the name of the variables
    n=N
    txsd=b1_sd
    
    # set obs `n'
    df <- data.frame(matrix(ncol = 0, nrow = n))
    
    #Why tx is arg if then it's replaced
    #generate id = _n
    df$id=seq(1,n,by=1)
    
    #generate tx = rbinomial(1,0.5)
    df$tx=rbinom(n,1,0.5)
    
    #gen txeffect = rnormal(`tx',`txsd')
    df$txeffect = rnorm(n,b1,txsd)
    
    #gen u_i = runiform(-2,3.9)
    df$u_i= runif(n,-2,3.9) 
    
    #gen skew = runiform() if u_i>0.5
    m=length(df[df['u_i'] > 0.5,'u_i'])
    df[df$u_i > 0.5, 'skew']=runif(m) #when df$u <=0.5 skew = NA
    
    #replace u_i = runiform(-1.4,1.0) if skew<0.8
    m=length(df[!is.na(df['skew']) & df['skew'] < 0.8,'skew']) #should I select the NA? Currently don't
    df[!is.na(df['skew']) & df['skew'] < 0.8,'u_i']=runif(m, -1.4, 1)
    
    # ===== The expanding step
    
    df<-rbind(df,df)
    df$time=1
    df[seq(n+1,n*2,by=1),'time']=2
    
    # ========
    
    n=n*2
    
    # generate crudeeij = rnormal(0,1.27)
    df$crudeeij = rnorm(n,0,1.27)
    
    # gen 	skew2 = runiform() if crudeeij <-1
    m=length(df[df['crudeeij'] < -1,'crudeeij'])
    df[df['crudeeij'] < -1,'skew2'] = runif(m) #when df$crudeeij >= -1 skew2 = NA
    
    # replace crudeeij = runiform(0,4.8) if skew2<0.15 
    m=length(df[!is.na(df['skew2']) & df['skew2'] < 0.15,'skew2'])
    df[!is.na(df['skew2']) & df['skew2'] < 0.15,'crudeeij'] = runif(m, 0, 4.8)  
    
    # replace crudeeij = runiform(-1,0) if crudeeij<-2.7
    m=length(df[df['crudeeij'] < -2.7,'crudeeij'])
    df[df['crudeeij'] < -2.7,'crudeeij'] = runif(m, -1, 0)  
    
    # gen 	skew3 =runiform() if crudeeij>0.3 & crudeeij<2.8
    m=length(df[df['crudeeij'] > 0.3 & df['crudeeij'] < 2.8,'crudeeij'])
    df[df['crudeeij'] > 0.3 & df['crudeeij'] < 2.8,'skew3'] = runif(m) #when df$crudeeij not meet conditions skew3 = NA
    
    # replace crudeeij = runiform(-1,0) if skew3<0.2
    m=length(df[!is.na(df['skew3']) & df['skew3'] < 0.2,'skew3'])
    df[!is.na(df['skew3']) & df['skew3'] < 0.2,'crudeeij'] = runif(m, -1, 0) 
    
    # ==========
    
    
    #generate crudelci = 9.1 + u_i + crudeeij  
    df$crudelci = 9.1 + df$u_i + df$crudeeij 
    
    # gen e_ij = rnormal(-0.8,0.8) if crudelci<=8
    m=length(df[df['crudelci'] <=8,'crudelci'])
    df[df['crudelci'] <=8,'e_ij'] = rnorm(m,-0.8,0.8) #when df$crudelci not meet conditions e_ij = NA
    
    # replace e_ij = rnormal(0.2,1.3) if crudelci>8 & crudelci<13
    m=length(df[df['crudelci'] > 8 & df['crudelci'] < 13,'crudelci'])
    df[df['crudelci'] > 8 & df['crudelci'] < 13,'e_ij'] = rnorm(m,0.2,1.3) 
    
    # replace e_ij = rnormal(2.5,1.7) if crudelci>=13
    m=length(df[df['crudelci'] >= 13,'crudelci'])
    df[df['crudelci'] >= 13,'e_ij'] = rnorm(m,2.5,1.7) #Since here and in the last block of code use the complement e_ij has no NA
    
    # generate lci = 8.5 + txeffect*tx + u_i + e_ij if time==2
    sel=(df['time']==2)
    df[sel,'lci_2'] = 8.5 + df$txeffect[sel]*df$tx[sel] + df$u_i[sel] + df$e_ij[sel]
    
    # replace lci = 8.5 + u_i + e_ij if time==1
    sel=(df['time']==1)
    df[sel,'lci_2'] = 8.5 + df$u_i[sel] + df$e_ij[sel]
    
    # replace lci = runiform(5,7) if lci<5 & time==1 
    m=length(df[df['lci_2'] < 5 & df['time'] == 1 ,'lci_2'])
    df[df['lci_2'] < 5 & df['time'] == 1,'lci_2'] = runif(m,5,7)
    
    #bysort id: gen lcibase = lci[1]
    df$lci_orig[df$time==2]=df$lci_2[df$time==1]
    df <- df[order(df$id),]
    
    #drop if time ==1 
    df = df[df['time']!=1,]
    
  }
  
  if (design_name=='longitudinal') {
    
    #study_duration: Duration of the study in years
    #msm_per_year: # of follow up measurements per year
    
    n1=as.integer(study_duration*msm_per_year)+1
    
    n=N
    txsd=b1_sd
    
    # set obs `n'
    df <- data.frame(matrix(ncol = 0, nrow = n))
    
    #generate id = _n
    df$id=seq(1,n,by=1)
    
		#gen slope = `slope'
    # slope will be b1. It is assumed to be an annual slope
    slope=b1
    
    #gen u_i = runiform(-2,3.9)
    df$u_i= runif(n,-2,3.9) 
    
    #gen skew = runiform() if u_i>2 
    m=length(df[df['u_i'] > 2,'u_i'])
    df[df$u_i > 2, 'skew']=runif(m) #when df$u <=2 skew = NA
    
    #replace u_i = runiform(-1.5,0.5) if skew<0.70
    m=length(df[!is.na(df['skew']) & df['skew'] < 0.7,'skew']) 
    df[!is.na(df['skew']) & df['skew'] < 0.7,'u_i']=runif(m, -1.5, 0.5)
    
    #gen skewt = runiform() if u_i>0 & u_i<2
    m=length(df[df['u_i'] >0 & df['u_i'] < 2,'u_i'])
    df[df['u_i'] >0 & df['u_i'] < 2,'skewt'] = runif(m)
    
    #replace u_i =runiform(-1,0.5) if skewt<0.6
    selRows=!is.na(df['skewt']) & df['skewt'] < 0.6
    m=length(df[selRows,'skewt']) 
    df[selRows,'u_i']=runif(m, -1, 0.5)
    
    #expand `n1'
    #    bysort id: generate time = _n
    
    df$time=1   
    df1=df
    for (i in 2:n1) {
      df<-rbind(df,df1)
      df[seq(((i-1)*n)+1,i*n,by=1),'time']=i
    }
    
    # ========
    
    n=n*n1
  
    #generate crudeeij = rnormal(0,1.15)
    df$crudeeij = rnorm(n,0,1.15)
    
		#gen 	skew2 = runiform() if crudeeij <-0.4
    selRows=df['crudeeij'] < 0.4
    m=length(df[selRows,'crudeeij'])
    df[selRows,'skew2'] = runif(m) #when df$crudeeij >= 0.4 skew2 = NA
    
		#replace crudeeij = runiform(2,5.5) if skew2<0.2 
    selRows=!is.na(df['skew2']) & df['skew2'] < 0.2
    m=length(df[selRows,'skew2'])
    df[selRows,'crudeeij'] = runif(m,2,5.5)
    
    #generate crudelci = 9 + u_i + crudeeij
    df$crudelci = 9 + df$u_i + df$crudeeij

		#gen e_ij = rnormal(-0.5,0.5) if crudelci<=8
    selRows=!is.na(df['crudelci']) & df['crudelci'] <= 8
    m=length(df[selRows,'crudelci'])
    df[selRows,'e_ij'] = rnorm(m,-0.5,0.5)
    
		#gen skew3 = runiform() if e_ij>0
    selRows=!is.na(df['e_ij']) & df['e_ij'] >0
    m=length(df[selRows,'e_ij'])
    df[selRows,'skew3'] = runif(m)
    
		#replace e_ij = runiform(-3,-1) if skew3<0.1
    selRows=!is.na(df['skew3']) & df['skew3'] <0.1
    m=length(df[selRows,'skew3'])
    df[selRows,'e_ij'] = runif(m,-3,-1)
		
		#replace e_ij = rnormal(0.5,1) if crudelci>8 & crudelci<13 
    selRows=!is.na(df['crudelci']) & df['crudelci']>8 & df['crudelci']<13
    m=length(df[selRows,'skew3'])
    df[selRows,'e_ij'] = rnorm(m,0.5,1)
    
		#gen skew4 = rnormal() if e_ij>0.8 & e_ij<2.5 & crudelci>8 & crudelci<13
    selRows=df['e_ij']>0.8 & df['e_ij']<2.5 & df['crudelci']>8 & df['crudelci']<13
    m=length(df[selRows,'e_ij'])
    df[selRows,'skew4'] = rnorm(m)
    
    #replace e_ij = runiform(2,5.5) if skew3<0.05
    selRows=!is.na(df['skew3']) & df['skew3']<0.05
    m=length(df[selRows,'skew3'])
    df[selRows,'e_ij'] = runif(m,2,5.5)
    
		#replace e_ij = rnormal(0.5,1.5) if crudelci>=13 
    selRows=df['crudelci']>=13
    m=length(df[selRows,'crudelci'])
    df[selRows,'e_ij'] = rnorm(m,0.5,1.5)
    
    #generate lci = 8.5 + u_i + e_ij if time==1
    # Instead of lci I used lci_2 for standardize nomenclature
    selRows=df['time']==1
    m=length(df[selRows,'time'])
    df[selRows,'lci_2'] = 8.5 + df$u_i[selRows] + df$e_ij[selRows]
    
    #replace lci = 8.5 + u_i + e_ij + slope if time>1 
    selRows=df['time']>1
    m=length(df[selRows,'time'])
    df[selRows,'lci_2'] = 8.5 + df$u_i[selRows] + df$e_ij[selRows] + (slope/msm_per_year)*(df$time[selRows]-1)
     
    # replace lci = 8.5 + u_i + e_ij + ((slope/(`n1'-1))*(time-1)) if time>1  // * Study Duration of 0.25 Years 
    # replace lci = 8.5 + u_i + e_ij + slope if time>1                        // * Study Duration of 0.5 Years 
    # replace lci = 8.5 + u_i + e_ij + ((slope/(2))*(time-1)) if time>1       // * Study Duration of 0.5 Years 6months measured quarterly (3 times)
    # replace lci = 8.5 + u_i + e_ij + slope if time>1                        // * Study Duration of 1 Year 
    # replace lci = 8.5 + u_i + e_ij + ((slope/2)*(time-1)) if time>1         // * Study Duration of 1 Year * semiannual measurement (3)
  }
  
  return(df)
  
}

