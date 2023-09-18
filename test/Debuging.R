
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

source("C:/Users/dv661427/OneDrive - Dalhousie University/Simulation Code/ShinyCalculator/cfSampleCalculator/utils/sim_functions.R")
source("C:/Users/dv661427/OneDrive - Dalhousie University/Simulation Code/ShinyCalculator/cfSampleCalculator/utils/signif_functions.R")

# My search function, for searching until reach a power threshold
results_table_ms=size_search(b1_values=c(-2), b1_sd=1, designs=c('parallel'), pop_mean=8.1, pop_sd=1.4, iter=800,baseline=TRUE)

Sys.time()
results_table_ms=size_search(b1_values=c(0.2, 0.4, 0.6), designs=c('longitudinal'), pop_mean=8.1, pop_sd=1.4, study_duration=2, msm_per_year=2, iter=300)
Sys.time()
results_table_ms


# ==== For DEBUGGING ====

design='parallel'
N=40
pMean=8.1
pVar=1.4
b1SD=1
dtd=lci_simulation(N, pop_mean=pMean, pop_sd=pVar, b1=-2, b1_sd=b1SD,design)
dtc=lci_simulation_cole(N, pop_mean=pMean, pop_sd=pVar, b1=-2, b1_sd=b1SD,design)

tx_info=study_significance(dtd,design_name=design,baseline=TRUE,alpha)

par(mfrow=c(2,1),pin=c(3,1.9),mai=c(0.4, 0.4, 0.4, 0.3))

hist(dtd$lci_orig,freq=FALSE,main="Gamma",xlab="",cex.main=0.9, cex.lab=1.5, cex.axis=0.75)
hist(dtc$lci_orig,freq=FALSE,main="Cole's",xlab="",cex.main=0.9, cex.lab=1.5, cex.axis=0.75)

dt=dtd

# likelihood-ratio test
model_full <- lm(dt$lci_2 ~ 1 + dt$tx + dt$lci_orig)
model_reduced <- lm(dt$lci_2 ~ 1 + dt$lci_orig)
log_likelihood_full <- logLik(model_full)
log_likelihood_reduced <- logLik(model_reduced)
likelihood_ratio <- -2 * (log_likelihood_reduced - log_likelihood_full)
p_value <- 1 - pchisq(likelihood_ratio, df = df.residual(model_reduced) - df.residual(model_full))
p_tx = p_value[1]
sig_tx <- p_tx < 0.05




design='longitudinal'
N=40
pMean=8.1
pVar=1.4
sDuration=1
msmtYear=2
dtd=lci_simulation_exact_cole(N, pop_mean=pMean, pop_sd=pVar, b1=0.6, design_name=design, study_duration=sDuration, msm_per_year=msmtYear)

model_full <- lmer(lci_2 ~ time + (1|id), data=dtd)
model_reduced <- lmer(lci_2 ~ (1|id), data=dtd)

log_likelihood_full <- logLik(model_full)
log_likelihood_reduced <- logLik(model_reduced)
likelihood_ratio <- -2 * (log_likelihood_reduced - log_likelihood_full)

summary(model_full, ddf = "Satterthwaite")

Conv=summary(model_full)$optinfo$conv$opt+summary(model_reduced)$optinfo$conv$opt

p_value <- 1 - pchisq(likelihood_ratio, df = df.residual(model_reduced) - df.residual(model_full))      

if(Conv==0) {
  p_tx = p_value[1]
}else{
  p_value = NA
}

if (!is.na(p_value)) {
  sig_tx <- p_tx < 0.05
}else{
  
  sig_tx=NA
}


design='longitudinal'
N=40
pMean=8.1
pVar=1.4
sDuration=1
msmtYear=2

dtc_stata=read.csv("./longi.csv")
colnames(dtc_stata)[1]="lci_2"

dtd=lci_simulation_exact_cole(N, pop_mean=pMean, pop_sd=pVar, b1=0.6, design_name=design, study_duration=sDuration, msm_per_year=msmtYear)

data_model=dtd

study_significance(data=data_model,design_name=design,alpha=0.05)

model_full <- lmer(lci_2 ~ time + (0+time|id), data=data_model)
model_reduced <- lmer(lci_2 ~ (1|id), data=data_model)

lrtest(model_full, model_reduced)
lrtest(model_full, model_reduced)[2,'Pr(>Chisq)']

p_tx_ttest <- coef(summary(model_full))['time', 'Pr(>|t|)']
p_tx_ttest

env=new.env()
assign("something", TRUE, env=env)
get("something", env=env)
tryCatch({model_full2 <- nlme::lme(lci_2 ~ time, random=~time|id, data=data_model);
p_tx_ttest_lme <- summary(model_full2)$tTable['time', 'p-value']},
error = function(e){
  message("An error occurred:\n", e);
  p_tx_ttest_lme <- NA
},
warning = function(w){
  message("A warning occured:\n", w)
})

p_tx_ttest_lme





#Not used in function
#Manual likelihood ratio test

log_likelihood_full <- logLik(model_full)
log_likelihood_reduced <- logLik(model_reduced) 
likelihood_ratio <- -2 * (log_likelihood_reduced - log_likelihood_full) # https://en.wikipedia.org/wiki/Likelihood-ratio_test
p_value <- 1 - pchisq(-likelihood_ratio, df = df.residual(model_reduced) - df.residual(model_full)) 
p_value