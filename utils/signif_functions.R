
library('lme4')
library("lmerTest")
library("lmtest")

# ===========  SIGNIFICANCE OF TREATMENT ================

study_significance<-function(data,design_name,baseline=TRUE,alpha) {
  
  if (design_name=='parallel') {    

    p_tx_ttest <- NA
    p_tx_likrtest2 <- NA
    p_tx_ttest_lme <- 0.00001
    tryCatch({
              if (baseline) {
                
                model_full <- lm(lci_2 ~ 1 + tx + lci_orig, data=data)
                model_reduced <- lm(lci_2 ~ 1 + lci_orig, data=data)
                
              }else{
                
                model_full <- lm(lci_2 ~ 1 + tx, data)
                model_reduced <- lm(lci_2 ~ 1, data)
              }
      
              p_tx_ttest <- coef(summary(model_full))['tx', 'Pr(>|t|)']
              p_tx_likrtest2 = lrtest(model_full, model_reduced)[2,'Pr(>Chisq)']},
              error = function(e){
                message("An error in lmer fitting occurred:\n", e)
              },
              warning = function(w){
                message("A warning in lmer fitting occured:\n", w)
              })

  }

  if (design_name=='crossover') {    
    
    p_tx_ttest <- NA
    p_tx_likrtest2 <- NA
    tryCatch({
      
              #boundary (singular) fit: see help('isSingular')
              model_full <- lmer(lci_2 ~ 1 + tx + lci_orig + (0+tx|id), data=data)
              model_reduced<- lmer(lci_2 ~ 1 + lci_orig + (1|id), data=data)

              p_tx_ttest <- coef(summary(model_full))['tx', 'Pr(>|t|)']
              p_tx_likrtest2 = lrtest(model_full, model_reduced)[2,'Pr(>Chisq)']},
              error = function(e){
                message("An error in lmer fitting occurred:\n", e)
              },
              warning = function(w){
                message("A warning in lmer fitting occured:\n", w)
    })
    
    p_tx_ttest_lme <- 0
    #using lme and using t test again. Catching non convergence error
    tryCatch({
              model_full <- nlme::lme(lci_2 ~ 1 + tx + lci_orig, random=~(0+tx)|id, data=data)
              p_tx_ttest_lme <- summary(model_full)$tTable['tx', 'p-value']},
              error = function(e){
                #message("An error in nlme occurred:\n", e)
              },
              warning = function(w){
                message("A warning in nlme occured:\n", w)
    })
    
  }
  
    
  if (design_name=='longitudinal') {    
    
    p_tx_ttest <- NA
    p_tx_likrtest2 <- NA
    tryCatch({model_full <- lmer(lci_2 ~ time + (0+time|id), data=data)
              model_reduced <- lmer(lci_2 ~ (1|id), data=data)
              p_tx_ttest <- coef(summary(model_full))['time', 'Pr(>|t|)']
              p_tx_likrtest2 = lrtest(model_full, model_reduced)[2,'Pr(>Chisq)']},
             error = function(e){
               message("An error in lmer fitting occurred:\n", e)
             },
             warning = function(w){
               message("A warning in lmer fitting occured:\n", w)
             })
    
    p_tx_ttest_lme <- NA
    #using lme and using t test again. Catching non convergence error
    tryCatch({model_full <- nlme::lme(lci_2 ~ time, random=~time|id, data=data);
              p_tx_ttest_lme <- summary(model_full)$tTable['time', 'p-value']},
             error = function(e){
               #message("An error in nlme occurred:\n", e)
             },
             warning = function(w){
               message("A warning in nlme occured:\n", w)
             })
  
  }
  
  sig_tx_ttest=NA
  sig_tx_ttest_lme = NA
  sig_tx_likrtest2 = NA
  
  if (!is.na(p_tx_ttest)) {
    sig_tx_ttest <- p_tx_ttest < alpha    
  }
  
  if (!is.na(p_tx_ttest_lme)) {
    sig_tx_ttest_lme <- p_tx_ttest_lme < alpha    
  }
  
  if (!is.na(p_tx_likrtest2)) {
    sig_tx_likrtest2 <- p_tx_likrtest2 < alpha    
  }
  
  return(list(pvalue_ttest=p_tx_ttest, sig_ttest=sig_tx_ttest, pvalue_ttest_lme=p_tx_ttest_lme, sig_ttest_lme=sig_tx_ttest_lme, pvalue_likrtest2=p_tx_likrtest2, sig_likrtest2=sig_tx_likrtest2))
  
}
