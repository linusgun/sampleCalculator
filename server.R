#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(nleqslv)
library(shinyjs)
library(knitr)
library(dplyr)
library(kableExtra)
library(flextable)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  values <- reactiveValues()

  observeEvent(input$method, {
    if(input$method == "Longitudinal"){
      shinyjs::enable("study_duration")
      shinyjs::enable("msm_per_year")
      shinyjs::disable("sd_tx")
    }else{
      shinyjs::disable("study_duration")
      shinyjs::disable("msm_per_year")
      shinyjs::enable("sd_tx")
    }
    
    if (input$method == "Parallel") {
      shinyjs::enable("baseline")
    }else {
      shinyjs::disable("baseline")
    }
    
    
  })
  
  mean_powers <- eventReactive(input$go, {
    
    source(file="utils/sample_estimation.R", local = TRUE)
    source(file="utils/signif_functions.R", local = TRUE)
    source(file="utils/sim_functions.R", local = TRUE)
    
    N=40
    pop_mean=input$mean
    pop_sd=input$sd
    b1=input$tx
    b1Sd=input$sd_tx
    design=tolower(input$method)
    niter=input$iter
    tpower=input$target_power
    sdError=input$sd_error
    
    #print(paste(pop_mean,pop_sd,b1,b1Sd,design))
    
    baseTerm=TRUE
    if (input$baseline=="No") {baseTerm=FALSE}
    
    study_duration=0.5
    if (input$study_duration=="One") study_duration=1
    if (input$study_duration=="Two") study_duration=2

    msm_per_year=3
    if (input$msm_per_year=="Semi.annual") msm_per_year=2
    if (input$msm_per_year=="Annual") msm_per_year=1
    
    values$iniTime=Sys.time()

    data<-size_search(b1_values=c(b1), designs=c(design), pop_mean=pop_mean, pop_sd=pop_sd, b1_sd=b1Sd, baseline=baseTerm, study_duration=study_duration, msm_per_year=msm_per_year, iter=niter, power_threshold=tpower, sd_err=sdError)
    
    values$endTime=Sys.time()
    values$design=design
    
    data
    
  })
  
  output$sample <- renderText({
    data = mean_powers()
    dfm <- transform(data, Sample = as.numeric(Sample))
    n=max(dfm['Sample'])
    exp=""
    if (values$design=="parallel") exp=paste(". In this case - parallel design - this requires ",n/2," per group")
    paste("The Total Sample Size Estimate is: ", n,exp)
    
  })

  output$time <- eventReactive(values$endTime, {
    paste("End time: ",values$endTime," - Initial Time: ",values$iniTime," | Duration: ", format(values$endTime - values$iniTime, nsmall = 2))
  })

  # output$table <-  renderUI({
  #   
  #   data=mean_powers()
  #   #print(str(data))
  #   data = transform(
  #     data,
  #     tx = as.numeric(tx),
  #     Sample = as.integer(Sample),
  #     m_power_ttest = as.double(m_power_ttest),
  #     m_power_ttest_lme = as.double(m_power_ttest_lme),
  #     m_power_likrtest  = as.double(m_power_likrtest)
  #   )
  #   
  #   
  #   data = data[, 3:6]
  #   
  #   ft <- flextable(data)
  #   ft = ft %>%
  #     add_header_row(values = c(" ", "Mean Power with"),
  #                    colwidths = c(1, 3)) %>%
  #     
  #     theme_vanilla() %>%
  #     set_table_properties(layout = "autofit") %>%
  #     set_header_labels(
  #       m_power_ttest = "t",
  #       m_power_ttest_lme = "t (lme)",
  #       m_power_likrtest = "lik-Ratio"
  #     ) %>%
  #     align(j =  ~ Sample, align = "center") %>%
  #     align(align = "center", part = "all") %>%
  #     colformat_double(
  #       j = 2:4,
  #       big.mark = ",",
  #       decimal.mark = ".",
  #       na_str = "na",
  #       digits = 2
  #     )
  #   
  #   
  #   htmltools_value(ft)
  # 
  #   })
  
}
