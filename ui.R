#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(bslib)
library(shinyjs)
library(shinycssloaders)
library(knitr)

#Variables
dfmethods = data.frame(
  Parallel = c(1),
  Crossover = c(1),
  Longitudinal = c(1)
)

dfDuration = data.frame(Half = c(1),
                        One = c(1),
                        Two = c(1))
dfMeasures = data.frame(
  Quarterly = c(1),
  Semi.annual = c(1),
  Annualy = c(1)
)
dfYesNo = data.frame(Yes = c(1), No = c(1))

fluidPage(
  shinyjs::useShinyjs(),
  tags$head(
    tags$style(
      "
    h3 {
      background-color: lightblue;
    }

    a {
      color: #11671E;
    }

    #wrapper {
      justify-content: center;
    }

    #bottom {
      justify-content: center;

    }
   "
    )
  ),
  
  theme = bs_theme(version = 4, bootswatch = "minty"),
  
  # withMathJax(),
  # Application title
  HTML(
    "
    <h3>Minimum sample size for interventional and observational studies using the lung clearence index as an endpoint. <a href='https://www.sciencedirect.com/science/article/pii/S1569199322014126 '>Bowerman et al., Journal of Cystic Fibrosis (2022) Vol 22, Issue 2, 356-362 </a></h3>
    <p>This tool is designed to estimate the sample size for various study designs utilizing the lung clearance index. A full description of the methodology and assumptions can be found in the <a href='https://www.sciencedirect.com/science/article/pii/S1569199322014126'>original publication</a></p>
    "
  ),

  fluidRow (
    id = "wrapper",
    column(
      3,
      h4("Assumptions"),
      sliderInput(
        "iter",
        "Number of iterations per Simulation:",
        min = 500,
        max = 1000,
        value = 600,
        step = 10
      ),
      selectInput("target_power", "Target power", choices = c(0.8, 0.9)),
      #p(
      #  "When the mean power - of the simulations for a specific sample size - reachs the selected threshold, then that sample size is returned)"
      #),
      #sd_err
      sliderInput(
        "sd_error",
        "Standard deviation of the non-random error",
        min = 0,
        max = 5,
        value = 1.3,
        step = 0.1
      ),
      HTML("<p>This term accounts for measurement error, and other sources of non random variability. It is distributed as <code>N(0,sd)</code>
            It is possible to vary this - but the original paper used values between 1.3 and 1.7</p>"),
    ),
    column(
      2,
      h4("Population Parameters"),
      sliderInput(
        "mean",
        "Mean:",
        min = 6,
        max = 15,
        value = 8.1,
        step = 0.1
      ),
      sliderInput(
        "sd",
        "Std. Deviation:",
        min = 1,
        max = 3,
        value = 1.4,
        step = 0.02
      ),
    ),
    column(1, ""),
    column(
      2.5,
      h4("Study Design Information"),
      selectInput(
        "method",
        "Method",
        choices = c("Parallel", "Crossover", "Longitudinal")
      ),
      varSelectInput("baseline", "Baseline Term: ", dfYesNo),
      varSelectInput("study_duration", "Study Duration (Years)", dfDuration),
      varSelectInput("msm_per_year", "Follow ups: ", dfMeasures),
    ),
    column(1, ""),
    column(
      2,
      h4("Treatment Effect"),
      numericInput("tx", "Mean effect/Slope", value = -2),
      p(
        "Please note: For large effect sizes, the sample size estimate will be small and more variable."
      ),
      numericInput("sd_tx", "Std. Deviation", value = 1),
    ),
  ),
  fluidRow (id = "bottom",
            column(6.5, actionButton("go", "Estimate")),),
  fluidRow(
    column(
      12,
      br(),
      #withSpinner(uiOutput('table'), type = 1),
      withSpinner(verbatimTextOutput("sample"), type = 1),
      tags$head(
        tags$style(
          "#sample{color:#5353ac; font-size:18px; font-style:italic;
          align-text: center; max-height: 50px; background: ghostwhite;}"
        )
      ),
      verbatimTextOutput("initime"),
      verbatimTextOutput("endtime"),
      verbatimTextOutput("time"),
    )
  ),
  
)
