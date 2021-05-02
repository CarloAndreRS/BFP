# BFP web aplication with WPD and HMM

## Libraries required
library(seewave)
library(e1071)
library(wavethresh)
library(depmixS4)
library(shiny)

ui <- (fluidPage(
  
  ## MainPanel UI
  
  ##  1.  App. title
  titlePanel("WPD & CHMM Methods For Bearing Failure Prognostics"),
  ##  2.  Sidebar
  sidebarLayout(
  sidebarPanel(
  ##  3.  Side bar menu
  titlePanel(h5("Analysis steps")),
  selectInput(inputId = "Steps",label = "Steps:", 
  choices = c("1. Samples load, read and plot","2. Wavelet Analysis & Energy Sequence","3. CHMM Trainning", 
              "4. Prognostics"),
  selected = "Load data samples"),
  ##  3.a  Sample load, read & plot  
  conditionalPanel(condition = "input.Steps == '1. Samples load, read and plot'",
  wellPanel(h5("My data:"),
  fileInput("file","Upload Samples", multiple = TRUE), 
  helpText(h5("Selected reading parameters:")),
  checkboxInput(inputId = 'header', label = 'Header', value = TRUE),
  radioButtons(inputId = 'sep', label = 'Separator', 
  choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ''), 
  helpText("Select sample rate:"),
  numericInput("obs", "Selected sample rate (Hz):", 20000, min = 1, max = 30000),
  helpText("Select row to skip:"),
  numericInput("rowskip", "Selected", 1, min = 0, max = 10),
  helpText("Select column:"),
  numericInput("colsel", "Selected", 2, min = 0, max = 8),
  uiOutput("selectfile")
  )
  ),
  ##  3.b  Wavelet Analysis & Energy Sequence
  conditionalPanel(condition = "input.Steps == '2. Wavelet Analysis & Energy Sequence'",
               
                   wellPanel(h5("Feature Extraction:"),
                             selectInput(inputId = "Wavelemethod",label = "Wavelet Method", 
                                         choices = c("WPD", "Descrete Wavelet Tranform"),selected = "WPD"),
                             helpText("Wavelet sample size must be a multiple of 2^n "), 
                             selectInput(inputId = "Wavesize",label = "Wavelet sample size", 
                                         choices = c(256, 512, 1024, 2048),selected = 256),
                             selectInput(inputId = "family",label = "Wavelet family", 
                                         choices = c("DaubLeAsymm","DaubExPhase"),
                                         selected = "DaubLeAsymm"),
                             numericInput("wavefilter", "Wavelet filter", 4, min = 1, max = 10),
                             uiOutput("selectfile1")         
                   )
  ), 
  ##  3.c  CHMM Trainning
  conditionalPanel(condition = "input.Steps == '3. CHMM Trainning'",
                   wellPanel(h5("Train samples:"),
                             fileInput("file1","Upload Trainning Samples", multiple = TRUE),
                             selectInput(inputId = "Trainparameters",label = "Trainning Parameters", 
                                         choices = c("3 State Custom parameters", "Random Parameters"),
                                         selected = "3 State Custom parameters"),
                             actionButton(inputId="action2",label="Train") ,
                             conditionalPanel(condition = "input.Trainparameters == '3 State Custom parameters'",
                                              wellPanel(h5("Initial state Prob:"),
                                                        numericInput("s1", "s1", 0, min = 0, max = 1, step=0.1),
                                                        numericInput("s2", "s2", 0.5, min = 0, max = 1, step=0.1),
                                                        numericInput("s3", "s3", 1, min = 0, max = 1, step=0.1),
                                                        
                                              ),
                                              wellPanel(h5("Trans Matrix:"),
                                                        numericInput("s1s1", "s1s1", 0.5, min = 0, max = 1, step=0.1),
                                                        numericInput("s1s2", "s1s2", 0.3, min = 0, max = 1, step=0.1),
                                                        numericInput("s1s3", "s1s3", 0.2, min = 0, max = 1, step=0.1),
                                                        numericInput("s2s1", "s2s1", 0.1, min = 0, max = 1, step=0.1),
                                                        numericInput("s2s2", "s2s2", 0.8, min = 0, max = 1, step=0.1),
                                                        numericInput("s2s3", "s2s3", 0.1, min = 0, max = 1, step=0.1),
                                                        numericInput("s3s1", "s3s1", 0.1, min = 0, max = 1, step=0.1),
                                                        numericInput("s3s2", "s3s2", 0.1, min = 0, max = 1, step=0.1),
                                                        numericInput("s3s3", "s3s3", 0.8, min = 0, max = 1, step=0.1),
                                              )),
                             conditionalPanel(condition = "input.Trainparameters == 'Random Parameters'",
                                              wellPanel(h5("number of states:"),
                                                        numericInput("states", "states", 3, min = 3, max = 6),
                                                       
                                                        
                                              )),
                            
                   )),
  ##  3.d  Prognostic
  conditionalPanel(condition = "input.Steps == '4. Prognostics'",
                 
                   wellPanel(h5("Prognostics"), 
                            
                             actionButton(inputId="action1",label="Plot") 
                   ),
                   wellPanel(h5("Common Methods"), helpText("Select Method:"),
                             selectInput(inputId = "common",label = "Common Method:", 
                                         choices = c("rms", "kurtosis"),selected = "rms"),
                             actionButton(inputId="action",label="Plot") 
                   ))
  
  ),
  ## 4. MainPanel tabset
  mainPanel(
  uiOutput("tb")
      
  )
    
  )
))
