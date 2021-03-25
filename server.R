# BFP App. Application for Bearing Failure Prognosis, applying WPD and CHMM

## Libraries required
library(seewave)
library(e1071)
library(wavethresh)
library(depmixS4)

server <-(function(input,output, session) {
  
## MainPanel tabset renderUI code 
  output$tb <- renderUI({
    if (is.null(input$file)) {
      return()
    }
    else
      tabsetPanel(
        id = "inTabset",
        tabPanel(
          "1. Samples load & Data Reading",
          h4("1.1. Sample Data structure"),
          verbatimTextOutput("datastr"),
          h4("2.1. Data Plot"),
          plotOutput("Amplitude"),
          h4("1.2. Sample view"),
          
          tableOutput("table1")
        ),
        tabPanel(
          "2. Data Plot, Wavelet Analysis & Energy Sequence",
        
          h4("2.2. Wavelet Analysis"),
          plotOutput("waveplot"),
          h4("2.3. Energy Sequence"),
          plotOutput("fileseqplot")
        ),
        tabPanel(
          "3. CHMM Trainning",
          h4("3.1. Trainning Sequence"),
          plotOutput("trainningplot"),
          h4("3.2. Trainned Sequence"),
          plotOutput("trainningplot1"),
          h4("3.3. Trainned Parameters"),
          h5("Initial State Probability"),
          tableOutput("init"),
          h5("Transtition Matrix"),
          tableOutput("fromS1"),
          tableOutput("fromS2"),
          tableOutput("fromS3")
        ),
        tabPanel(
          "4. Common Methods & Prognostics",
          h4("4.1 Common Methods"),
          plotOutput("rms"),
          h4("4.2. Prognostics"),
          plotOutput("prognost")
        )
      )
  })
  
  ## 1.a Select sample to read
  output$selectfile <- renderUI({
    if (is.null(input$file)) {
      return()
    }
    list(
      hr(),
      helpText("Select sample to see data and summary stats"),
      selectInput("Select", "Select", choices = input$file$name)
    )
  })
  
  ## 1.3 Read selected sample
   samplered <- reactive({
    if (is.null(input$file)) {
      return ()
    }
    
     read.table(
       file = input$file$datapath[input$file$name == input$Select],
       sep = input$sep,
       header = input$header,
       stringsAsFactors = input$stringAsFactors, skip=input$rowskip
     )[,input$colsel]
    
  })
  
   samplered1 <- reactive({
     if (is.null(input$file)) {
       return ()
     }
     
     read.table(
       file = input$file$datapath[input$file$name == input$Select],
       sep = input$sep,
       header = input$header,
       stringsAsFactors = input$stringAsFactors, skip=input$rowskip
     )
     
   })
  
   samplered2 <- reactive({
     if (is.null(input$file)) {
       return ()
     }
     
     read.table(
       file = input$file$datapath[input$file$name == input$Select1],
       sep = input$sep,
       header = input$header,
       stringsAsFactors = input$stringAsFactors, skip=input$rowskip
     )
     
   })
   
   samplered3 <- reactive({
     if (is.null(input$file)) {
       return ()
     }
     
     read.table(
       file = input$file$datapath[input$file$name == input$Select1],
       sep = input$sep,
       header = input$header,
       stringsAsFactors = input$stringAsFactors, skip=input$rowskip
     )[,input$colsel]
     
   })
   
  ## 1.2 Sample Data structure
    output$datastr <- renderPrint({
    if (is.null(input$file)) {
      return ()
    }
    str(
      samplered1()
    )
  })
  
  ## 1.3 Sample View
    output$table1 <- renderTable({
      if (is.null(input$file)) {
        return ()
      }
      
        head(samplered1(),30)
      
    },digits =4)
    
    ## 1.3 Loaded vectors list
  
    loadedvectlist <- reactive({
      epochs1 <- lapply(input$file$datapath, read.table, sep=input$sep, 
      skip=input$rowskip, header = input$header, stringsAsFactors = input$stringAsFactors)
      epochs2 <- as.data.frame(epochs1)
      seq2 <- as.vector(seq1())
      epochs3 <- lapply(seq2,function(i){
        cbind(epochs2[[i]])
      })
      epochs4 <- as.data.frame(epochs3)
    })
    
    
    ## 1.3 Sequence to read vector list
     seq1 <- reactive({
      a <- samplered1()
      b <-ncol(samplered1())
       seq(input$colsel,length(a)*b,b)
    })

    
    ## 1.3 Render vector list
    output$test1 <- renderTable({
      packets3 <- loadedvectlist()
     
    })
    
# 2. Amplitud Plot & Feature Extraction with Wavelet
  
  ## 2.1 Plot Amplitude of sample
  output$Amplitude <- renderPlot({
    if (is.null(input$file)) {
      return ()
    }
    Amplitude <-
      samplered()
    Amplitude1 <- ts(Amplitude,  start = 0, frequency = input$obs)
    plot(Amplitude1, main = "Amplitude of sample")
  })
  
  ## 2.2 Plot Wavelet
  output$waveplot <- renderPlot({
    if (is.null(input$file$datapath[input$file$name == input$Select1])) {
      return ()
    }
    
    if (input$Wavelemethod == "WPD") {
      plot(wp(
        samplered2()[1:input$Wavesize,input$colsel ],
        filter.number = input$wavefilter,
        family = input$family
      ))
    }
    if (input$Wavelemethod == "Descrete Wavelet Tranform") {
      plot(wd(
        samplered2()[1:input$Wavesize, input$colsel],
        filter.number = input$wavefilter,
        family = input$family
      ))
    }
  })
  
  ## 2.3 Plot normilized average-energy sequence
  fileseqplot <- reactive({
    if(is.null(input$file)) {return()}
    if (input$Wavesize == "256") {
      
      #packets1 <- lapply(input$file$datapath[input$file$name==input$Select1], read.table, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
      
      packets2 <- as.data.frame(samplered3())
      packets3 <- reactiveValues()
      packets3 <- lapply(names(packets2),  function(i){
        
        e.1.p.1<- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                                level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=0))
        e.1.p.2 <- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=1))
        e.1.p.3 <- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=2))
        e.1.p.4 <- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=3))
        e.1.p.5 <- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=4))
        e.1.p.6 <- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=5))
        e.1.p.7 <- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=6))
        e.1.p.8 <- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=7))
        a1<-rbind(e.1.p.1, e.1.p.2, e.1.p.3, e.1.p.4, e.1.p.5, e.1.p.6, e.1.p.7, e.1.p.8)
        b1<- (a1-min(a1))/(max(a1)-min(a1)) 
        
        e.2.p.1<- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                                level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=0))
        e.2.p.2 <- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=1))
        e.2.p.3 <- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=2))
        e.2.p.4 <- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=3))
        e.2.p.5 <- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=4))
        e.2.p.6 <- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=5))
        e.2.p.7 <- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=6))
        e.2.p.8 <- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=7))
        a2<-rbind(e.2.p.1, e.2.p.2, e.2.p.3, e.2.p.4, e.2.p.5, e.2.p.6, e.2.p.7, e.2.p.8)
        b2<- (a2-min(a2))/(max(a2)-min(a2)) 
        
        e.3.p.1<- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                                level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=0))
        e.3.p.2 <- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=1))
        e.3.p.3 <- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=2))
        e.3.p.4 <- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=3))
        e.3.p.5 <- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=4))
        e.3.p.6 <- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=5))
        e.3.p.7 <- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=6))
        e.3.p.8 <- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=7))
        a3<-rbind(e.3.p.1, e.3.p.2, e.3.p.3, e.3.p.4, e.3.p.5, e.3.p.6, e.3.p.7, e.3.p.8)
        b3<- (a3-min(a3))/(max(a3)-min(a3)) 
        
        e.4.p.1<- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=0))
        e.4.p.2 <- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=1))
        e.4.p.3 <- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=2))
        e.4.p.4 <- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=3))
        e.4.p.5 <- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=4))
        e.4.p.6 <- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=5))
        e.4.p.7 <- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=6))
        e.4.p.8 <- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=7))
        a4<-rbind(e.4.p.1, e.4.p.2, e.4.p.3, e.4.p.4, e.4.p.5, e.4.p.6, e.4.p.7, e.4.p.8)
        b4<- (a4-min(a4))/(max(a4)-min(a4)) 
        
        e.5.p.1<- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                                level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=0))
        e.5.p.2 <- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=1))
        e.5.p.3 <- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=2))
        e.5.p.4 <- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=3))
        e.5.p.5 <- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=4))
        e.5.p.6 <- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=5))
        e.5.p.7 <- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=6))
        e.5.p.8 <- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=7))
        a5<-rbind(e.5.p.1, e.5.p.2, e.5.p.3, e.5.p.4, e.5.p.5, e.5.p.6, e.5.p.7, e.5.p.8)
        b5<- (a5-min(a5))/(max(a5)-min(a5)) 
        
        e.6.p.1<- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                                level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=0))
        e.6.p.2 <- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=1))
        e.6.p.3 <- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=2))
        e.6.p.4 <- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=3))
        e.6.p.5 <- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=4))
        e.6.p.6 <- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=5))
        e.6.p.7 <- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=6))
        e.6.p.8 <- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=7))
        a6<-rbind(e.6.p.1, e.6.p.2, e.6.p.3, e.6.p.4, e.6.p.5, e.6.p.6, e.6.p.7, e.6.p.8)
        b6<- (a6-min(a6))/(max(a6)-min(a6)) 
        
        e.7.p.1<- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                                level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=0))
        e.7.p.2 <- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=1))
        e.7.p.3 <- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=2))
        e.7.p.4 <- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=3))
        e.7.p.5 <- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=4))
        e.7.p.6 <- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=5))
        e.7.p.7 <- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=6))
        e.7.p.8 <- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=7))
        a7<-rbind(e.7.p.1, e.7.p.2, e.7.p.3, e.7.p.4, e.7.p.5, e.7.p.6, e.7.p.7, e.7.p.8)
        b7<- (a7-min(a7))/(max(a7)-min(a7)) 
        
        e.8.p.1<- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=0))
        e.8.p.2 <- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=1))
        e.8.p.3 <- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=2))
        e.8.p.4 <- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=3))
        e.8.p.5 <- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=4))
        e.8.p.6 <- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=5))
        e.8.p.7 <- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=6))
        e.8.p.8 <- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=7))
        a8<-rbind(e.8.p.1, e.8.p.2, e.8.p.3, e.8.p.4, e.8.p.5, e.8.p.6, e.8.p.7, e.8.p.8)
        b8<- (a8-min(a8))/(max(a8)-min(a8)) 
        
        rbind(b1, b2, b3, b4, b5, b6,b7, b8)
      })
      names(packets3)<- c(names(packets2))
      packets4 <- as.data.frame(packets3)
      
      packets5 <- lapply(names(packets4),  function(i){
        t(packets4[[i]])
      })
      packets6 <- as.data.frame(packets5)
      packets7 <- t(packets6)
      sampleseq <- ts(packets7)
      plot(sampleseq, xlab = "Sequence",main = "Sample Sequence")
    }
    if (input$Wavesize == "512") {
      
      packets2 <- as.data.frame(samplered3())
      packets3 <- reactiveValues()
    packets3 <- lapply(names(packets2),  function(i){
      
      e.1.p.1<- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                              level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                       family=input$family),level=5, index=0))
      e.1.p.2 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=1))
      e.1.p.3 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=2))
      e.1.p.4 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=3))
      e.1.p.5 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=4))
      e.1.p.6 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=5))
      e.1.p.7 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=6))
      e.1.p.8 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=7))
      e.1.p.9 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=8)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=8))
      e.1.p.10 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=9)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=9))
      e.1.p.11 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=10)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=10))
      e.1.p.12 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                level=5, index=11)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=11))
      e.1.p.13 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                level=5, index=12)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=12))
      e.1.p.14 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                level=5, index=13)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=13))
      e.1.p.15 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                level=5, index=14)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=14))
      e.1.p.16 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                level=5, index=15)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=15))
      
      a1<-rbind(e.1.p.1, e.1.p.2, e.1.p.3, e.1.p.4, e.1.p.5, e.1.p.6, e.1.p.7, e.1.p.8, e.1.p.9, e.1.p.10, e.1.p.11, e.1.p.12,
                e.1.p.13, e.1.p.14, e.1.p.15, e.1.p.16)
      
      b1<- (a1-min(a1))/(max(a1)-min(a1)) 
      
      e.2.p.1<- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                              level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                       family=input$family),level=5, index=0))
      e.2.p.2 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=1))
      e.2.p.3 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=2))
      e.2.p.4 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=3))
      e.2.p.5 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=4))
      e.2.p.6 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=5))
      e.2.p.7 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=6))
      e.2.p.8 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=7))
      e.2.p.9 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=8)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=8))
      e.2.p.10 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=9)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=9))
      e.2.p.11 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=10)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=10))
      e.2.p.12 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=11)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=11))
      e.2.p.13 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=12)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=12))
      e.2.p.14 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=13)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=13))
      e.2.p.15 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=14)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=14))
      e.2.p.16 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=15)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=15))
      
      
      a2<-rbind(e.2.p.1, e.2.p.2, e.2.p.3, e.2.p.4, e.2.p.5, e.2.p.6, e.2.p.7, e.2.p.8, e.2.p.9, e.2.p.10, e.2.p.11, e.2.p.12,
                e.2.p.13, e.2.p.14, e.2.p.15, e.2.p.16)
      
      b2<- (a2-min(a2))/(max(a2)-min(a2)) 
      
      e.3.p.1<- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                              level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                       family=input$family),level=5, index=0))
      e.3.p.2 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=1))
      e.3.p.3 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=2))
      e.3.p.4 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=3))
      e.3.p.5 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=4))
      e.3.p.6 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=5))
      e.3.p.7 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=6))
      e.3.p.8 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=7))
      e.3.p.9 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=8)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=8))
      e.3.p.10 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                level=5, index=9)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=9))
      e.3.p.11 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                level=5, index=10)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=10))
      e.3.p.12 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                level=5, index=11)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=11))
      e.3.p.13 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                level=5, index=12)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=12))
      e.3.p.14 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                level=5, index=13)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=13))
      e.3.p.15 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                level=5, index=14)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=14))
      e.3.p.16 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                level=5, index=15)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=15))
      
      
      a3<-rbind(e.3.p.1, e.3.p.2, e.3.p.3, e.3.p.4, e.3.p.5, e.3.p.6, e.3.p.7, e.3.p.8, e.3.p.9, e.3.p.10, e.3.p.11, e.3.p.12,
                e.3.p.13, e.3.p.14, e.3.p.15, e.3.p.16)
      
      b3<- (a3-min(a3))/(max(a3)-min(a3)) 
      
      e.4.p.1<- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                              level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                       family=input$family),level=5, index=0))
      e.4.p.2 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=1))
      e.4.p.3 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=2))
      e.4.p.4 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=3))
      e.4.p.5 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=4))
      e.4.p.6 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=5))
      e.4.p.7 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=6))
      e.4.p.8 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=7))
      e.4.p.9 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=8)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=8))
      e.4.p.10 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=9)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=9))
      e.4.p.11 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=10)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=10))
      e.4.p.12 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=11)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=11))
      e.4.p.13 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=12)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=12))
      e.4.p.14 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=13)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=13))
      e.4.p.15 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=14)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=14))
      e.4.p.16 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=15)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=15))
      
      
      a4<-rbind(e.4.p.1, e.4.p.2, e.4.p.3, e.4.p.4, e.4.p.5, e.4.p.6, e.4.p.7, e.4.p.8, e.4.p.9, e.4.p.10, e.4.p.11, e.4.p.12,
                e.4.p.13, e.4.p.14, e.4.p.15, e.4.p.16)
      
      b4<- (a4-min(a4))/(max(a4)-min(a4)) 
      
      rbind(b1, b2, b3, b4)
    })
    names(packets3)<- c(names(packets2))
    packets4 <- as.data.frame(packets3)
    
    packets5 <- lapply(names(packets4),  function(i){
      t(packets4[[i]])
    })
    packets6 <- as.data.frame(packets5)
    packets7 <- t(packets6)
    sampleseq <- ts(packets7)
    plot(sampleseq, xlab = "Sequence",main = "Sample Sequence")
    }
 
  })
  
    ### 2.3.1 Render Plot normilized average-energy sequence
  output$fileseqplot <- renderPlot({
    fileseqplot()
  })
  
  ## 2.4 Select sample to plot
  output$selectfile1 <- renderUI({
    if(is.null(input$file)) {return()}
    list(hr(), 
         helpText("Select sample to see WPD"),
         selectInput("Select1", "Select", choices=input$file$name)
    )})
  
# 3. CHMM Training

  ## 3.1 Normilized average-energy trainning Sequence
  trainningplot <- reactive({
    if(is.null(input$file1)) {return()}
    if (input$Wavesize == "256") {
    packets1 <- lapply(input$file1$datapath, read.table, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    packets2 <- as.data.frame(packets1)
    packets3 <- reactiveValues()
    packets3 <- lapply(names(packets2),  function(i){
      
      e.1.p.1<- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                              level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                       family=input$family),level=5, index=0))
      e.1.p.2 <- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                               level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=1))
      e.1.p.3 <- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                               level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=2))
      e.1.p.4 <- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                               level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=3))
      e.1.p.5 <- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                               level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=4))
      e.1.p.6 <- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                               level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=5))
      e.1.p.7 <- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                               level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=6))
      e.1.p.8 <- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                               level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=7))
      a1<-rbind(e.1.p.1, e.1.p.2, e.1.p.3, e.1.p.4, e.1.p.5, e.1.p.6, e.1.p.7, e.1.p.8)
      b1<- (a1-min(a1))/(max(a1)-min(a1)) 
      
      e.2.p.1<- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                              level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                       family=input$family),level=5, index=0))
      e.2.p.2 <- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=1))
      e.2.p.3 <- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=2))
      e.2.p.4 <- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=3))
      e.2.p.5 <- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=4))
      e.2.p.6 <- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=5))
      e.2.p.7 <- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=6))
      e.2.p.8 <- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=7))
      a2<-rbind(e.2.p.1, e.2.p.2, e.2.p.3, e.2.p.4, e.2.p.5, e.2.p.6, e.2.p.7, e.2.p.8)
      b2<- (a2-min(a2))/(max(a2)-min(a2)) 
      
      e.3.p.1<- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                              level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                       family=input$family),level=5, index=0))
      e.3.p.2 <- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                               level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=1))
      e.3.p.3 <- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                               level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=2))
      e.3.p.4 <- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                               level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=3))
      e.3.p.5 <- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                               level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=4))
      e.3.p.6 <- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                               level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=5))
      e.3.p.7 <- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                               level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=6))
      e.3.p.8 <- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                               level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=7))
      a3<-rbind(e.3.p.1, e.3.p.2, e.3.p.3, e.3.p.4, e.3.p.5, e.3.p.6, e.3.p.7, e.3.p.8)
      b3<- (a3-min(a3))/(max(a3)-min(a3)) 
      
      e.4.p.1<- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                              level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                       family=input$family),level=5, index=0))
      e.4.p.2 <- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=1))
      e.4.p.3 <- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=2))
      e.4.p.4 <- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=3))
      e.4.p.5 <- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=4))
      e.4.p.6 <- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=5))
      e.4.p.7 <- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=6))
      e.4.p.8 <- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=7))
      a4<-rbind(e.4.p.1, e.4.p.2, e.4.p.3, e.4.p.4, e.4.p.5, e.4.p.6, e.4.p.7, e.4.p.8)
      b4<- (a4-min(a4))/(max(a4)-min(a4)) 
      
      e.5.p.1<- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                              level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                       family=input$family),level=5, index=0))
      e.5.p.2 <- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                               level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=1))
      e.5.p.3 <- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                               level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=2))
      e.5.p.4 <- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                               level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=3))
      e.5.p.5 <- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                               level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=4))
      e.5.p.6 <- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                               level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=5))
      e.5.p.7 <- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                               level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=6))
      e.5.p.8 <- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                               level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=7))
      a5<-rbind(e.5.p.1, e.5.p.2, e.5.p.3, e.5.p.4, e.5.p.5, e.5.p.6, e.5.p.7, e.5.p.8)
      b5<- (a5-min(a5))/(max(a5)-min(a5)) 
      
      e.6.p.1<- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                              level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                       family=input$family),level=5, index=0))
      e.6.p.2 <- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=1))
      e.6.p.3 <- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=2))
      e.6.p.4 <- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=3))
      e.6.p.5 <- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=4))
      e.6.p.6 <- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=5))
      e.6.p.7 <- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=6))
      e.6.p.8 <- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=7))
      a6<-rbind(e.6.p.1, e.6.p.2, e.6.p.3, e.6.p.4, e.6.p.5, e.6.p.6, e.6.p.7, e.6.p.8)
      b6<- (a6-min(a6))/(max(a6)-min(a6)) 
      
      e.7.p.1<- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                              level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                       family=input$family),level=5, index=0))
      e.7.p.2 <- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                               level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=1))
      e.7.p.3 <- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                               level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=2))
      e.7.p.4 <- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                               level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=3))
      e.7.p.5 <- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                               level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=4))
      e.7.p.6 <- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                               level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=5))
      e.7.p.7 <- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                               level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=6))
      e.7.p.8 <- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                               level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=7))
      a7<-rbind(e.7.p.1, e.7.p.2, e.7.p.3, e.7.p.4, e.7.p.5, e.7.p.6, e.7.p.7, e.7.p.8)
      b7<- (a7-min(a7))/(max(a7)-min(a7)) 
      
      e.8.p.1<- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                              level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                       family=input$family),level=5, index=0))
      e.8.p.2 <- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=1))
      e.8.p.3 <- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=2))
      e.8.p.4 <- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=3))
      e.8.p.5 <- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=4))
      e.8.p.6 <- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=5))
      e.8.p.7 <- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=6))
      e.8.p.8 <- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=7))
      a8<-rbind(e.8.p.1, e.8.p.2, e.8.p.3, e.8.p.4, e.8.p.5, e.8.p.6, e.8.p.7, e.8.p.8)
      b8<- (a8-min(a8))/(max(a8)-min(a8)) 
      
      rbind(b1, b2, b3, b4, b5, b6,b7, b8)
    })
    names(packets3)<- c(names(packets2))
    packets4 <- as.data.frame(packets3)
    
    packets5 <- lapply(names(packets4),  function(i){
      t(packets4[[i]])
    })
    packets6 <- as.data.frame(packets5)
    packets7 <- t(packets6)
    trainseq <- ts(packets7)
    plot(trainseq, xlab = "Sequence",main = "Trainning Sequence")
    }
    if (input$Wavesize == "512") {
      
      packets1 <- lapply(input$file1$datapath, read.table, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
      packets2 <- as.data.frame(packets1)
      packets3 <- reactiveValues()
      packets3 <- lapply(names(packets2),  function(i){
        
        e.1.p.1<- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=0))
        e.1.p.2 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=1))
        e.1.p.3 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=2))
        e.1.p.4 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=3))
        e.1.p.5 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=4))
        e.1.p.6 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=5))
        e.1.p.7 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=6))
        e.1.p.8 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=7))
        e.1.p.9 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=8)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=8))
        e.1.p.10 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=9)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                           family=input$family),level=5, index=9))
        e.1.p.11 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=10)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=10))
        e.1.p.12 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=11)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=11))
        e.1.p.13 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=12)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=12))
        e.1.p.14 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=13)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=13))
        e.1.p.15 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=14)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=14))
        e.1.p.16 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=15)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=15))
        
        a1<-rbind(e.1.p.1, e.1.p.2, e.1.p.3, e.1.p.4, e.1.p.5, e.1.p.6, e.1.p.7, e.1.p.8, e.1.p.9, e.1.p.10, e.1.p.11, e.1.p.12,
                  e.1.p.13, e.1.p.14, e.1.p.15, e.1.p.16)
        
        b1<- (a1-min(a1))/(max(a1)-min(a1)) 
        
        e.2.p.1<- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=0))
        e.2.p.2 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=1))
        e.2.p.3 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=2))
        e.2.p.4 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=3))
        e.2.p.5 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=4))
        e.2.p.6 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=5))
        e.2.p.7 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=6))
        e.2.p.8 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=7))
        e.2.p.9 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=8)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=8))
        e.2.p.10 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=9)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                           family=input$family),level=5, index=9))
        e.2.p.11 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=10)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=10))
        e.2.p.12 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=11)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=11))
        e.2.p.13 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=12)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=12))
        e.2.p.14 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=13)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=13))
        e.2.p.15 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=14)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=14))
        e.2.p.16 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=15)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=15))
        
        
        a2<-rbind(e.2.p.1, e.2.p.2, e.2.p.3, e.2.p.4, e.2.p.5, e.2.p.6, e.2.p.7, e.2.p.8, e.2.p.9, e.2.p.10, e.2.p.11, e.2.p.12,
                  e.2.p.13, e.2.p.14, e.2.p.15, e.2.p.16)
        
        b2<- (a2-min(a2))/(max(a2)-min(a2)) 
        
        e.3.p.1<- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=0))
        e.3.p.2 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=1))
        e.3.p.3 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=2))
        e.3.p.4 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=3))
        e.3.p.5 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=4))
        e.3.p.6 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=5))
        e.3.p.7 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=6))
        e.3.p.8 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=7))
        e.3.p.9 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=8)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=8))
        e.3.p.10 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=9)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                           family=input$family),level=5, index=9))
        e.3.p.11 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=10)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=10))
        e.3.p.12 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=11)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=11))
        e.3.p.13 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=12)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=12))
        e.3.p.14 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=13)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=13))
        e.3.p.15 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=14)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=14))
        e.3.p.16 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=15)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=15))
        
        
        a3<-rbind(e.3.p.1, e.3.p.2, e.3.p.3, e.3.p.4, e.3.p.5, e.3.p.6, e.3.p.7, e.3.p.8, e.3.p.9, e.3.p.10, e.3.p.11, e.3.p.12,
                  e.3.p.13, e.3.p.14, e.3.p.15, e.3.p.16)
        
        b3<- (a3-min(a3))/(max(a3)-min(a3)) 
        
        e.4.p.1<- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=0))
        e.4.p.2 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=1))
        e.4.p.3 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=2))
        e.4.p.4 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=3))
        e.4.p.5 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=4))
        e.4.p.6 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=5))
        e.4.p.7 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=6))
        e.4.p.8 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=7))
        e.4.p.9 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=8)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=8))
        e.4.p.10 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=9)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                           family=input$family),level=5, index=9))
        e.4.p.11 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=10)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=10))
        e.4.p.12 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=11)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=11))
        e.4.p.13 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=12)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=12))
        e.4.p.14 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=13)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=13))
        e.4.p.15 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=14)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=14))
        e.4.p.16 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=15)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=15))
        
        
        a4<-rbind(e.4.p.1, e.4.p.2, e.4.p.3, e.4.p.4, e.4.p.5, e.4.p.6, e.4.p.7, e.4.p.8, e.4.p.9, e.4.p.10, e.4.p.11, e.4.p.12,
                  e.4.p.13, e.4.p.14, e.4.p.15, e.4.p.16)
        
        b4<- (a4-min(a4))/(max(a4)-min(a4)) 
        
        rbind(b1, b2, b3, b4)
      })
      names(packets3)<- c(names(packets2))
      packets4 <- as.data.frame(packets3)
      
      packets5 <- lapply(names(packets4),  function(i){
        t(packets4[[i]])
      })
      packets6 <- as.data.frame(packets5)
      packets7 <- t(packets6)
      trainseq <- ts(packets7)
      plot(trainseq, xlab = "Sequence",main = "Trainning Sequence")
    }
    
  })
  
  ### 3.1.1 Render Plot Normilized average-energy trainning Sequence
  output$trainningplot <- renderPlot({
    trainningplot()
  })
  
  # joined training sample  
  jointrainsamp <- reactive({
    epochs1 <- lapply(input$file1$datapath, read.table, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    epochs2 <- as.data.frame(epochs1)
  })
  
  # HMM Model ntime  
  ntime2 <- eventReactive(input$action2,{
    if (input$Wavesize == "256") {
      
      c(rep(8,length(jointrainsamp())*8))
    }
    else {
      c(rep(16,length(jointrainsamp())*4))
    }
  })
  
  # train seq
  trainningdata <- eventReactive(input$action2,{
  if (input$Wavesize == "256"){
    if(is.null(input$file1)) {return()}
    
      packets1 <- lapply(input$file1$datapath, read.table, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
      packets2 <- as.data.frame(packets1)
      packets3 <- reactiveValues()
      packets3 <- lapply(names(packets2),  function(i){
        
        e.1.p.1<- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                                level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=0))
        e.1.p.2 <- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=1))
        e.1.p.3 <- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=2))
        e.1.p.4 <- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=3))
        e.1.p.5 <- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=4))
        e.1.p.6 <- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=5))
        e.1.p.7 <- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=6))
        e.1.p.8 <- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=7))
        a1<-rbind(e.1.p.1, e.1.p.2, e.1.p.3, e.1.p.4, e.1.p.5, e.1.p.6, e.1.p.7, e.1.p.8)
        b1<- (a1-min(a1))/(max(a1)-min(a1)) 
        
        e.2.p.1<- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                                level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=0))
        e.2.p.2 <- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=1))
        e.2.p.3 <- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=2))
        e.2.p.4 <- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=3))
        e.2.p.5 <- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=4))
        e.2.p.6 <- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=5))
        e.2.p.7 <- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=6))
        e.2.p.8 <- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=7))
        a2<-rbind(e.2.p.1, e.2.p.2, e.2.p.3, e.2.p.4, e.2.p.5, e.2.p.6, e.2.p.7, e.2.p.8)
        b2<- (a2-min(a2))/(max(a2)-min(a2)) 
        
        e.3.p.1<- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                                level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=0))
        e.3.p.2 <- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=1))
        e.3.p.3 <- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=2))
        e.3.p.4 <- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=3))
        e.3.p.5 <- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=4))
        e.3.p.6 <- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=5))
        e.3.p.7 <- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=6))
        e.3.p.8 <- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=7))
        a3<-rbind(e.3.p.1, e.3.p.2, e.3.p.3, e.3.p.4, e.3.p.5, e.3.p.6, e.3.p.7, e.3.p.8)
        b3<- (a3-min(a3))/(max(a3)-min(a3)) 
        
        e.4.p.1<- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=0))
        e.4.p.2 <- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=1))
        e.4.p.3 <- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=2))
        e.4.p.4 <- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=3))
        e.4.p.5 <- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=4))
        e.4.p.6 <- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=5))
        e.4.p.7 <- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=6))
        e.4.p.8 <- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=7))
        a4<-rbind(e.4.p.1, e.4.p.2, e.4.p.3, e.4.p.4, e.4.p.5, e.4.p.6, e.4.p.7, e.4.p.8)
        b4<- (a4-min(a4))/(max(a4)-min(a4)) 
        
        e.5.p.1<- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                                level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=0))
        e.5.p.2 <- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=1))
        e.5.p.3 <- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=2))
        e.5.p.4 <- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=3))
        e.5.p.5 <- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=4))
        e.5.p.6 <- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=5))
        e.5.p.7 <- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=6))
        e.5.p.8 <- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=7))
        a5<-rbind(e.5.p.1, e.5.p.2, e.5.p.3, e.5.p.4, e.5.p.5, e.5.p.6, e.5.p.7, e.5.p.8)
        b5<- (a5-min(a5))/(max(a5)-min(a5)) 
        
        e.6.p.1<- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                                level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=0))
        e.6.p.2 <- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=1))
        e.6.p.3 <- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=2))
        e.6.p.4 <- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=3))
        e.6.p.5 <- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=4))
        e.6.p.6 <- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=5))
        e.6.p.7 <- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=6))
        e.6.p.8 <- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=7))
        a6<-rbind(e.6.p.1, e.6.p.2, e.6.p.3, e.6.p.4, e.6.p.5, e.6.p.6, e.6.p.7, e.6.p.8)
        b6<- (a6-min(a6))/(max(a6)-min(a6)) 
        
        e.7.p.1<- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                                level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=0))
        e.7.p.2 <- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=1))
        e.7.p.3 <- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=2))
        e.7.p.4 <- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=3))
        e.7.p.5 <- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=4))
        e.7.p.6 <- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=5))
        e.7.p.7 <- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=6))
        e.7.p.8 <- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=7))
        a7<-rbind(e.7.p.1, e.7.p.2, e.7.p.3, e.7.p.4, e.7.p.5, e.7.p.6, e.7.p.7, e.7.p.8)
        b7<- (a7-min(a7))/(max(a7)-min(a7)) 
        
        e.8.p.1<- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=0))
        e.8.p.2 <- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=1))
        e.8.p.3 <- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=2))
        e.8.p.4 <- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=3))
        e.8.p.5 <- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=4))
        e.8.p.6 <- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=5))
        e.8.p.7 <- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=6))
        e.8.p.8 <- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=7))
        a8<-rbind(e.8.p.1, e.8.p.2, e.8.p.3, e.8.p.4, e.8.p.5, e.8.p.6, e.8.p.7, e.8.p.8)
        b8<- (a8-min(a8))/(max(a8)-min(a8)) 
        
        rbind(b1, b2, b3, b4, b5, b6,b7, b8)
      })
      names(packets3)<- c(names(packets2))
      packets4 <- as.data.frame(packets3)
      packets5 <- lapply(names(packets4),  function(i){
        t(packets4[[i]])})
      packets6 <- as.data.frame(packets5)
      packets7 <- t(packets6)
    
    
  }
  else {
    
    packets1 <- lapply(input$file1$datapath, read.table, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    packets2 <- as.data.frame(packets1)
    packets3 <- reactiveValues()
    packets3 <- lapply(names(packets2),  function(i){
      
      e.1.p.1<- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                              level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                       family=input$family),level=5, index=0))
      e.1.p.2 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=1))
      e.1.p.3 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=2))
      e.1.p.4 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=3))
      e.1.p.5 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=4))
      e.1.p.6 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=5))
      e.1.p.7 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=6))
      e.1.p.8 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=7))
      e.1.p.9 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=8)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=8))
      e.1.p.10 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                level=5, index=9)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=9))
      e.1.p.11 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                level=5, index=10)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=10))
      e.1.p.12 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                level=5, index=11)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=11))
      e.1.p.13 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                level=5, index=12)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=12))
      e.1.p.14 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                level=5, index=13)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=13))
      e.1.p.15 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                level=5, index=14)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=14))
      e.1.p.16 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                level=5, index=15)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=15))
      
      a1<-rbind(e.1.p.1, e.1.p.2, e.1.p.3, e.1.p.4, e.1.p.5, e.1.p.6, e.1.p.7, e.1.p.8, e.1.p.9, e.1.p.10, e.1.p.11, e.1.p.12,
                e.1.p.13, e.1.p.14, e.1.p.15, e.1.p.16)
      
      b1<- (a1-min(a1))/(max(a1)-min(a1)) 
      
      e.2.p.1<- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                              level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                       family=input$family),level=5, index=0))
      e.2.p.2 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=1))
      e.2.p.3 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=2))
      e.2.p.4 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=3))
      e.2.p.5 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=4))
      e.2.p.6 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=5))
      e.2.p.7 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=6))
      e.2.p.8 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=7))
      e.2.p.9 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=8)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=8))
      e.2.p.10 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=9)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=9))
      e.2.p.11 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=10)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=10))
      e.2.p.12 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=11)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=11))
      e.2.p.13 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=12)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=12))
      e.2.p.14 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=13)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=13))
      e.2.p.15 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=14)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=14))
      e.2.p.16 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=15)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=15))
      
      
      a2<-rbind(e.2.p.1, e.2.p.2, e.2.p.3, e.2.p.4, e.2.p.5, e.2.p.6, e.2.p.7, e.2.p.8, e.2.p.9, e.2.p.10, e.2.p.11, e.2.p.12,
                e.2.p.13, e.2.p.14, e.2.p.15, e.2.p.16)
      
      b2<- (a2-min(a2))/(max(a2)-min(a2)) 
      
      e.3.p.1<- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                              level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                       family=input$family),level=5, index=0))
      e.3.p.2 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=1))
      e.3.p.3 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=2))
      e.3.p.4 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=3))
      e.3.p.5 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=4))
      e.3.p.6 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=5))
      e.3.p.7 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=6))
      e.3.p.8 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=7))
      e.3.p.9 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=8)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=8))
      e.3.p.10 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                level=5, index=9)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=9))
      e.3.p.11 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                level=5, index=10)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=10))
      e.3.p.12 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                level=5, index=11)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=11))
      e.3.p.13 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                level=5, index=12)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=12))
      e.3.p.14 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                level=5, index=13)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=13))
      e.3.p.15 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                level=5, index=14)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=14))
      e.3.p.16 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                level=5, index=15)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=15))
      
      
      a3<-rbind(e.3.p.1, e.3.p.2, e.3.p.3, e.3.p.4, e.3.p.5, e.3.p.6, e.3.p.7, e.3.p.8, e.3.p.9, e.3.p.10, e.3.p.11, e.3.p.12,
                e.3.p.13, e.3.p.14, e.3.p.15, e.3.p.16)
      
      b3<- (a3-min(a3))/(max(a3)-min(a3)) 
      
      e.4.p.1<- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                              level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                       family=input$family),level=5, index=0))
      e.4.p.2 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=1))
      e.4.p.3 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=2))
      e.4.p.4 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=3))
      e.4.p.5 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=4))
      e.4.p.6 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=5))
      e.4.p.7 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=6))
      e.4.p.8 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=7))
      e.4.p.9 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=8)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=8))
      e.4.p.10 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=9)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=9))
      e.4.p.11 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=10)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=10))
      e.4.p.12 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=11)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=11))
      e.4.p.13 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=12)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=12))
      e.4.p.14 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=13)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=13))
      e.4.p.15 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=14)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=14))
      e.4.p.16 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=15)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=15))
      
      
      a4<-rbind(e.4.p.1, e.4.p.2, e.4.p.3, e.4.p.4, e.4.p.5, e.4.p.6, e.4.p.7, e.4.p.8, e.4.p.9, e.4.p.10, e.4.p.11, e.4.p.12,
                e.4.p.13, e.4.p.14, e.4.p.15, e.4.p.16)
      
      b4<- (a4-min(a4))/(max(a4)-min(a4)) 
      
      rbind(b1, b2, b3, b4)
    })
    names(packets3)<- c(names(packets2))
    packets4 <- as.data.frame(packets3)
    packets5 <- lapply(names(packets4),  function(i){
      t(packets4[[i]])})
    packets6 <- as.data.frame(packets5)
    packets7 <- t(packets6)
  }
  })
  
  # Train Model with train seq
  trainedmod <- eventReactive(input$action2,{
    if(is.null(input$file1)) {return()}
    
    isolate( if(input$Trainparameters == "3 State Custom parameters"){
      packets4 <- as.data.frame(trainningdata())
      
      packets5 <- lapply(names(packets4),  function(i){
        t(packets4[[i]])
      })
      packets6 <- as.data.frame(packets5)
      packets7 <- t(packets6)
      packets8 <- as.data.frame(packets7)
      set.seed(2)
      trst <- c(input$s1s1, input$s1s2, input$s1s2, input$s2s1, 
                input$s2s2, input$s2s3, input$s3s1, input$s3s2,input$s3s3)
      instart<- c(input$s1, input$s2, input$s3)
      mod1.4 <- depmix(V1~1,nstates=3, 
                       data=packets8,family=gaussian(),trst=trst, 
                       instart=instart, 
                       ntimes = ntime2())
      
      fm1.3 <- fit(mod1.4, emc=em.control(rand=FALSE))
    } else  {
      packets4 <- as.data.frame(trainningdata())
      
      packets5 <- lapply(names(packets4),  function(i){
        t(packets4[[i]])
      })
      packets6 <- as.data.frame(packets5)
      packets7 <- t(packets6)
      packets8 <- as.data.frame(packets7)
      set.seed(2)
      
      mod1.4 <- depmix(V1~1,nstates=input$states, 
                       data=packets8,family=gaussian(), 
                       ntimes = ntime2())
      
      fm1.3 <- fit(mod1.4)  
    }) 
    
    
  })
  
  # HMM trained state prob
  output$init <- renderTable({
    if(input$Trainparameters == "3 State Custom parameters" | 
       input$states == 3){
      init <-data.frame("S1"=getpars(trainedmod())[1], 
                        "S2"=getpars(trainedmod())[2], 
                        "S3"=getpars(trainedmod())[3])
      as.data.frame(init)
    }})
  
  # HMM trained trans matrix
  output$fromS1 <- renderTable({
    if(input$Trainparameters == "3 State Custom parameters" | 
       input$states == 3){
      fromS1 <-data.frame("S1S1"=getpars(trainedmod())[4], 
                          "S1S2"=getpars(trainedmod())[5], 
                          "S1S3"=getpars(trainedmod())[6])
      as.data.frame(fromS1)
    }})
  # HMM trained trans matrix
  output$fromS2 <- renderTable({
    if(input$Trainparameters == "3 State Custom parameters" | 
       input$states == 3){
      fromS2 <-data.frame("S2S1"=getpars(trainedmod())[7], 
                          "S2S2"=getpars(trainedmod())[8], 
                          "S2S3"=getpars(trainedmod())[9])
      as.data.frame(fromS2)
    }})
  # HMM trained trans matrix
  output$fromS3 <- renderTable({
    if(input$Trainparameters == "3 State Custom parameters" | 
       input$states == 3){
      fromS3 <-data.frame("S3S1"=getpars(trainedmod())[10], 
                          "S3S2"=getpars(trainedmod())[11], 
                          "S3S3"=getpars(trainedmod())[12])
      as.data.frame(fromS3)
    }})
  
  ## 3.2 Trainned Sequence
  trainningplot1 <- eventReactive(input$action2,{
    if(is.null(input$file1)) {return()}
    if (input$Wavesize == "256"){
    packets1 <- lapply(input$file1$datapath, read.table, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    packets2 <- as.data.frame(packets1)
    packets3 <- reactiveValues()
    packets3 <- lapply(names(packets2),  function(i){
      
      e.1.p.1<- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                              level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                       family=input$family),level=5, index=0))
      e.1.p.2 <- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                               level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=1))
      e.1.p.3 <- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                               level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=2))
      e.1.p.4 <- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                               level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=3))
      e.1.p.5 <- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                               level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=4))
      e.1.p.6 <- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                               level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=5))
      e.1.p.7 <- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                               level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=6))
      e.1.p.8 <- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                               level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=7))
      a1<-rbind(e.1.p.1, e.1.p.2, e.1.p.3, e.1.p.4, e.1.p.5, e.1.p.6, e.1.p.7, e.1.p.8)
      b1<- (a1-min(a1))/(max(a1)-min(a1)) 
      
      e.2.p.1<- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                              level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                       family=input$family),level=5, index=0))
      e.2.p.2 <- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=1))
      e.2.p.3 <- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=2))
      e.2.p.4 <- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=3))
      e.2.p.5 <- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=4))
      e.2.p.6 <- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=5))
      e.2.p.7 <- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=6))
      e.2.p.8 <- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=7))
      a2<-rbind(e.2.p.1, e.2.p.2, e.2.p.3, e.2.p.4, e.2.p.5, e.2.p.6, e.2.p.7, e.2.p.8)
      b2<- (a2-min(a2))/(max(a2)-min(a2)) 
      
      e.3.p.1<- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                              level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                       family=input$family),level=5, index=0))
      e.3.p.2 <- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                               level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=1))
      e.3.p.3 <- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                               level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=2))
      e.3.p.4 <- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                               level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=3))
      e.3.p.5 <- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                               level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=4))
      e.3.p.6 <- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                               level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=5))
      e.3.p.7 <- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                               level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=6))
      e.3.p.8 <- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                               level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=7))
      a3<-rbind(e.3.p.1, e.3.p.2, e.3.p.3, e.3.p.4, e.3.p.5, e.3.p.6, e.3.p.7, e.3.p.8)
      b3<- (a3-min(a3))/(max(a3)-min(a3)) 
      
      e.4.p.1<- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                              level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                       family=input$family),level=5, index=0))
      e.4.p.2 <- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=1))
      e.4.p.3 <- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=2))
      e.4.p.4 <- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=3))
      e.4.p.5 <- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=4))
      e.4.p.6 <- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=5))
      e.4.p.7 <- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=6))
      e.4.p.8 <- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=7))
      a4<-rbind(e.4.p.1, e.4.p.2, e.4.p.3, e.4.p.4, e.4.p.5, e.4.p.6, e.4.p.7, e.4.p.8)
      b4<- (a4-min(a4))/(max(a4)-min(a4)) 
      
      e.5.p.1<- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                              level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                       family=input$family),level=5, index=0))
      e.5.p.2 <- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                               level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=1))
      e.5.p.3 <- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                               level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=2))
      e.5.p.4 <- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                               level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=3))
      e.5.p.5 <- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                               level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=4))
      e.5.p.6 <- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                               level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=5))
      e.5.p.7 <- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                               level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=6))
      e.5.p.8 <- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                               level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=7))
      a5<-rbind(e.5.p.1, e.5.p.2, e.5.p.3, e.5.p.4, e.5.p.5, e.5.p.6, e.5.p.7, e.5.p.8)
      b5<- (a5-min(a5))/(max(a5)-min(a5)) 
      
      e.6.p.1<- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                              level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                       family=input$family),level=5, index=0))
      e.6.p.2 <- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=1))
      e.6.p.3 <- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=2))
      e.6.p.4 <- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=3))
      e.6.p.5 <- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=4))
      e.6.p.6 <- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=5))
      e.6.p.7 <- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=6))
      e.6.p.8 <- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=7))
      a6<-rbind(e.6.p.1, e.6.p.2, e.6.p.3, e.6.p.4, e.6.p.5, e.6.p.6, e.6.p.7, e.6.p.8)
      b6<- (a6-min(a6))/(max(a6)-min(a6)) 
      
      e.7.p.1<- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                              level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                       family=input$family),level=5, index=0))
      e.7.p.2 <- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                               level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=1))
      e.7.p.3 <- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                               level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=2))
      e.7.p.4 <- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                               level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=3))
      e.7.p.5 <- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                               level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=4))
      e.7.p.6 <- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                               level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=5))
      e.7.p.7 <- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                               level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=6))
      e.7.p.8 <- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                               level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=7))
      a7<-rbind(e.7.p.1, e.7.p.2, e.7.p.3, e.7.p.4, e.7.p.5, e.7.p.6, e.7.p.7, e.7.p.8)
      b7<- (a7-min(a7))/(max(a7)-min(a7)) 
      
      e.8.p.1<- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                              level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                       family=input$family),level=5, index=0))
      e.8.p.2 <- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=1))
      e.8.p.3 <- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=2))
      e.8.p.4 <- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=3))
      e.8.p.5 <- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=4))
      e.8.p.6 <- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=5))
      e.8.p.7 <- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=6))
      e.8.p.8 <- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=7))
      a8<-rbind(e.8.p.1, e.8.p.2, e.8.p.3, e.8.p.4, e.8.p.5, e.8.p.6, e.8.p.7, e.8.p.8)
      b8<- (a8-min(a8))/(max(a8)-min(a8)) 
      
      rbind(b1, b2, b3, b4, b5, b6,b7, b8)
    })
    names(packets3)<- c(names(packets2))
    packets4 <- as.data.frame(packets3)
    
    packets5 <- lapply(names(packets4),  function(i){
      t(packets4[[i]])
    })
    packets6 <- as.data.frame(packets5)
    packets7 <- t(packets6)
    trainseq <- ts(packets7)
    plot(trainseq, xlab = "Sequence",main = "Trainning Sequence with states")
    text(trainseq, col=trainedmod()@posterior[,1], labels=trainedmod()@posterior[,1])
    }
    else{
      
      packets1 <- lapply(input$file1$datapath, read.table, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
      packets2 <- as.data.frame(packets1)
      packets3 <- reactiveValues()
      packets3 <- lapply(names(packets2),  function(i){
        
        e.1.p.1<- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=0))
        e.1.p.2 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=1))
        e.1.p.3 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=2))
        e.1.p.4 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=3))
        e.1.p.5 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=4))
        e.1.p.6 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=5))
        e.1.p.7 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=6))
        e.1.p.8 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=7))
        e.1.p.9 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=8)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=8))
        e.1.p.10 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=9)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                           family=input$family),level=5, index=9))
        e.1.p.11 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=10)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=10))
        e.1.p.12 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=11)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=11))
        e.1.p.13 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=12)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=12))
        e.1.p.14 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=13)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=13))
        e.1.p.15 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=14)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=14))
        e.1.p.16 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=15)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=15))
        
        a1<-rbind(e.1.p.1, e.1.p.2, e.1.p.3, e.1.p.4, e.1.p.5, e.1.p.6, e.1.p.7, e.1.p.8, e.1.p.9, e.1.p.10, e.1.p.11, e.1.p.12,
                  e.1.p.13, e.1.p.14, e.1.p.15, e.1.p.16)
        
        b1<- (a1-min(a1))/(max(a1)-min(a1)) 
        
        e.2.p.1<- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=0))
        e.2.p.2 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=1))
        e.2.p.3 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=2))
        e.2.p.4 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=3))
        e.2.p.5 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=4))
        e.2.p.6 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=5))
        e.2.p.7 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=6))
        e.2.p.8 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=7))
        e.2.p.9 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=8)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=8))
        e.2.p.10 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=9)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                           family=input$family),level=5, index=9))
        e.2.p.11 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=10)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=10))
        e.2.p.12 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=11)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=11))
        e.2.p.13 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=12)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=12))
        e.2.p.14 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=13)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=13))
        e.2.p.15 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=14)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=14))
        e.2.p.16 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=15)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=15))
        
        
        a2<-rbind(e.2.p.1, e.2.p.2, e.2.p.3, e.2.p.4, e.2.p.5, e.2.p.6, e.2.p.7, e.2.p.8, e.2.p.9, e.2.p.10, e.2.p.11, e.2.p.12,
                  e.2.p.13, e.2.p.14, e.2.p.15, e.2.p.16)
        
        b2<- (a2-min(a2))/(max(a2)-min(a2)) 
        
        e.3.p.1<- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=0))
        e.3.p.2 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=1))
        e.3.p.3 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=2))
        e.3.p.4 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=3))
        e.3.p.5 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=4))
        e.3.p.6 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=5))
        e.3.p.7 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=6))
        e.3.p.8 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=7))
        e.3.p.9 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=8)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=8))
        e.3.p.10 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=9)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                           family=input$family),level=5, index=9))
        e.3.p.11 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=10)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=10))
        e.3.p.12 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=11)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=11))
        e.3.p.13 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=12)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=12))
        e.3.p.14 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=13)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=13))
        e.3.p.15 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=14)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=14))
        e.3.p.16 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=15)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=15))
        
        
        a3<-rbind(e.3.p.1, e.3.p.2, e.3.p.3, e.3.p.4, e.3.p.5, e.3.p.6, e.3.p.7, e.3.p.8, e.3.p.9, e.3.p.10, e.3.p.11, e.3.p.12,
                  e.3.p.13, e.3.p.14, e.3.p.15, e.3.p.16)
        
        b3<- (a3-min(a3))/(max(a3)-min(a3)) 
        
        e.4.p.1<- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=0))
        e.4.p.2 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=1))
        e.4.p.3 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=2))
        e.4.p.4 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=3))
        e.4.p.5 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=4))
        e.4.p.6 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=5))
        e.4.p.7 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=6))
        e.4.p.8 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=7))
        e.4.p.9 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=8)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=8))
        e.4.p.10 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=9)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                           family=input$family),level=5, index=9))
        e.4.p.11 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=10)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=10))
        e.4.p.12 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=11)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=11))
        e.4.p.13 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=12)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=12))
        e.4.p.14 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=13)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=13))
        e.4.p.15 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=14)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=14))
        e.4.p.16 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=15)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=15))
        
        
        a4<-rbind(e.4.p.1, e.4.p.2, e.4.p.3, e.4.p.4, e.4.p.5, e.4.p.6, e.4.p.7, e.4.p.8, e.4.p.9, e.4.p.10, e.4.p.11, e.4.p.12,
                  e.4.p.13, e.4.p.14, e.4.p.15, e.4.p.16)
        
        b4<- (a4-min(a4))/(max(a4)-min(a4)) 
        
        rbind(b1, b2, b3, b4)
      })
      names(packets3)<- c(names(packets2))
      packets4 <- as.data.frame(packets3)
      
      packets5 <- lapply(names(packets4),  function(i){
        t(packets4[[i]])
      })
      packets6 <- as.data.frame(packets5)
      packets7 <- t(packets6)
      trainseq <- ts(packets7)
      plot(trainseq, xlab = "Sequence",main = "Trainning Sequence with states")
      text(trainseq, col=trainedmod()@posterior[,1], labels=trainedmod()@posterior[,1])
      
    }
    
  })
  
  ### 3.2.1 Render Plot Trainned Sequence
  output$trainningplot1 <- renderPlot({
    trainningplot1()
  })
  

# Feature vectors  
featvector <- eventReactive(input$action1,{
  if(is.null(input$file)) {return()}
  if (input$Wavesize == "256") {
  packets1 <- lapply(input$file$datapath, read.table, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
  packets2 <- as.data.frame(packets1)
  packets3 <- reactiveValues()
  packets3 <- lapply(names(packets2),  function(i){
    
    e.1.p.1<- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                            level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                     family=input$family),level=5, index=0))
    e.1.p.2 <- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                             level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=1))
    e.1.p.3 <- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                             level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=2))
    e.1.p.4 <- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                             level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=3))
    e.1.p.5 <- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                             level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=4))
    e.1.p.6 <- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                             level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=5))
    e.1.p.7 <- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                             level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=6))
    e.1.p.8 <- sum(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, family=input$family),
                             level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1:256], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=7))
    a1<-rbind(e.1.p.1, e.1.p.2, e.1.p.3, e.1.p.4, e.1.p.5, e.1.p.6, e.1.p.7, e.1.p.8)
    b1<- (a1-min(a1))/(max(a1)-min(a1)) 
    
    e.2.p.1<- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                            level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                     family=input$family),level=5, index=0))
    e.2.p.2 <- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                             level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=1))
    e.2.p.3 <- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                             level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=2))
    e.2.p.4 <- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                             level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=3))
    e.2.p.5 <- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                             level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=4))
    e.2.p.6 <- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                             level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=5))
    e.2.p.7 <- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                             level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=6))
    e.2.p.8 <- sum(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, family=input$family),
                             level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][257:512], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=7))
    a2<-rbind(e.2.p.1, e.2.p.2, e.2.p.3, e.2.p.4, e.2.p.5, e.2.p.6, e.2.p.7, e.2.p.8)
    b2<- (a2-min(a2))/(max(a2)-min(a2)) 
    
    e.3.p.1<- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                            level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                     family=input$family),level=5, index=0))
    e.3.p.2 <- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                             level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=1))
    e.3.p.3 <- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                             level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=2))
    e.3.p.4 <- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                             level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=3))
    e.3.p.5 <- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                             level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=4))
    e.3.p.6 <- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                             level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=5))
    e.3.p.7 <- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                             level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=6))
    e.3.p.8 <- sum(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, family=input$family),
                             level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][513:768], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=7))
    a3<-rbind(e.3.p.1, e.3.p.2, e.3.p.3, e.3.p.4, e.3.p.5, e.3.p.6, e.3.p.7, e.3.p.8)
    b3<- (a3-min(a3))/(max(a3)-min(a3)) 
    
    e.4.p.1<- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                            level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                     family=input$family),level=5, index=0))
    e.4.p.2 <- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                             level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=1))
    e.4.p.3 <- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                             level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=2))
    e.4.p.4 <- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                             level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=3))
    e.4.p.5 <- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                             level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=4))
    e.4.p.6 <- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                             level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=5))
    e.4.p.7 <- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                             level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=6))
    e.4.p.8 <- sum(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, family=input$family),
                             level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][769:1024], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=7))
    a4<-rbind(e.4.p.1, e.4.p.2, e.4.p.3, e.4.p.4, e.4.p.5, e.4.p.6, e.4.p.7, e.4.p.8)
    b4<- (a4-min(a4))/(max(a4)-min(a4)) 
    
    e.5.p.1<- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                            level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                     family=input$family),level=5, index=0))
    e.5.p.2 <- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                             level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=1))
    e.5.p.3 <- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                             level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=2))
    e.5.p.4 <- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                             level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=3))
    e.5.p.5 <- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                             level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=4))
    e.5.p.6 <- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                             level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=5))
    e.5.p.7 <- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                             level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=6))
    e.5.p.8 <- sum(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, family=input$family),
                             level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1025:1280], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=7))
    a5<-rbind(e.5.p.1, e.5.p.2, e.5.p.3, e.5.p.4, e.5.p.5, e.5.p.6, e.5.p.7, e.5.p.8)
    b5<- (a5-min(a5))/(max(a5)-min(a5)) 
    
    e.6.p.1<- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                            level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                     family=input$family),level=5, index=0))
    e.6.p.2 <- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                             level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=1))
    e.6.p.3 <- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                             level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=2))
    e.6.p.4 <- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                             level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=3))
    e.6.p.5 <- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                             level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=4))
    e.6.p.6 <- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                             level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=5))
    e.6.p.7 <- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                             level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=6))
    e.6.p.8 <- sum(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, family=input$family),
                             level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1281:1536], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=7))
    a6<-rbind(e.6.p.1, e.6.p.2, e.6.p.3, e.6.p.4, e.6.p.5, e.6.p.6, e.6.p.7, e.6.p.8)
    b6<- (a6-min(a6))/(max(a6)-min(a6)) 
    
    e.7.p.1<- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                            level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                     family=input$family),level=5, index=0))
    e.7.p.2 <- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                             level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=1))
    e.7.p.3 <- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                             level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=2))
    e.7.p.4 <- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                             level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=3))
    e.7.p.5 <- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                             level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=4))
    e.7.p.6 <- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                             level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=5))
    e.7.p.7 <- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                             level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=6))
    e.7.p.8 <- sum(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, family=input$family),
                             level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1537:1792], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=7))
    a7<-rbind(e.7.p.1, e.7.p.2, e.7.p.3, e.7.p.4, e.7.p.5, e.7.p.6, e.7.p.7, e.7.p.8)
    b7<- (a7-min(a7))/(max(a7)-min(a7)) 
    
    e.8.p.1<- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                            level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                     family=input$family),level=5, index=0))
    e.8.p.2 <- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                             level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=1))
    e.8.p.3 <- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                             level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=2))
    e.8.p.4 <- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                             level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=3))
    e.8.p.5 <- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                             level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=4))
    e.8.p.6 <- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                             level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=5))
    e.8.p.7 <- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                             level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=6))
    e.8.p.8 <- sum(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, family=input$family),
                             level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1793:2048], filter.number=input$wavefilter, 
                                                                      family=input$family),level=5, index=7))
    a8<-rbind(e.8.p.1, e.8.p.2, e.8.p.3, e.8.p.4, e.8.p.5, e.8.p.6, e.8.p.7, e.8.p.8)
    b8<- (a8-min(a8))/(max(a8)-min(a8)) 
    
    rbind(b1, b2, b3, b4, b5, b6,b7, b8)
  })
  packets4 <- as.data.frame(packets3)
  names(packets4) <- names(packets2)
  packets4
  }
  else{
    
    packets1 <- lapply(input$file$datapath, read.table, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    packets2 <- as.data.frame(packets1)
    packets3 <- reactiveValues()
    packets3 <- lapply(names(packets2),  function(i){
      
      e.1.p.1<- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                              level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                       family=input$family),level=5, index=0))
      e.1.p.2 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=1))
      e.1.p.3 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=2))
      e.1.p.4 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=3))
      e.1.p.5 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=4))
      e.1.p.6 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=5))
      e.1.p.7 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=6))
      e.1.p.8 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=7))
      e.1.p.9 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                               level=5, index=8)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=8))
      e.1.p.10 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                level=5, index=9)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=9))
      e.1.p.11 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                level=5, index=10)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=10))
      e.1.p.12 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                level=5, index=11)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=11))
      e.1.p.13 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                level=5, index=12)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=12))
      e.1.p.14 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                level=5, index=13)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=13))
      e.1.p.15 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                level=5, index=14)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=14))
      e.1.p.16 <- sum(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, family=input$family),
                                level=5, index=15)^2)/length(getpacket(wp(packets2[[i]][1:512], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=15))
      
      a1<-rbind(e.1.p.1, e.1.p.2, e.1.p.3, e.1.p.4, e.1.p.5, e.1.p.6, e.1.p.7, e.1.p.8, e.1.p.9, e.1.p.10, e.1.p.11, e.1.p.12,
                e.1.p.13, e.1.p.14, e.1.p.15, e.1.p.16)
      
      b1<- (a1-min(a1))/(max(a1)-min(a1)) 
      
      e.2.p.1<- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                              level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                       family=input$family),level=5, index=0))
      e.2.p.2 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=1))
      e.2.p.3 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=2))
      e.2.p.4 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=3))
      e.2.p.5 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=4))
      e.2.p.6 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=5))
      e.2.p.7 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=6))
      e.2.p.8 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=7))
      e.2.p.9 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=8)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=8))
      e.2.p.10 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=9)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=9))
      e.2.p.11 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=10)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=10))
      e.2.p.12 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=11)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=11))
      e.2.p.13 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=12)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=12))
      e.2.p.14 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=13)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=13))
      e.2.p.15 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=14)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=14))
      e.2.p.16 <- sum(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=15)^2)/length(getpacket(wp(packets2[[i]][513:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=15))
      
      
      a2<-rbind(e.2.p.1, e.2.p.2, e.2.p.3, e.2.p.4, e.2.p.5, e.2.p.6, e.2.p.7, e.2.p.8, e.2.p.9, e.2.p.10, e.2.p.11, e.2.p.12,
                e.2.p.13, e.2.p.14, e.2.p.15, e.2.p.16)
      
      b2<- (a2-min(a2))/(max(a2)-min(a2)) 
      
      e.3.p.1<- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                              level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                       family=input$family),level=5, index=0))
      e.3.p.2 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=1))
      e.3.p.3 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=2))
      e.3.p.4 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=3))
      e.3.p.5 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=4))
      e.3.p.6 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=5))
      e.3.p.7 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=6))
      e.3.p.8 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=7))
      e.3.p.9 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                               level=5, index=8)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=8))
      e.3.p.10 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                level=5, index=9)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=9))
      e.3.p.11 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                level=5, index=10)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=10))
      e.3.p.12 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                level=5, index=11)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=11))
      e.3.p.13 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                level=5, index=12)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=12))
      e.3.p.14 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                level=5, index=13)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=13))
      e.3.p.15 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                level=5, index=14)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=14))
      e.3.p.16 <- sum(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, family=input$family),
                                level=5, index=15)^2)/length(getpacket(wp(packets2[[i]][1025:1536], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=15))
      
      
      a3<-rbind(e.3.p.1, e.3.p.2, e.3.p.3, e.3.p.4, e.3.p.5, e.3.p.6, e.3.p.7, e.3.p.8, e.3.p.9, e.3.p.10, e.3.p.11, e.3.p.12,
                e.3.p.13, e.3.p.14, e.3.p.15, e.3.p.16)
      
      b3<- (a3-min(a3))/(max(a3)-min(a3)) 
      
      e.4.p.1<- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                              level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                       family=input$family),level=5, index=0))
      e.4.p.2 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=1))
      e.4.p.3 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=2))
      e.4.p.4 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=3))
      e.4.p.5 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=4))
      e.4.p.6 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=5))
      e.4.p.7 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=6))
      e.4.p.8 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=7))
      e.4.p.9 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=8)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=8))
      e.4.p.10 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=9)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=9))
      e.4.p.11 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=10)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=10))
      e.4.p.12 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=11)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=11))
      e.4.p.13 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=12)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=12))
      e.4.p.14 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=13)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=13))
      e.4.p.15 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=14)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=14))
      e.4.p.16 <- sum(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=15)^2)/length(getpacket(wp(packets2[[i]][1537:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=15))
      
      
      a4<-rbind(e.4.p.1, e.4.p.2, e.4.p.3, e.4.p.4, e.4.p.5, e.4.p.6, e.4.p.7, e.4.p.8, e.4.p.9, e.4.p.10, e.4.p.11, e.4.p.12,
                e.4.p.13, e.4.p.14, e.4.p.15, e.4.p.16)
      
      b4<- (a4-min(a4))/(max(a4)-min(a4)) 
      
      rbind(b1, b2, b3, b4)
    })
    packets4 <- as.data.frame(packets3)
    names(packets4) <- names(packets2)
    packets4
    
  }
})

# Plot log-prob 
prognost <- eventReactive(input$action1, {
  
  if(is.null(input$file)) {return()}
  if (input$Wavesize == "256") {
  isolate( if(input$Trainparameters == "3 State Custom parameters"){
  packets5 <- as.data.frame(featvector())
  packets6 <- lapply(names(packets5),  function(j){
    forwardbackward(setpars(depmix(packets5[[j]]~1,nstates=3, 
                                   data=packets5,family=gaussian(),
                                   ntimes = c(8,8,8,8,8,8,8,8)), getpars(trainedmod())))
  })
 
  packets7 <-lapply(1:length(packets6),  function(i){
    packets6[[i]][["logLike"]]
  })
  packets8 <-as.data.frame(packets7)
  packets9 <-t(packets8)
  LogProbabilily <-ts(packets9)
  plot(LogProbabilily,xlab = "Samples",main = "Log-Probabilily")
  } else  {
    packets5 <- as.data.frame(featvector())
    packets6 <- lapply(names(packets5),  function(j){
      forwardbackward(setpars(depmix(packets5[[j]]~1,nstates=input$states, 
                                     data=packets5,family=gaussian(),
                                     ntimes = c(8,8,8,8,8,8,8,8)), getpars(trainedmod())))
    })
    
    packets7 <-lapply(1:length(packets6),  function(i){
      packets6[[i]][["logLike"]]
    })
    packets8 <-as.data.frame(packets7)
    packets9 <-t(packets8)
    LogProbabilily <-ts(packets9)
    plot(LogProbabilily,xlab = "Samples",main = "Log-Probabilily")  
  })
  }
  else {
    isolate( if(input$Trainparameters == "3 State Custom parameters"){
      packets5 <- as.data.frame(featvector())
      packets6 <- lapply(names(packets5),  function(j){
        forwardbackward(setpars(depmix(packets5[[j]]~1,nstates=3, 
                                       data=packets5,family=gaussian(),
                                       ntimes = c(16,16,16,16)), getpars(trainedmod())))
      })
      
      packets7 <-lapply(1:length(packets6),  function(i){
        packets6[[i]][["logLike"]]
      })
      packets8 <-as.data.frame(packets7)
      packets9 <-t(packets8)
      LogProbabilily <-ts(packets9)
      plot(LogProbabilily,xlab = "Samples",main = "Log-Probabilily")
    } else  {
      packets5 <- as.data.frame(featvector())
      packets6 <- lapply(names(packets5),  function(j){
        forwardbackward(setpars(depmix(packets5[[j]]~1,nstates=input$states, 
                                       data=packets5,family=gaussian(),
                                       ntimes = c(16,16,16,16)), getpars(trainedmod())))
      })
      
      packets7 <-lapply(1:length(packets6),  function(i){
        packets6[[i]][["logLike"]]
      })
      packets8 <-as.data.frame(packets7)
      packets9 <-t(packets8)
      LogProbabilily <-ts(packets9)
      plot(LogProbabilily,xlab = "Samples",main = "Log-Probabilily")  
    })
  }
})

# Render Plot log-prob 
output$prognost <- renderPlot({
  if(is.null(input$action1)) {return()}
  prognost()
})

# Plot rms & kurtosis
rmsplot <- eventReactive(input$action, {
  
  isolate( if(input$common == "rms"){
    rms1 <- lapply(input$file$datapath, read.table, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    rms2 <- as.data.frame(rms1)
    rms3 <- reactiveValues()
    rms3 <- lapply(names(rms2),  function(i){
      sqrt(sum(rms2[i]^2)/length(rms2[i]))})
    rms <-ts(rms3)
    plot(rms,xlab = "Samples",main = "rms of samples") 
  } else{
    kurt1 <- lapply(input$file$datapath, read.table, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    kurt2 <- as.data.frame(kurt1)
    kurt3 <- reactiveValues()
    kurt3 <- lapply(names(kurt2),  function(i){
      kurtosis(kurt2[,i])})
    kurt <-ts(kurt3)
    plot(kurt,xlab = "Samples",main = "Kurtosis of samples") 
  })
})

# Render Plot rms & kurtosis
output$rms <- renderPlot({
  
  if(is.null(input$file)) {return()}
  else
    rmsplot()
})



# Step and tab trans
  observeEvent(input$Steps, {
    updateTabsetPanel(session, "inTabset",
                      selected = input$Steps )})
  # Step and tab trans
  observeEvent(input$tb , {
updateVarSelectInput(session, "Steps",
                      selected = input$tb)})

})