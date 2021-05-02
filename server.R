# BFP web application with WPD and CHMM

server <-(function(input,output, session) {
  
## 5. MainPanel tabset 
  output$tb <- renderUI({
    if (is.null(input$file)) {
      return()
    }
    else
      tabsetPanel(
        id = "inTabset",
        ##  3.a  Sample load, read & plot 
        tabPanel(
          "1. Samples load, read and plot",
          h4("1.1. Sample Data structure"),
          verbatimTextOutput("datastr"),
          h4("1.2. Sample Plot"),
          plotOutput("Amplitude"),
          h4("1.3. Sample view"),
          dataTableOutput("table1")
        ),
        ##  3.b  Wavelet and norm. energy sequence
        tabPanel(
          "2. Wavelet Analysis & Energy Sequence",
          h4("2.0. Sample Plot"),
          plotOutput("Amplitudew"),
          h4("2.1. Wavelet Analysis"),
          plotOutput("waveplot"),
          
          h4("2.2. Energy Sequence"),
          plotOutput("normeneseqplot")
        ),
        ##  3.c  Model training
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
        ##  3.d  Prognostic
        tabPanel(
          "4. Prognostics",
          h4("4.1. Prognostics"),
          plotOutput("prognost",click = "plot_click"),
          verbatimTextOutput("plot_clickinfo1"),
          h4("4.2 Common Methods"),
          plotOutput("rms",click = "plot_click"),
          verbatimTextOutput("plot_clickinfo")
        ),
        ##  3.e  References
        tabPanel(
          "5. References",
        
            mainPanel(
                      h4("[1] H. Ocak, K.A. Loparo, F.M. Discenzo, Online tracking of bearing wear using wavelet packet decomposition and 	probabilistic modeling: A method for bearing prognostics. Journal of Sound and Vibration 302 (2007) 951-961.
"),
                      h4("[2] H. Ocak, K.A. Loparo, A new bearing fault detection and diagnosis scheme based on Hidden Markov Modeling of vibration signals. Acoustics, Speech, and Signal Processing, 1988. ICASSP-88., 1988 International Conference on February 2001.
"),
                      h4("[3] R.H. Shumway, D.S. Stoffer, Time Series Analysis and Its Applications With R Examples, Fourth Edition. Springer International Publishing AG 1999, 2012, 2016, 2017.
"),
                      h4("[4] G.P. Nason, Wavelet Methods in Statistics with R. Springer Science+Business Media, LLC, 2008.
"),
                      h4("[5] W. Zucchini, I.L. MacDonald, R. Langrock, Hidden Markov Models for Time Series, An Introduction Using R, Second Edition. CRC Press, Taylor & Francis Group, Boca Raton, FL 33487-2742, 2016.
"),
                      h4("[6] C. Beeley, Web Application Development with R Using Shiny. Packt Publishing Ltd., Birmingham-Mumbai, 2013.
"),
                      h4("[7] R Core Team (2013). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL http://www.R-project.org/.
"),
                      h4("[8] Guy Nason (2016). wavethresh: Wavelets Statistics and Transforms. R package version 4.6.8. https://CRAN.R-project.org/package=wavethresh
"),
                      h4("[9] Ingmar Visser, Maarten Speekenbrink (2010). depmixS4: An R Package for Hidden Markov Models. Journal of Statistical Software, 36(7), 1-21. URL http://www.jstatsoft.org/v36/i07/.
"),
                      h4("[10] Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (2020). shiny: Web Application Framework for R. R package version 1.5.0. https://CRAN.R-project.org/package=shiny
"),
                      h4("[11] NSF I/UCRC Center for Intelligent Maintenance Systems, Prognostic data repository: Bearing data set, in http://ti.arc.nasa.gov/tech/dash/pcoe/prognostic-data-repository/, online in 2010.")
            )
          
        )
      )
  })
  
  ## 5.a Select sample to read
  output$selectfile <- renderUI({
    if (is.null(input$file)) {
      return()
    }
    list(
      hr(),
      helpText("Select loaded sample:"),
      selectInput("Select", "Select", choices = input$file$name)
    )
  })
  
  ## 5.b Read selected sample from selectfile with colsel
   samplered <- reactive({
    if (is.null(input$file)) {
      return ()
    }
    
     read.table(
       file = input$file$datapath[input$file$name == input$Select],
       sep = input$sep,
       header = input$header, skip=input$rowskip
     )[,input$colsel]
    
  })
   
   ## 5.c Read selected sample from selectfile without colsel
   samplered1 <- reactive({
     if (is.null(input$file)) {
       return ()
     }
     
     read.table(
       file = input$file$datapath[input$file$name == input$Select],
       sep = input$sep,
       header = input$header, skip=input$rowskip
     )
     
   })
   
   ## 5.d Read selected sample from selectfile1 without colsel
   samplered2 <- reactive({
     if (is.null(input$file)) {
       return ()
     }
     
     read.table(
       file = input$file$datapath[input$file$name == input$Select1],
       sep = input$sep,
       header = input$header, skip=input$rowskip
     )
     
   })
   
   ## 5.e Read selected sample from selectfile1 with colsel
   samplered3 <- reactive({
     if (is.null(input$file)) {
       return ()
     }
     
     samred <-read.table(
       file = input$file$datapath[input$file$name == input$Select1],
       sep = input$sep,
       header = input$header, skip=input$rowskip
     )
     samred1 <- as.data.frame(samred)
     samred1[,input$colsel]
   })
   
   ## 5.f Read merged samples with colsel
   mersamplered <- reactive({
     if (is.null(input$file)) {
       return ()
     }
     
   
    msred <- lapply(input$file$datapath, read.table, sep=input$sep,skip=input$rowskip, header = input$header)
     
    msred1 <- as.data.frame(msred) 
   })
   
   ## TEST
   filelist <- reactive({
   if (is.null(input$file)) {
     return ()
   }
   
   filelist1 <- as.list(input$file)
   filelist2  <- as.list(filelist1$name)
   filelist3 <- seq(1,(length(filelist1$name)))
   filelist4 <- cbind(filelist3,filelist2)
   filelist5 <- as.data.frame(filelist4)
   
})

   output$plot_clickinfo <- reactive({
     if (is.null(input$plot_click$x))
       return()
     clickinfo<-as.data.frame(filelist()) 
     cbind(round(input$plot_click$x,0),clickinfo[round(input$plot_click$x,0),2])
   
   })
   output$plot_clickinfo1 <- reactive({
     if (is.null(input$plot_click$x))
       return()
     clickinfo<-as.data.frame(filelist()) 
     cbind(round(input$plot_click$x,0),clickinfo[round(input$plot_click$x,0),2])
    
   })
   
   ## 5.g Read merged train samples with colsel
   mertrasamplered <- reactive({
     if (is.null(input$file1)) {
       return ()
     }
     
     
     msred <- lapply(input$file1$datapath, read.table, sep=input$sep, 
                     skip=input$rowskip, header = input$header)
     
     msred1 <- as.data.frame(msred) 
   })
    
    ## 5.1.a Loaded sample list
    loadedvectlist <- reactive({
      
      lenloadedvectlist <- as.data.frame(mersamplered())
      epochs2 <- as.data.frame(mersamplered())
      seq1 <- as.vector(seq(input$colsel,length(lenloadedvectlist),ncol(samplered1())))
      epochs3 <- lapply(seq1,function(i){
        cbind(epochs2[[i]])
      })

      epochs4 <- as.vector(epochs3)
    
    })

    ## 5.1.b Loaded train sample list
    loadedtrainvectlist <- reactive({
  
      lenloadedvectlist <- as.data.frame(mertrasamplered())
      epochs2 <- as.data.frame(mersamplered())
      seq1 <- as.vector(seq(input$colsel,length(lenloadedvectlist),ncol(samplered1())))
      epochs3 <- lapply(seq1,function(i){
        cbind(epochs2[[i]])
      })
      
      epochs4 <- as.vector(epochs3)
      
    })
    
    ## 3.a.1. Samples load, read and plot
    ## 3.a.1.1. Sample Data structure
    output$datastr <- renderPrint({
      if (is.null(input$file)) {
        return ()
      }
      str(
        samplered1()
      )
    })
    
    ## 3.a.1.2. Sample Plot
    output$Amplitude <- renderPlot({
      if (is.null(input$file)) {
        return ()
      }
      Amplitude1 <-
        samplered()
      Amplitude <- ts(Amplitude1,  start = 0, frequency = input$obs)
      plot(Amplitude, main = "Amplitude of sample", col="blue")
    })
    
    ## 3.a.1.3. Sample view
    output$table1 <- renderDataTable({
      if (is.null(input$file)) {return ()}
      samplered1()
    })
    
  ## 3.b.2. Wavelet Analysis & Energy Sequence
  ## 3.b.2.1.a Select sample to plot wavelet
    output$selectfile1 <- renderUI({
      if(is.null(input$file)) {return()}
      list(hr(), 
           helpText("Select sample to see WPD"),
           selectInput("Select1", "Select", choices=input$file$name)
      )})
  
    ## 3.a.1.2. Sample Plot
    output$Amplitudew <- renderPlot({
      if (is.null(input$file)) {return ()}
      if (input$Wavesize == "256") {
      Amplitude1 <-
        samplered3()
      Amplitude <- ts(Amplitude1)
      plot(Amplitude, main = "Amplitude of sample", col="blue")
      abline(v=seq(256,2048,256), col="gray", lty=c("55") )
      }
      else if (input$Wavesize == "512") {
        Amplitude1 <-
          samplered3()
        Amplitude <- ts(Amplitude1)
        plot(Amplitude, main = "Amplitude of sample", col="blue")
        abline(v=seq(512,2048,512), col="gray", lty=c("55") )
      }
      else if (input$Wavesize == "1024") {
        Amplitude1 <-
          samplered3()
        Amplitude <- ts(Amplitude1)
        plot(Amplitude, main = "Amplitude of sample", col="blue")
        abline(v=seq(1024,2048,1024), col="gray", lty=c("55") )
      }
    })
      
  ## 3.b.2.1. Wavelet Analysis plot
  output$waveplot <- renderPlot({
    if (is.null(input$Select1)) {return ()}
    
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
  
  ## 3.b.2.a. normilized average-energy sequence
  normeneseq <- reactive({
    if (input$Wavesize == "256") {
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
    }
    else if (input$Wavesize == "512"){
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
    }
    else {
      packets2 <- as.data.frame(samplered3())
      packets3 <- reactiveValues()
      packets3 <- lapply(names(packets2),  function(i){
        
        e.1.p.1<- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=0))
        e.1.p.2 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=1))
        e.1.p.3 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=2))
        e.1.p.4 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=3))
        e.1.p.5 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=4))
        e.1.p.6 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=5))
        e.1.p.7 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=6))
        e.1.p.8 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=7))
        e.1.p.9 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=8)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=8))
        e.1.p.10 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=9)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                           family=input$family),level=5, index=9))
        e.1.p.11 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=10)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=10))
        e.1.p.12 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=11)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=11))
        e.1.p.13 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=12)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=12))
        e.1.p.14 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=13)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=13))
        e.1.p.15 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=14)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=14))
        e.1.p.16 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=15)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=15))
        e.1.p.17<- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=16)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                           family=input$family),level=5, index=16))
        e.1.p.18 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=17)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=17))
        e.1.p.19 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=18)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=18))
        e.1.p.20 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=19)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=19))
        e.1.p.21 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=20)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=20))
        e.1.p.22 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=21)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=21))
        e.1.p.23 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=22)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=22))
        e.1.p.24 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=23)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=23))
        e.1.p.25 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=24)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=24))
        e.1.p.26 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=25)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=25))
        e.1.p.27 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=26)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=26))
        e.1.p.28 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=27)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=27))
        e.1.p.29 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=28)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=28))
        e.1.p.30 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=29)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=29))
        e.1.p.31 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=30)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=30))
        e.1.p.32 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=31)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=31))
        
        a1<-rbind(e.1.p.1, e.1.p.2, e.1.p.3, e.1.p.4, e.1.p.5, e.1.p.6, e.1.p.7, e.1.p.8, e.1.p.9, e.1.p.10, e.1.p.11, e.1.p.12,
                  e.1.p.13, e.1.p.14, e.1.p.15, e.1.p.16,e.1.p.17, e.1.p.18, e.1.p.19, e.1.p.20, e.1.p.21, e.1.p.22, e.1.p.23, 
                  e.1.p.24, e.1.p.25, e.1.p.26, e.1.p.27, e.1.p.28,e.1.p.29, e.1.p.30, e.1.p.31, e.1.p.32)
        
        b1<- (a1-min(a1))/(max(a1)-min(a1)) 
        
        e.2.p.1<- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=0))
        e.2.p.2 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=1))
        e.2.p.3 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=2))
        e.2.p.4 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=3))
        e.2.p.5 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=4))
        e.2.p.6 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=5))
        e.2.p.7 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=6))
        e.2.p.8 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=7))
        e.2.p.9 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=8)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=8))
        e.2.p.10 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=9)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                           family=input$family),level=5, index=9))
        e.2.p.11 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=10)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=10))
        e.2.p.12 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=11)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=11))
        e.2.p.13 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=12)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=12))
        e.2.p.14 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=13)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=13))
        e.2.p.15 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=14)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=14))
        e.2.p.16 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=15)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=15))
        e.2.p.17<- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=16)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                           family=input$family),level=5, index=16))
        e.2.p.18 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=17)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=17))
        e.2.p.19 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=18)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=18))
        e.2.p.20 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=19)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=19))
        e.2.p.21 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=20)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=20))
        e.2.p.22 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=21)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=21))
        e.2.p.23 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=22)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=22))
        e.2.p.24 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=23)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=23))
        e.2.p.25 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=24)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=24))
        e.2.p.26 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=25)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=25))
        e.2.p.27 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=26)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=26))
        e.2.p.28 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=27)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=27))
        e.2.p.29 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=28)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=28))
        e.2.p.30 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=29)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=29))
        e.2.p.31 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=30)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=30))
        e.2.p.32 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=31)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=31))
        
        a2<-rbind(e.2.p.1, e.2.p.2, e.2.p.3, e.2.p.4, e.2.p.5, e.2.p.6, e.2.p.7, e.2.p.8, e.2.p.9, e.2.p.10, e.2.p.11, e.2.p.12,
                  e.2.p.13, e.2.p.14, e.2.p.15, e.2.p.16,e.2.p.17, e.2.p.18, e.2.p.19, e.2.p.20, e.2.p.21, e.2.p.22, e.2.p.23, 
                  e.2.p.24, e.2.p.25, e.2.p.26, e.2.p.27, e.2.p.28,e.2.p.29, e.2.p.30, e.2.p.31, e.2.p.32)
        
        b2<- (a2-min(a2))/(max(a2)-min(a2)) 
        
        rbind(b1, b2)
      })
    }
  })
  
  ## 3.b.2.2. Plot normilized average-energy sequence
  normeneseqplot <- reactive({
    if(is.null(input$file)) {return()}
    if (input$Wavesize == "256") {
     
      packets2 <- as.data.frame(samplered3())
      packets3 <- as.data.frame(normeneseq())
      names(packets3)<- c(names(packets2))
      packets4 <- as.data.frame(packets3)
      packets5 <- lapply(names(packets4),  function(i){
        t(packets4[[i]])
      })
      packets6 <- as.data.frame(packets5)
      packets7 <- t(packets6)
      sampleseq <- ts(packets7)
      
      plot(sampleseq, xlab = "Sequence",main = "Sample Sequence",
           pch = 20, cex = 1.2, col="blue", type="b")
      abline(v=seq(8,64,8), col="gray", lty=c("55") )
    }
    else if (input$Wavesize == "512") {
      
      packets2 <- as.data.frame(samplered3())
      packets3 <- as.data.frame(normeneseq())
      names(packets3)<- c(names(packets2))
      packets4 <- as.data.frame(packets3)
    
    packets5 <- lapply(names(packets4),  function(i){
      t(packets4[[i]])
    })
    packets6 <- as.data.frame(packets5)
    packets7 <- t(packets6)
    sampleseq <- ts(packets7)
    plot(sampleseq, xlab = "Sequence",main = "Sample Sequence",
         pch = 20, cex = 1.2, col="blue", type="b")
    abline(v=seq(16,64,16), col="gray", lty=c("55") )
    }
    else if (input$Wavesize == "1024") {
      
      packets2 <- as.data.frame(samplered3())
      packets3 <- as.data.frame(normeneseq())
      names(packets3)<- c(names(packets2))
      packets4 <- as.data.frame(packets3)
      
      packets5 <- lapply(names(packets4),  function(i){
        t(packets4[[i]])
      })
      packets6 <- as.data.frame(packets5)
      packets7 <- t(packets6)
      sampleseq <- ts(packets7)
      plot(sampleseq, xlab = "Sequence",main = "Sample Sequence",
           pch = 20, cex = 1.2, col="blue", type="b")
      abline(v=seq(32,64,32), col="gray", lty=c("55") )
    }
    
  })
  
  ## 3.b.2.2.a Render Plot normilized average-energy sequence
   output$normeneseqplot <- renderPlot({
      normeneseqplot()
  })
  

    # 3.c.3 CHMM Trainning

   ## 3.c.3.a. Normilized average-energy Trainning Sequence
   normenetraseq <- reactive({
     if (input$Wavesize == "256") {
       packets2 <- as.data.frame(loadedtrainvectlist())
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
     }
     else if (input$Wavesize == "512"){
       packets2 <- as.data.frame(loadedtrainvectlist())
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
     }
     else {
       packets2 <- as.data.frame(loadedtrainvectlist())
       packets3 <- reactiveValues()
       packets3 <- lapply(names(packets2),  function(i){
         
         e.1.p.1<- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=0))
         e.1.p.2 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                           family=input$family),level=5, index=1))
         e.1.p.3 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                           family=input$family),level=5, index=2))
         e.1.p.4 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                           family=input$family),level=5, index=3))
         e.1.p.5 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                           family=input$family),level=5, index=4))
         e.1.p.6 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                           family=input$family),level=5, index=5))
         e.1.p.7 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                           family=input$family),level=5, index=6))
         e.1.p.8 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                           family=input$family),level=5, index=7))
         e.1.p.9 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=8)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                           family=input$family),level=5, index=8))
         e.1.p.10 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=9)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=9))
         e.1.p.11 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=10)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=10))
         e.1.p.12 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=11)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=11))
         e.1.p.13 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=12)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=12))
         e.1.p.14 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=13)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=13))
         e.1.p.15 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=14)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=14))
         e.1.p.16 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=15)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=15))
         e.1.p.17<- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=16)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=16))
         e.1.p.18 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=17)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=17))
         e.1.p.19 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=18)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=18))
         e.1.p.20 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=19)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=19))
         e.1.p.21 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=20)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=20))
         e.1.p.22 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=21)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=21))
         e.1.p.23 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=22)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=22))
         e.1.p.24 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=23)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=23))
         e.1.p.25 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=24)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=24))
         e.1.p.26 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=25)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=25))
         e.1.p.27 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=26)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=26))
         e.1.p.28 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=27)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=27))
         e.1.p.29 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=28)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=28))
         e.1.p.30 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=29)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=29))
         e.1.p.31 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=30)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=30))
         e.1.p.32 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=31)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=31))
         
         a1<-rbind(e.1.p.1, e.1.p.2, e.1.p.3, e.1.p.4, e.1.p.5, e.1.p.6, e.1.p.7, e.1.p.8, e.1.p.9, e.1.p.10, e.1.p.11, e.1.p.12,
                   e.1.p.13, e.1.p.14, e.1.p.15, e.1.p.16,e.1.p.17, e.1.p.18, e.1.p.19, e.1.p.20, e.1.p.21, e.1.p.22, e.1.p.23, 
                   e.1.p.24, e.1.p.25, e.1.p.26, e.1.p.27, e.1.p.28,e.1.p.29, e.1.p.30, e.1.p.31, e.1.p.32)
         
         b1<- (a1-min(a1))/(max(a1)-min(a1)) 
         
         e.2.p.1<- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                 level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=0))
         e.2.p.2 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                           family=input$family),level=5, index=1))
         e.2.p.3 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                           family=input$family),level=5, index=2))
         e.2.p.4 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                           family=input$family),level=5, index=3))
         e.2.p.5 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                           family=input$family),level=5, index=4))
         e.2.p.6 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                           family=input$family),level=5, index=5))
         e.2.p.7 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                           family=input$family),level=5, index=6))
         e.2.p.8 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                           family=input$family),level=5, index=7))
         e.2.p.9 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=8)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                           family=input$family),level=5, index=8))
         e.2.p.10 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=9)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=9))
         e.2.p.11 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=10)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=10))
         e.2.p.12 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=11)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=11))
         e.2.p.13 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=12)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=12))
         e.2.p.14 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=13)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=13))
         e.2.p.15 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=14)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=14))
         e.2.p.16 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=15)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=15))
         e.2.p.17<- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                  level=5, index=16)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                            family=input$family),level=5, index=16))
         e.2.p.18 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=17)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=17))
         e.2.p.19 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=18)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=18))
         e.2.p.20 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=19)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=19))
         e.2.p.21 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=20)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=20))
         e.2.p.22 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=21)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=21))
         e.2.p.23 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=22)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=22))
         e.2.p.24 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=23)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=23))
         e.2.p.25 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=24)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=24))
         e.2.p.26 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=25)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=25))
         e.2.p.27 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=26)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=26))
         e.2.p.28 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=27)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=27))
         e.2.p.29 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=28)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=28))
         e.2.p.30 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=29)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=29))
         e.2.p.31 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=30)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=30))
         e.2.p.32 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                   level=5, index=31)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                             family=input$family),level=5, index=31))
         
         a2<-rbind(e.2.p.1, e.2.p.2, e.2.p.3, e.2.p.4, e.2.p.5, e.2.p.6, e.2.p.7, e.2.p.8, e.2.p.9, e.2.p.10, e.2.p.11, e.2.p.12,
                   e.2.p.13, e.2.p.14, e.2.p.15, e.2.p.16,e.2.p.17, e.2.p.18, e.2.p.19, e.2.p.20, e.2.p.21, e.2.p.22, e.2.p.23, 
                   e.2.p.24, e.2.p.25, e.2.p.26, e.2.p.27, e.2.p.28,e.2.p.29, e.2.p.30, e.2.p.31, e.2.p.32)
         
         b2<- (a2-min(a2))/(max(a2)-min(a2)) 
         
         rbind(b1, b2)
       })
     }
   })
   
  ## 3.c.3.1 Plot normilized average-energy Trainning Sequence
  trainningplot <- reactive({
    if(is.null(input$file1)) {return()}
    if (input$Wavesize == "256") {
      packets2 <- as.data.frame(loadedtrainvectlist())
      packets3 <- as.data.frame(normenetraseq())
     
    names(packets3)<- c(names(packets2))
    packets4 <- as.data.frame(packets3)
    
    packets5 <- lapply(names(packets4),  function(i){
      t(packets4[[i]])
    })
    packets6 <- as.data.frame(packets5)
    packets7 <- t(packets6)
    trainseq <- ts(packets7)
    plot(trainseq, xlab = "Sequence",main = "Sample Sequence",
         pch = 20, cex = 1.2, col="blue", type="b")
    abline(v=seq(8,(length(jointrainsamp())/ncol(samplered1()))*64,8), col="gray", lty=c("55") )
  }
    if (input$Wavesize == "512") {
      
      packets2 <- as.data.frame(loadedtrainvectlist())
      packets3 <- as.data.frame(normenetraseq())
      names(packets3)<- c(names(packets2))
      packets4 <- as.data.frame(packets3)
      
      packets5 <- lapply(names(packets4),  function(i){
        t(packets4[[i]])
      })
      packets6 <- as.data.frame(packets5)
      packets7 <- t(packets6)
      trainseq <- ts(packets7)
      plot(trainseq, xlab = "Sequence",main = "Sample Sequence",
           pch = 20, cex = 1.2, col="blue", type="b")
      abline(v=seq(16,(length(jointrainsamp())/ncol(samplered1()))*64,16), col="gray", lty=c("55") )
    }
    if (input$Wavesize == "1024") {
      
      packets2 <- as.data.frame(loadedtrainvectlist())
      packets3 <- as.data.frame(normenetraseq())
      names(packets3)<- c(names(packets2))
      packets4 <- as.data.frame(packets3)
      
      packets5 <- lapply(names(packets4),  function(i){
        t(packets4[[i]])
      })
      packets6 <- as.data.frame(packets5)
      packets7 <- t(packets6)
      trainseq <- ts(packets7)
      plot(trainseq, xlab = "Sequence",main = "Sample Sequence",
           pch = 20, cex = 1.2, col="blue", type="b")
      abline(v=seq(32,(length(jointrainsamp())/ncol(samplered1()))*64,32), col="gray", lty=c("55") )
    }
  })
  
  ### 3.c.3.1.a Render Plot Normilized average-energy Trainning Sequence
  output$trainningplot <- renderPlot({
    trainningplot()
  })
  
  # training sample length for ntimes
  jointrainsamp <- reactive({
    epochs1 <- lapply(input$file1$datapath, read.table, sep=input$sep, 
                      skip=input$rowskip, header = input$header)
    epochs2 <- as.data.frame(epochs1)
  })
  
  # HMM Model ntime  
  ntimelength <- eventReactive(input$action2,{
    if (input$Wavesize == "256") {
    
      c(rep(8,(length(jointrainsamp())/ncol(samplered1()))*8))
    }
    else if (input$Wavesize == "512") {
      c(rep(16,(length(jointrainsamp())/ncol(samplered1()))*4))
    }
    else {
      
      c(rep(32,(length(jointrainsamp())/ncol(samplered1()))*2))
    }
 
  })
  
  # train seq
  trainningdata <- eventReactive(input$action2,{
  if (input$Wavesize == "256"){
    if(is.null(input$file1)) {return()}
    
    packets2 <- as.data.frame(loadedtrainvectlist())
    packets3 <- as.data.frame(normenetraseq())
    names(packets3)<- c(names(packets2))
    packets4 <- as.data.frame(packets3)
    
    packets5 <- lapply(names(packets4),  function(i){
      t(packets4[[i]])
    })
    packets6 <- as.data.frame(packets5)
    packets7 <- t(packets6)
    
    
  }
    else if (input$Wavesize == "512"){
    
    packets2 <- as.data.frame(loadedtrainvectlist())
    packets3 <- as.data.frame(normenetraseq())
    names(packets3)<- c(names(packets2))
    packets4 <- as.data.frame(packets3)
    packets5 <- lapply(names(packets4),  function(i){
      t(packets4[[i]])})
    packets6 <- as.data.frame(packets5)
    packets7 <- t(packets6)
  }
    else if (input$Wavesize == "1024"){
      
      packets2 <- as.data.frame(loadedtrainvectlist())
      packets3 <- as.data.frame(normenetraseq())
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
                       ntimes = ntimelength())
      
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
                       ntimes = ntimelength())
      
      fm1.3 <- fit(mod1.4)  
    }) 
    
    
  })
  
  # HMM trained state prob
  output$init <- renderTable({
    if(is.null(input$file1)) {return()}
    if(input$Trainparameters == "3 State Custom parameters" | 
       input$states == 3){
      init <-data.frame("S1"=getpars(trainedmod())[1], 
                        "S2"=getpars(trainedmod())[2], 
                        "S3"=getpars(trainedmod())[3])
      as.data.frame(init)
    }})
  
  # HMM trained trans matrix
  output$fromS1 <- renderTable({
    if(is.null(input$file1)) {return()}
    if(input$Trainparameters == "3 State Custom parameters" | 
       input$states == 3){
      fromS1 <-data.frame("S1S1"=getpars(trainedmod())[4], 
                          "S1S2"=getpars(trainedmod())[5], 
                          "S1S3"=getpars(trainedmod())[6])
      as.data.frame(fromS1)
    }})
  # HMM trained trans matrix
  output$fromS2 <- renderTable({
    if(is.null(input$file1)) {return()}
    if(input$Trainparameters == "3 State Custom parameters" | 
       input$states == 3){
      fromS2 <-data.frame("S2S1"=getpars(trainedmod())[7], 
                          "S2S2"=getpars(trainedmod())[8], 
                          "S2S3"=getpars(trainedmod())[9])
      as.data.frame(fromS2)
    }})
  # HMM trained trans matrix
  output$fromS3 <- renderTable({
    if(is.null(input$file1)) {return()}
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
    packets2 <- as.data.frame(loadedtrainvectlist())
    packets3 <- as.data.frame(normenetraseq())
    names(packets3)<- c(names(packets2))
    packets4 <- as.data.frame(packets3)
    
    packets5 <- lapply(names(packets4),  function(i){
      t(packets4[[i]])
    })
    packets6 <- as.data.frame(packets5)
    packets7 <- t(packets6)
    trainseq <- ts(packets7)
    plot(trainseq, xlab = "Sequence",main = "Trainning Sequence with states",
    pch = 20, cex = 1.2, col="blue")
    abline(v=seq(8,(length(jointrainsamp())/ncol(samplered1()))*64,8), col="gray", lty=c("55") )
    text(trainseq, col=trainedmod()@posterior[,1], labels=trainedmod()@posterior[,1])
    }
    else if (input$Wavesize == "512"){
      
      packets2 <- as.data.frame(loadedtrainvectlist())
      packets3 <- as.data.frame(normenetraseq())
      names(packets3)<- c(names(packets2))
      packets4 <- as.data.frame(packets3)
      
      packets5 <- lapply(names(packets4),  function(i){
        t(packets4[[i]])
      })
      packets6 <- as.data.frame(packets5)
      packets7 <- t(packets6)
      trainseq <- ts(packets7)
      plot(trainseq, xlab = "Sequence",main = "Trainning Sequence with states",
      pch = 20, cex = 1.2, col="blue")
      abline(v=seq(16,(length(jointrainsamp())/ncol(samplered1()))*64,16), col="gray", lty=c("55") )
      text(trainseq, col=trainedmod()@posterior[,1], labels=trainedmod()@posterior[,1])
      
    }
    else if (input$Wavesize == "1024") {
      
      packets2 <- as.data.frame(loadedtrainvectlist())
      packets3 <- as.data.frame(normenetraseq())
      names(packets3)<- c(names(packets2))
      packets4 <- as.data.frame(packets3)
      
      packets5 <- lapply(names(packets4),  function(i){
        t(packets4[[i]])
      })
      packets6 <- as.data.frame(packets5)
      packets7 <- t(packets6)
      trainseq <- ts(packets7)
      plot(trainseq, xlab = "Sequence",main = "Trainning Sequence with states",
           pch = 20, cex = 1.2, col="blue")
      abline(v=seq(16,(length(jointrainsamp())/ncol(samplered1()))*64,16), col="gray", lty=c("55") )
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
  packets2 <- as.data.frame(loadedvectlist())
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
  else if (input$Wavesize == "512") {
    
    packets2 <- as.data.frame(loadedvectlist())
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
  else if (input$Wavesize == "1024") {
    
    packets2 <- as.data.frame(loadedvectlist())
    packets3 <- reactiveValues()
    packets3 <- lapply(names(packets2),  function(i){
      
      e.1.p.1<- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                              level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                       family=input$family),level=5, index=0))
      e.1.p.2 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=1))
      e.1.p.3 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=2))
      e.1.p.4 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=3))
      e.1.p.5 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=4))
      e.1.p.6 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=5))
      e.1.p.7 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=6))
      e.1.p.8 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=7))
      e.1.p.9 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=8)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=8))
      e.1.p.10 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=9)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=9))
      e.1.p.11 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=10)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=10))
      e.1.p.12 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=11)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=11))
      e.1.p.13 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=12)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=12))
      e.1.p.14 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=13)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=13))
      e.1.p.15 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=14)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=14))
      e.1.p.16 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=15)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=15))
      e.1.p.17<- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                               level=5, index=16)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=16))
      e.1.p.18 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=17)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=17))
      e.1.p.19 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=18)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=18))
      e.1.p.20 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=19)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=19))
      e.1.p.21 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=20)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=20))
      e.1.p.22 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=21)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=21))
      e.1.p.23 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=22)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=22))
      e.1.p.24 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=23)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=23))
      e.1.p.25 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=24)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=24))
      e.1.p.26 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=25)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=25))
      e.1.p.27 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=26)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=26))
      e.1.p.28 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=27)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=27))
      e.1.p.29 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=28)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=28))
      e.1.p.30 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=29)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=29))
      e.1.p.31 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=30)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=30))
      e.1.p.32 <- sum(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, family=input$family),
                                level=5, index=31)^2)/length(getpacket(wp(packets2[[i]][1:1024], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=31))
      
      a1<-rbind(e.1.p.1, e.1.p.2, e.1.p.3, e.1.p.4, e.1.p.5, e.1.p.6, e.1.p.7, e.1.p.8, e.1.p.9, e.1.p.10, e.1.p.11, e.1.p.12,
                e.1.p.13, e.1.p.14, e.1.p.15, e.1.p.16,e.1.p.17, e.1.p.18, e.1.p.19, e.1.p.20, e.1.p.21, e.1.p.22, e.1.p.23, 
                e.1.p.24, e.1.p.25, e.1.p.26, e.1.p.27, e.1.p.28,e.1.p.29, e.1.p.30, e.1.p.31, e.1.p.32)
      
      b1<- (a1-min(a1))/(max(a1)-min(a1)) 
      
      e.2.p.1<- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                              level=5, index=0)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                       family=input$family),level=5, index=0))
      e.2.p.2 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=1)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=1))
      e.2.p.3 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=2)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=2))
      e.2.p.4 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=3)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=3))
      e.2.p.5 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=4)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=4))
      e.2.p.6 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=5)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=5))
      e.2.p.7 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=6)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=6))
      e.2.p.8 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=7)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=7))
      e.2.p.9 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=8)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                        family=input$family),level=5, index=8))
      e.2.p.10 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=9)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=9))
      e.2.p.11 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=10)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=10))
      e.2.p.12 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=11)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=11))
      e.2.p.13 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=12)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=12))
      e.2.p.14 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=13)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=13))
      e.2.p.15 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=14)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=14))
      e.2.p.16 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=15)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=15))
      e.2.p.17<- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                               level=5, index=16)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                         family=input$family),level=5, index=16))
      e.2.p.18 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=17)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=17))
      e.2.p.19 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=18)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=18))
      e.2.p.20 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=19)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=19))
      e.2.p.21 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=20)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=20))
      e.2.p.22 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=21)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=21))
      e.2.p.23 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=22)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=22))
      e.2.p.24 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=23)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=23))
      e.2.p.25 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=24)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=24))
      e.2.p.26 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=25)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=25))
      e.2.p.27 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=26)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=26))
      e.2.p.28 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=27)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=27))
      e.2.p.29 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=28)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=28))
      e.2.p.30 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=29)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=29))
      e.2.p.31 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=30)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=30))
      e.2.p.32 <- sum(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, family=input$family),
                                level=5, index=31)^2)/length(getpacket(wp(packets2[[i]][1025:2048], filter.number=input$wavefilter, 
                                                                          family=input$family),level=5, index=31))
      
      a2<-rbind(e.2.p.1, e.2.p.2, e.2.p.3, e.2.p.4, e.2.p.5, e.2.p.6, e.2.p.7, e.2.p.8, e.2.p.9, e.2.p.10, e.2.p.11, e.2.p.12,
                e.2.p.13, e.2.p.14, e.2.p.15, e.2.p.16,e.2.p.17, e.2.p.18, e.2.p.19, e.2.p.20, e.2.p.21, e.2.p.22, e.2.p.23, 
                e.2.p.24, e.2.p.25, e.2.p.26, e.2.p.27, e.2.p.28,e.2.p.29, e.2.p.30, e.2.p.31, e.2.p.32)
      
      b2<- (a2-min(a2))/(max(a2)-min(a2)) 
      
      rbind(b1, b2)
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
  plot(LogProbabilily,xlab = "Samples",main = "Log-Probabilily",
       pch = 20, cex = 1.2, col="blue", type="b")
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
    plot(LogProbabilily,xlab = "Samples",main = "Log-Probabilily",
         pch = 20, cex = 1.2, col="blue", type="b")  
  })
  }
  else if (input$Wavesize == "512") {
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
      plot(LogProbabilily,xlab = "Samples",main = "Log-Probabilily",
           pch = 20, cex = 1.2, col="blue", type="b")
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
      plot(LogProbabilily,xlab = "Samples",main = "Log-Probabilily",
           pch = 20, cex = 1.2, col="blue", type="b")  
    })
  }
  else {
    isolate( if(input$Trainparameters == "3 State Custom parameters"){
      packets5 <- as.data.frame(featvector())
      packets6 <- lapply(names(packets5),  function(j){
        forwardbackward(setpars(depmix(packets5[[j]]~1,nstates=3, 
                                       data=packets5,family=gaussian(),
                                       ntimes = c(32,32)), getpars(trainedmod())))
      })
      
      packets7 <-lapply(1:length(packets6),  function(i){
        packets6[[i]][["logLike"]]
      })
      packets8 <-as.data.frame(packets7)
      packets9 <-t(packets8)
      LogProbabilily <-ts(packets9)
      plot(LogProbabilily,xlab = "Samples",main = "Log-Probabilily",
           pch = 20, cex = 1.2, col="blue", type="b")
    } else  {
      packets5 <- as.data.frame(featvector())
      packets6 <- lapply(names(packets5),  function(j){
        forwardbackward(setpars(depmix(packets5[[j]]~1,nstates=input$states, 
                                       data=packets5,family=gaussian(),
                                       ntimes = c(32,32)), getpars(trainedmod())))
      })
      
      packets7 <-lapply(1:length(packets6),  function(i){
        packets6[[i]][["logLike"]]
      })
      packets8 <-as.data.frame(packets7)
      packets9 <-t(packets8)
      LogProbabilily <-ts(packets9)
      plot(LogProbabilily,xlab = "Samples",main = "Log-Probabilily",
           pch = 20, cex = 1.2, col="blue", type="b")  
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
      rms2 <- as.data.frame(loadedvectlist())
    rms3 <- reactiveValues()
    rms3 <- lapply(names(rms2),  function(i){
      sqrt(sum(rms2[i]^2)/length(rms2[i]))})
    rms <-ts(rms3)
    plot(rms,xlab = "Samples",main = "rms of samples",
         pch = 20, cex = 1.2, col="blue", type="b")
   
  } else{
      kurt2 <- as.data.frame(loadedvectlist())
    kurt3 <- reactiveValues()
    kurt3 <- lapply(names(kurt2),  function(i){
      kurtosis(kurt2[,i])})
    kurt <-ts(kurt3)
    plot(kurt,xlab = "Samples",main = "Kurtosis of samples",
         pch = 20, cex = 1.2, col="blue", type="b") 
    
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