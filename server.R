source("F1M.R") #kontrola jakosci
source("filtracja2.R") # filtracja
source("Random_Forest.R") # klasyfikacja
source("biblioteki.R") # biblioteki

options(shiny.maxRequestSize = 500*1024^2)
shinyServer(function(input, output, session){
  
  ## input data 
  myData <- reactive({
    inFile <- input$file_ES
    if (is.null(inFile))
      return(NULL)
    newPath <- file.path(dirname(inFile$datapath), inFile$name)
    file.rename(inFile$datapath, newPath)
    load(newPath)
    var.name<-as.name(load(newPath))
    eval(var.name)
  })
  
  ## measure
  rangeMax <- reactive({
    if (!is.null(myData())) { 
      myDataExp <- exprs(myData())
      maxData <- dim(myDataExp)[2]
    }
  })
  
  # Filtracja
  dataFiltered <- reactive({
     if (!is.null(myData())) {
        if (input$filtration == "ON") {
          if (input$filtration_type == "PRZEPUSTOWA") {
            filtered <- filtracja2(myData(), input$cutoff, input$mode)
          } else if (input$filtration_type == "TEST T") {
            
          }
        } 
        else if (input$filtration == "OFF") {
            filtered <- myData()
        }
     }
  })
  
  
  observe({
    if (!is.null(myData())) { 
      rMax <- rangeMax()
      updateSliderInput(session, "range", value = c(1,rMax), min = 1, max = rMax, step = 1)
    }
  })
  
  ######################################### Raw data ############################################ 
  
  output$dataMessage <- renderUI ({
    if (is.null(dataFiltered())) { 
      br()
      h3("Proszę wrzucić plik ExpressionSet")
    }
  })
  
  output$abatch <- renderTable({
    if (!is.null(myData())) {
     # withProgress( message="Trwa wczytywanie danych..", {
        exprs(dataFiltered())
        #Sys.sleep(25)
    #})
    }
  }, include.rownames=TRUE)
  
  
  ######################################### Results ############################################  
  
  observe ({
    if (!is.null(dataFiltered())) {
      myDataExp <- exprs(dataFiltered())
      
  ################################### Quality ###################################################
        
        if (input$menu == "quality") {
            observeEvent(input$start, {
            output$plots <- renderPlot({
              if (input$plot_type == "HEXBIN") {
                HB(myDataExp, input$range[1]:input$range[2],input$scale)
              }
              else if (input$plot_type == "MVA"){
                MVA(myDataExp, c(input$range[1]:input$range[2]),input$scale)
              }
              else if (input$plot_type == "BOXPLOT") {
                BOXY(dataFiltered(), c(input$range[1]:input$range[2]),input$scale)
              }
              else if (input$plot_type == "SCATTER PLOT") {
                SCATTER(myDataExp, c(input$range[1]:input$range[2]),input$scale)
              }
              else if (input$plot_type == "HEATMAP") {
                HM(myDataExp,c(input$range[1]:input$range[2]),input$order)
              }
              else if (input$plot_type == "MA PLOT") {
                MAPLOTS(dataFiltered(),c(input$range[1]:input$range[2]),input$scale)
              }
              else if (input$plot_type == "HISTOGRAM") {
                rM(myDataExp, c(input$range[1]:input$range[2]),0.5)
              }
              else if (input$plot_type == "DENSITY PLOT") {
                rM(dataFiltered(), c(input$range[1]:input$range[2]),input$scale)
              }
            })
            })
            observeEvent(input$stap, {
              output$plots <- renderPlot({
                return()
              })
            })
          }
          
        ################################### Classification  ############################################
        else if (input$menu == "class") {
          if (input$type == "DRZEWA LOSOWE") {
             verbatimTextOutput(Random_Forest(dataFiltered()))
            
            
            output$forests <- renderPlot ({
              Random_Forest(dataFiltered())
            })
          }
        }
        
        ################################### Clasterization  ############################################
        else if (input$menu == "clast") {
          
        }
      }
   
  
  })
 
  
  ######################################### Download ############################################  
  
  
})