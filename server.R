source("F1M.R") #kontrola jakosci
source("filtracja2.R") # filtracja

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
      Data <- myData()
      myDataExp <- exprs(Data)
      maxData <- dim(myDataExp)[2]
    }
  })

  #output$max <- renderUI({
  #  if (!is.null(myData())) { 
  #    rMax <- rangeMax()
  #    browser()
  #    sliderInput("max", "Zakres:", min=1, max=rMax, value=rMax, step=1)
  #  }
  #})
  #}
  
  observe({
    if (!is.null(myData())) { 
      rMax <- rangeMax()
      updateSliderInput(session, "range", value = c(2,rMax), min = 2, max = rMax, step = 1)
    }
  })
  ######################################### Raw data ############################################ 
  output$dataMessage <- renderUI ({
    if (is.null(myData())) { 
      br()
      h3("Proszę wrzucić plik ExpressionSet")
    }
  })
  
  output$abatch <- renderTable({
    if (!is.null(myData())) {
      exprs(myData())
    }
  })
  
  
  ######################################### Results ############################################  
  
  #output$results <- renderUI ({
  observe ({
    if (!is.null(myData())) {
      myDataExp <- exprs(myData())
      #maxData <- dim(Data)
      
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
                BOXY(myData(), c(input$range[1]:input$range[2]),input$scale)
              }
              else if (input$plot_type == "SCATTER PLOT") {
                SCATTER(myDataExp, c(input$range[1]:input$range[2]),input$scale)
              }
              else if (input$plot_type == "HEATMAP") {
                HM(myDataExp,c(input$range[1]:input$range[2]),input$order)
              }
              else if (input$plot_type == "MA PLOT") {
                MAPLOTS(myData(),c(input$range[1]:input$range[2]),input$scale)
              }
              else if (input$plot_type == "HISTOGRAM") {
                rM(myDataExp, c(input$range[1]:input$range[2]),0.5)
              }
              else if (input$plot_type == "DENSITY PLOT") {
                rM(myData(), c(input$range[1]:input$range[2]),input$scale)
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
          
        }
        
        ################################### Clasterization  ############################################
        else if (input$menu == "clast") {
          
        }
      }
    
    
  ################################### Filtration ###################################################
        else if (input$menu == "filtration") {
          if (input$filtration_type == "cutoff") {
            
            filtered <- filtracja2(myData())
            eval(var.name)
            #exprs(filtered)
          }
          else if (input$filtration_type == "test T") {
            
          }
        }
  
  })
 
  
  ######################################### Download ############################################  
  
  
})