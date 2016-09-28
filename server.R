source("F1M.R") #kontrola jakosci
source("Klasteryzacja.R")

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
      updateSliderInput(session, "range", value = c(1,rMax), min = 1, max = rMax, step = 1)
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
        #browser()
        output$plots <- renderPlot({
          if (input$plot_type == "HEXBIN") {
            HB(myDataExp, input$range)
          }
          else if (input$plot_type == "MVA"){
            MVA(myDataExp, c(input$range[1]:input$range[2]))
          }
          else if (input$plot_type == "BOXPLOT") {
            BOXY(myData(), c(input$range[1]:input$range[2]))
          }
          else if (input$plot_type == "SCATTER PLOT") {
            browser()
            SCATTER(myDataExp, c(input$range[1]:input$range[2]))
          }
          else if (input$plot_type == "HEATMAP") {
            HM(myDataExp,input$range[2])
          }
          else if (input$plot_type == "MA PLOT") {
            MAPLOTS(myData(),c(input$range[1]:input$range[2]))
          }
          
        })
        
      }
      ################################### Classification  ############################################
      else if (input$menu == "class") {
        
      }
      
      ################################### Clasterization  ############################################
      else if (input$menu == "clast") {

        observeEvent(input$start, {
          output$plot_clast <- renderPlot({
            if(input$clust_type == "K-means"){
            
              Funkcja(myDataExp,input$num_clast)
            } else if(input$clust_type == "Hierarchiczna"){
            
              Cluster_function(myDataExp, input$num_clast)
              
          }
        })
          
            output$table_clust <- renderTable({
              Cluster_function(myDataExp, input$num_clast)
            }, include.rownames=TRUE)
         })
        observeEvent(input$stop, {
          output$plot_clast <- renderPlot({
            return()
          })
          output$table_clust <- renderTable({
            return()
          })
          
        })
      }
    }
  })
  
  
  
  
  
  
  
  ######################################### Download ############################################  
  
  
})