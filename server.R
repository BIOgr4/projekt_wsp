#source("F1M.R") #kontrola jakosci

options(shiny.maxRequestSize = 500*1024^2)
shinyServer(function(input, output){
  
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
    
  
  output$results <- renderUI ({
    if (is.null(myData())) {
      br()
      h3("Proszę wrzucić plik ExpressionSet")
    }
  })  
  
  output$results <- renderUI ({
    if (!is.null(myData())) {
      myDataExp <- exprs(myData())
      maxData <- dimData()
      
  ################################### Quality ###################################################
        
        if (input$option == "quality") {
          output$plots <- renderPlot({
            if (input$plot_type == "HEXBIN") {
              HB(myDataExp, c(input$range[[1]]:input$range[[2]]))
              #img(src = name[1], align="center")
            }
            else if (input$plot_type == "MVA"){
              MVA(myDataExp, c(input$range[[1]]:input$range[[2]]))
            }
            else if (input$plot_type == "BOXPLOT") {
              BOXY(myDataExp, c(input$range[[1]]:input$range[[2]]))
            }
            else if (input$plot_type == "SCATTER PLOT") {
              SCATTER(myDataExp, c(input$range[[1]]:input$range[[2]]))
            }
            else if (input$plot_type == "HEATMAP") {
              HM(myDataExp,input$range[[2]])
            }
            else if (input$plot_type == "MA PLOT") {
              MAPLOTS(mydata,c(input$range[[1]]:input$range[[2]]))
            }
            
          })
          
        }
        ################################### Classification  ############################################
        else if (input$option == "class") {
          
        }
        
        ################################### Clasterization  ############################################
        else if (input$option == "clast") {
          
        }
      }
    })
    
   
    
  
 
 
  
  ######################################### Download ############################################  
  
  
})