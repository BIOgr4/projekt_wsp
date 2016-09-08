shinyUI(fluidPage(
  titlePanel("Analiza mikromacierzowa"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file_ES", label = h4("Wprowadź plik:"), multiple = FALSE, accept = c(".Rda")),
      
      selectInput("option", "Opcje:", c("Dane"="raw",
                                        "Klasteryzacja"="clast",
                                        "Klasyfikacja"="class",
                                        "Kontrola jakości"="quality"),
                                        selected="quality", multiple=FALSE),
    
      conditionalPanel(condition="input.option == 'quality'",
        radioButtons("plot_type", label="Rodzaj wykresu:",
                                  choices = c("MVA","HEXBIN","BOXPLOT","SCATTER PLOT","HEATMAP","MA PLOT"), 
                                  selected="MVA"),
        
     
    
    sliderInput("range", "Zakres danych:",
                min = 1, max = 4, value = c(1,4), step= 1))
  
  
  
),

mainPanel(
  tabsetPanel(type = "pills",
              tabPanel("Opis"),
              tabPanel("Dane",uiOutput("dataMessage"),textOutput("dim"),tableOutput("abatch")),
              #tabPanel("Classification"),
              #tabPanel("Clasterization"),                             
              #tabPanel("Wykresy",imageOutput("image")),
              tabPanel("Wyniki", uiOutput("resultsMessage"), uiOutput("results"),plotOutput("plots")),
              tabPanel("Pobieranie wyników"))
)
)
))