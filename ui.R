shinyUI(fluidPage(
  titlePanel("Analiza mikromacierzowa"),
  
  fileInput("file_ES", label = h4("Wprowadź plik:"), multiple = FALSE, accept = c(".Rda")),
  
  
  navbarPage("Menu", id="menu",
             tabPanel("Opis"),
             tabPanel("Dane",value="abatch"),
             navbarMenu("Opcje", 
                        tabPanel("Klasyfikacja", value="class"),
                        tabPanel("Klasteryzacja", value="clast"),                             
                        tabPanel("Kontrola jakości",value="quality")),
             tabPanel("Pobieranie wyników")),
  
   
  radioButtons("filtration", "Filtracja", c("ON","OFF"), selected="OFF"),
  conditionalPanel(condition="input.filtration == 'ON'",
        selectInput("filtration_type", label="Rodzaj filtracji:",
                                   choices = c("PRZEPUSTOWA","TEST T"), 
                                   selected = "PRZEPUSTOWA")),
  fluidRow(   
        conditionalPanel(condition="input.filtration_type == 'PRZEPUSTOWA' & input.filtration == 'ON'",
      column(6,
        sliderInput("cutoff","Odcięcie", min=0.1, max=0.9, value=0.5, step=0.1)),
      column(6,
        radioButtons("mode", "Rodzaj przepustu:", choices=c("UP","DOWN"), selected="UP"))),
  
      
  
      conditionalPanel(condition="input.menu == 'abatch'",
        column(12, uiOutput("dataMessage"), textOutput("dim"),tableOutput("abatch"))), 
      
      conditionalPanel(condition="input.menu == 'quality'",
          column(4, selectInput("plot_type",
                                label="Rodzaj wykresu:",
                                choices = c("MVA","HEXBIN","BOXPLOT","SCATTER PLOT","HEATMAP","MA PLOT","HISTOGRAM","DENSITY PLOT"), 
                                selected="BOXPLOT")),
          column(4, sliderInput("range", "Zakres danych:",
                                min = 1, max = 3 , value = c(1,3), step= 1)),
                    uiOutput("max"),
          column(4, h5("Skala"), checkboxInput("scale", "logarytmiczna", FALSE),
                    h5("Kolejność"), checkboxInput("order", "zmniejszająca się", FALSE)),
          
          column(12, actionButton("start","Go!"), actionButton("stap","Stop"), plotOutput("plots", height = "900px"))),
      
      conditionalPanel(condition="input.menu == 'class'",
          column(4, selectInput("type", label="Rodzaj klasyfikacji:",
                                        choices = c("DRZEWA LOSOWE"), 
                                        selected="DRZEWA LOSOWE")),    
          column(8, plotOutput("forests", height = "600px"))),
      
      
      conditionalPanel(condition="input.menu == 'clast'",
          column(6,           selectInput("clust_type", label = "Rodzaj klasteryzacji:",
                                          choices = c("K-means", "Hierarchiczna"), selected = "K-means"),
                              sliderInput("num_clast", label = "Ilość klas:",
                                          min = 2, max = 4, value = 1),
                              actionButton("startK", "Go!"), actionButton("stopK", "Stop")),
                       
                       
          column(12, plotOutput("plot_clast")),
          conditionalPanel(condition="input.clust_type == 'Hierarchiczna'",
          column(12, tableOutput("table_clust"))))

)
))