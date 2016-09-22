shinyUI(fluidPage(
  titlePanel("Analiza mikromacierzowa"),
  
  fileInput("file_ES", label = h4("Wprowadź plik:"), multiple = FALSE, accept = c(".Rda")),
  
  navbarPage("Menu", id="menu",
             tabPanel("Opis"),
             tabPanel("Dane",value="abatch"),
             navbarMenu("Opcje", 
                        tabPanel("Selekcja"),
                                  "Dane przed selekcja",
                                  "Dane po selekcji",
                        tabPanel("Klasyfikacja", value="class"),
                        tabPanel("Klasteryzacja", value="clast"),                             
                        tabPanel("Kontrola jakości",value="quality")),
             tabPanel("Pobieranie wyników")),
      
      
  fluidRow(
      conditionalPanel(condition="input.menu == 'abatch'",
        column(12, uiOutput("dataMessage"),textOutput("dim"),tableOutput("abatch"))), 
      
      conditionalPanel(condition="input.menu == 'quality'",
        column(6,
        selectInput("plot_type", label="Rodzaj wykresu:",
                                  choices = c("MVA","HEXBIN","BOXPLOT","SCATTER PLOT","HEATMAP","MA PLOT"), 
                                  selected="BOXPLOT")),
        column(6,
        sliderInput("range", "Zakres danych:", min = 1, max = 3 , value = c(1,3), step= 1)),
        uiOutput("max")),

        column(12, plotOutput("plots"))
  )
)
)