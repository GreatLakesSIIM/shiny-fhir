
library(shiny)
library(shinyFiles)
library(jsonlite)
library(tidyverse)
library(DT)
library(rjson)
library(httr)
library(stringi)
library(RJSONIO)


post_data <- function(resourceType, data){
    POST(paste0('http://hackathon.siim.org/fhir/',resourceType),
         add_headers('apikey' = Sys.getenv(x='SiimApiKey'),
                     'Content-Type' = 'application/fhir+json'),
         body=data,
         encode="text")
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("JSON Test"),
    fileInput("Json", "Choose Json File",
              multiple = FALSE,
              accept = c(".json")),
    DTOutput('tbl')
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    output$tbl = renderDT({
        req(input$Json)
        jsonfile <- as.character(input$Json)
        print(jsonfile)
        string <- readChar(jsonfile, file.info(jsonfile)$size)
        postAttempt <- post_data("",string)
        print(postAttempt)
        as.data.frame(fromJSON(file = input$Json$datapath))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
