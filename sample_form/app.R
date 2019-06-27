#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("shiny")
library("shinydashboard")
library("shinyjs")
library("DT")
require("httr")
require("jsonlite")
library("listviewer")

all_patient_json <- function(){
    json <- fromJSON(content(GET('http://hackathon.siim.org/fhir/Patient',
                                accept_json(),
                                add_headers('apikey' = Sys.getenv(x='SiimApiKey'))),"text"),
                     flatten=TRUE)
    return(json)
}

patient_json <- function(patientId){
    json <- fromJSON(content(GET(paste0('http://hackathon.siim.org/fhir/Patient/',patientId),
                                         accept_json(),
                                         add_headers('apikey' = Sys.getenv(x='SiimApiKey'))),"text"),
                             flatten=TRUE)
    return(json)
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Access Patient Info"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
        sidebarPanel(
            
            selectInput(inputId="organization",
                        label="Organization:", 
                        choices=c("siim"),
                        selected = "siim")
            
            , selectInput(inputId="patientId",
                        label="Patient Id:", 
                        choices=all_patient_json()$entry$resource.id,
                        selected=all_patient_json()$entry$resource.id[1])
            
        ),
        
        # Show a options to select
        mainPanel(
            
            DT::dataTableOutput("patientInfo")
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    patient_id <- reactive({input$patientId})
    
    obs <- observe({
        
        patient_data <- as.data.frame(patient_json(patient_id()))
        
        output$patientInfo <- DT::renderDataTable({ patient_data })
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
