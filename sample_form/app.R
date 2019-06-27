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
            
            , htmlOutput("selectUI")
            
        ),
        
        # Show a options to select
        mainPanel(
            
            tableOutput('patientInfo')
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    organization <- reactive({get(intput$organization)})
    
    patient_json <- fromJSON(content(GET('http://hackathon.siim.org/fhir/Patient',
                                         accept_json(),
                                         add_headers('apikey' = Sys.getenv(x='SiimApiKey'))),"text"),
                             flatten=TRUE)
    patientIdList <- patient_json$entry$resource.id
    
    output$selectUI <- renderUI({
        selectInput("patientId", "Patient Id:",patientIdList,selected=patientIdList[1])
    })
    
    #patient <- reactive({get(input$patient)})
    
    #patient_json <- fromJSON(content(GET(paste('http://hackathon.siim.org/fhir/Patient/',patient,sep=""),
    #                                    accept_json(),
    #                                    add_headers('apikey' = Sys.getenv(x='SiimApiKey'))),
    #                                "text"),
    #                        flatten = TRUE)
    
    #patientData <- as.data.frame(patient_json)
    
    #output$patientInfo <- renderTable({ patientData })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
