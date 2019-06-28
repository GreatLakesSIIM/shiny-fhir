#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("shinydashboard")
library("shiny")
library("shinyjs")
library("DT")
require("httr")
require("jsonlite")
library("listviewer")
jscode <- "shinyjs.refresh = function() { location.reload(); }"

# all_patient_json <- function(){
#     json <- fromJSON(content(GET('http://hackathon.siim.org/fhir/Patient',
#                                 accept_json(),
#                                 add_headers('apikey' = Sys.getenv(x='SiimApiKey'))),"text"),
#                      flatten=TRUE)
#     return(json)
# }

# patient_json <- function(patientId){
#     json <- fromJSON(content(GET(paste0('http://hackathon.siim.org/fhir/Patient/',patientId),
#                                          accept_json(),
#                                          add_headers('apikey' = Sys.getenv(x='SiimApiKey'))),"text"),
#                              flatten=TRUE)
#     return(json)
# }


save_json <- function(data) {
    fileName <- sprintf("outputfile_%s.json", format(Sys.time(), "%Y%m%d-%H%M%OS"))
    write_json(
        x = data,
        path = file.path(file.path("."), fileName)
    )
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Access Patient Info"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
        sidebarPanel(
            
            # selectInput(inputId="organization",
            #             label="Organization:", 
            #             choices=c("siim"),
            #             selected = "siim")
            
            # , selectInput(inputId="patientId",
            #             label="Patient Id:", 
            #             choices=all_patient_json()$entry$resource.id,
            #             selected=all_patient_json()$entry$resource.id[1])
            br()
            , fluidRow(
                column(6,
                       div(id = "form",
                           textInput("name.given", ("First Name")),
                           textInput("name.family", ("Last Name")),
                           actionButton("submit", "Submit", class = "btn-primary")
                           )
                       )
                )
            ),
        
        mainPanel(
            
            # DT::dataTableOutput("patientInfo")
            
            textOutput("patientJson")
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # patient_id <- reactive({input$patientId})
    
    # obs <- observe({
    #     
    #     patient_data <- as.data.frame(patient_json(patient_id()))
    #     
    #     output$patientInfo <- DT::renderDataTable({ patient_data })
    #     
    # })
    
    patientFormData <- reactive({
        toJSON(
            c({"name.given" = input$name.given}, 
              {"name.family" = input$name.family},
              {"timestamp" = as.integer(Sys.time())})
            )
    })
    
    obsSubmitPatientInfo <- observeEvent(input$submit,{
        # User-experience stuff
        shinyjs::disable("submit")
        shinyjs::hide("error")
        
        # Save the data (show an error message in case of error)
        tryCatch({
            output$patientJson <- patientFormData()
            shinyjs::reset("form")
            shinyjs::hide("form")
        },
        error = function(err) {
            shinyjs::html("error_msg", err$message)
            shinyjs::show(id = "error",
                          anim = TRUE,
                          animType = "fade")
        },
        finally = {
            shinyjs::enable("submit")
        })
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
