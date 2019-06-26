#introduction to shiny dashboard

library("shinydashboard")
library("shiny")
library("shinyjs")
library("DT")
require("httr")
require("jsonlite")
library("listviewer")

labelMandatory <- function(label) {
  tagList(label,
          span("*", class = "mandatory_star"))
}

#UI
ui <- dashboardPage(
  dashboardHeader(title = "SIIM Excitement"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Dashboard",
      tabName = "dashboard",
      icon = icon("dashboard")
    ),
    menuItem("Patient", tabName = "patient", icon = icon("th"))
  )),
  dashboardBody(#tab items must correspond with values for tab names
    tabItems(
      #first tab
      tabItem(tabName = "dashboard",
              #boxes are put in rows or columns
              fluidRow(
                box(DT::dataTableOutput("patient"), title = "Patient Data"),
                box(jsoneditOutput("edits"))
              )),
      #2nd tab
      tabItem(tabName = "patient",
              fluidPage(title = "Patient form example",
                        fluidRow(column(
                          6,
                          div(
                            id = "form",
                            
                            textInput("name", labelMandatory("Name"), "Patient Name"),
                            dateInput("dob", labelMandatory("DOB")),
                            checkboxInput("isSmoker", "Is smoker?", FALSE),
                            sliderInput("r_num_years", "Number of years using R", 0, 25, 2, ticks = FALSE),
                            selectInput(
                              "os_type",
                              "Operating system used most frequently",
                              c("",  "Windows", "Mac", "Linux")
                            ),
                            actionButton("submit", "Submit", class = "btn-primary"),
                            
                            shinyjs::hidden(span(id = "submit_msg", "Submitting..."),
                                            div(id = "error",
                                                div(
                                                  br(), tags$b("Error: "), span(id = "error_msg")
                                                ))))))))))
)

server <- function(input, output) {
  url <- "http://hackathon.siim.org/fhir/Patient/siimjoe"
  # dependency: local system variable called SiimApiKey
  myKey <- Sys.getenv('SiimApiKey')
  patientReturn <-
    GET(url, accept_json(), add_headers('apikey' = myKey))
  get_patient_text <- content(patientReturn, "text")
  get_patient_json <- fromJSON(get_patient_text, flatten = TRUE)
  # table of returned data
  patientData <- as.data.frame(get_patient_json)
  output$patient <- DT::renderDataTable({
    patientData[, c(2,14:22)]
  })
  # interactive json editor
  output$edits <- renderJsonedit({
    jsonedit(
      as.list( data )
      ,"change" = htmlwidgets::JS('function(){
                                  console.log( event.currentTarget.parentNode.editor.get() )
  }')
    )
    
})
}

shinyApp(ui, server)