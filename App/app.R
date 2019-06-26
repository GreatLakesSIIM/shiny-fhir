#introduction to shiny dashboard

library("shinydashboard")
library("shiny")
library("DT")
require("httr")
require("jsonlite")

#UI
ui <- dashboardPage(
  dashboardHeader(title = "SIIM Excitement"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    #tab items must correspond with values for tab names
    tabItems(
      #first tab
      tabItem(tabName = "dashboard",
              #boxes are put in rows or columns
              fluidRow(
                box(DT::dataTableOutput("patient"), title = "Patient Data")
              )
      ),
      #2nd tab
      tabItem(tabName = "widgets",
              h2("Widgets tab content"))
      
    )
  )
)

server <- function(input, output){
  url <- "http://hackathon.siim.org/fhir/Patient/siimjoe"
  #dependency: local system variable called SiimApiKey
  myKey <-Sys.getenv('SiimApiKey')
  patientReturn <- GET(url, accept_json(), add_headers('apikey' = myKey))
  get_patient_text <- content(patientReturn, "text")
  get_patient_json <- fromJSON(get_patient_text, flatten = TRUE) 
  #table of returned data
  patientData <- as.data.frame(get_patient_json)
  output$patient <- DT::renderDataTable({ patientData })
}

shinyApp(ui, server)