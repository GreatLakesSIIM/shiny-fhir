# SIIM Shiny Dashboard

library("shinydashboard")
library("shiny")
library("shinyjs")
library("DT")
require("httr")
require("jsonlite")
library("listviewer")

patientDefaults <-
  c(
    "Ms.",
    "Patient",
    "Name",
    "Jr.",
    "9195555555",
    "patientX@gmail.com",
    "Home",
    "F",
    "2000-08-12",
    "555 Home Street",
    "City",
    "State",
    "55555",
    "USA",
    "S",
    "TRUE",
    "Dr. Gregory House",
    "Hospital 1",
    "Ms.",
    "Patient",
    "Name",
    "Jr.",
    "9195555555",
    "patientX@gmail.com",
    "Home"
  )
patientDefaults <-
  c(
    "",
    "Patient",
    "Name",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "USA",
    "",
    "FALSE",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    ""
  )

fieldsAll <-
  c(
    "prefix",
    "given",
    "family",
    "suffix",
    "phone",
    "email",
    "use",
    "sex",
    "dob",
    "line",
    "city",
    "state",
    "postalCode",
    "country",
    "maritalStatus",
    "isSmoker",
    "generalPractioner",
    "managingOrganization",
    "Eprefix",
    "Egiven",
    "Efamily",
    "Esuffix",
    "Ephone",
    "Eemail",
    "Euse"
  )

names(patientDefaults) <- fieldsAll

doctorList <-
  list(
    "Dr. Gregory House" = 1,
    "Dr. Sheila Pathologist" = 2,
    "Dr. Patty Radiologist" = 3,
    "Unknown" = 4
  )
organizationList <-
  list(
    "Hospital 1" = 1,
    "Hospital 2" = 2,
    "Hospital 3" = 3,
    "Unknown" = 4
  )

labelMandatory <- function(label) {
  tagList(label,
          span("*", class = "mandatory_star"))
}

fieldsMandatory <- c("given", "dob")

humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}
epochTime <- function() {
  return(as.integer(Sys.time()))
}
responsesDir <- file.path(".")

saveData <- function(data) {
  fileName <- sprintf("%s_%s.csv",
                      humanTime(),
                      digest::digest(data))
  write.csv(
    x = data,
    file = file.path(responsesDir, fileName),
    row.names = FALSE,
    quote = TRUE
  )
}

patientTab <- tabItem(tabName = "patient",
  fluidPage(title = "Patient form example",
      fluidRow(column(
        6,
        div(
          id = "form",
          textInput("prefix", ("Prefix"), patientDefaults[["prefix"]]),
          textInput("given", labelMandatory("First Name"), patientDefaults[["given"]]),
          textInput("family", labelMandatory("Last Name"), patientDefaults[["family"]]),
          textInput("suffix", ("Suffix"), patientDefaults[["suffix"]]),
          
          numericInput("phone", labelMandatory("Phone Number"), patientDefaults[["phone"]]),
          textInput("email", labelMandatory("Email"), patientDefaults[["email"]]),
          selectInput(
            "use",
            labelMandatory("Phone Type"),
            choices = list(
              "Home" = "home",
              "Work" = "work",
              "Mobile" = "mobile",
              "Old" = "old"
            )
          ),
          selectInput(
            "sex",
            labelMandatory("Sex"),
            choices = list(
              "Male" = "male",
              "Female" = "female",
              "Other" = "other",
              "Unknown" = "unknown"
            )
          ),
          dateInput(
            "dob",
            labelMandatory("DOB"),
            value = patientDefaults[["dob"]],
            format = "yyyy-mm-dd"
          ),
          
          textInput("line", labelMandatory("Street Address"), patientDefaults[["line"]]),
          textInput("city", labelMandatory("City"), patientDefaults[["city"]]),
          textInput("state", labelMandatory("State"), patientDefaults[["state"]]),
          textInput("postalCode", labelMandatory("postalCode"), patientDefaults[["postalCode"]]),
          textInput("country", labelMandatory("Country"), patientDefaults[["country"]]),
          
          selectInput(
            "maritalStatus",
            labelMandatory("Marital Status"),
            choices = list(
              "Married" = "M",
              "Single" = "S",
              "Divorced" = "D",
              "Unknown" = "U"
            )
          ),
          
          checkboxInput("isSmoker", ("Is smoker?"), value = (patientDefaults[["isSmoker"]] ==
                                                               "TRUE")),
          
          textInput("Eprefix", ("Emergency Contact Prefix"), patientDefaults[["Eprefix"]]),
          textInput("Egiven", labelMandatory("First Name"), patientDefaults[["Egiven"]]),
          textInput("Efamily", labelMandatory("Last Name"), patientDefaults[["Efamily"]]),
          textInput("Esuffix", ("Suffix"), patientDefaults[["Esuffix"]]),
          numericInput("Ephone", labelMandatory("Phone Number"), patientDefaults[["Ephone"]]),
          textInput("Eemail", labelMandatory("Email"), patientDefaults[["Eemail"]]),
          selectInput(
            "Euse",
            labelMandatory("Phone Type"),
            choices = list(
              "Home" = "home",
              "Work" = "work",
              "Mobile" = "mobile",
              "Old" = "old"
            )
          ),
          
          selectInput(
            "generalPractioner",
            labelMandatory("Practioner"),
            selected = patientDefaults[["generalPractioner"]],
            choices = doctorList
          ),
          selectInput(
            "managingOrganization",
            labelMandatory("Organization"),
            selected = patientDefaults[["managingOrganization"]],
            choices = organizationList
          ),
          
          actionButton("submit", "Submit", class = "btn-primary")
        )
      ))))

#UI
ui <- dashboardPage(
  dashboardHeader(title = "SIIM Excitement"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Edit JSON",
      tabName = "json",
      icon = icon("dashboard")
    ),
    menuItem(
      "Patient", 
      tabName = "patient", 
      icon = icon("th")
    ),
    menuItem(
      "Data Table", 
      tabName = "dataTable", 
      icon = icon("th")
    )
  )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "json",
              fluidRow(
                box(jsoneditOutput("edits"))
              )),
      tabItem(tabName = "dataTable",
              fluidRow(
                box(DT::dataTableOutput("patient"), title = "Patient Data")
              )),
      #2nd tab
      patientTab
    ))
)

server <- function(input, output) {
  patient_json <- fromJSON(content(GET('http://hackathon.siim.org/fhir/Patient',
                                       accept_json(),
                                       add_headers('apikey' = Sys.getenv(x='SiimApiKey'))),"text"),
                           flatten=TRUE)
  patientIdList <- patient_json$entry$resource.id
  
  output$selectUI <- renderUI({
    selectedpatientId <- selectInput("patientId", labelMandatory("PatientId:"),patientIdList,selected=patientIdList[1])
  })
  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               tryCatch({
                 !is.null(input[[x]]) && (toString(input[[x]]) != "")
               })
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
  formData <- reactive({
    data <- sapply(fieldsAll, function(x)
      input[[x]])
    data <- c(data, timestamp = epochTime())
    data <- t(data)
    data
  })
  observeEvent(input$submit, {
    # User-experience stuff
    shinyjs::disable("submit")
    shinyjs::hide("error")
    
    # Save the data (show an error message in case of error)
    tryCatch({
      saveData(formData())
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

      tabItem(tabName = "patient",
              fluidPage(title = "Patient form example",
                        fluidRow(column(
                          6,
                          div(
                            id = "form",
                            htmlOutput("selectUI"),
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
                                                ))))))))

  url <- "http://hackathon.siim.org/fhir/Patient/siimjoe"
  # dependency: local system variable called SiimApiKey
  myKey <- Sys.getenv('SiimApiKey')
  patientReturn <- GET(
    url, 
    accept_json(), 
    add_headers('apikey' = myKey))
  get_patient_text <- content(patientReturn, "text")
  get_patient_json <- fromJSON(get_patient_text, flatten = TRUE)
  # table of returned data
  patientData <- as.data.frame(get_patient_json)
  output$patient <- DT::renderDataTable({
    patientData[, c(2,14:22)]
  })
  # interactive json editor of data
  output$edits <- renderJsonedit({
    jsonedit(
      as.list( patientData ),
      "change" = htmlwidgets::JS('function(){
                                  console.log( event.currentTarget.parentNode.editor.get() )
  }'))
    })
}

shinyApp(ui, server)