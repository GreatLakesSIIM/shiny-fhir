# SIIM Shiny Dashboard

library("shinydashboard")
library("shiny")
library("shinyjs")
library("DT")
require("httr")
require("stringi")
require("jsonlite")
library("listviewer")

jscode <- "shinyjs.refresh = function() { location.reload(); }"

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

HumanNameFields <- c("prefix",
                     "given",
                     "family",
                     "suffix")

AddressFields <- c("line",
                   "city",
                   "state",
                   "postalCode",
                   "country")

ContactFields <- c("value",
                   "email",
                   "use")

DRDefaults <- c(
  "resourceType",
  "category",
  "code",
  "subject",
  "encounter",
  "performer",
  "resultsInterpreter",
  "result",
  "imagingStudy",
  "conclusion",
  "conclusionCode"
  )
DRFields <-  c(
  "resourceType",
  "category",
  "code",
  "subject",
  "encounter",
  "performer",
  "resultsInterpreter",
  "result",
  "imagingStudy",
  "conclusion",
  "conclusionCode"
)
names(DRDefaults) <- DRFields

practitionerDefaults <- c(
  name = list( HumanNameFields),
  contact = list(ContactFields),
  address = list(AddressFields),
  "F",
  "dob",
  "communication"
)
practitionerFields <- c(
  name = list( HumanNameFields),
  contact = list(ContactFields),
  address = list(AddressFields),
  "gender",
  "dob",
  "communication"
)
names(practitionerDefaults) <- practitionerFields

orgType <-
  c("prov",
    "dept",
    "team",
    "govt",
    "ins",
    "pay",
    "edu",
    "reli",
    "crs",
    "cg",
    "bus",
    "other")

orgDefaults <- c(
  "type",
  "name",
  "alias",
  "contact.name.prefix",
  "contact.name.given",
  "contact.name.family",
  "contact.name.suffix",
  "telecom.value",
  "telecom.email",
  "telecom.use",
  "address.line",
  "address.city",
  "address.state",
  "address.postalCode",
  "address.country",
  "partOf"
)
orgFields <- c(
  "type.type",
  "name",
  "alias",
  "contact.name.prefix",
  "contact.name.given",
  "contact.name.family",
  "contact.name.suffix",
  "telecom.value",
  "telecom.email",
  "telecom.use",
  "address.line",
  "address.city",
  "address.state",
  "address.postalCode",
  "address.country",
  "partOf"
)
names(orgDefaults) <- orgFields

patientDefaults <-
  c(
    "Patient",
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
    "address.city",
    "address.state",
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
    "Patient",
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
    "resourceType",
    "name.prefix",
    "name.given",
    "name.family",
    "name.suffix",
    "telecom.value",
    "telecom.email",
    "telecom.use",
    "gender",
    "dob",
    "address.line",
    "address.city",
    "address.state",
    "address.postalCode",
    "address.country",
    "maritalStatus",
    "isSmoker",
    "generalPractioner",
    "managingOrganization",
    "contact.name.prefix",
    "contact.name.given",
    "contact.name.family",
    "contact.name.suffix",
    "contact.telecom.value",
    "contact.telecom.email",
    "contact.telecom.use"
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

fieldsMandatory <- c("name.given", "dob")

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
DRTab <- tabItem(tabName = "DR",
                           fluidPage(title = "DR form example",
                                     fluidRow(column(
                                       6,
                                       div(
                                         id = "DRForm",
                                         textInput("category", ("Category"), DRDefaults[["category"]]),
                                         textInput("code", ("Code"), DRDefaults[["code"]]),
                                         selectInput(inputId="patientId",
                                                     label="Subject", 
                                                     choices=all_patient_json()$entry$resource.id,
                                                     selected=all_patient_json()$entry$resource.id[1]),
                                         
                                         textInput("encounter", labelMandatory("Encounter"), DRDefaults[["encounter"]]),
                                         textInput("performer", labelMandatory("Practitioner"), DRDefaults[["performer"]]),
                                         textInput("resultsInterpreter", ("Results Interpreter"), DRDefaults[["resultsInterpreter"]]),
                                         textInput("result", ("Result/Observation"), DRDefaults[["result"]]),
                                         textInput("imagingStudy", ("Imaging Study"), DRDefaults[["imagingStudy"]]),
                                         
                                         
                                         textInput("conclusion", labelMandatory("Conclusion"), DRDefaults[["conclusion"]]),
                                         textInput("conclusionCode", labelMandatory("Conclusion Code"), DRDefaults[["conclusionCode"]]),
                                        
                                         actionButton("DRSubmit", "Submit", class = "btn-primary")
                                       )
                                     ))))

practitionerTab <- tabItem(tabName = "practitioner",
                           fluidPage(title = "Practitioner form example",
                                     fluidRow(column(
                                       6,
                                       div(
                                         id = "practitionerForm",
                                         textInput("name.prefix", ("Prefix"), practitionerDefaults[["name.prefix"]]),
                                         textInput("name.given", labelMandatory("First Name"), practitionerDefaults[["name.given"]]),
                                         textInput("name.family", labelMandatory("Last Name"), practitionerDefaults[["name.family"]]),
                                         textInput("name.suffix", ("Suffix"), practitionerDefaults[["name.suffix"]]),
                                         
                                         numericInput(
                                           "telecom.value",
                                           labelMandatory("Phone"),
                                           practitionerDefaults[["telecom.value"]]
                                         ),
                                         textInput("telecom.email", labelMandatory("Email"), practitionerDefaults[["telecom.email"]]),
                                         selectInput(
                                           "telecom.use",
                                           labelMandatory("Use"),
                                           choices = list(
                                             "Home" = "home",
                                             "Work" = "work",
                                             "Mobile" = "mobile",
                                             "Old" = "old"
                                           )
                                         ),
                                         
                                         textInput(
                                           "address.line",
                                           labelMandatory("Street Address"),
                                           practitionerDefaults[["address.line"]]
                                         ),
                                         textInput("address.city", labelMandatory("City"), practitionerDefaults[["address.city"]]),
                                         textInput("address.state", labelMandatory("State"), practitionerDefaults[["address.state"]]),
                                         textInput(
                                           "address.postalCode",
                                           labelMandatory("Postal Code"),
                                           practitionerDefaults[["address.postalCode"]]
                                         ),
                                         textInput("address.country", labelMandatory("Country"), practitionerDefaults[["address.country"]]),
                                         
                                         selectInput(
                                           "gender",
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
                                           value = practitionerDefaults[["dob"]],
                                           format = "yyyy-mm-dd"
                                         ),
                                         textInput("communication", ("Languages"), practitionerDefaults[["communication"]]),
                                         
                                         actionButton("practitionerSubmit", "Submit", class = "btn-primary")
                                       )
                                     ))))
orgTab <- tabItem(tabName = "organization",
                  fluidPage(title = "Organization form example",
                            fluidRow(column(
                              6,
                              div(
                                id = "orgForm",
                                selectInput("type",
                                            labelMandatory("Org Type"),
                                            choices = orgType),
                                textInput("name", labelMandatory("Name of Organization"), orgDefaults[["name"]]),
                                textInput("alias", ("Alias of Organization"), orgDefaults[["alias"]]),
                                textInput("name.prefix", ("Prefix"), orgDefaults[["contact.name.prefix"]]),
                                textInput("name.given", labelMandatory("First Name"), orgDefaults[["contact.name.given"]]),
                                textInput("name.family", labelMandatory("Last Name"), orgDefaults[["contact.name.family"]]),
                                textInput("name.suffix", ("Suffix"), orgDefaults[["contact.name.suffix"]]),
                                
                                numericInput("telecom.value", labelMandatory("Phone"), orgDefaults[["telecom.value"]]),
                                textInput("telecom.email", labelMandatory("Email"), orgDefaults[["telecom.email"]]),
                                selectInput(
                                  "telecom.use",
                                  labelMandatory("Use"),
                                  choices = list(
                                    "Home" = "home",
                                    "Work" = "work",
                                    "Mobile" = "mobile",
                                    "Old" = "old"
                                  )
                                ),
                                textInput("address.line", labelMandatory("Street Address"), orgDefaults[["address.line"]]),
                                textInput("address.city", labelMandatory("City"), orgDefaults[["address.city"]]),
                                textInput("address.state", labelMandatory("State"), orgDefaults[["address.state"]]),
                                textInput("address.postalCode", labelMandatory("Postal Code"), orgDefaults[["address.postalCode"]]),
                                textInput("address.country", labelMandatory("Country"), orgDefaults[["address.country"]]),
                                
                                textInput("partOf", ("Part of:"), orgDefaults[["partOf"]]),
                                actionButton("submitOrg", "Submit", class = "btn-primary")
                                
                              )
                            ))))
patientTab <- tabItem(tabName = "patient",
                      fluidPage(title = "Patient form example",
                                fluidRow(column(
                                  6,
                                  div(
                                    id = "form",
                                    textInput("name.prefix", ("Prefix"), patientDefaults[["name.prefix"]]),
                                    textInput("name.given", labelMandatory("First Name"), patientDefaults[["name.given"]]),
                                    textInput("name.family", labelMandatory("Last Name"), patientDefaults[["name.family"]]),
                                    textInput("name.suffix", ("Suffix"), patientDefaults[["name.suffix"]]),
                                    
                                    numericInput("telecom.value", labelMandatory("Phone Number"), patientDefaults[["telecom.value"]]),
                                    textInput("telecom.email", labelMandatory("Email"), patientDefaults[["telecom.email"]]),
                                    selectInput(
                                      "telecom.use",
                                      labelMandatory("Use"),
                                      choices = list(
                                        "Home" = "home",
                                        "Work" = "work",
                                        "Mobile" = "mobile",
                                        "Old" = "old"
                                      )
                                    ),
                                    selectInput(
                                      "gender",
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
                                    
                                    textInput("address.line", labelMandatory("Street Address"), patientDefaults[["address.line"]]),
                                    textInput("address.city", labelMandatory("City"), patientDefaults[["address.city"]]),
                                    textInput("address.state", labelMandatory("State"), patientDefaults[["address.state"]]),
                                    textInput("address.postalCode", labelMandatory("Postal Code"), patientDefaults[["address.postalCode"]]),
                                    textInput("address.country", labelMandatory("Country"), patientDefaults[["address.country"]]),
                                    
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
                                    
                                    textInput("contact.name.prefix", ("Emergency Contact Name Prefix"), patientDefaults[["contact.name.prefix"]]),
                                    textInput("contact.name.given", labelMandatory("First Name"), patientDefaults[["contact.name.given"]]),
                                    textInput("contact.name.family", labelMandatory("Last Name"), patientDefaults[["contact.name.family"]]),
                                    textInput("contact.name.suffix", ("Suffix"), patientDefaults[["contact.name.suffix"]]),
                                    numericInput("telecom.value", labelMandatory("Phone Number"), patientDefaults[["telecom.value"]]),
                                    textInput("telecom.email", labelMandatory("Email"), patientDefaults[["telecom.email"]]),
                                    selectInput(
                                      "Use",
                                      labelMandatory("Type"),
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
                                    
                                    actionButton("submit", "Submit", class = "btn-primary"),
                                    actionButton("addDR", "Add Diagnostic Report", class = "btn-primary"),
                                    actionButton("addObservation", "Add Observation", class = "btn-primary"),
                                    actionButton("addCondition", "Add Condition", class = "btn-primary"),
                                    actionButton("addImagingStudy", "Add Imaging Study", class = "btn-primary")
                                  )
                                ))))

#UI
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "SIIM Forms on FHIR", titleWidth = 350),

  dashboardSidebar(sidebarMenu(
    id = "tabs",
    # Custom CSS to hide the default logout panel
    tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
    
    # The dynamically-generated user panel
    uiOutput("userpanel"),
    menuItem(
      "Patient", 
      tabName = "patient", 
      icon = icon("th")
    ),
    menuItem(
      "Organization", 
      tabName = "organization", 
      icon = icon("th")
    ),
    menuItem(
      "Practitioner", 
      tabName = "practitioner", 
      icon = icon("th")
    ),
    menuItem(
      "Diagnostic Report", 
      tabName = "DR", 
      icon = icon("th")
    ),
    menuItem(
      "Data Table", 
      tabName = "dataTable", 
      icon = icon("th")
    )
  )),
  dashboardBody(
    tags$head(tags$style(HTML('
      .main-header .logo {
                              font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;
                              font-size: 24px;
                              }
                              '))),
    tabItems(
      tabItem(tabName = "dataTable",
              fluidRow(
                box(
                  
                  selectInput(inputId="organization",
                              label="Organization:", 
                              choices=c("siim"),
                              selected = "siim")
                  
                  , selectInput(inputId="patientId",
                                label="Patient Id:", 
                                choices=all_patient_json()$entry$resource.id,
                                selected=all_patient_json()$entry$resource.id[1])
                  
                  , DT::dataTableOutput("patientInfo")
                  , title = "Patient Data"
                  , width = 12
                )
              )),
      #2nd tab
      patientTab,
      orgTab,
      practitionerTab,
      DRTab
    )
  )
)


server <- function(input, output, session) {
  
  # reactive patient data
  patient_id <- reactive({input$patientId})
  obs <- observe({
    patient_data <- as.data.frame(patient_json(patient_id()))
    output$patientInfo <- DT::renderDataTable({ patient_data })
  })
  
  output$userpanel <- renderUI({
    # session$user is non-NULL only in authenticated sessions
    if (is.null(session$user)) {
      sidebarUserPanel(
        span("Logged in as Dr. Barrington"),
        subtitle = a(icon("sign-out"), "Logout", href="https://twitter.com/logout")
      )
    }
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
    
  })
  
  formData <- reactive({
    data <- sapply(fieldsAll, function(x)
      input[[x]])
    data <- c(data, timestamp = epochTime())
    data <- t(data)
    data
  })
  formDataOrg <- reactive({
    data <- sapply(orgFields, function(x)
      input[[x]])
    data <- c(data, timestamp = epochTime())
    data <- t(data)
    data
  })
  formDataPractitioner <- reactive({
    data <- sapply(practitionerFields, function(x)
      input[[x]])
    data <- c(data, timestamp = epochTime())
    data <- t(data)
    data
  })
  formDataDR <- reactive({
    data <- sapply(DRFields, function(x) input[[x]])
    #data <- c(data, timestamp = epochTime())
    data <- t(data)
    print(data)
    data
  })
  observeEvent(input$submit, {
    # User-experience stuff
    shinyjs::disable("submit")
    shinyjs::hide("error")
    
    # Save the data (show an error message in case of error)
    tryCatch({
      data <- c(formData())
      names(data) <- fieldsAll
      data[["resourceType"]] <- "Patient"
      data <- toJSON(data,auto_unbox =TRUE)
      
      putAttempt = POST('http://hackathon.siim.org/fhir/Patient',
                        add_headers('apikey' = Sys.getenv(x='SiimApiKey')),
                        body=data,
                        encode="raw")
      print(putAttempt)
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
  
  observeEvent(input$addDR, {
    # User-experience stuff
    shinyjs::disable("addDR")

    # Save the data (show an error message in case of error)
    tryCatch({
      data <- formData()
      names(data) <- fieldsAll
      DRDefaults[["subject"]] <- paste(data[["name.prefix"]],data[["name.given"]],data[["name.family"]],data[["name.suffix"]],sep=" ")
      
      updateTextInput(session, "subject", value = paste(data[["name.prefix"]],data[["name.given"]],data[["name.family"]],data[["name.suffix"]],sep=" "))
      updateTabItems(session,"tabs",selected="DR")
    },
    error = function(err) {
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error",
                    anim = TRUE,
                    animType = "fade")
    },
    finally = {
      shinyjs::enable("addDR")
    })
  })
  observeEvent(input$submitOrg, {
    # User-experience stuff
    shinyjs::disable("submitOrg")
    
    tryCatch({
      data <- c(formDataOrg())
      names(data) <- orgFields
      data[["resourceType"]] <- "Organization"
      data <- toJSON(data,auto_unbox =TRUE)
      
      putAttempt = POST('http://hackathon.siim.org/fhir/Organization',
                        add_headers('apikey' = Sys.getenv(x='SiimApiKey')),
                        body=data,
                        encode="raw")
      print(putAttempt)
      },
    error = function(err) {
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error",
                    anim = TRUE,
                    animType = "fade")
    },
    finally = {
      shinyjs::enable("submitOrg")
    })
  })
  
  observeEvent(input$practitionerSubmit, {
    # User-experience stuff
    shinyjs::disable("practitionerSubmit")
    
    # Save the data (show an error message in case of error)
    tryCatch({
      data <- c(formDataPractitioner())
      names(data) <- practitionerFields
      data[["resourceType"]] <- "Practitioner"
      data <- toJSON(data,auto_unbox =TRUE)
      
      putAttempt = POST('http://hackathon.siim.org/fhir/Practitioner',
                        add_headers('apikey' = Sys.getenv(x='SiimApiKey')),
                        body=data,
                        encode="raw")
      print(putAttempt)
e    },
    error = function(err) {
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error",
                    anim = TRUE,
                    animType = "fade")
    },
    finally = {
      shinyjs::enable("practitionerSubmit")
    })
  })
  observeEvent(input$DRSubmit, {
    # User-experience stuff
    shinyjs::disable("DRSubmit")
    shinyjs::hide("error")
    
    # Save the data (show an error message in case of error)
    tryCatch({
      data <- c(formDataDR())
      names(data) <- DRFields
      data[["subject"]] <- list("reference"=patient_id(),"type" = "Patient")
      data[["resourceType"]] <- "DiagnosticReport"
      data <- toJSON(data,auto_unbox =TRUE)
      print(data)
      
      putAttempt = POST('http://hackathon.siim.org/fhir/DiagnosticReport',
                        add_headers('apikey' = Sys.getenv(x='SiimApiKey')),
                        body=data,
                        encode="raw")
      print(putAttempt)
      shinyjs::reset("DRForm")
    },
    error = function(err) {
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error",
                    anim = TRUE,
                    animType = "fade")
    },
    finally = {
      shinyjs::enable("DRSubmit")
    })
  })
  
}

shinyApp(ui, server)