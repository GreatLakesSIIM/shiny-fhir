library("shinydashboard")
library("shiny")
library("shinyjs")
library("DT")
require("httr")
require("stringi")
require("jsonlite")
library("listviewer")

jscode <- "shinyjs.refresh = function() { location.reload(); }"

all_ofResourceType_json <- function(resourceType){
  json <- jsonlite::fromJSON(content(GET(paste0('http://hackathon.siim.org/fhir/',resourceType),
                                         accept_json(),
                                         add_headers('apikey' = "428c400d-5f92-40be-9bf5-f27cc8a3e483",
                                                     'Content-Type' = 'application/fhir+json')
  ),"text"),
  flatten=TRUE)
  return(json)
}

patient_json <- function(patientId){
  json <- jsonlite::fromJSON(content(GET(paste0('http://hackathon.siim.org/fhir/Patient/',patientId),
                                         accept_json(),
                                         add_headers('apikey' = "428c400d-5f92-40be-9bf5-f27cc8a3e483")
  ),
  "text"),
  flatten=TRUE)
  return(json)
}

post_data <- function(resourceType, data){
  POST(paste0('http://hackathon.siim.org/fhir/',resourceType),
       add_headers('apikey' = "428c400d-5f92-40be-9bf5-f27cc8a3e483",
                   'Content-Type' = 'application/fhir+json'),
       body=data,
       encode="text")
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
  name = list(HumanNameFields),
  contact = list(ContactFields),
  address = list(AddressFields),
  "F",
  "2000-08-12",
  "communication"
)
practitionerFields <- c(
  name = list(HumanNameFields),
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
    "generalPractitioner",
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
                 fluidPage(
                   title = "DR form example",
                   fluidRow(
                     column(
                       6,
                       div(
                         id = "DRForm",
                         textInput("category", ("Category"), DRDefaults[["category"]]), # https://www.hl7.org/fhir/v2/0074/index.html
                         textInput("code", ("Code"), DRDefaults[["code"]]), # http://hl7.org/fhir/ValueSet/report-codes
                         selectInput(inputId="patientId",
                                     label="Subject", 
                                     choices=all_ofResourceType_json("Patient")$entry$resource.id,
                                     selected=all_ofResourceType_json("Patient")$entry$resource.id[1]),
                         
                         selectInput(inputId="performer",
                                     label=labelMandatory("Practitioner"), 
                                     choices=all_ofResourceType_json("Practitioner")$entry$resource.id,
                                     selected=all_ofResourceType_json("Practitioner")$entry$resource.id[1]),
                         
                         selectInput(inputId="resultsInterpreter",
                                     label=labelMandatory("Results Interpreter"), 
                                     choices=all_ofResourceType_json("Practitioner")$entry$resource.id,
                                     selected=all_ofResourceType_json("Practitioner")$entry$resource.id[1]),
                         
                         selectInput(inputId="specimen",
                                     label=labelMandatory("Specimen"), 
                                     choices=all_ofResourceType_json("Specimen")$entry$resource.id,
                                     selected=all_ofResourceType_json("Specimen")$entry$resource.id[1]),
                         
                         selectInput(inputId="result",
                                     label=labelMandatory("Result/Observation"), 
                                     choices=all_ofResourceType_json("Observation")$entry$resource.id,
                                     selected=all_ofResourceType_json("Observation")$entry$resource.id[1]),
                         
                         selectInput(inputId = "imagingStudy", 
                                     label = labelMandatory("Imaging Study"), 
                                     choices = all_ofResourceType_json("ImagingStudy")$entry$resource.id,
                                     selected = all_ofResourceType_json("ImagingStudy")$entry$resource.id[1]),
                         
                         
                         textInput("conclusion", labelMandatory("Conclusion"), DRDefaults[["conclusion"]]),
                         
                         selectInput("conclusionCode", 
                                     labelMandatory("Conclusion Code"),
                                     choices=c('1648002','2713001'), #SNOMED CT Codes
                                     selected=c('1648002','2713001')[1]
                                     #choices=GET('http://hl7.org/fhir/ValueSet/clinical-findings')
                         ),
                         
                         actionButton("DRSubmit", "Submit", class = "btn-primary")
                       )
                     )
                   )
                 )
)

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
                                      "generalPractitioner",
                                      labelMandatory("Practitioner"),
                                      choices=all_ofResourceType_json("Practitioner")$entry$resource.id,
                                      selected = all_ofResourceType_json("Practitioner")$entry$resource.id[1]
                                    ),
                                    selectInput(
                                      "managingOrganization",
                                      labelMandatory("Organization"),
                                      choices=all_ofResourceType_json("Organization")$entry$resource.name,
                                      selected = all_ofResourceType_json("Organization")$entry$resource.name[1]
                                    ),
                                    
                                    actionButton("submitPat", "Submit", class = "btn-primary"),
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
      "Data Table", 
      tabName = "dataTable", 
      icon = icon("th")
    ),
    menuItem(
      "Upload Data",
      tabName="upload",
      icon=icon("th")
    ),
    menuItem(
      "Patient",
      tabName = "patient",
      icon = icon("th")
    ),
    menuItem(
      "Diagnostic Report", 
      tabName = "DR", 
      icon = icon("th")
    ),
    menuItem(
      "Practitioner", 
      tabName = "practitioner", 
      icon = icon("th")
    ),
    menuItem(
      "Organization", 
      tabName = "organization", 
      icon = icon("th")
    )
  )),
  dashboardBody(
    tags$head(tags$style(HTML('.main-header .logo {
                              font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;
                              font-size: 24px;
                              }
                              '))),
    tabItems(
      tabItem(tabName = "dataTable",
              fluidRow(
                box(
                  title = "Patient Data",
                  width = 12,
                  selectInput(inputId="organization",
                              label="Organization:",
                              choices=all_ofResourceType_json("Organization")$entry$resource.name,
                              selected = all_ofResourceType_json("Organization")$entry$resource.name[1]),
                  selectInput(inputId="patientId",
                              label="Patient Id:",
                              choices=all_ofResourceType_json("Patient")$entry$resource.id,
                              selected=all_ofResourceType_json("Patient")$entry$resource.id[1]),
                  DTOutput("patientInfoTable")
                )
              )),
      tabItem(tabName = "upload",
              fluidRow(
                box(
                  fileInput("jsonFile", "Choose JSON File", accept = c(".json")),
                  actionButton("uploadJson","Upload")
                )
              )
      ),
      patientTab,
      orgTab,
      practitionerTab,
      DRTab
    )
  )
)


server <- function(input, output, session) {
  
  output$userpanel <- renderUI({
    # session$user is non-NULL only in authenticated sessions
    if (is.null(session$user)) {
      sidebarUserPanel(
        span("Logged in as Dr. House"),
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
  
  # reactive patient data
  patient_id <- reactive({input$patientId})
  
  output$patientInfoTable <- renderDT({
    pat <- patient_json(patient_id())
    print(pat)
    if(is.null(pat$extension)){
      return(as.data.frame(pat))
    }
    pat$extension = NULL
    pat$identifier = NULL
    return(as.data.frame(pat))
  }, options=list(scrollX=TRUE))
  
  formData <- reactive({
    data <- sapply(fieldsAll, function(x)
      input[[x]])
    data <- c(data, timestamp = epochTime())
    data <- t(data)
    data
  })
  
  formDataPractitioner <- reactive({
    data <- sapply(practitionerFields, function(x) input[[x]])
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
  
  observeEvent(input$uploadJson,{
    shinyjs::disable('uploadJson')
    
    tryCatch({
      inFile <- input$jsonFile
      if(is.null(inFile)){
        return(NULL)
      }
      print(inFile$datapath)
      print("Posting Data...")
      json_file <- upload_file(path=inFile$datapath)
      resp <- POST('http://hackathon.siim.org/fhir/',
                   add_headers(
                     'apikey' = "428c400d-5f92-40be-9bf5-f27cc8a3e483",
                     'Content-Type' = 'application/fhir+json'),
                   body=json_file)
      
      print(resp)
      print("Finished Posting Data")
      
      #refresh selectInput objects
      patient_resource <- all_ofResourceType_json("Patient")$entry$resource.id
      updateSelectInput(session,
                        inputId="patientId",
                        label="Patient Id:",
                        choices=patient_resource,
                        selected=patient_resource[1])
      updateSelectInput(session,
                        inputId="patientId",
                        label="Subject", 
                        choices=patient_resource,
                        selected=patient_resource[1])
    },
    error = function(err){
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error",
                    anim = TRUE,
                    animType = "fade")
    },
    finally = {
      shinyjs::enable("uploadJson")
    })
  })
  
  observeEvent(input$submitPat, {
    # User-experience stuff
    shinyjs::disable("submitPat")
    shinyjs::hide("error")
    
    # Save the data (show an error message in case of error)
    tryCatch({
      data <- c(formData())
      names(data) <- fieldsAll
      data[["resourceType"]] <- "Patient"
      data <- jsonlite::toJSON(data,auto_unbox =TRUE)
      
      putAttempt <- post_data('Patient',data)
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
      shinyjs::enable("submitPat")
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
  
  formDataOrg <- reactive({
    data <- sapply(orgFields, function(x)
      input[[x]])
    data <- c(data, timestamp = epochTime())
    data <- t(data)
    data
  })
  observeEvent(input$submitOrg, {
    # User-experience stuff
    shinyjs::disable("submitOrg")
    
    tryCatch({
      data <- c(formDataOrg())
      names(data) <- orgFields
      data[["resourceType"]] <- "Organization"
      data <- jsonlite::toJSON(data,auto_unbox =TRUE)
      print(data)
      
      putAttempt <- post_data('Organization',data)
      print(putAttempt)
      
      org_resource_names <- all_ofResourceType_json("Organization")$entry$resource.name
      
      updateSelectInput(session,
                        "managingOrganization",
                        labelMandatory("Organization"),
                        choices=org_resource_names,
                        selected = org_resource_names[1])
      updateSelectInput(session,
                        inputId="organization",
                        label="Organization:",
                        choices=org_resource_names,
                        selected = org_resource_names[1])
    },
    error = function(err) {
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error",
                    anim = TRUE,
                    animType = "fade")
    },
    finally = {
      shinyjs::reset("orgForm")
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
      data <- jsonlite::toJSON(data,auto_unbox =TRUE)
      
      putAttempt <- post_data('Practitioner',data)
      print(putAttempt)
    },
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
      data <- jsonlite::toJSON(data,auto_unbox =TRUE)
      print(data)
      
      putAttempt <- post_data('DiagnosticReport',data)
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