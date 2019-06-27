# SIIM Shiny Dashboard

library("shinydashboard")
library("shiny")
library("shinyjs")
library("DT")
require("httr")
require("jsonlite")
library("listviewer")


DRDefaults <- c(
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
  "prefix",
  "given",
  "family",
  "suffix",
  "phone",
  "email",
  "use",
  "line",
  "city",
  "state",
  "postalCode",
  "country",
  "sex",
  "dob",
  "communication"
)
practitionerFields <- c(
  "prefix",
  "given",
  "family",
  "suffix",
  "phone",
  "email",
  "use",
  "line",
  "city",
  "state",
  "postalCode",
  "country",
  "sex",
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
  "prefix",
  "given",
  "family",
  "suffix",
  "phone",
  "email",
  "use",
  "line",
  "city",
  "state",
  "postalCode",
  "country",
  "partOf"
)
orgFields <- c(
  "type",
  "name",
  "alias",
  "prefix",
  "given",
  "family",
  "suffix",
  "phone",
  "email",
  "use",
  "line",
  "city",
  "state",
  "postalCode",
  "country",
  "partOf"
)
names(orgDefaults) <- orgFields

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
DRTab <- tabItem(tabName = "DR",
                           fluidPage(title = "DR form example",
                                     fluidRow(column(
                                       6,
                                       div(
                                         id = "DRForm",
                                         textInput("category", ("Category"), DRDefaults[["category"]]),
                                         textInput("code", ("Code"), DRDefaults[["code"]]),
                                         textInput("subject", ("Subject"), DRDefaults[["subject"]]),
                                         
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
                                         textInput("prefix", ("Prefix"), practitionerDefaults[["prefix"]]),
                                         textInput("given", labelMandatory("First Name"), practitionerDefaults[["given"]]),
                                         textInput("family", labelMandatory("Last Name"), practitionerDefaults[["family"]]),
                                         textInput("suffix", ("Suffix"), practitionerDefaults[["suffix"]]),
                                         
                                         numericInput(
                                           "phone",
                                           labelMandatory("Phone Number"),
                                           practitionerDefaults[["phone"]]
                                         ),
                                         textInput("email", labelMandatory("Email"), practitionerDefaults[["email"]]),
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
                                         
                                         textInput(
                                           "line",
                                           labelMandatory("Street Address"),
                                           practitionerDefaults[["line"]]
                                         ),
                                         textInput("city", labelMandatory("City"), practitionerDefaults[["city"]]),
                                         textInput("state", labelMandatory("State"), practitionerDefaults[["state"]]),
                                         textInput(
                                           "postalCode",
                                           labelMandatory("postalCode"),
                                           practitionerDefaults[["postalCode"]]
                                         ),
                                         textInput("country", labelMandatory("Country"), practitionerDefaults[["country"]]),
                                         
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
                                textInput("prefix", ("Prefix"), orgDefaults[["prefix"]]),
                                textInput("given", labelMandatory("First Name"), orgDefaults[["given"]]),
                                textInput("family", labelMandatory("Last Name"), orgDefaults[["family"]]),
                                textInput("suffix", ("Suffix"), orgDefaults[["suffix"]]),
                                
                                numericInput("phone", labelMandatory("Phone Number"), orgDefaults[["phone"]]),
                                textInput("email", labelMandatory("Email"), orgDefaults[["email"]]),
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
                                textInput("line", labelMandatory("Street Address"), orgDefaults[["line"]]),
                                textInput("city", labelMandatory("City"), orgDefaults[["city"]]),
                                textInput("state", labelMandatory("State"), orgDefaults[["state"]]),
                                textInput("postalCode", labelMandatory("postalCode"), orgDefaults[["postalCode"]]),
                                textInput("country", labelMandatory("Country"), orgDefaults[["country"]]),
                                
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
                                    
                                    actionButton("submit", "Submit", class = "btn-primary"),
                                    actionButton("addDR", "Add Diagnostic Report", class = "btn-primary"),
                                    actionButton("addObservation", "Add Observation", class = "btn-primary"),
                                    actionButton("addCondition", "Add Condition", class = "btn-primary"),
                                    actionButton("addImagingStudy", "Add Imaging Study", class = "btn-primary")
                                  )
                                ))))

#UI
ui <- dashboardPage(
  dashboardHeader(title = "SIIM Excitement"),

  dashboardSidebar(sidebarMenu(
    # Custom CSS to hide the default logout panel
    tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
    
    # The dynamically-generated user panel
    uiOutput("userpanel"),
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
      "Organization", 
      tabName = "organization", 
      icon = icon("th")
    ),
    menuItem(
      "Data Table", 
      tabName = "dataTable", 
      icon = icon("th")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "json",
              fluidRow(box(
                jsoneditOutput("edits"), width = 12
              ))),
      tabItem(tabName = "dataTable",
              fluidRow(
                box(
                  DT::dataTableOutput("patient"),
                  title = "Patient Data",
                  width = 12
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
)

server <- function(input, output, session) {
  output$userpanel <- renderUI({
    # session$user is non-NULL only in authenticated sessions
    if (is.null(session$user)) {
      sidebarUserPanel(
        span("Logged in as Dr. Barrington"),
        subtitle = a(icon("sign-out"), "Logout", href="https://twitter.com/logout")
      )
    }
  })
  patient_json <- fromJSON(content(GET('http://hackathon.siim.org/fhir/Patient',
                                       accept_json(),
                                       add_headers('apikey' = Sys.getenv(x='SiimApiKey'))),"text"),
                           flatten=TRUE)
  patientIdList <- patient_json$entry$resource.id
  
  output$selectUI <- renderUI({
    selectedpatientId <-
      selectInput("patientId",
                  labelMandatory("PatientId:"),
                  patientIdList,
                  selected = patientIdList[1])
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
    data <- sapply(DRFields, function(x)
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

  observeEvent(input$addDR, {
    # User-experience stuff
    shinyjs::disable("addDR")

    # Save the data (show an error message in case of error)
    tryCatch({
      updateTabsetPanel(formData(),selected="DR")
      shinyjs::reset("form")
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
    
    # Save the data (show an error message in case of error)
    tryCatch({
      saveData(formDataOrg())
      shinyjs::reset("orgForm")
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
      saveData(formDataPractitioner())
      shinyjs::reset("practitionerForm")
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

    # Save the data (show an error message in case of error)
    tryCatch({
      saveData(formDataDR())
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
      shinyjs::enable("DRSubmit")
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
                        
                        shinyjs::hidden(
                          span(id = "submit_msg", "Submitting..."),
                          div(id = "error",
                              div(
                                br(), tags$b("Error: "), span(id = "error_msg")
                              ))
                        )
                      )
                    ))))
  
  url <- "http://hackathon.siim.org/fhir/Patient/siimjoe"
  # dependency: local system variable called SiimApiKey
  myKey <- Sys.getenv('SiimApiKey')
  patientReturn <- GET(url,
                       accept_json(),
                       add_headers('apikey' = myKey))
  get_patient_text <- content(patientReturn, "text")
  get_patient_json <- fromJSON(get_patient_text, flatten = TRUE)
  # table of returned data
  patientData <- as.data.frame(get_patient_json)
  output$patient <- DT::renderDataTable({
    patientData[, c(2, 14:22)]
  })
  # interactive json editor of data
  output$edits <- renderJsonedit({
    jsonedit(
      as.list(patientData),
      "change" = htmlwidgets::JS(
        'function(){
                                  console.log( event.currentTarget.parentNode.editor.get() )
  }'
      )
    )
  })
}

shinyApp(ui, server)