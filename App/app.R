#introduction to shiny dashboard

library("shinydashboard")
library("shiny")

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
                box(plotOutput("Plot1", height = 300)),
                box(
                  title = "Controls", 
                  sliderInput("slider", "Number of Observations:", 1, 100, 50)
                )
              )
      ),
      #2nd tab
      tabItem(tabName = "widgets",
              h2("Widgets tab content"))
      
    )
  )
)

server <- function(input, output){
  set.seed(324)
  histdata <- rnorm(400)
  output$Plot1 <-renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)