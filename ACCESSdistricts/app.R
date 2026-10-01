library(shiny)

# 1. User Interface (UI)
ui <- navbarPage(
  title = "Multi-Page Data Downloader",
  
  # Page 1: Motor Trend Car Road Tests
  tabPanel(
    title = "MT Cars Data",
    fluidPage(
      h3("Explore and Download mtcars Dataset"),
      sidebarLayout(
        sidebarPanel(
          downloadButton(outputId = "downloadMtcars", label = "Download CSV")
        ),
        mainPanel(
          tableOutput(outputId = "mtcarsTable")
        )
      )
    )
  ),
  
  # Page 2: Iris Flower Dataset
  tabPanel(
    title = "Iris Data",
    fluidPage(
      h3("Explore and Download iris Dataset"),
      sidebarLayout(
        sidebarPanel(
          downloadButton(outputId = "downloadIris", label = "Download CSV")
        ),
        mainPanel(
          tableOutput(outputId = "irisTable")
        )
      )
    )
  )
)

# 2. Server Logic
server <- function(input, output, session) {
  
  # --- Page 1 Server Logic ---
  output$mtcarsTable <- renderTable({
    head(mtcars)
  })
  
  output$downloadMtcars <- downloadHandler(
    filename = function() {
      paste("mtcars-data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(mtcars, file, row.names = FALSE)
    }
  )
  
  # --- Page 2 Server Logic ---
  output$irisTable <- renderTable({
    head(iris)
  })
  
  output$downloadIris <- downloadHandler(
    filename = function() {
      paste("iris-data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(iris, file, row.names = FALSE)
    }
  )
}

# 3. Run the Application
shinyApp(ui = ui, server = server)