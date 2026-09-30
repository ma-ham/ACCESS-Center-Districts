library(shiny)

# 1. User Interface (UI)
ui <- fluidPage(
  titlePanel("Data Dashboard & Downloader"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filter Data"),
      # Dropdown to filter by Species
      selectInput(
        inputId = "species_select", 
        label = "Select Species:", 
        choices = c("All", unique(as.character(iris$Species))),
        selected = "All"
      ),
      
      hr(),
      h4("Export Options"),
      # The download button element
      downloadButton(outputId = "download_data", label = "Download Filtered Data (CSV)")
    ),
    
    mainPanel(
      h3("Filtered Dataset Preview"),
      # Placeholder to display the data table
      tableOutput("data_table")
    )
  )
)

# 2. Server Logic
server <- function(input, output, session) {
  
  # Reactive expression to dynamically filter data based on user input
  filtered_data <- reactive({
    data <- iris
    if (input$species_select != "All") {
      data <- data[data$Species == input$species_select, ]
    }
    return(data)
  })
  
  # Render the preview table to the UI
  output$data_table <- renderTable({
    filtered_data()
  })
  
  # Handle the data download process
  output$download_data <- downloadHandler(
    filename = function() {
      # Sets the default file name with the chosen filter and timestamp
      paste0("filtered_iris_", tolower(input$species_select), "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # Writes the reactive data frame into the temporary file path provided by Shiny
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

# 3. Run the Application
shinyApp(ui = ui, server = server)