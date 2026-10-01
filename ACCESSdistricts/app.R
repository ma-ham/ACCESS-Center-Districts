library(shiny)
library(data.table) # Optimized for quick reading of large online files
library(DT)         # Better table formatting on multi-pages

# Define the live file URL from the Oregon Department of Education
LIVE_DATA_URL <- "https://www.ode.state.or.us/data/ReportCard/Media/DownloadFile?schlYr=24&fldr=stateData&flNm=AAGmediaSchoolsDisaggregated"

ui <- navbarPage("Oregon School Data Portal",
                 
                 # Page 1: Live Fetch & Raw Download
                 tabPanel("Data Synchronization",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Data Control Center"),
                              p("Click below to fetch the latest disaggregated school metrics directly from the Oregon Department of Education repository."),
                              actionButton("fetchBtn", "Fetch Live Data", class = "btn-primary"),
                              hr(),
                              uiOutput("downloadUI")
                            ),
                            mainPanel(
                              h3("Data Feed Preview"),
                              textOutput("statusText"),
                              br(),
                              tableOutput("rawPreview")
                            )
                          )
                 ),
                 
                 # Page 2: Filterable Records View
                 tabPanel("School Explorer",
                          sidebarLayout(
                            sidebarPanel(
                              uiOutput("countyFilterUI"),
                              uiOutput("groupFilterUI")
                            ),
                            mainPanel(
                              h3("Disaggregated School Performance Table"),
                              DTOutput("explorerTable")
                            )
                          )
                 )
)

server <- function(input, output, session) {
  
  # Reactive storage for our live dataset
  liveDataset <- reactiveVal(NULL)
  
  # Fetch data from the URL when button is pressed
  observeEvent(input\$fetchBtn, {
    showNotification("Downloading live data from Oregon State servers...", type = "message", duration = 5)
    
    tryCatch({
      # fread handles external HTTP URLs natively and efficiently
      downloadedData <- fread(LIVE_DATA_URL)
      liveDataset(downloadedData)
      showNotification("Data successfully loaded!", type = "default", duration = 3)
    }, error = function(e) {
      showNotification(paste("Error downloading data:", e\$message), type = "error", duration = NULL)
    })
  })
  
  # Informative status text
  output\$statusText <- renderText({
    if (is.null(liveDataset())) {
      "No data loaded yet. Please click 'Fetch Live Data' on the sidebar panel."
    } else {
      paste("Successfully synchronized", nrow(liveDataset()), "rows of school data records.")
    }
  })
  
  # Preview the first 10 rows on Page 1
  output\$rawPreview <- renderTable({
    req(liveDataset())
    head(liveDataset(), 10)
  })
  
  # Render the download button only when data exists
  output\$downloadUI <- renderUI({
    req(liveDataset())
    downloadButton("downloadCSV", "Export Local Copy (CSV)", class = "btn-success")
  })
  
  # Handle downloading the dynamically fetched data locally
  output\$downloadCSV <- downloadHandler(
    filename = function() {
      paste0("Oregon_School_Data_Export_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(liveDataset(), file, row.names = FALSE)
    }
  )
  
  # Dynamic UI Filters for Page 2
  output\$countyFilterUI <- renderUI({
    req(liveDataset())
    selectInput("selectedCounty", "Filter by County:", 
                choices = c("All", sort(unique(liveDataset()\$County))))
  })
  
  output\(groupFilterUI <- renderUI({     req(liveDataset())     selectInput("selectedGroup", "Filter by Student Group:",                  choices = c("All", sort(unique(liveDataset()\)`Student Group`))))
  })
  
  # Filtered dataset processing for Page 2 Explorer
  processedData <- reactive({
    req(liveDataset())
    df <- liveDataset()
    
    if (!is.null(input\(selectedCounty) && input\)selectedCounty != "All") {
      df <- df[County == input\$selectedCounty]
    }
    if (!is.null(input\(selectedGroup) && input\)selectedGroup != "All") {
      df <- df[`Student Group` == input\$selectedGroup]
    }
    return(df)
  })
  
  # Render the data explorer table using DT for pagination and searching
  output\$explorerTable <- renderDT({
    req(processedData())
    datatable(processedData(), options = list(pageLength = 15, scrollX = TRUE))
  })
}

shinyApp(ui, server)