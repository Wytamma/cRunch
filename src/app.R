library(shiny)
library(jqbr)

# Workaround for Chromium Issue 468227
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

ui <- fluidPage(
  # App title ----
  titlePanel("cRunch - query your data!"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),

      # Horizontal line ----
      tags$hr(),


      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),

      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      downloadButton("downloadData", "Download Data")
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      DT::dataTableOutput("table"),
      useQueryBuilder(),
      uiOutput("query_builder")  # Dynamic UI for the query builder,
    )

  )
)

create_filters <- function(df) {
  # Check the type of each column and create corresponding filter
  filters <- lapply(names(df), function(col) {
    col_type <- class(df[[col]])

    if (col_type %in% c("factor", "character")) {
      list(id = col, title = col, type = "string")
    } else if (col_type %in% c("integer", "numeric", "double")) {
      list(id = col, title = col, type = "double",
           validation = list(min = min(df[[col]], na.rm = TRUE), max = max(df[[col]], na.rm = TRUE)),
           step = 0.01)
    } else if (col_type %in% c("Date", "POSIXct")) {
      list(id = col, title = col, type = "date")
    } else {
      list(id = col, title = col, type = "string") # Default fallback
    }
  })

  return(filters)
}


server <- function(input, output) {
  # Reactive expression to hold the current dataframe
  reactive_data <- reactive({
    if (is.null(input$file1)) {
      datapath <- "data/nextclade.csv"
    } else {
      datapath <- input$file1$datapath
    }

    tryCatch({
      df <- read.csv(datapath, header = TRUE, sep = input$sep, quote = input$quote)
    }, error = function(e) {
      print(e)
      stop(safeError(e))
    })

    df
  })

  # Render DataTable
  output$table <- DT::renderDataTable({
    df <- reactive_data()
    df_filtered <- filter_table(data = df, filters = input$qb)  # Assuming `filter_table` applies the filters
    DT::datatable(df_filtered, options = list(scrollX = TRUE, autoWidth = TRUE, paging = TRUE))
  })

  # Dynamic Query Builder UI
  output$query_builder <- renderUI({
    df <- reactive_data()
    queryBuilderInput("qb", filters = create_filters(df))
  })

  # Download handler for filtered data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered-data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      # Access the latest filtered data
      df_filtered <- data.table::copy(isolate({
        df <- reactive_data()
        filter_table(data = df, filters = input$qb)
      }))
      write.csv(df_filtered, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
