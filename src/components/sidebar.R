sidebarComponent <- function(input, output, session, reactive_data) {
  # Define the UI elements for the data table
  ui <- div(
    h4("Load data"),
    fileInput(
      "file1",
      "Choose CSV File",
      multiple = FALSE,
      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
    ),
    tags$hr(),
    h4("Filter data"),
    useQueryBuilder(bs_version = "5"),
    uiOutput("query_builder"),
    downloadButton("downloadData", "Download Data"),
    tags$hr(),
    h4("Privacy"),
    p(
      "cRunch operates entirely on the client-side, meaning all computations are performed in your browser. Your data remains on your computer and is never transmitted to external servers. This ensures that cRunch is safe for handling sensitive data, as it maintains your privacy by not exposing your information beyond your local environment."
    )
    )

  query_filter <- reactive({
    df <- reactive_data()
    req(df)
    create_filters(df)
  })

  # Dynamic Query Builder UI
  output$query_builder <- renderUI({
    df <- reactive_data()
    queryBuilderInput("qb", filters = query_filter())
  })

  # Download handler for filtered data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("cRunch-", sub(" ", "T", as.character(Sys.time())), ".csv", sep =
              "")
    },
    content = function(file) {
      df_filtered <- filtered_data()  # Get the latest filtered data
      write.csv(df_filtered, file, row.names = FALSE)
    }
  )

  return(ui)
}

create_filters <- function(df) {
  # Check the type of each column and create corresponding filter
  filters <- lapply(names(df), function(col) {
    col_type <- class(df[[col]])
    if (any(col_type %in% c("factor", "character"))) {
      list(id = col,
           title = col,
           type = "string",
           value_separator=",")
    } else if (col_type %in% c("integer", "numeric", "double")) {
      list(
        id = col,
        title = col,
        type = "double",
        validation = list(
          min = min(df[[col]], na.rm = TRUE),
          max = max(df[[col]], na.rm = TRUE)
        ),
        step = 0.01
      )
    } else if (col_type %in% c("Date", "POSIXct")) {
      list(id = col,
           title = col,
           type = "date")
    } else {
      list(id = col,
           title = col,
           type = "string") # Default fallback
    }
  })

  return(filters)
}
