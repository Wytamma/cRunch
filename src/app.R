library(shiny)
library(jqbr)
library(ggplot2)

# Workaround for Chromium Issue 468227
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

ui <- fluidPage(
  # App title ----
  titlePanel("cRunch your data!"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      plotOutput("plot"),
      selectInput("xcol", "X-Axis", choices = NULL),
      selectInput("ycol", "Y-Axis", choices = NULL),
      selectInput("groupcol", "Group By", choices = NULL, selected = "------"),
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


server <- function(input, output, session) {
  # Reactive expression to hold the current dataframe
  reactive_data <- reactive({
    if (is.null(input$file1)) {
      return(iris)
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
  # Reactive expression for the filtered data
  filtered_data <- reactive({
    df <- reactive_data()
    if (is.null(input$qb)) return(df)  # Return unfiltered data if no filters are set
    filter_table(data = df, filters = input$qb)  # Assuming filter_table function applies the query filters
  })
  # Update the choices for xcol, ycol, and groupcol based on the uploaded file
  observe({
    df <- reactive_data()
    if (!is.null(df)) {
      colnames <- names(df)
      updateSelectInput(session, "xcol", choices = colnames, selected = colnames[1])
      updateSelectInput(session, "ycol", choices = colnames, selected = colnames[2])
      updateSelectInput(session, "groupcol", choices = c("------", colnames))
    }
  })

  # Generate plot based on input selections and filtered data
  output$plot <- renderPlot({
    req(filtered_data())  # Ensure the filtered data is available
    df <- filtered_data()
    if (is.null(input$xcol) || is.null(input$ycol)) return(NULL)
    p <- ggplot(df, aes_string(x = input$xcol, y = input$ycol))
    if (input$groupcol != "------" && input$groupcol != "") {
      p <- p + aes_string(color = input$groupcol)
    }
    p + geom_point() +
      labs(x = input$xcol, y = input$ycol) +
      theme_minimal()
  })

  # Render DataTable using filtered data
  output$table <- DT::renderDataTable({
    df_filtered <- filtered_data()
    DT::datatable(df_filtered, options = list(scrollX = TRUE, paging = TRUE))
  })

  # Dynamic Query Builder UI
  output$query_builder <- renderUI({
    df <- reactive_data()
    queryBuilderInput("qb", filters = create_filters(df))
  })

  # Download handler for filtered data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("cRunch-", sub(" ", "T", as.character(Sys.time())), ".csv", sep="")
    },
    content = function(file) {
      df_filtered <- filtered_data()  # Get the latest filtered data
      write.csv(df_filtered, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
