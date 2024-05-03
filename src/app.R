library(shiny)
library(jqbr)
library(ggplot2)
library(bslib)
library(bsicons)
library(tools)

# components
source("components/dataTable.R")
source("components/summary.R")
source("components/scatterPlot.R")
source("components/histPlot.R")
source("components/boxPlot.R")
source("components/sidebar.R")


# Workaround for Chromium Issue 468227
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

ui <- page_navbar(
  title = "cRunch!",
  theme = bs_theme(preset = "minty"),
  tags$head(
    tags$style(HTML("
        html, body {
          height: 100vh !important;
        }
      "))
  ),
  sidebar = sidebar(uiOutput("sidebar"), width = 400),
  nav_panel("Summary", uiOutput("summary_panel")),
  nav_panel("Scatter", uiOutput("scatter_plot")),
  nav_panel("Histogram", uiOutput("hist_plot")),
  nav_panel("Box", uiOutput("box_plot")),
  nav_panel("Data", uiOutput("table_panel")),
  nav_spacer(),
  nav_item(
    tags$a(
      bs_icon("github"), "Code",
      href = "https://github.com/Wytamma/cRunch",
      target = "_blank"
    )
  )
)

server <- function(input, output, session) {
  # Reactive expression to hold the current dataframe
  reactive_data <- reactive({
    if (is.null(input$file1)) {
      return(iris)  # Default dataset if no file is uploaded
    } else {
      datapath <- input$file1$datapath
      file_extension <- tolower(file_ext(datapath))
    }

    # Choose the separator based on file extension
    separator <- ifelse(file_extension == "tsv", "\t", ",")

    tryCatch({
      df <- read.csv(
        datapath,
        header = TRUE,
        sep = separator  # Use the determined separator
      )
    }, error = function(e) {
      print(e)
      stop(safeError(e))
    })

    df
  })
  # Reactive expression for the filtered data
  filtered_data <- reactive({
    df <- reactive_data()
    if (is.null(input$qb))
      return(df)  # Return unfiltered data if no filters are set
    filter_table(data = df, filters = input$qb)  # Assuming filter_table function applies the query filters
  })


  output$sidebar <- renderUI({
    sidebarComponent(input, output, session, reactive_data)
  })


  # Render component
  output$table_panel <- renderUI({
    dataTableComponent(input, output, session, filtered_data)
  })

  output$summary_panel <- renderUI({
    summaryTableComponent(input, output, session, filtered_data)
  })

  output$scatter_plot <- renderUI({
    scatterPlotComponent(input, output, session, filtered_data)
  })

  output$hist_plot <- renderUI({
    histogramPlotComponent(input, output, session, filtered_data)
  })

  output$box_plot <- renderUI({
    boxPlotComponent(input, output, session, filtered_data)
  })

}

shinyApp(ui, server)
