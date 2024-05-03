histogramPlotComponent <-
  function(input, output, session, filtered_data) {
    # UI components and server logic for the histogram plot
    ui <- page_fluid(
      plotOutput("histPlot"),
      h4("Histogram customization"),
      layout_columns(
        selectInput("histCol", "Column", choices = NULL),
        selectInput(
          "histGroupCol",
          "Colour By",
          choices = NULL,
          selected = "------"
        ),
        selectInput(
          "histFacetCol",
          "Facet by",
          choices = NULL,
          selected = "------"
        ),
      ),
      checkboxInput("histDensity", "Add Density Line", value = FALSE),
      checkboxInput("histShowBars", "Show Histogram Bars", value = TRUE),
      layout_columns(
        sliderInput(
          "histBins",
          "Number of Bins",
          min = 1,
          max = 50,
          value = 30,
          step = 1
        ),
        sliderInput(
          "histLabelSize",
          "Label Size",
          min = 8,
          max = 20,
          value = 12,
          step = 1
        ),
        sliderInput(
          "histTickSize",
          "Tick Size",
          min = 8,
          max = 20,
          value = 12,
          step = 1
        )
      )
    )

    # Reactive value to initialize and update dropdowns
    values <- reactiveValues(colnames = NULL)

    observe({
      df <- filtered_data()
      colnames <- names(df)
      if (!is.null(df) && !all(colnames %in% values$colnames)) {
        updateSelectInput(session, "histCol", choices = colnames, selected = colnames[1])
        updateSelectInput(session, "histFacetCol", choices = c("------", colnames))
        updateSelectInput(session, "histGroupCol", choices = c("------", colnames))
        values$colnames <- colnames
      }
    })

    output$histPlot <- renderPlot({
      req(filtered_data())  # Ensure that the reactive data object is available
      df <- filtered_data()

      # Check that the column is selected
      if (is.null(input$histCol))
        return(NULL)

      # Start the plot with selected column
      p <- ggplot(df, aes_string(x = input$histCol))

      # Add group color aesthetic if a group column is selected
      if (input$histGroupCol != "------" && input$histGroupCol != "") {
        p <- p + aes_string(fill = input$histGroupCol)
      }

      # Add histogram with customized number of bins and toggle visibility
      if (input$histShowBars) {
        p <- p + geom_histogram(bins = input$histBins)
      }

      # Add density line if checked
      if (input$histDensity) {
        p <- p + geom_density(alpha = .2)
      }

      # Add faceting if selected
      if (input$histFacetCol != "------" && input$histFacetCol != "") {
        p <- p + facet_wrap(~get(input$histFacetCol))
      }

      # Customize the plot labels and tick text sizes
      p <-
        p + labs(x = input$histCol, y = "Frequency") + theme_minimal() +
        theme(
          text = element_text(size = input$histLabelSize),
          axis.text.x = element_text(size = input$histTickSize),
          axis.text.y = element_text(size = input$histTickSize)
        )

      # Return the final plot
      return(p)
    })

    return(ui)
  }
