scatterPlotComponent <-
  function(input, output, session, filtered_data) {
    # UI components and server logic for the scatter plot
    ui <- page_fluid(
      plotOutput("plot"),
      h4("Plot customization"),
      layout_columns(
        selectInput("xcol", "X-Axis", choices = NULL),
        selectInput("ycol", "Y-Axis", choices = NULL),
        selectInput(
          "groupcol",
          "Colour By",
          choices = NULL,
          selected = "------"
        ),
        selectInput(
          "facetcol",
          "Facet By",
          choices = NULL,
          selected = "------"
        ),
      ),
      checkboxInput("smooth", "Add Trendline", value = FALSE),
      layout_columns(
        sliderInput(
          "alpha",
          "Point Alpha",
          min = 0,
          max = 1,
          value = 1,
          step = 0.1
        ),
        sliderInput(
          "pointsize",
          "Point Size",
          min = 0,
          max = 10,
          value = 3,
          step = 0.5
        ),
        sliderInput(
          "labletextsize",
          "Label Size",
          min = 8,
          max = 20,
          value = 12,
          step = 1
        ),
        sliderInput(
          "ticktextsize",
          "Tick Size",
          min = 8,
          max = 20,
          value = 12,
          step = 1
        )
      )
    )

    # Reactive value to check if the dropdowns have been initialized
    values <- reactiveValues(colnames = NULL)

    observe({
      df <- filtered_data()
      colnames <- names(df)
      if (!is.null(df) && !all(colnames %in% values$colnames)) {
        updateSelectInput(session,
                          "xcol",
                          choices = colnames,
                          selected = colnames[1])
        updateSelectInput(session,
                          "ycol",
                          choices = colnames,
                          selected = colnames[2])
        updateSelectInput(session, "groupcol", choices = c("------", colnames))
        updateSelectInput(session, "facetcol", choices = c("------", colnames))
        values$colnames <- colnames
      }
    })

    output$plot <- renderPlot({
      req(filtered_data())  # Ensure that the reactive data object is available
      df <- filtered_data()

      # Check that the x and y columns are selected
      if (is.null(input$xcol) || is.null(input$ycol))
        return(NULL)

      # Start the plot with x and y axis
      p <- ggplot(df, aes_string(x = input$xcol, y = input$ycol))

      # Add group color aesthetic if a group column is selected
      if (input$groupcol != "------" && input$groupcol != "") {
        p <- p + aes_string(color = input$groupcol)
      }

      # Add faceting if a facet column is selected
      if (input$facetcol != "------" && input$facetcol != "") {
        p <- p + facet_wrap( ~ get(input$facetcol))
      }

      # Add a smooth line if the checkbox is checked
      if (input$smooth) {
        p <- p + geom_smooth(formula = y ~ x, method = "lm")
      }

      # Add points with customized alpha and size
      p <- p + geom_point(alpha = input$alpha, size = input$pointsize)

      # Customize the plot labels and tick text sizes
      p <-
        p + labs(x = input$xcol, y = input$ycol) + theme_minimal() +
        theme(
          text = element_text(size = input$labletextsize),
          # Set general text size for labels
          axis.text.x = element_text(size = input$ticktextsize),
          # X axis ticks
          axis.text.y = element_text(size = input$ticktextsize)   # Y axis ticks
        )

      # Return the final plot
      return(p)
    })


    return(ui)
  }
