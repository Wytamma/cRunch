scatterPlotComponent <-
  function(input, output, session, filtered_data) {
    # UI components and server logic for the scatter plot
    ui <- page_fluid(
      plotOutput("scatterPlot"),
      h4("Scatter Plot customization"),
      layout_columns(
        selectInput("scatterXcol", "X-Axis", choices = NULL),
        selectInput("scatterYcol", "Y-Axis", choices = NULL),
        selectInput(
          "scatterGroupCol",
          "Colour By",
          choices = NULL,
          selected = "------"
        ),
        selectInput(
          "scatterFacetCol",
          "Facet By",
          choices = NULL,
          selected = "------"
        ),
      ),
      checkboxInput("scatterSmooth", "Add Trendline", value = FALSE),
      layout_columns(
        sliderInput(
          "scatterAlpha",
          "Point Alpha",
          min = 0,
          max = 1,
          value = 1,
          step = 0.1
        ),
        sliderInput(
          "scatterPointSize",
          "Point Size",
          min = 0,
          max = 10,
          value = 3,
          step = 0.5
        ),
        sliderInput(
          "scatterLabelSize",
          "Label Size",
          min = 8,
          max = 20,
          value = 12,
          step = 1
        ),
        sliderInput(
          "scatterTickSize",
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
                          "scatterXcol",
                          choices = colnames,
                          selected = colnames[1])
        updateSelectInput(session,
                          "scatterYcol",
                          choices = colnames,
                          selected = colnames[2])
        updateSelectInput(session, "scatterGroupCol", choices = c("------", colnames))
        updateSelectInput(session, "scatterFacetCol", choices = c("------", colnames))
        values$colnames <- colnames
      }
    })

    output$scatterPlot <- renderPlot({
      req(filtered_data())  # Ensure that the reactive data object is available
      df <- filtered_data()

      # Check that the x and y columns are selected
      if (is.null(input$scatterXcol) || is.null(input$scatterYcol))
        return(NULL)

      # Start the plot with x and y axis
      p <- ggplot(df, aes_string(x = input$scatterXcol, y = input$scatterYcol))

      # Add group color aesthetic if a group column is selected
      if (input$scatterGroupCol != "------" && input$scatterGroupCol != "") {
        p <- p + aes_string(color = input$scatterGroupCol)
      }

      # Add faceting if a facet column is selected
      if (input$scatterFacetCol != "------" && input$scatterFacetCol != "") {
        p <- p + facet_wrap( ~ get(input$scatterFacetCol))
      }

      # Add a smooth line if the checkbox is checked
      if (input$scatterSmooth) {
        p <- p + geom_smooth(formula = y ~ x, method = "lm")
      }

      # Add points with customized alpha and size
      p <- p + geom_point(alpha = input$scatterAlpha, size = input$scatterPointSize)

      # Customize the plot labels and tick text sizes
      p <-
        p + labs(x = input$scatterXcol, y = input$scatterYcol) + theme_minimal() +
        theme(
          text = element_text(size = input$scatterLabelSize),
          axis.text.x = element_text(size = input$scatterTickSize),
          axis.text.y = element_text(size = input$scatterTickSize)
        )

      # Return the final plot
      return(p)
    })

    return(ui)
  }
