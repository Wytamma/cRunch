boxPlotComponent <-
  function(input, output, session, filtered_data) {
    # UI components and server logic for the box plot
    ui <- page_fluid(
      plotOutput("boxPlot"),
      h4("Box Plot customization"),
      layout_columns(
        selectInput("boxCol", "Column", choices = NULL),
        selectInput(
          "boxGroupCol",
          "Colour By",
          choices = NULL,
          selected = "------"
        ),
        selectInput(
          "boxFacetCol",
          "Facet By",
          choices = NULL,
          selected = "------"
        ),
      ),
      checkboxInput("boxOutlier", "Show Outliers", value = TRUE),
      layout_columns(
        sliderInput(
          "boxLabelSize",
          "Label Size",
          min = 8,
          max = 20,
          value = 12,
          step = 1
        ),
        sliderInput(
          "boxTickSize",
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
        updateSelectInput(session, "boxCol", choices = colnames, selected = colnames[1])
        updateSelectInput(session, "boxGroupCol", choices = c("------", colnames))
        updateSelectInput(session, "boxFacetCol", choices = c("------", colnames))
        values$colnames <- colnames
      }
    })

    output$boxPlot <- renderPlot({
      req(filtered_data())  # Ensure that the reactive data object is available
      df <- filtered_data()

      # Start the plot with x and y axis
      p <- ggplot(df, aes_string(y = input$boxCol))

      # Add group color aesthetic if a group column is selected
      if (input$boxGroupCol != "------" && input$boxGroupCol != "") {
        p <- p + aes_string(color = input$boxGroupCol)
      }

      # Add faceting if a facet column is selected
      if (input$boxFacetCol != "------" && input$boxFacetCol != "") {
        p <- p + facet_wrap( ~ get(input$boxFacetCol))
      }

      # Add boxplot
      p <- p + geom_boxplot(outlier.shape = ifelse(input$boxOutlier, 1, NA))

      # Customize the plot labels and tick text sizes
      p <-
        p + labs(y = input$boxCol) + theme_minimal() +
        theme(
          text = element_text(size = input$boxLabelSize),
          # X axis ticks
          axis.text.y = element_text(size = input$boxTickSize),   # Y axis ticks
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()
        )

      # Return the final plot
      return(p)
    })

    return(ui)
  }
