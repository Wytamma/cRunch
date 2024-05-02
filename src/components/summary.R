summaryTableComponent <- function(input, output, session, filtered_data) {
  # Define the UI elements for the numeric and character data tables
  ui <- fluidPage(
    layout_columns(
      value_box(
        title = "Rows",
        value = textOutput("number_rows"),
        showcase = bs_icon("body-text"),
        p("Number of Rows"),
      ),
      value_box(
        title = "Columns",
        value = textOutput("number_cols"),
        showcase = bs_icon("bar-chart"),
        p("Number of Columns"),
      ),
      value_box(
        title = "Missing",
        value = textOutput("number_missing"),
        showcase = bs_icon("x-circle"),
        p("Number of Missing Values"),
      )
    ),
    h3("Summary Statistics for Numeric Variables"),
    DT::dataTableOutput("numericSummary"),
    tags$br(),
    h3("Characteristics of Categorical Variables"),
    DT::dataTableOutput("characterSummary"),
    tags$br()
  )

  # Server logic for numeric summaries
  output$numericSummary <- DT::renderDataTable({
    df <- filtered_data()
    numeric_columns <- sapply(df, is.numeric)
    numeric_df <- df[, numeric_columns, drop = FALSE]
    numeric_summary <- data.frame(t(sapply(numeric_df, summary)))
    colnames(numeric_summary) <- c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max")
    rownames(numeric_summary) <- colnames(numeric_df)
    DT::datatable(numeric_summary, options = list(scrollX = TRUE, paging = FALSE, searching = FALSE, info = FALSE))
  })

  # Server logic for character and factor summaries
  output$characterSummary <- DT::renderDataTable({
    df <- filtered_data()
    # Treat factors as characters
    df[] <- lapply(df, function(x) if(is.factor(x)) as.character(x) else x)
    character_columns <- sapply(df, is.character)
    character_df <- df[, character_columns, drop = FALSE]
    # Update default column names based on actual character columns
    col_names <- names(character_df)
    # Safely calculate character summaries with error handling
    safe_character_summaries <- tryCatch({
      character_summaries <- lapply(names(character_df), function(col) {
        unique_values <- unique(character_df[[col]])
        most_common <- names(sort(table(character_df[[col]]), decreasing = TRUE))[1]
        count_most_common <- max(table(character_df[[col]]))
        total_unique <- length(unique_values)
        data.frame(
          MostCommonValue = most_common,
          FrequencyOfMostCommon = count_most_common,
          TotalUniqueValues = total_unique
        )
      })
      all_character_summaries <- do.call(rbind, character_summaries)
      rownames(all_character_summaries) <- col_names
      all_character_summaries
    }, error = function(e) {
      # Return an empty data frame with the correct structure and row names
      empty_df <- data.frame(
        MostCommonValue = NA_character_,
        FrequencyOfMostCommon = NA_integer_,
        TotalUniqueValues = NA_integer_
      )
      # Set the correct number of rows based on expected column names
      empty_df <- empty_df[rep(1, length(col_names)), , drop = FALSE]
      rownames(empty_df) <- col_names
      empty_df
    })

    # Render the DataTable
    DT::datatable(safe_character_summaries, options = list(scrollX = TRUE, paging = FALSE, searching = FALSE, info = FALSE))
  })


  # Reactive output for number of rows
  output$number_rows <- renderText({
    df <- filtered_data()
    nrow(df)
  })

  # Reactive output for number of columns
  output$number_cols <- renderText({
    df <- filtered_data()
    ncol(df)
  })

  # Reactive output for number of missing values
  output$number_missing <- renderText({
    df <- filtered_data()
    sum(is.na(df))
  })

  return(ui)
}
