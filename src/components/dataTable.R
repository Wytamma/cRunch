dataTableComponent <- function(input, output, session, filtered_data) {
  # Define the UI elements for the data table
  ui <- DT::dataTableOutput("table")

  # Server logic to render the data table and calculate stats
  output$table <- DT::renderDataTable({
    df <- filtered_data()  # Ensure the reactive data is fetched
    DT::datatable(df, options = list(scrollX = TRUE, paging = TRUE))
  })

  return(ui)
}
