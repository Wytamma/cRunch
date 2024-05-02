![](images/6c32010b-0d77-4404-a38c-1cd3676611a7.webp)
# cRunch

cRunch is a Shiny application designed for interactive data analysis and visualization. It allows users to upload CSV files, apply filters using a dynamic query builder, and visualize the data. Additionally, users can download the filtered dataset for offline analysis.

You can find a version of the app here ->
[https://crunch.wytamma.com/](https://crunch.wytamma.com/)

>[!important]
>cRunch operates entirely on the client-side, meaning all computations are performed in your browser. Your data remains on your computer and is never transmitted to external servers. This ensures that cRunch is safe for handling sensitive data, as it maintains your privacy by not exposing your information beyond your local environment.

<img  alt="image" src="https://github.com/Wytamma/cRunch/assets/13726005/95e0f814-a1e4-4f78-8f0d-779bba9b0e8e">

## Features
- **CSV File Upload**: Users can upload CSV files to be analyzed directly in the app.
- **Data Filtering**: A dynamic query builder allows users to specify filters based on the data columns.
- **Data Visualization**: Data is displayed in a scrollable and sortable table, making it easy to browse through even large datasets.
- **Data Download**: Users can download the filtered data as a CSV file, allowing for further analysis outside the app.

## Installation

To run cRunch locally, you need to have R and Shiny installed on your machine. Follow these steps to get started:

1. **Install R**:
   - Download and install R from [The Comprehensive R Archive Network (CRAN)](https://cran.r-project.org/).

2. **Install Shiny and Required Packages**:
   - Open R and install the Shiny package along with other required packages by running the following commands:
     ```R
     install.packages("shiny")
     install.packages("DT")
     install.packages("jqbr")
     ```

3. **Run the App**:
   - You can run the app by saving the script files `ui.R` and `server.R` in a directory and then running the following command in R:
     ```R
     library(shiny)
     runApp('path_to_app_directory')
     ```

## Usage

### Uploading Data
- Click the "Choose CSV File" button in the sidebar to upload your data file. Ensure your CSV file has the appropriate format, using commas, semicolons, or tabs as separators.

### Applying Filters
- After uploading the data, use the query builder in the sidebar to apply filters based on your data's specific attributes.

### Viewing and Downloading Data
- The main panel displays the data in a DataTable. You can sort and scroll through this table to review your data.
- Use the "Download Data" button to save the currently viewed (and filtered) data to your machine.

## Contributing
Contributions to cRunch are welcome! Please feel free to fork the repository, make changes, and submit pull requests. You can also open issues if you find bugs or have feature suggestions.

## License
This project is open-source and available under the [MIT License](https://opensource.org/licenses/MIT).
