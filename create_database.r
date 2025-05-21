# Install required packages if not already installed
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
if (!requireNamespace("RSQLite", quietly = TRUE)) install.packages("RSQLite")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")

# Load libraries
library(readxl)
library(dplyr)
library(tidyr)
library(RSQLite)
library(lubridate)
library(DBI)

# Function to read and process tax data from the Excel file
process_tax_file <- function(file_path = "collections_22_24.xlsx") {
  # Check if file exists
  if (!file.exists(file_path)) {
    stop(paste("File not found:", file_path))
  }
  
  cat("Reading file:", file_path, "\n")
  
  # Read data from Excel
  tax_data <- read_excel(file_path)
  
  # Check if data has expected structure with Year and Month columns
  if (!all(c("Year", "Month") %in% colnames(tax_data))) {
    stop("Excel file must contain 'Year' and 'Month' columns")
  }
  
  # Clean numeric columns - convert any character representations to numeric
  numeric_cols <- setdiff(colnames(tax_data), c("Year", "Month"))
  
  for (col in numeric_cols) {
    # Replace any comma-separated values and convert to numeric
    tax_data[[col]] <- as.numeric(gsub(",", "", as.character(tax_data[[col]])))
  }
  
  # Make sure Year and Month are correct types
  tax_data$Year <- as.integer(tax_data$Year)
  
  # If Month is numeric, convert to month name
  if (is.numeric(tax_data$Month[1]) || grepl("^\\d+$", as.character(tax_data$Month[1]))) {
    tax_data$Month <- month.abb[as.integer(tax_data$Month)]
  }
  
  # Create a Date column for time series analysis
  tax_data <- tax_data %>%
    mutate(
      # Create date from Year and Month (set to 1st day of month)
      Date = as.Date(paste(Year, match(Month, month.abb), "01", sep = "-"))
    ) %>%
    # Arrange by date
    arrange(Date)
  
  return(tax_data)
}

# Process tax data file
cat("Reading tax data file...\n")

# Use the specific file you mentioned
tax_data <- tryCatch({
  process_tax_file("collections_22_24.xlsx")
}, error = function(e) {
  cat("Error reading file:", e$message, "\n")
  cat("Please check that collections_22_24.xlsx exists in your working directory.\n")
  cat("Current working directory:", getwd(), "\n")
  stop(e)
})

# Print summary of the data
cat("Data summary:\n")
cat("- Years:", paste(unique(tax_data$Year), collapse = ", "), "\n")
cat("- Number of months:", nrow(tax_data), "\n")
cat("- Number of tax categories:", ncol(tax_data) - 3, "\n")  # Subtract Year, Month, Date columns

# Create SQLite database
cat("Creating database...\n")
con <- dbConnect(RSQLite::SQLite(), "tax_data.db")

# Create tables in the database
dbWriteTable(con, "tax_collections", tax_data, overwrite = TRUE)

# Create useful views
cat("Creating database views...\n")

# Yearly totals view
dbExecute(con, "
  CREATE VIEW IF NOT EXISTS yearly_totals AS
  SELECT Year, SUM(Total) as YearlyTotal
  FROM tax_collections
  GROUP BY Year
")

# Tax categories (dynamically get all columns except Year, Month, Date, Total)
tax_cols <- setdiff(colnames(tax_data), c("Year", "Month", "Date", "Total"))

# Function to create SQL for summing categories
sum_categories_sql <- function(categories) {
  paste0("SUM(", categories, ") as ", categories, "_Total", collapse = ",\n         ")
}

# Create view with yearly totals by category
cat_yearly_sql <- paste0("
  CREATE VIEW IF NOT EXISTS category_yearly_totals AS
  SELECT Year, 
         ", sum_categories_sql(tax_cols), "
  FROM tax_collections
  GROUP BY Year
")

# Try to create the view, but handle if it's too large
tryCatch({
  dbExecute(con, cat_yearly_sql)
}, error = function(e) {
  cat("Note: Couldn't create category_yearly_totals view - likely too many columns.\n")
  cat("Will use dynamic queries instead.\n")
})

# Monthly averages view
dbExecute(con, "
  CREATE VIEW IF NOT EXISTS monthly_averages AS
  SELECT Month, AVG(Total) as MonthlyAvg
  FROM tax_collections
  GROUP BY Month
")

# List tables and views in database
tables <- dbListTables(con)
cat("Created in database:", paste(tables, collapse = ", "), "\n")

# Close the connection
dbDisconnect(con)

cat("Database created successfully: tax_data.db\n")