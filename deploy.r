# Deploy script for the Tax Dashboard

# Install required packages for deployment
if (!require(rsconnect)) install.packages("rsconnect")
library(rsconnect)

# Set your Shiny account information
# rsconnect::setAccountInfo(name="YOUR_ACCOUNT",
#                          token="YOUR_TOKEN",
#                          secret="YOUR_SECRET")

# Make sure database exists
if (!file.exists("tax_data.db")) {
  source("create_database.R")
  cat("Database created before deployment\n")
}

# Deploy the app (local testing)
cat("Starting local Shiny server...\n")
shiny::runApp()

# To deploy to shinyapps.io, uncomment:
rsconnect::deployApp(
appName = "tax-dashboard",
appTitle = "Tax Collection Analysis Dashboard"
 )