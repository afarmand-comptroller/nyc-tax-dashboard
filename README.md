
An interactive R Shiny dashboard for analyzing tax collection data from 2022-2024.

## Features

- Overview of tax collection trends
- Time series analysis with year-over-year comparisons
- Tax category comparisons
- Data filtering and export capabilities

## Setup

1. Clone this repository
2. Run `create_database.R` to generate the SQLite database
3. Run `app.R` to launch the dashboard

## Requirements

- R 4.0.0 or higher
- Required packages: shiny, shinydashboard, dplyr, ggplot2, plotly, DT, RSQLite, lubridate, scales, DBI, shinyWidgets
