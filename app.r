# Install required packages if not already installed
packages <- c("shiny", "shinydashboard", "dplyr", "ggplot2", "plotly", 
              "DT", "RSQLite", "lubridate", "scales", "DBI", "shinyWidgets")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Load libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(RSQLite)
library(lubridate)
library(scales)
library(DBI)
library(shinyWidgets)

# Connect to the database
con <- dbConnect(RSQLite::SQLite(), "tax_data.db")

# Check if database exists and has the expected tables
if (!dbExistsTable(con, "tax_collections")) {
  stop("Database table 'tax_collections' not found. Please run create_database.R first.")
}

# Get list of all tax categories by checking column names
tax_cols_query <- "PRAGMA table_info(tax_collections)"
all_cols <- dbGetQuery(con, tax_cols_query)$name

# Remove non-tax columns
tax_categories <- setdiff(all_cols, c("Year", "Month", "Date", "Total"))

# Get available years
years_query <- "SELECT DISTINCT Year FROM tax_collections ORDER BY Year"
available_years <- dbGetQuery(con, years_query)$Year

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Tax Collection Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Trend Analysis", tabName = "trends", icon = icon("chart-line")),
      menuItem("Comparisons", tabName = "comparisons", icon = icon("balance-scale")),
      menuItem("Data Explorer", tabName = "data", icon = icon("table")),
      hr(),
      selectInput("year_range", "Select Years:",
                  choices = available_years,
                  selected = available_years,  # Select all years by default
                  multiple = TRUE),
      pickerInput("tax_category", "Select Tax Categories:",
                  choices = tax_categories,
                  selected = head(tax_categories, 5),  # Default to first 5 categories
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE, `live-search` = TRUE))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                box(
                  title = "Tax Dashboard Overview",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  p("This dashboard analyzes tax collection data from 2022-2024. Select different tax categories and years to explore trends and patterns.")
                )
              ),
              fluidRow(
                box(
                  title = "Total Tax Collection by Year",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("yearly_totals_plot", height = 300)
                )
              ),
              fluidRow(
                box(
                  title = "Tax Category Distribution",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("category_distribution_plot", height = 300)
                ),
                box(
                  title = "Monthly Collection Trends",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("monthly_trends_plot", height = 300)
                )
              ),
              fluidRow(
                box(
                  title = "Key Statistics",
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
                  tableOutput("key_stats_table")
                )
              )
      ),
      
      # Trend Analysis Tab
      tabItem(tabName = "trends",
              fluidRow(
                box(
                  title = "Time Series Analysis",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("time_series_plot", height = 400)
                )
              ),
              fluidRow(
                box(
                  title = "Year-over-Year Growth",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("yoy_growth_plot", height = 300)
                ),
                box(
                  title = "Seasonality Analysis",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("seasonality_plot", height = 300)
                )
              )
      ),
      
      # Comparisons Tab
      tabItem(tabName = "comparisons",
              fluidRow(
                box(
                  title = "Category Comparison",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("category_comparison_plot", height = 400)
                )
              ),
              fluidRow(
                box(
                  title = "Category Contribution by Year",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("contribution_plot", height = 350)
                )
              ),
              fluidRow(
                box(
                  title = "Top Categories by Year",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  selectInput("top_year", "Select Year:",
                              choices = available_years,
                              selected = max(available_years)),
                  numericInput("top_n", "Number of Categories:", 10, min = 5, max = 20),
                  plotlyOutput("top_categories_plot", height = 300)
                )
              )
      ),
      
      # Data Explorer Tab
      tabItem(tabName = "data",
              fluidRow(
                box(
                  title = "Filter Data",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  fluidRow(
                    column(width = 3,
                           selectInput("month_filter", "Month:",
                                       choices = c("All", month.abb),
                                       selected = "All")
                    ),
                    column(width = 3,
                           numericInput("min_amount", "Minimum Total:", 0)
                    ),
                    column(width = 6,
                           actionButton("apply_filters", "Apply Filters", 
                                        icon = icon("filter"), 
                                        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                    )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Tax Collection Data",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("data_table")
                )
              ),
              fluidRow(
                box(
                  title = "Download Data",
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
                  downloadButton("download_data", "Download Filtered Data (CSV)")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive function to get filtered data
  filtered_data <- reactive({
    # Base query with year filters
    if (length(input$year_range) > 0) {
      year_list <- paste(input$year_range, collapse = ",")
      query <- paste0("
        SELECT * FROM tax_collections
        WHERE Year IN (", year_list, ")
      ")
    } else {
      query <- "SELECT * FROM tax_collections"
    }
    
    # Execute the query
    data <- dbGetQuery(con, query)
    
    # Convert Date back to proper format
    data$Date <- as.Date(data$Date)
    
    return(data)
  })
  
  # Yearly totals plot
  output$yearly_totals_plot <- renderPlotly({
    query <- "SELECT Year, SUM(Total) as YearlyTotal FROM tax_collections GROUP BY Year"
    yearly_data <- dbGetQuery(con, query)
    
    p <- ggplot(yearly_data, aes(x = factor(Year), y = YearlyTotal, fill = factor(Year))) +
      geom_bar(stat = "identity") +
      labs(x = "Year", y = "Total Tax Collection", fill = "Year") +
      scale_y_continuous(labels = dollar_format(scale = 1e-6, suffix = "M")) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p) %>% layout(annotations = list(
      x = 0.5, y = 1.05,
      text = "Total Tax Collection by Year",
      showarrow = FALSE,
      xref = 'paper',
      yref = 'paper'
    ))
  })
  
  # Category distribution plot
  output$category_distribution_plot <- renderPlotly({
    # Get selected categories
    selected_cats <- input$tax_category
    
    if (length(selected_cats) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Please select at least one tax category") +
               theme_void())
    }
    
    # Safe query building - ensure categories exist
    valid_cats <- intersect(selected_cats, tax_categories)
    if (length(valid_cats) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No valid tax categories selected") +
               theme_void())
    }
    
    # Create the query with selected categories and years
    selected_years <- paste(input$year_range, collapse = ",")
    valid_cats_sql <- paste0("SUM(", valid_cats, ") as ", valid_cats, collapse = ", ")
    
    query <- paste0("
      SELECT ", valid_cats_sql, "
      FROM tax_collections
      WHERE Year IN (", selected_years, ")
    ")
    
    tryCatch({
      cat_data <- dbGetQuery(con, query)
      
      # Transform to long format for plotting
      cat_data_long <- tidyr::pivot_longer(cat_data, cols = everything(), 
                                           names_to = "Category", values_to = "Amount")
      
      # Filter out categories with 0 or NA values
      cat_data_long <- cat_data_long %>%
        filter(!is.na(Amount), Amount > 0)
      
      if (nrow(cat_data_long) == 0) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "No data for selected categories") +
                 theme_void())
      }
      
      p <- ggplot(cat_data_long, aes(x = "", y = Amount, fill = Category)) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y", start = 0) +
        theme_minimal() +
        labs(fill = "Tax Category") +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank())
      
      ggplotly(p)
    }, error = function(e) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = paste("Error in plot:", e$message)) +
               theme_void())
    })
  })
  
  # Monthly trends plot
  output$monthly_trends_plot <- renderPlotly({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data for selected years") +
               theme_void())
    }
    
    # Calculate monthly totals across years
    monthly_totals <- data %>%
      group_by(Month) %>%
      summarize(MonthlyAvg = mean(Total, na.rm = TRUE)) %>%
      mutate(Month = factor(Month, levels = month.abb))
    
    p <- ggplot(monthly_totals, aes(x = Month, y = MonthlyAvg, group = 1)) +
      geom_line(color = "steelblue", size = 1) +
      geom_point(color = "steelblue", size = 3) +
      theme_minimal() +
      labs(x = "Month", y = "Average Collection") +
      scale_y_continuous(labels = dollar_format(scale = 1e-6, suffix = "M"))
    
    ggplotly(p)
  })
  
  # Key statistics table
  output$key_stats_table <- renderTable({
    data <- filtered_data()
    
    # Selected categories
    selected_cats <- input$tax_category
    
    if (length(selected_cats) == 0) {
      return(data.frame(Message = "Please select at least one tax category"))
    }
    
    # Calculate stats for each selected category
    tryCatch({
      stats_list <- lapply(selected_cats, function(cat) {
        # Check if column exists
        if (!cat %in% colnames(data)) return(NULL)
        
        values <- data[[cat]]
        if (all(is.na(values))) {
          return(NULL)  # Skip categories with all NA values
        }
        
        # Calculate growth only if multiple years
        growth <- NA
        if (length(unique(data$Year)) > 1) {
          latest_year <- max(data$Year)
          prev_year <- latest_year - 1
          
          # Only calculate if we have data for both years
          if (any(data$Year == latest_year) && any(data$Year == prev_year)) {
            latest_sum <- sum(data[data$Year == latest_year, cat], na.rm = TRUE)
            prev_sum <- sum(data[data$Year == prev_year, cat], na.rm = TRUE)
            
            if (prev_sum > 0) {
              growth <- (latest_sum - prev_sum) / prev_sum * 100
            }
          }
        }
        
        data.frame(
          Category = cat,
          Total = sum(values, na.rm = TRUE),
          Average = mean(values, na.rm = TRUE),
          Maximum = max(values, na.rm = TRUE),
          Growth = growth
        )
      })
      
      # Remove NULL entries and combine all stats
      stats_list <- stats_list[!sapply(stats_list, is.null)]
      if (length(stats_list) == 0) {
        return(data.frame(Message = "No valid data for selected categories"))
      }
      
      result <- do.call(rbind, stats_list)
      # Format numbers to be more readable
      result$Total <- round(result$Total, 2)
      result$Average <- round(result$Average, 2)
      result$Maximum <- round(result$Maximum, 2)
      result$Growth <- round(result$Growth, 2)
      
      result
    }, error = function(e) {
      return(data.frame(Message = paste("Error calculating statistics:", e$message)))
    })
  })
  
  # Time series plot
  output$time_series_plot <- renderPlotly({
    data <- filtered_data()
    selected_cats <- input$tax_category
    
    if (length(selected_cats) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Please select at least one tax category") +
               theme_void())
    }
    
    # Limit to max 5 categories for readability if more are selected
    if (length(selected_cats) > 5) {
      selected_cats <- selected_cats[1:5]
    }
    
    # Check which selected categories exist in the data
    valid_cats <- intersect(selected_cats, colnames(data))
    
    if (length(valid_cats) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No valid categories selected") +
               theme_void())
    }
    
    # Filter for selected categories
    plot_data <- tryCatch({
      data %>%
        select(Date, all_of(valid_cats)) %>%
        tidyr::pivot_longer(cols = -Date, names_to = "Category", values_to = "Amount")
    }, error = function(e) {
      NULL
    })
    
    if (is.null(plot_data) || nrow(plot_data) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data available for selected categories") +
               theme_void())
    }
    
    p <- ggplot(plot_data, aes(x = Date, y = Amount, color = Category)) +
      geom_line() +
      theme_minimal() +
      labs(x = "Date", y = "Amount", color = "Tax Category") +
      scale_y_continuous(labels = dollar_format(scale = 1e-6, suffix = "M")) +
      scale_color_brewer(palette = "Set1")
    
    ggplotly(p)
  })
  
  # Year-over-Year growth plot
  output$yoy_growth_plot <- renderPlotly({
    # Need to calculate YoY growth for selected categories
    years <- sort(input$year_range)
    selected_cats <- input$tax_category
    
    if (length(years) < 2 || length(selected_cats) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = "Need at least 2 years and 1 category for comparison") +
               theme_void())
    }
    
    # Limit to 5 categories for readability
    if (length(selected_cats) > 5) {
      selected_cats <- selected_cats[1:5]
    }
    
    # Get yearly totals for selected categories
    tryCatch({
      # Build query safely
      valid_cats <- intersect(selected_cats, tax_categories)
      if (length(valid_cats) == 0) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "No valid categories selected") +
                 theme_void())
      }
      
      cat_cols <- paste0("SUM(", valid_cats, ") as ", valid_cats, collapse = ", ")
      query <- paste0("
        SELECT Year, ", cat_cols, "
        FROM tax_collections
        WHERE Year IN (", paste(years, collapse = ","), ")
        GROUP BY Year
        ORDER BY Year
      ")
      
      yearly_data <- dbGetQuery(con, query)
      
      # Calculate growth rates
      growth_data <- yearly_data %>%
        tidyr::pivot_longer(cols = -Year, names_to = "Category", values_to = "Amount") %>%
        arrange(Category, Year) %>%
        group_by(Category) %>%
        mutate(Growth = (Amount / lag(Amount) - 1) * 100) %>%
        filter(!is.na(Growth)) %>%
        ungroup()
      
      # If no growth data, return message
      if (nrow(growth_data) == 0) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "No valid growth data available") +
                 theme_void())
      }
      
      p <- ggplot(growth_data, aes(x = factor(Year), y = Growth, fill = Category)) +
        geom_bar(stat = "identity", position = "dodge") +
        theme_minimal() +
        labs(x = "Year", y = "Growth Rate (%)", fill = "Tax Category") +
        scale_y_continuous(labels = function(x) paste0(x, "%"))
      
      ggplotly(p)
    }, error = function(e) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = paste("Error generating growth plot:", e$message)) +
               theme_void())
    })
  })
  
  # Seasonality plot
  output$seasonality_plot <- renderPlotly({
    data <- filtered_data()
    
    # Make sure we have data and selected categories
    if (nrow(data) == 0 || length(input$tax_category) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = "No data available for the selected criteria") +
               theme_void())
    }
    
    # Get monthly data for the primary selected category
    primary_cat <- input$tax_category[1]
    
    # Verify the column exists
    if (!primary_cat %in% colnames(data)) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = paste("Category", primary_cat, "not found in data")) +
               theme_void())
    }
    
    tryCatch({
      seasonal_data <- data %>%
        group_by(Year, Month) %>%
        summarize(Amount = sum(.data[[primary_cat]], na.rm = TRUE), .groups = "drop") %>%
        mutate(Month = factor(Month, levels = month.abb))
      
      p <- ggplot(seasonal_data, aes(x = Month, y = Amount, group = Year, color = factor(Year))) +
        geom_line() +
        geom_point() +
        theme_minimal() +
        labs(x = "Month", y = "Amount", color = "Year",
             title = paste("Monthly Pattern for", primary_cat)) +
        scale_y_continuous(labels = dollar_format(scale = 1e-6, suffix = "M"))
      
      ggplotly(p)
    }, error = function(e) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = paste("Error in seasonality plot:", e$message)) +
               theme_void())
    })
  })
  
  # Category comparison plot
  output$category_comparison_plot <- renderPlotly({
    # Get data for selected categories
    selected_cats <- input$tax_category
    if (length(selected_cats) < 2) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Select at least 2 categories to compare") +
               theme_void())
    }
    
    # Limit to 5 categories for readability
    if (length(selected_cats) > 5) {
      selected_cats <- selected_cats[1:5]
    }
    
    tryCatch({
      # Verify categories exist
      valid_cats <- intersect(selected_cats, colnames(filtered_data()))
      if (length(valid_cats) < 2) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "Need at least 2 valid categories") +
                 theme_void())
      }
      
      data <- filtered_data() %>%
        select(Year, Month, all_of(valid_cats)) %>%
        tidyr::pivot_longer(cols = -c(Year, Month), names_to = "Category", values_to = "Amount") %>%
        group_by(Year, Category) %>%
        summarize(Total = sum(Amount, na.rm = TRUE), .groups = "drop")
      
      p <- ggplot(data, aes(x = factor(Year), y = Total, fill = Category)) +
        geom_bar(stat = "identity", position = "stack") +
        theme_minimal() +
        labs(x = "Year", y = "Total Amount", fill = "Category") +
        scale_y_continuous(labels = dollar_format(scale = 1e-6, suffix = "M"))
      
      ggplotly(p)
    }, error = function(e) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = paste("Error in comparison plot:", e$message)) +
               theme_void())
    })
  })
  
  # Contribution plot
  output$contribution_plot <- renderPlotly({
    data <- filtered_data()
    selected_cats <- input$tax_category
    
    if (length(selected_cats) < 2) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Select at least 2 categories for contribution analysis") +
               theme_void())
    }
    
    # Limit to 5 categories for readability
    if (length(selected_cats) > 5) {
      selected_cats <- selected_cats[1:5]
    }
    
    # Calculate the contribution of each category to the total
    tryCatch({
      # Verify categories exist
      valid_cats <- intersect(selected_cats, colnames(data))
      if (length(valid_cats) < 2) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "Need at least 2 valid categories") +
                 theme_void())
      }
      
      contrib_data <- data %>%
        group_by(Year) %>%
        summarize(across(all_of(valid_cats), sum, na.rm = TRUE), .groups = "drop") %>%
        mutate(Total = rowSums(across(all_of(valid_cats)), na.rm = TRUE)) %>%
        tidyr::pivot_longer(cols = all_of(valid_cats), names_to = "Category", values_to = "Amount") %>%
        mutate(Percentage = Amount / Total * 100)
      
      p <- ggplot(contrib_data, aes(x = factor(Year), y = Percentage, fill = Category)) +
        geom_bar(stat = "identity", position = "stack") +
        theme_minimal() +
        labs(x = "Year", y = "Contribution (%)", fill = "Category") +
        scale_y_continuous(labels = function(x) paste0(x, "%"))
      
      ggplotly(p)
    }, error = function(e) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = paste("Error in contribution plot:", e$message)) +
               theme_void())
    })
  })
  
  # Top categories plot
  output$top_categories_plot <- renderPlotly({
    # Get top N categories for selected year
    selected_year <- input$top_year
    top_n <- input$top_n
    
    if (is.null(selected_year)) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Please select a year") +
               theme_void())
    }
    
    # Get data for the selected year
    query <- paste0("SELECT * FROM tax_collections WHERE Year = ", selected_year)
    year_data <- dbGetQuery(con, query)
    
    if (nrow(year_data) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data for selected year") +
               theme_void())
    }
    
    # Get tax categories (all columns except Year, Month, Date, Total)
    tax_cols <- setdiff(colnames(year_data), c("Year", "Month", "Date", "Total"))
    
    # Calculate total for each category
    category_totals <- sapply(tax_cols, function(col) sum(year_data[[col]], na.rm = TRUE))
    
    # Create data frame and sort
    category_df <- data.frame(
      Category = names(category_totals),
      Total = as.numeric(category_totals)
    ) %>%
      arrange(desc(Total)) %>%
      # Filter out categories with 0 or negative totals
      filter(Total > 0) %>%
      # Take top N categories
      head(top_n)
    
    if (nrow(category_df) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No positive revenue categories found") +
               theme_void())
    }
    
    # Create plot
    p <- ggplot(category_df, aes(x = reorder(Category, Total), y = Total, fill = Total)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_minimal() +
      labs(x = "", y = "Total Amount", 
           title = paste("Top", top_n, "Tax Categories in", selected_year)) +
      scale_y_continuous(labels = dollar_format(scale = 1e-6, suffix = "M")) +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  # Data table with filters
  filtered_table_data <- eventReactive(input$apply_filters, {
    data <- filtered_data()
    
    # Apply additional filters
    if (input$month_filter != "All") {
      data <- data %>% filter(Month == input$month_filter)
    }
    
    if (!is.null(input$min_amount) && input$min_amount > 0) {
      data <- data %>%
        filter(Total >= input$min_amount)
    }
    
    return(data)
  }, ignoreNULL = FALSE)
  
  output$data_table <- renderDT({
    data <- filtered_table_data()
    
    # If no data, return empty data frame
    if (nrow(data) == 0) {
      return(datatable(data.frame(Message = "No data matching the selected filters")))
    }
    
    # Select columns to display
    selected_cols <- c("Year", "Month", input$tax_category, "Total")
    # Ensure all columns exist
    display_cols <- intersect(selected_cols, colnames(data))
    
    if (length(display_cols) <= 2) {  # If only Year and Month remain
      display_cols <- c("Year", "Month", "Total")  # Add Total as fallback
    }
    
    # Format the data
    datatable(data[, display_cols], options = list(
      pageLength = 10,
      scrollX = TRUE,
      autoWidth = TRUE
    )) %>%
      formatCurrency(columns = setdiff(display_cols, c("Year", "Month")), 
                     currency = "$", digits = 2)
  })
  
  # Download handler for CSV
  output$download_data <- downloadHandler(
    filename = function() {
      paste("tax_data_", format(Sys.Date(), "%Y%m%d"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_table_data(), file, row.names = FALSE)
    }
  )
  
  # Close database connection when session ends
  session$onSessionEnded(function() {
    dbDisconnect(con)
  })
}

# Run the app
shinyApp(ui, server)