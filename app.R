# ----------------------------------------------------------
# Homework_Phillips_Curve Shiny App - Enhanced Version
# ----------------------------------------------------------
# Description: An interactive Shiny application to visualize the 
#              Phillips Curve, showcasing the relationship between 
#              unemployment and inflation rates across different 
#              countries. I also visualized the unemployment and 
#              inflation by countries, and made and aggregated Phillips 
#              curve for all countries.
#              The app highlights the discrepancies in the
#              Phillips Curves theory and practice, using 
#              real-world data.
#
# Author: Solymár-Müller László Róbert
# Date: 2024.11.
# ----------------------------------------------------------

# -------------------------------
# Step 1: Load Necessary Libraries
# -------------------------------
# These libraries provide essential functions for building the app,
# data manipulation, and visualization.

library(shiny)            # Core package for building Shiny apps
library(ggplot2)          # For creating advanced plots
library(dplyr)            # For data manipulation and transformation
library(shinycssloaders)  # To add loading animations (spinners) to outputs
library(lubridate)        # For easy date manipulation
library(highcharter)      # For interactive and dynamic charts
library(shinythemes)      # To apply pre-built themes for better aesthetics
library(tidyr)            # For data tidying
library(plotly)           # For interactive ggplot2 plots

# -------------------------------
# Step 2: Load and Prepare the Data
# -------------------------------
# I already have the dataset named 'data' loaded in the environment.
# If not, you can get it from here:

# Install and load necessary packages
#install.packages("wbstats")    WORLD BANK STATISTICS
#library(wbstats)

# Define the indicators for unemployment and inflation
#indicators <- c(
#  unemployment = "SL.UEM.TOTL.ZS",   This is how they refer to the unemplaoyment stats
#  inflation = "FP.CPI.TOTL.ZG"       And iflation
#)

# Fetch the data for all available countries and years
#data <- wb_data(
#  indicator = indicators,
#  start_date = 1960,                               A bunch of them of course don't go back this far.
#  end_date = as.numeric(format(Sys.Date(), "%Y"))
#)

# Preview the first few rows of the dataset to double check
#head(data)


# Since the data contains NA values we'll clean it by removing those rows to ensure accurate visualizations.

# Clean the data by filtering out rows with NA in 'inflation' or 'unemployment'
clean_data <- data %>%
  filter(!is.na(inflation) & !is.na(unemployment))

# Optional: 
# head(clean_data)
# Some countries have data, a later data, but I don't want to loose too much data because of this.
# I'll put up a slider for the dates

# -------------------------------
# Step 3: Define the User Interface (UI)
# -------------------------------
# The UI defines how the app looks and what input/output elements it contains.

ui <- fluidPage(      #This is the proger rder for it 
  
  # I'll apply a theme for better aesthetics using shinythemes
  theme = shinytheme("cerulean"),  # The others are like "flatly", "yeti", etc. google it. 
  
  # ----------------------------
  # Application Title
  # ----------------------------
  titlePanel("Phillips Curve Analysis"),
  
  # ----------------------------
  # Sidebar Layout with Inputs
  # ----------------------------
  sidebarLayout(
    
    # This is for the inputs
    sidebarPanel(
      
      # Input: Select Multiple Countries with Removable Tags
      selectizeInput(
        inputId = "selected_countries",                     # This will provide the name of the country
        label = "Choose Countries:",                        # Label displayed above the dropdown menu
        choices = sort(unique(clean_data$country)),         # Sort makes it Alphabetical
        selected = "Hungary",                               # By Default Hungary.
        multiple = TRUE,                                    # Allow multiple selections
        options = list(
          plugins = list('remove_button')                   # Enable the little "X" to remove selected items
        )
      ),
      
      # Input: Select Year Range with Slider
      sliderInput(
        inputId = "year_range",                                # Years
        label = "Select Year Range:",                          # Label displayed above the slider
        min = min(clean_data$date),                            # Minimum year from the data (So all the data would be in it this way)
        max = max(clean_data$date),                            # Maximum year from the data (Out hands are tied here, but if the database is refreshed, so will be this)
        value = c(min(clean_data$date), max(clean_data$date)), # Default selected range (full range)
        step = 1,                                              # Slider increments by 1 year
        sep = ""                                               # Removes thousand separators bc we don't need it in dates
      ),
      
      # Input: Checkbox to Show/Hide Linear Trend Line
      checkboxInput(
        inputId = "show_linear_trend",                       # Name for the checkbox
        label = "Show Linear Trend Line",                    # Label displayed next to the checkbox
        value = TRUE                                         # Default value (checked)
      ),
      
      # Input: Checkbox to Show/Hide Non-Linear Phillips Curve
      checkboxInput(
        inputId = "show_phillips_curve",                     # Name for the checkbox
        label = "Show Phillips Curve (Non-Linear)",          # Label displayed next to the checkbox
        value = FALSE                                        # Default value (unchecked)
      ),
      
    
      # Additional stuff for Time Series Graph:
      
      
      # Button to Toggle Independent Time Series Selections, for when we have a ton of data on the main graph, but we want to see fewer data (or just different) in the aid-graph
      actionButton(
        inputId = "toggle_time_series_selection",            # Name for the button
        label = "Customize Time Series Selections"           # Label displayed on the button
      ),
      
      # Conditional Panel: Show Time Series Country Selection Only When needed
      conditionalPanel(
        condition = "input.toggle_time_series_selection % 2 == 1",  # Show when button is toggled
        selectizeInput(
          inputId = "selected_time_series_countries",          # Unique identifier for the input
          label = "Select Countries for Time Series Plot:",    # Label displayed above the dropdown
          choices = sort(unique(clean_data$country)),          # Alphabetically sorted country list
          selected = sort(unique(clean_data$country)),         # Default selected countries (all of them)
          multiple = TRUE,                                     # Allow multiple selections
          options = list(
            plugins = list('remove_button')                    # Enable the little "X" to remove selected items
          )
        )
      ),
      
      # ----------------------------
      # Inputs for Aggregated Phillips Curves
      # ----------------------------
      
      # Input: Checkbox to Aggregate All Countries' Phillips Curves
      checkboxInput(
        inputId = "aggregate_all",                             # Name for the checkbox
        label = "Aggregate Phillips Curves for All Countries", # Label displayed next to the checkbox
        value = FALSE                                          # Default value (unchecked)
      )
      
    ),
    
    # Main Panel for Outputs
    mainPanel(
      
      # Output: Phillips Curve Plot with Loading Spinner
      withSpinner(highchartOutput("phillips_plot_highc")),  # 'phillips_plot_highc' is the plot's name
      
      # Line Break for spacing
      br(),
      
      # Output: Correlation Coefficient Display
      verbatimTextOutput("correlation_text"),               # This is for numerically presenting the correlations, which I think is neat
      
      br(),
      
      # Output: Download Button for Filtered Data
      downloadButton("download_data", "Download Filtered Data"),  # Button to download the data, which again, I think is neat
      
      # Horizontal Line Separator
      hr(),
      
      # Output: Time Series Plot for Unemployment and Inflation Rates
      # Fixed height, so chaos wouldn't ensue
      div(
        style = "height:600px; overflow-y: auto;",                       # Fixed height and vertical scroll (this doesn't work properly, no idea why)
        withSpinner(plotlyOutput("time_series_plot", height = "600px"))  # 'time_series_plot' is the plot's naem
      ),
      
      br(),
      
      # Now the biggie: All Phillips Curves Window
      # Fixed height to maintain graph sizes
      div(
        style = "height:600px; overflow-y: auto;",              # Fixed height
        withSpinner(highchartOutput("all_phillips_curves"))     # 'all_phillips_curves' is the plot's name
      ),
      
      br(),
      
      # Output: Aggregated Phillips Curves Window
      div(
        style = "height:600px; overflow-y: auto;",          
        withSpinner(highchartOutput("aggregate_phillips_curves"))    # 'aggregate_phillips_curves'
      ),
      
      br()
      
    )
  )
)

# -------------------------------
# Step 4: Defining the Server Logic
# -------------------------------
# This is where all the instructions are so that the app knows what to do.

server <- function(input, output, session) {
  
  
  
  # Reactive Expression: Filtered Data for Main Plots
  # This reactive expression thing filters the data based on main user inputs. So this is where the magic happens. Filterwise.
  
  filtered_data <- reactive({
    clean_data %>%
      filter(
        country %in% input$selected_countries,             # Filter data for the selected countries
        date >= input$year_range[1],                       # For the start year
        date <= input$year_range[2]                        # And for the end year
      )
  })
  

  # Synchronize Time Series Selections:
  # By default, Time Series Plot mirrors the Phillips Curve Plot's country selections.
  # When the user toggles customization, this allows independent selections.
  
  observe({
    # If the Time Series Selection button has not been toggled (even number of clicks with the modulo),
    # synchronize the Time Series countries with the main selected countries.
    if(input$toggle_time_series_selection %% 2 == 0){                            # INPUT
      updateSelectizeInput(session, "selected_time_series_countries",
                           selected = input$selected_countries)
    }
    # If toggled (odd number of clicks), do not synchronize, allowing independent selection.
    # Users can then manually select countries for the Time Series Plot.
  })
  
  # ----------------------------
  # Reactive Expressions: Filtered Data for Time Series Plot
  # ----------------------------
  # This reactive expression filters the data based on time series user inputs.
  # It automatically updates whenever the inputs change, so when we screw around with the thingamabobs, they graph changes.
  
  filtered_time_series_data <- reactive({
    # If customization is enabled, use the independently selected countries
    if(input$toggle_time_series_selection %% 2 == 1){
      # If no countries are selected for the time series, return NULL, so that it won't freak out.
      if (is.null(input$selected_time_series_countries) || length(input$selected_time_series_countries) == 0) {
        return(NULL)
      }
      
      clean_data %>%  #'this the PIPE OPERATOR, it is used to pass in the next operation
        filter(       # we only need those columns, that are selected
          country %in% input$selected_time_series_countries, # Filter data for the selected time series countries
          date >= input$year_range[1],                       # Ensure within the main year range
          date <= input$year_range[2]
        )
    } else {
      # When not customized, mirror the main selected countries
      filtered_data()
    }
  })
  
  # ----------------------------
  # Render the Phillips Curve Plot using Highcharter
  # ----------------------------
  # This output generates an interactive scatter plot showing the relationship
  # between unemployment and inflation rates, including linear and non-linear trend lines.
  # This is the hearth and soul of the whole App
  # DO NOT TOUCH THIS PART OR THE PHILLIPS PLOT WILL DISAPPEAR AGAIN FOR SOME REASON
  # Date won't work, but I can't take it out or else the whole plot won't work
  # Can't even properly comment some parts, so yea, the JAva script part is from ChatGPT.
  
  output$phillips_plot_highc <- renderHighchart({
    
    # Retrieve the filtered data
    data <- filtered_data()
    
    # Debugging: Print number of rows
    print(paste("Number of data points:", nrow(data)))
    
    # Check if there is data to plot
    if(nrow(data) == 0){
      return(highchart() %>% hc_title(text = "No data available for the selected filters."))
    }
    
    # Create the Highcharter scatter plot
    hc <- highchart() %>%
      hc_title(text = "Phillips Curve Analysis") %>%  # Static plot title
      hc_xAxis(title = list(text = "Unemployment Rate (%)")) %>%  # Label for x-axis
      hc_yAxis(title = list(text = "Inflation Rate (%)")) %>%     # Label for y-axis
      hc_add_series_list(
        lapply(unique(data$country), function(ctry) {
          # Subset data for each country
          country_data <- data %>% filter(country == ctry)
          
          # Ensure there are enough points to plot
          if(nrow(country_data) < 2){
            return(NULL)  # Skip countries with insufficient data
          }
          
          # Convert 'date' to UNIX timestamp in milliseconds. IT won't help, but again, can't take it out now!! :D 
          country_data <- country_data %>%
            mutate(date = as.numeric(as.POSIXct(date)) * 1000)
          
          # Debugging: Print first few rows of country_data
          print(paste("Country:", ctry))
          print(head(country_data))
          
          # Create a scatter series for the country
          list(
            name = ctry,  # Series name (country)
            data = list_parse2(country_data %>% select(x = unemployment, y = inflation, date)),  # Data points with x, y, and date
            type = "scatter",  # Scatter plot type
            color = NULL,  # Automatic color assignment
            marker = list(symbol = "circle", radius = 7)  # Marker aesthetics
          )
        })
      ) %>%
      hc_tooltip(
        formatter = JS("function() {                            
          return '<b>' + this.series.name + '</b><br>' +
                 'Unemployment: ' + this.point.x + '%<br>' +
                 'Inflation: ' + this.point.y + '%<br>' +
                 'Date: ' + Highcharts.dateFormat('%Y-%m-%d', this.point.date);
        }")  
      )
    
    # Add Linear Trend Lines if selected
    if (input$show_linear_trend) {
      for (ctry in unique(data$country)) {
        # Subset data for the country
        country_data <- data %>% filter(country == ctry)
        
        # Ensure there are enough points to fit a linear model
        if(nrow(country_data) < 2){
          next  # Skip countries with insufficient data
        }
        
        # We will fit the regression line in 4 steps, in order to have a nice, precise line, according to the sources:
        
        # Fit a linear model (linear regression)
        model <- lm(inflation ~ unemployment, data = country_data)
        
        # Create a sequence of unemployment rates so to have a continuous line
        unemployment_seq <- seq(min(country_data$unemployment), max(country_data$unemployment), length.out = 100)
        
        # Predict inflation rates based on the linear model (connecting the dots basically)
        predicted_inflation <- predict(model, newdata = data.frame(unemployment = unemployment_seq))
        
        # Prepare the df for the trend line with 'x' and 'y' columns
        trend_data <- data.frame(x = unemployment_seq, y = predicted_inflation)
        
        
        # Add the trend line series to the Highcharter plot
        hc <- hc %>%
          hc_add_series(
            name = paste(ctry, "Linear Trend"),  # Series name
            data = list_parse2(trend_data),      # Data points with x and y
            type = "line",                       # Line plot type
            color = NULL,                        # Automatic color assignment
            dashStyle = "ShortDash",             # Dashed line style
            marker = list(enabled = FALSE)       # Disable markers for trend line
          )
        # Thus we can hover over every part of the smooth regression line to get the data we need and the ratio based variables.
      }
    }
    
    # Add Non-Linear Phillips Curve if selected
    if (input$show_phillips_curve) {
      for (ctry in unique(data$country)) {
        # Subset data for the country
        country_data <- data %>% filter(country == ctry)
        
        # Ensure there are enough points to fit a quadratic model (we need 3 now!!!)
        if(nrow(country_data) < 3){
          next  # Skip countries with insufficient data
        }
        
        # We do the same steps as before, but in polynomal version:
        
        # Fit a quadratic model (second-degree polynomial)
        model <- lm(inflation ~ poly(unemployment, 2), data = country_data)
        
        # Create a sequence of unemployment rates for prediction
        unemployment_seq <- seq(min(country_data$unemployment), max(country_data$unemployment), length.out = 100)
        
        # Predict inflation rates based on the quadratic model
        predicted_inflation <- predict(model, newdata = data.frame(unemployment = unemployment_seq))
        
        # Prepare data frame for the Phillips curve with 'x' and 'y' columns
        phillips_curve_data <- data.frame(x = unemployment_seq, y = predicted_inflation)
        
        # Add the Phillips Curve series to the Highcharter plot
        hc <- hc %>%
          hc_add_series(
            name = paste(ctry, "Phillips Curve"),     # Series name
            data = list_parse2(phillips_curve_data),  # Data points with x and y
            type = "line",                            # Line plot type
            color = NULL,                             # Automatic color assignment
            dashStyle = "Solid",                      # Solid line style
            marker = list(enabled = FALSE)            # Disable markers for trend line
          )
      }
    }
    
    # Return the completed Highcharter object to be rendered
    hc
  })


  # ----------------------------------
  # Render the Correlation Coefficient
  # ----------------------------------
  # This output calculates and displays the Pearson correlation coefficient
  # between unemployment and inflation rates for each selected country to see
  # the correlation between them.
  
  output$correlation_text <- renderPrint({
    
    # Retrieve the filtered data
    data <- filtered_data()
    
    # Check if there's enough data to calculate correlation
    if(nrow(data) > 1){
      
      # Calculate Pearson correlation coefficient for each country
      correlation_df <- data %>%
        group_by(country) %>%                                              # Group data by country
        summarize(
          correlation = cor(unemployment, inflation, method = "pearson"),
          .groups = 'drop'                                                 # Ungroup after summarizing
        ) 
      
      # Display the correlation coefficients
      print(correlation_df)
      
    } else {
      
      # Inform the user if there's not enough data to compute correlation
      cat("Not enough data to calculate correlation.")
    }
    
  })
  
  # -----------------------------------------
  # Download Handler: Download Filtered Data
  # -----------------------------------------
  # This allows users to download the filtered dataset as a CSV file, which is always easier for my mind for some reason.
  
  output$download_data <- downloadHandler(
    filename = function() {
      # Create a dynamic filename based on selected countries and year range
      paste("Phillips_Data_", 
            paste(input$selected_countries, collapse = "_"), "_", 
            input$year_range[1], "-", 
            input$year_range[2], ".csv", 
            sep = "")
    },
    content = function(file) {
      # Write the filtered data to the specified file in CSV format
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # ----------------------------
  # Render the Time Series Plot
  # ----------------------------
  # This output generates an interactive time series plot showing both unemployment
  # and inflation rates over time for the selected countries.
  
  output$time_series_plot <- renderPlotly({
    
    # Retrieve the filtered data for the time series
    data <- filtered_time_series_data()
    
    # Check if there is data to plot
    if(is.null(data) || nrow(data) == 0){
      return(plotly_empty(type = "scatter", mode = "text") %>%
               layout(title = "No data available for the selected Time Series filters."))
    }
    
    # Reshape the data to long format for easier plotting with ggplot2
    long_data <- data %>%
      pivot_longer(cols = c("unemployment", "inflation"),
                   names_to = "metric",
                   values_to = "value")
    
    # Create the ggplot
    p <- ggplot(long_data, aes(x = date, y = value, color = metric, linetype = metric)) +
      geom_line(size = 1) +                                      # Add lines for each metric
      facet_wrap(~ country, scales = "free_y") +                 # Create the separate panels for each country
      labs(title = "Unemployment and Inflation Rates Over Time",
           x = "Year",
           y = "Rate (%)",
           color = "Metric",
           linetype = "Metric") +
      theme_minimal() +                                          # Let's try the minimal
      theme(legend.position = "bottom")                          # Position the legend at the bottom
    
    # Convert ggplot to Plotly for interactivity
    ggplotly(p, tooltip = c("x", "y", "color")) %>%
      layout(autosize = TRUE)                                    # This adjusts the plot to the container size
  })
  
  # --------------------------------------
  # Render the All Phillips Curves Window
  # --------------------------------------
  # This output generates an interactive plot displaying all Phillips Curves for the selected countries.
  # This is almost the same code for the third time.
  
  output$all_phillips_curves <- renderHighchart({
    
    # Retrieve the filtered data
    data <- filtered_data()
    
    # Check if there is data to plot
    if(nrow(data) == 0){
      return(highchart() %>% hc_title(text = "No data available for the selected filters."))
    }
    
    # Create the Highcharter plot
    hc <- highchart() %>%
      hc_title(text = "All Phillips Curves") %>%                  # Static plot title
      hc_xAxis(title = list(text = "Unemployment Rate (%)")) %>%  # Label for x-axis
      hc_yAxis(title = list(text = "Inflation Rate (%)")) %>%     # Label for y-axis
      hc_add_series_list(
        lapply(unique(data$country), function(ctry) {
          # Subset data for each country
          country_data <- data %>% filter(country == ctry)
          
          # Ensure there are enough points to fit a quadratic model yet again
          if(nrow(country_data) < 3){
            return(NULL)  # Skip countries with insufficient data
          }
          
          # Fit a quadratic model (second-degree polynomial) for the Phillips Curve
          model <- lm(inflation ~ poly(unemployment, 2), data = country_data)
          
          # Create a sequence of unemployment rates for prediction
          unemployment_seq <- seq(min(country_data$unemployment), max(country_data$unemployment), length.out = 100)
          
          # Predict inflation rates based on the quadratic model
          predicted_inflation <- predict(model, newdata = data.frame(unemployment = unemployment_seq))
          
          # Prepare data frame for the Phillips curve with 'x' and 'y' columns
          phillips_curve_data <- data.frame(x = unemployment_seq, y = predicted_inflation)
          
          # Add the Phillips Curve series to the Highcharter plot
          list(
            name = paste(ctry, "Phillips Curve"),                 # Series name
            data = list_parse2(phillips_curve_data),              # Data points with x and y
            type = "line",                                        # Line plot type
            color = NULL,                                         # Automatic color assignment
            dashStyle = "Solid",                                  # Solid line style
            marker = list(enabled = FALSE)                        # Disable markers for trend line
          )
        })
      ) %>%
      hc_tooltip(pointFormat = "Unemployment: {point.x}%<br>Inflation: {point.y}%")  # Tooltip content
    
    # Return the completed Highcharter object to be rendered
    hc
  })
  
  
  # Do I need all these? Can't I just write a loop or something??
  
  # --------------------------------------------
  # Render the Aggregated Phillips Curves Window
  # --------------------------------------------
  
  # This output generates an interactive plot aggregating Phillips Curves based on user selections, so that we can see
  # what would happen if the whole World would be one country. 
  
  output$aggregate_phillips_curves <- renderHighchart({
    
    # Determine if the aggregation tick is enabled
    aggregate_flag <- input$aggregate_all
    
    # Determine which countries to aggregate
    if(aggregate_flag){
      countries_to_aggregate <- unique(clean_data$country)     # All countries
    } else {
      countries_to_aggregate <- input$selected_countries
    }
    
    # If no countries are selected and aggregation is not enabled, we don't plot
    if(length(countries_to_aggregate) == 0){
      return(highchart() %>% hc_title(text = "No countries selected for aggregation."))
    }
    
    # Filter data based on aggregation choice
    aggregated_data <- clean_data %>%
      filter(country %in% countries_to_aggregate) %>%
      filter(
        date >= input$year_range[1],
        date <= input$year_range[2]
      )
    
    # Check if there is data to plot
    if(nrow(aggregated_data) == 0){
      return(highchart() %>% hc_title(text = "No data available for the aggregation filters."))
    }
    
    # Fit a quadratic model to the aggregated data, just like before
    
    # This treats all selected countries as one combined dataset
    aggregated_model <- lm(inflation ~ poly(unemployment, 2), data = aggregated_data)
    
    # Create a sequence of unemployment rates for prediction
    unemployment_seq <- seq(min(aggregated_data$unemployment), max(aggregated_data$unemployment), length.out = 100)
    
    # Predict inflation rates based on the aggregated model
    predicted_inflation <- predict(aggregated_model, newdata = data.frame(unemployment = unemployment_seq))
    
    # Prepare data frame for the aggregated Phillips curve with 'x' and 'y' columns
    aggregated_phillips_curve <- data.frame(x = unemployment_seq, y = predicted_inflation)
    
    # Create the Highcharter plot
    hc <- highchart() %>%
      hc_title(text = "Aggregated Phillips Curve") %>%           # Static plot title
      hc_xAxis(title = list(text = "Unemployment Rate (%)")) %>% # Label for x-axis
      hc_yAxis(title = list(text = "Inflation Rate (%)")) %>%    # Label for y-axis
      hc_add_series(
        name = "Aggregated Phillips Curve",                      # Series name
        data = list_parse2(aggregated_phillips_curve),           # Data points with x and y
        type = "line",                                           # Line plot type
        color = "black",                                         # Fixed color for aggregation
        dashStyle = "Dash",                                      # Dashed line style for distinction
        marker = list(enabled = FALSE)                           # Disable markers
      ) %>%
      hc_tooltip(pointFormat = "Unemployment: {point.x}%<br>Inflation: {point.y}%")  # Tooltip content
    
    # Return the completed Highcharter object to be rendered
    hc
  })
  
}

# --------------------------
# Step 5: Run the Shiny App
# --------------------------
# This is the command  that launches the Shiny app by combining the UI and server components.

shinyApp(ui = ui, server = server)
