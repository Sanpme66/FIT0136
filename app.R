#install.packages("forecast")
#install.packages('leaflet.extras')
#install.packages("shinyjs")
#install.packages("shinydashboardPlus")


# Load required packages
required_packages <- c("shinydashboard", "shinydashboardPlus", "leaflet.extras", "ggplot2", "dplyr", "lubridate", "plotly", "gganimate", "randomForest", "tidyverse", "data.table")


# Check if packages are installed and load them if not
for (package in required_packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}


# Load necessary libraries
library(shinyjs)                 # Shiny JavaScript functions
library(shinydashboard)          # Dashboard layout for Shiny
library(shinydashboardPlus)      # Enhanced dashboard features
library(leaflet.extras)          # Additional features for Leaflet maps
library(ggplot2)                 # Data visualization package
library(dplyr)                   # Data manipulation package
library(lubridate)               # Date and time functions
library(plotly)                  # Interactive plotting
library(gganimate)               # Animation package
library(randomForest)            # Random Forest model
library(tidyverse)               # Data manipulation and visualization
library(data.table)              # Data manipulation package optimized for speed

# Set working directory if necessary
# setwd("~/Monash/FIT5147/DVP/Final")



#Data load

load("data/trips_by_hour_day.RData")
load("data/trips_by_months.RData")

avg_pickups <- readRDS("data/avg_pickups.RDS")
avg_fare <- readRDS("data/avg_fare.RDS")
avg_trip_distance  <- readRDS("data/avg_trip_distance.RDS")
load("data/pickup_datetime_df.RData")
load("data/dropoff_datetime_df.RData")
rf_model <- readRDS("data/rf_model.rds")
load("data/Model_merged_df.Rdata")

load("data/merged_df_plot2.RData")
load("data/summary_borough.Rdata")

load("data/hr_trip_group.Rdata")

load("data/sunburstDF.Rdata")
load("data/payment_type_sunburst.Rdata")

load("data/trips_per_day_year.Rdata")

load("data/trip_speed_group.Rdata")

load("data/trip_density_df.Rdata")

load("data/hourly_avg_tip.Rdata")
load("data/long_tip_amount_data.Rdata")

load("data/season_count.Rdata")
load("data/monthly_agg_data.Rdata")

json_file <- "data/sankey_data_new.json"
sankey_data <- jsonlite::fromJSON(json_file)

#install.packages("jsonlite")
url <- "https://raw.githubusercontent.com/Sanpme66/D3_network_visualisation/main/NYC_weather_data_2022.json"
weather_data <- jsonlite::fromJSON(url)

weather_data$date <- as.Date(weather_data$date, format = "%Y-%m-%d")
weather_data$month <- month(weather_data$date)
weather_data <- na.omit(weather_data)


weather_weekly_avg <- weather_data %>%
  mutate(week = lubridate::week(date)) %>%
  group_by(week) %>%
  summarise(
    avg_rain = mean(rain),
    avg_snowfall = mean(s_fall),
    avg_all_precip = mean(all_precip),
    avg_snow = mean(has_snow),
    avg_rainy = mean(has_rain),
    avg_snow_depth = mean(s_depth),
    avg_max_temp = mean(max_temp),
    avg_min_temp = mean(min_temp)
  )


weather_monthly_avg <- weather_data  %>%
  group_by(month) %>%
  summarise(
    avg_rain = mean(rain),
    avg_snowfall = mean(s_fall),
    avg_all_precip = mean(all_precip),
    avg_snow = mean(has_snow),
    avg_rainy = mean(has_rain),
    avg_snow_depth = mean(s_depth),
    avg_max_temp = mean(max_temp),
    avg_min_temp = mean(min_temp)
  )


# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "NYC Taxi Data Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Passenger", tabName = "passenger", icon = icon("user")),
      menuItem("Trips", tabName = "trips", icon = icon("road")),
      menuItem("Payments", tabName = "payments", icon = icon("dollar")),
      menuItem("Weather", tabName = "weather", icon = icon("cloud")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  # Dashbord body
  dashboardBody(
    ## modify the dashboard's skin color
    tags$style(
      HTML(
        '
                       /* logo */
                       .skin-blue .main-header .logo {
                       background-color: #006272;
                       }

                       /* logo when hovered */
                       .skin-blue .main-header .logo:hover {
                       background-color: #006272;
                       }

                       /* navbar (rest of the header) */
                       .skin-blue .main-header .navbar {
                       background-color: #006272;
                       }

                       /* active selected tab in the sidebarmenu */
                       .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                       background-color: #006272;
                                 }
                       '
      ),
      # You can include the CountUp.js library from a CDN in your Shiny UI
      tags$head(
        tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/countup.js/2.0.7/countUp.umd.js")
      ),
      
      
      
      
    ),
    #############--------------  overview   UI --------------##########
    tabItems(
      
      
      tabItem(
        tabName = "overview",
        fluidRow(box(
          h2("NYC Yellow Taxi Average Data in 2022", align = "center"),
          width = 12
        )),
        fluidRow(
          valueBoxOutput("avg_fare"),
          valueBoxOutput("avg_trip_distance"),
          valueBoxOutput("total_vendors"),
          valueBoxOutput("avg_pickups_box"),
          valueBoxOutput("avg_dropoffs")
        ),
        fluidRow(column(6, plotlyOutput("pickup_plot")),
                 column(6, plotlyOutput("dropoff_plot"))),
        fluidRow(
          column(12, 
                 tags$p("The 'pickup_plot' and 'dropoff_plot' show the distribution of taxi pickups and dropoffs across different locations in NYC. The plots provide insights into the hotspots and popular areas for taxi services. The color intensity represents the density of pickups and dropoffs in each location. This information can be useful for understanding the demand and patterns of taxi usage in different areas of the city. In the below plot shows trip counts over the day. Please click ", 
                        tags$span("play", style = "font-weight: bold;"), " to show.",
                        style = "font-style: italic; text-align: justify;"
                 )
          )
        ),
        fluidRow(column(
          width = 11, offset = 1,
                 # Add a spacer to push the button to the right
                 div(style = "display: flex; justify-content: flex-end;",
                 actionButton("info_button_over", "ℹ️ User Guide")
                 )
        )
        ),
        
        
        fluidRow(
          h2("Trip counts over days in Year 2022",align = "center")
        ),
        
        fluidRow(
          plotlyOutput("tripPlot_animation")
        ),
        fluidRow(
          p(HTML('<i class="fas fa-info-circle"></i> Note: This data is from a subset of 1,178,450 observations out of 8 million.'), style = "font-style: italic; text-align: center;")
        ),
        fluidRow(column(6,
                        plotlyOutput("density_distance_plot")
        ),
        column(6,
               
            plotlyOutput("density_fare_plot")
          
          
        )
          
          
        )
      ),
      
      
      
      
      ## ------------------------------Passenger UI-------------------------####
      
      
      tabItem(
        tabName = "passenger",
        h2("Passenger Information", style = "font-size: 24px; font-weight: bold; text-align: center; color: #88a0c1; text-transform: uppercase;") %>% 
          tags$br() %>% 
          tags$br(),
        fluidRow(
          column(
            3,
            offset = 4,
            # Add offset to center the checkboxGroupInput
            checkboxGroupInput(
              "vendor",
              "Vendor ID:",
              choices = unique(trips_by_months$VendorID),
              selected = unique(trips_by_months$VendorID)
            )
          ),
          
          
          column(3,
                 selectInput(
                   "borough",
                   "Pickup Borough:",
                   
                   choices = unique(trips_by_hour_day$pikup_Borough)
                 ))
          
          
          
          
          
        ),
        fluidRow(column(6, plotlyOutput("monthly_plot")),
                 column(6, plotlyOutput(
                   "passenger_heatmap"
                 ))),
        
        fluidRow(column(width = 12,align = "center",
              h3("Mapping Fare Price Relationships With Passengers: A Sankey Diagram")
          
        )
          
        ),
        
        fluidRow(
          column(12,
                 plotlyOutput("sankey_plot")
            
          )
          
        )
      ),
      
      
      
      
      # -----------------------Trip UI----------------------------------#######
      tabItem(
        tabName = "trips",
        h2("Trips Information", style = "font-size: 24px; font-weight: bold; text-align: center; color: #88a0c1; text-transform: uppercase;") %>% 
          tags$br() %>% 
          tags$br(),
        
        
        fluidRow(column(width = 6,align = "center",
          h4("Time of Day and Speed Analysis of Trips Map",style = "font-weight: bold;")
        ),
        
        column(width = 6,align = "center",
          h4("Hours of the Day and Monthly Trip Trends",style = "font-weight: bold;")
               
          
        )
          
        ),
        
        ####line plot
        
        fluidRow(
          column(6,
                 
                 selectInput("map_choice", "Select Map", choices = c("Speed Map", "Hourly Map"), selected = "Hourly Map")
          ),
          
          column(6, align = "left",
          selectInput(
            "pickup_borough_hr",
            "Select Pickup Borough:",
            choices = c("All", unique(merged_df_plot2$pikup_Borough)),
            selected = "All",
            
            
          )
          )
        ),
        
        fluidRow(
          
          column(6, conditionalPanel(condition = "input.map_choice == 'Speed Map'", leafletOutput("speed_map")),
                 conditionalPanel(condition = "input.map_choice == 'Hourly Map'", leafletOutput("hourly_Map"))
                 ),
          
          column(6,
      
          plotlyOutput("trip_line_plot")
        )),
        
        
        
        fluidRow(column(width = 12,align = "center",
                        h4("Fare Amount Prediction Model ",style = "font-weight: bold;")
                        
        )
        
        ),
        # User inputs
        fluidRow(
          column(
            width = 6,
            
            selectInput(
              "pikup_Borough_input",
              "Borough:",
              choices = unique(Model_merged_df$pikup_Borough)
            ),
            
            numericInput("trip_distance_input", "Trip Distance:", value = 0),
            numericInput("passenger_count_input", "Passenger Count:", value = 1),
            selectInput("month_input", "Month:",
                        choices = unique(Model_merged_df$month)),
            selectInput(
              "seasons_input",
              "Seasons:",
              choices = unique(Model_merged_df$seasons)
            )
          ),
          column(
            width = 6,
            numericInput("rain_input", "Rain (mm):", value = 0),
            numericInput("s_fall_input", "Snowfall (cm):", value = 0),
            numericInput("max_temp_input", "Max Temperature (°C):", value = 0),
            numericInput("min_temp_input", "Min Temperature (°C):", value = 0),
            selectInput(
              "wday_input",
              "Day of the Week:",
              choices = unique(Model_merged_df$wday)
            )
          )
        ),
        
        # Predict button
        actionButton("predict_button", "Predict"),
        
        # Result box
        fluidRow(column(
          width = 12,
          h4("Predicted Fare Amount($):"),
          verbatimTextOutput("fare_prediction")
        ))
      )
      
      
      ,
      
      ### --------------------------Payments UI------------------------_##########
      tabItem(tabName = "payments",
              h2("Payments Information", style = "font-size: 24px; font-weight: bold; text-align: center; color: #88a0c1; text-transform: uppercase;") %>% 
                tags$br() %>% 
                tags$br(),
              
              fluidRow(column(width = 6,align = "center",
                              h4(" Fare and Distance Relations: Business Days vs. Non-Business Days (Radial Plot)",style = "font-weight: bold;")
              ),
              
              column(width = 6,align = "center",
                     h4("Payment Type Distribution: Sunburst Plot",style = "font-weight: bold;")
                     
                     
              )
              
              ),
              
              
              fluidRow(
                
                column(6,
                
                plotlyOutput("radar_plot")
                
                
                
                
                ),
                column(6,
                plotlyOutput("sunburstPlot")
                  
                )
                
              ),
              fluidRow(column(width = 6,align = "center",
                              h4("Average Fare Amount vs Average Distance: Bubble Plot (Business Days vs. Non-Business Days)",style = "font-weight: bold;")
              ),
              
              column(width = 6,align = "center",
                     h4("Average Tip Amount by Hour of Day",style = "font-weight: bold;")
                     
                     
              )
              
              ),
              
              fluidRow(column(6,),
                       column(6,
                              selectInput("tipplotType", "Select plot type:", choices = c("Hourly Average Tips", "Payment Type Tips"), selected = "Hourly Average Tips")
                              
                              )
                       ),
              
              fluidRow(
                column(6,
                       plotlyOutput("bubblePlot"),
                       # Add a button to show/hide explanation
                       actionButton("show_bubble_plot_explanation", "Show buble plot Explanation", 
                                    icon = icon("question"), style = "font-weight:bold;"),
                       # Add a conditional panel for the explanation
                       conditionalPanel(
                         condition = "input.show_bubble_plot_explanation % 2 == 1",  # Updated condition
                         wellPanel(
                           h4("Bubble Chart Explanation"),
                           p("This interactive bubble chart represents the relationship between different variables. The x-axis represents the average distance, the y-axis represents the average fare per distance, and the size of the bubbles represents the count. Each bubble is color-coded based on the pickup borough. You can hover over the bubbles to see more details."),
                           p("Feel free to interact with the chart by zooming, panning, or clicking on the legend to filter the data."),
                           p("Click the 'Show Bubble Chart Explanation' button again to hide this explanation.")
                         )
                       )
                       
                  
                ),
                
                column(6,
                       plotlyOutput("tips_plot")
                )
                
              )
              
              
              ),
      
 
      
      
      
  ############### weather UI ----------------------------------------_#################
      
      tabItem(tabName = "weather",
              h2("Weather Information", style = "font-size: 24px; font-weight: bold; text-align: center; color: #88a0c1; text-transform: uppercase;") %>% 
                tags$br() %>% 
                tags$br(),
              fluidRow(
                
                column(6,
                       plotlyOutput("avg_weatherPlot")
                ),
                column(6,
                       plotlyOutput("bar_plot_output"))
              ),
              
              fluidRow(
                column(6,
                  p("Above")
                )
                
              ),
              
              fluidRow(column(12,
                plotlyOutput("weather_agg_plot")
              ))
              ),
      
  
  
  ##### ---------------------------------ABout UI ---------------------------------------- ###########
      tabItem(tabName = "about",
              h2("About This Project", style = "font-size: 24px; font-weight: bold; text-align: center; color: #88a0c1; text-transform: uppercase;") %>% 
                tags$br() %>% 
                tags$br(),
              
              fluidRow(
                includeHTML("./html/project_des.html")
              ),
              
              fluidRow(
                column(width =12,
                       
                includeHTML("./html/reference.html")
                
                
              )
                
              ),
              fluidRow(
                column(width = 12,
                       h2("ℹ️ Session Info", style = "font-weight: bold; font-size: 24px;"),
                       verbatimTextOutput("sessionInfoText")
                       
                       )
                )
              
              
              )
    )
  )
  
)

# Define server
server <- function(input, output, session) {
  
  #### Overview menue 
  #value Box
  
  ### For avg month count
  output$avg_pickups_box <- renderValueBox({
    valueBox(
      avg_pickups,
      subtitle = "Average Monthly Pickups",
      icon = icon("car", lib = "font-awesome"),
      color = "yellow"
    )
  })
  
  
  
  
  # create plots
  
  
  output$pickup_plot <- renderPlotly({
    Pickup_plot <- pickup_datetime_df %>%  # Convert POSIXct to Date
      ggplot(aes(pickup_date),  text = paste("date:", as.Date(pickup_date))) +
      
      geom_histogram(fill = "red", bins = 120) +
      theme(
        panel.background = element_rect(fill = '#ECF0F5'),
        plot.background = element_rect(fill = '#ECF0F5'),
        axis.line = element_line(colour = "black")
      ) +
      labs(title = "<b>Pickup Distribution over Months</b>", x = "Pickup dates", y = "Trips Count")+
      theme(plot.title = element_text(hjust = 0.5))  # Align title to the center
    ggplotly(Pickup_plot) %>% config(displayModeBar = F)
    
  })
  
  output$dropoff_plot <- renderPlotly({
    dropoff_plot <- dropoff_datetime_df %>%
      ggplot(aes(dropoff_date)) +
      geom_histogram(fill = "blue", bins = 120) + theme(
        panel.background = element_rect(fill = '#ECF0F5'),
        plot.background = element_rect(fill = '#ECF0F5'),
        axis.line = element_line(colour = "black")
      ) +
      labs(title = "<b>Dropoff Distribution over Months</b>", x = "Dropoff dates", y = "Trips Count")+
      theme(plot.title = element_text(hjust = 0.5))  # Align title to the center
    
    ggplotly(dropoff_plot) %>% config(displayModeBar = F)
    
    
  })
  
  
  ### AVG Fair amount per month
  
  output$avg_fare <- renderValueBox({
    valueBox(
      paste("$", round(avg_fare, 2)),
      subtitle = "Average Fare Amount Per Trip",
      icon = icon("dollar-sign", lib = "font-awesome"),
      color = "green"
    )
  })
  
  ## AVG trip distance
  output$avg_trip_distance <- renderValueBox({
    valueBox(
      round(avg_trip_distance, 2),
      subtitle = "Average Trip Distance (miles)",
      icon = icon("road", lib = "font-awesome"),
      color = "blue"
    )
  })
  
  ## Trips over days animation plot 
  # Render the plot
  output$tripPlot_animation <- renderPlotly({
  # Create the ggplot object
  
  p <- ggplot(trips_per_day_year, aes(Day, Number_of_Trips, frame = frame)) +
    geom_line(color = "blue") 
  
  # Convert the ggplot to Plotly
  fig <- ggplotly(p) %>%
    layout(
      yaxis = list(
        title = "Number of Trips"
      ),
      xaxis = list(
        title = "Days of the year(2022)"
      ),
      plot_bgcolor = "#ECF0F5",
      paper_bgcolor = "#ECF0F5"
    ) %>% 
    config(displayModeBar = FALSE)%>% 
    animation_opts(
      frame = 100, 
      transition = 0, 
      redraw = FALSE
    ) %>%
    animation_slider(
      currentvalue = list(
        prefix = "Day "
      )
    )
  
  # Display the plot
  fig
  
  
})
  
  
  output$density_distance_plot <- renderPlotly({
    gg <- ggplot(data = trip_density_df) +  
      geom_density(aes(x = trip_distance, color = month)) + 
      ylab("Density") + 
      xlab("Trip distance (Miles)") +
      labs(title = "<b>Density Plot of Trip Distance by Month</b>") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold")) 
    
    ggplotly(gg) %>%
      layout(
        legend = list(
          title = list(text = "<b>Month</b>"),
          bgcolor = "#ECF0F5"
        ),
        plot_bgcolor = "#ECF0F5",
        paper_bgcolor = "#ECF0F5"
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  
  output$density_fare_plot <- renderPlotly({
    gg2 <- ggplot(data = trip_density_df) +  
      geom_density(aes(x = fare_amount, color = month)) + 
      ylab("Density") + 
      xlab("Fare Amount($)") +
      labs(title = "<b>Density Plot of Fare Amount by Month</b>") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold")) 
    
    ggplotly(gg2) %>%
      layout(
        legend = list(
          title = list(text = "<b>Month</b>"),
          bgcolor = "#ECF0F5"
        ),
        plot_bgcolor = "#ECF0F5",
        paper_bgcolor = "#ECF0F5"
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  
  ###-------------------------Passenger Menus-------------------------------------###
  
  output$passenger_heatmap <- renderPlotly({
    # Filter the data by the selected Borough
    filtered_data <-
      trips_by_hour_day[trips_by_hour_day$pikup_Borough == input$borough,]
    
    pass_heatmap_plot <-
      ggplot(filtered_data, aes(x = hour, y = wday, fill = passenger_count)) +
      geom_tile(aes(
        fill = passenger_count,
        text = paste(
          'Hour:',
          hour,
          '<br>Day of the Week: ',
          wday,
          '<br>Passenger Count: ',
          passenger_count,
          '<br>Average Distance: ',
          round(avg_distance, 2)
        )
      )) +
      scale_fill_gradient(low = "white",
                          high = "steelblue",
                          limits = c(0, max(filtered_data$passenger_count))) +
      labs(
        x = "Hour of the Day",
        y = "Day of the Week",
        fill = "Passenger Count",
        title = "<b>Heatmap of Hourly Distribution of Passengers</b>"
      ) + theme(
        panel.background = element_rect(fill = '#ECF0F5'),
        plot.background = element_rect(fill = '#ECF0F5'),
        legend.background = element_rect(fill = '#ECF0F5'),
        axis.line = element_line(colour = "black")
      )
    
    ggplotly(pass_heatmap_plot, tooltip = "text") %>% config(displayModeBar = F)
    
  })
  
  ### Line plot
  
  output$monthly_plot <- renderPlotly({
    # Filter data based on the selected borough and vendor
    selected_data <- trips_by_months %>%
      filter(pikup_Borough == input$borough &
               (is.null(input$vendor) | VendorID %in% input$vendor))
    
    # Create the line plot
    plot_ly(
      selected_data,
      x = ~ month,
      y = ~ passenger_count,
      color = ~ as.factor(VendorID),
      colors = "Set1",
      type = 'scatter',
      mode = 'lines+markers',
      hoverinfo = 'text',
      text = ~ paste(
        'VendorID: ',
        VendorID,
        '<br>Passenger Count: ',
        passenger_count,
        '<br>Avg Distance: ',
        round(avg_distance, 2)
      ),
      showlegend = TRUE
    ) %>%
      layout(
        title = list(
          text = "<b>Monthly Passenger Count by Vendor for Selected Borough</b>",
          size = 8
        ),
        xaxis = list(title = "Month"),
        yaxis = list(title = "Passenger Count"),
        plot_bgcolor = "#ECF0F5",
        paper_bgcolor = "#ECF0F5",
        legend = list(
          title = list(text = "<b>VendorID</b>")
        )
      ) %>%
      config(displayModeBar = FALSE)  # Hide the mode bar
  })
  
  
  
  output$sankey_plot <- renderPlotly({
    
    fig <- plot_ly(
      type = "sankey",
      domain = list(
        x =  c(0,1),
        y =  c(0,1)
      ),
      orientation = "h",
      valuesuffix = " ($) Total",
      
      node = list(
        label = sankey_data$label,
        color = sankey_data$node_colors,
        pad = 15,
        thickness = 15,
        line = list(
          color = "black",
          width = 0.5
        )
      ),
      link = list(
        source = sankey_data$source,
        target = sankey_data$target,
        value =  sankey_data$value,
        label = sankey_data$label,
        color = sankey_data$link_colors
      )
    ) 
    
    fig <- fig %>% layout(
      #title = "Mapping Fare Price Relationships: A Sankey Diagram",
      font = list(family = "Arial",
                  size = 14,
                  color = "black",
                  weight = "bold"
      ),
      xaxis = list(showgrid = FALSE, zeroline = FALSE),
      yaxis = list(showgrid = FALSE, zeroline = FALSE),
      plot_bgcolor = "#ECF0F5",
      paper_bgcolor = "#ECF0F5"
    )%>%
      config(displayModeBar = FALSE)  # Hide the mode bar
    
    fig
  })
  
  ###------------------------------ Trip menu ----------------------------------######
  
  #### Model Prediction
  
  # Create an observer for the user input changes
  observeEvent(input$predict_button, {
    # Get the user input values
    trip_distance <- as.numeric(input$trip_distance_input)
    passenger_count <- as.integer(input$passenger_count_input)
    month <- as.integer(input$month_input)
    wday <-
      factor(input$wday_input, levels = unique(Model_merged_df$wday))
    seasons <-
      factor(input$seasons_input, levels = unique(Model_merged_df$seasons))
    rain <- as.numeric(input$rain_input)
    s_fall <- as.numeric(input$s_fall_input)
    max_temp <- as.integer(input$max_temp_input)
    min_temp <- as.integer(input$min_temp_input)
    pikup_Borough <-
      factor(input$pikup_Borough_input,
             levels = unique(Model_merged_df$pikup_Borough))
    
    # Create a data frame with the user input
    trip_data <- data.frame(
      trip_distance = trip_distance,
      passenger_count = passenger_count,
      month = month,
      wday = wday,
      seasons = seasons,
      rain = rain,
      s_fall = s_fall,
      max_temp = max_temp,
      min_temp = min_temp,
      pikup_Borough = pikup_Borough
    )
    
    # Check if any input is null
    trip_data_is_null <- any(is.null(trip_data) | trip_data == "")
    
    # print(trip_data )
    
    
    # Make the fare amount prediction
    # Check if any input is null or contains missing values
    if (any(is.null(trip_data) |
            trip_data == "" | is.na(trip_data))) {
      # Show notification for missing input
      output$fare_prediction <- renderText({
        showNotification(
          "Please enter all required inputs.",
          type = "warning",
          duration = 10,
          closeButton = TRUE
        )
        ""
      })
    } else {
      fare_prediction <- predict(rf_model, newdata = trip_data)
      
      # Check if the prediction is NA
      if (is.na(fare_prediction)) {
        # Show notification for prediction error
        output$fare_prediction <- renderText({
          showNotification(
            "An error occurred while predicting the fare amount.",
            type = "error",
            duration = 10,
            closeButton = TRUE
          )
          ""
        })
      } else {
        # Check if the predicted fare amount is greater than 200
        if (fare_prediction > 100) {
          # Show notification for fare amount greater than 200
          output$fare_prediction <- renderText({
            showNotification(
              "The predicted fare amount is greater than 100.\n please check input range",
              type = "message",
              duration = 20,
              closeButton = TRUE
            )
            paste("Predicted Fare Amount:",
                  round(fare_prediction, 2))
          })
        } else {
          # Display the predicted fare amount
          output$fare_prediction <- renderText({
            paste("Predicted Fare Amount:",
                  round(fare_prediction, 2))
          })
        }
      }
    }
    
  })
  
  ####### Houlry passanger count and fare amount
  
  
  output$trip_line_plot <- renderPlotly({
    pickup_borough <- input$pickup_borough_hr
    
    if (pickup_borough == "All") {
      filtered_data <- merged_df_plot2
    } else {
      filtered_data <-
        merged_df_plot2 %>% filter(pikup_Borough == pickup_borough)
    }
    
    p <- ggplot(data = filtered_data,
                aes(
                  hpick,
                  n,
                  color = Month,
                  pikup_Borough = pikup_Borough,
                  avg_fare = round(avg_fare, 2)
                )) +
      geom_line(size = 1.5) +
      labs(x = "Hour of the day", y = "Count")+ 
      theme(
        panel.background = element_rect(fill = '#ECF0F5'),
        plot.background = element_rect(fill = '#ECF0F5'),
        legend.background = element_rect(fill = '#ECF0F5'),
        axis.line = element_line(colour = "black")
      )
    
    p_plotly <-
      ggplotly(p, tooltip = c("pikup_Borough", "avg_fare","Month")) %>% config(displayModeBar = FALSE)
    
    p_plotly
  })
  
  

  
  
  
  # Render the speed map
  output$speed_map <- renderLeaflet({
    if (input$map_choice == "Speed Map") {
      pick_fast <- trip_speed_group %>%
        filter(tgroup == "fast") %>%
        select(long = pickup_longitude, lat = pickup_latitude)
      pick_slow <- trip_speed_group %>%
        filter(tgroup == "slow") %>%
        select(long = pickup_longitude, lat = pickup_latitude)
      
      leaflet(pick_fast) %>% 
        addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark Map") %>%
        addProviderTiles(providers$CartoDB.Positron, group = "Light Map") %>%
        addScaleBar() %>%
        setView(-73.95, 40.74, zoom = 11) %>%
        addHeatmap(max = 140, gradient = "Reds", radius = 12,
                   minOpacity = 0.15, blur = 15, group = "Fast Trips") %>%
        addHeatmap(lng = pick_slow$long, lat = pick_slow$lat,
                   max = 100, gradient = "Blues", radius = 12, 
                   minOpacity = 0.25, blur = 15, group = "Slow Trips") %>%
        addLayersControl(
          baseGroups = c("Dark Map", "Light Map"),
          overlayGroups = c("Fast Trips", "Slow Trips"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        addLegend(
          "bottomright",
          title = "Trip Speed",
          colors = c("#FF0000", "#0000FF"),
          labels = c("Fast", "Slow"),
          opacity = 0.8,
          na.label = "No Data"
        )
    }
  })
  
  ### Hourly Map 
 
  # Render the hourly map
  output$hourly_Map <- renderLeaflet({
    if (input$map_choice == "Hourly Map") {
      morning_trips <- hr_trip_group %>%
        filter(time_of_day == "Morning") %>%
        select(long = pickup_longitude, lat = pickup_latitude)
      afternoon_trips <- hr_trip_group %>%
        filter(time_of_day == "Afternoon") %>%
        select(long = pickup_longitude, lat = pickup_latitude)
      evening_trips <- hr_trip_group %>%
        filter(time_of_day == "Evening") %>%
        select(long = pickup_longitude, lat = pickup_latitude)
      late_night_trips <- hr_trip_group %>%
        filter(time_of_day == "Late night") %>%
        select(long = pickup_longitude, lat = pickup_latitude)
      
      # Create the leaflet map
      leaflet() %>%
        addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark map") %>%
        addProviderTiles(providers$CartoDB.Positron, group = "Light map") %>%
        addScaleBar() %>%
        setView(-73.95, 40.74, zoom = 11) %>%
        addHeatmap(data = morning_trips, max = 140, gradient = "Reds", radius = 12,
                   minOpacity = 0.15, blur = 15, group = "Morning trips") %>%
        addHeatmap(data = afternoon_trips, max = 100, gradient = "Blues", radius = 12,
                   minOpacity = 0.25, blur = 15, group = "Afternoon trips") %>%
        addHeatmap(data = evening_trips, max = 100, gradient = "Greens", radius = 12,
                   minOpacity = 0.25, blur = 15, group = "Evening trips") %>%
        addHeatmap(data = late_night_trips, max = 100, gradient = "Purples", radius = 12,
                   minOpacity = 0.25, blur = 15, group = "Late night trips") %>%
        addLayersControl(baseGroups = c("Dark map", "Light map"),
                         overlayGroups = c("Morning trips", "Afternoon trips", "Evening trips", "Late night trips"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addLegend(position = "bottomright",
                  colors = c("red", "blue", "green", "purple"),
                  labels = c("Morning", "Afternoon", "Evening", "Late night"),
                  title = "Time of Day")
        
    }
  })
  
  ##### _______________________________Payments menu _______________________________________ #####
  
  
  
    output$radar_plot <- renderPlotly({
      fig <- plot_ly(
        type = 'scatterpolar',
        fill = 'toself'
      )
      
      unique_boroughs <- unique(summary_borough$pikup_Borough)
      
      for (borough in unique_boroughs) {
        group_data <- subset(summary_borough, pikup_Borough == borough)
        
        fig <- fig %>%
          add_trace(
            r = c(group_data$AvgDistance_Biz, group_data$AvgDistance_NonBiz, group_data$AvgFarePerDistance_Biz, group_data$AvgFarePerDistance_NonBiz),
            theta = c("AvgDistance_Biz", "AvgDistance_NonBiz", "AvgFarePerDistance_Biz", "AvgFarePerDistance_NonBiz"),
            name = borough,
            mode = 'markers+lines'
          )
      }
      
      fig <- fig %>%
        layout(
          title = list(
            text = "Radar/Spider Chart \n",
            x = 0.5,
            xanchor = "center",
            y = 1,
            yanchor = "top"
          ),
          polar = list(
            radialaxis = list(
              visible = TRUE,
              range = c(0, max(
                summary_borough$AvgDistance_Biz,
                summary_borough$AvgDistance_NonBiz,
                summary_borough$AvgFarePerDistance_Biz,
                summary_borough$AvgFarePerDistance_NonBiz
              ) * 1.1
              ),
              title = list(
                text = "Values",
                font = list(size = 12)
              )
            )
          ),
          paper_bgcolor = "#ECF0F5"
        ) %>%
        layout(legend=list(title=list(text='<b> Pikup Borough  </b>'))) %>%
        config(displayModeBar = FALSE)
      
      fig
    })

  
  ####### Sunburst
    
    ## sunburst data fromate 
    as.sunburstDF <- function(DF, value_column = NULL, add_root = FALSE) {
      require(data.table)
      
      colNamesDF <- names(DF)
      
      if (is.data.table(DF)) {
        DT <- copy(DF)
      } else {
        DT <- data.table(DF, stringsAsFactors = FALSE)
      }
      
      if (add_root) {
        DT[, root := "Total"]
      }
      
      colNamesDT <- names(DT)
      hierarchy_columns <- setdiff(colNamesDT, value_column)
      DT[, (hierarchy_columns) := lapply(.SD, as.factor), .SDcols = hierarchy_columns]
      
      if (is.null(value_column) && add_root) {
        setcolorder(DT, c("root", colNamesDF))
      } else if (!is.null(value_column) && !add_root) {
        setnames(DT, value_column, "values", skip_absent = TRUE)
        setcolorder(DT, c(setdiff(colNamesDF, value_column), "values"))
      } else if (!is.null(value_column) && add_root) {
        setnames(DT, value_column, "values", skip_absent = TRUE)
        setcolorder(DT, c("root", setdiff(colNamesDF, value_column), "values"))
      }
      
      hierarchyList <- list()
      
      for (i in seq_along(hierarchy_columns)) {
        current_columns <- colNamesDT[1:i]
        if (is.null(value_column)) {
          currentDT <- unique(DT[, ..current_columns][, values := .N, by = current_columns], by = current_columns)
        } else {
          currentDT <- DT[, lapply(.SD, sum, na.rm = TRUE), by = current_columns, .SDcols = "values"]
        }
        setnames(currentDT, length(current_columns), "labels")
        hierarchyList[[i]] <- currentDT
      }
      
      hierarchyDT <- rbindlist(hierarchyList, use.names = TRUE, fill = TRUE)
      
      parent_columns <- setdiff(names(hierarchyDT), c("labels", "values", value_column))
      hierarchyDT[, parents := apply(.SD, 1, function(x) {
        fifelse(all(is.na(x)), yes = NA_character_, no = paste(x[!is.na(x)], sep = ":", collapse = " - "))
      }), .SDcols = parent_columns]
      hierarchyDT[, ids := apply(.SD, 1, function(x) {
        paste(x[!is.na(x)], collapse = " - ")
      }), .SDcols = c("parents", "labels")]
      hierarchyDT[, c(parent_columns) := NULL]
      return(hierarchyDT)
    }
    
    sunburstDF <- as.sunburstDF(payment_type_sunburst, value_column = "payment_type_label_count", add_root = TRUE)
    sunburstDF
    output$sunburstPlot <- renderPlotly({
      plot_ly(
        data = sunburstDF,
        ids = ~ids,
        labels = ~labels,
        parents = ~parents,
        values = ~values,
        type = 'sunburst',
        branchvalues = 'total'
      ) %>% 
        layout(
          plot_bgcolor = "#ECF0F5",
          paper_bgcolor = "#ECF0F5"
        ) %>% 
        config(displayModeBar = FALSE)
    })
    
  
  #### Buble plot 
    
    
    output$bubblePlot <- renderPlotly({
      # Create the bubble plot for business trips
      bubble_plot <- plot_ly(summary_borough, x = ~AvgDistance_Biz, y = ~AvgFarePerDistance_Biz, 
                             size =~Count_Biz, color = ~pikup_Borough, alpha = 0.7,
                             type = "scatter", mode = "markers",
                             text = paste("Business: Count -", summary_borough$Count_Biz),
                             hovertemplate = paste(
                               "<b>Business days Trip</b><br>",
                               "Avg Distance: %{x}<br>",
                               "Avg Fare per Distance: %{y}<br>",
                               "Scaled Count: %{marker.size}<br><br>",
                               "<extra></extra>"
                             )) %>%
        layout(xaxis = list(title = "Average Distance"),
               yaxis = list(title = "Average Fare per Distance", type= "log"),
               showlegend = TRUE)
      
      # Add another layer for non-business trips
      bubble_plot <- add_trace(bubble_plot, data = summary_borough, 
                               x = ~AvgDistance_NonBiz, y = ~AvgFarePerDistance_NonBiz, 
                               size = ~Count_NonBiz, color = ~pikup_Borough,
                               type = "scatter", mode = "markers",
                               text = paste("Non-Business: Count -", summary_borough$Count_NonBiz),
                               hovertemplate = paste(
                                 "<b>Non-Businessdays Trip</b><br>",
                                 "Avg Distance: %{x}<br>",
                                 "Avg Fare per Distance: %{y}<br>",
                                 "Scaled Count: %{marker.size:,}<br><br>",
                                 "<extra></extra>"
                               ),
                               showlegend = FALSE) %>%
        layout(legend = list(title = list(text = "<b>Pickup Borough</b>"))) %>% 
        layout(
          yaxis = list(type= "log"),
          plot_bgcolor = "#ECF0F5",
          paper_bgcolor = "#ECF0F5"
        ) %>% 
        config(displayModeBar = FALSE)
      
      bubble_plot
      
    })
    
    #tips plot 
    
    output$tips_plot <- renderPlotly({
      if (input$tipplotType == "Hourly Average Tips") {
        plot_ly(hourly_avg_tip, x = ~hour, y = ~avg_tip_amount, color = ~wday, type = "scatter", mode = "lines") %>%
          layout(
            xaxis = list(title = "Hour"),
            yaxis = list(title = "Average Tip Amount"),
            colorway = c("blue", "green", "red", "orange", "purple", "yellow", "cyan", "magenta"),
            legend = list(title = list(text = "Weekday")),
            plot_bgcolor = "#ECF0F5",
            paper_bgcolor = "#ECF0F5"
          ) %>%
          config(displayModeBar = FALSE)
      } else if (input$tipplotType == "Payment Type Tips") {
        plot_ly(long_tip_amount_data, x = ~hour, y = ~total_credit_tip_amount, name = "Credit", type = "scatter", mode = "lines") %>%
          add_trace(y = ~total_cash_tip_amount, name = "Cash", type = "scatter", mode = "lines") %>%
          layout(
            title = "Average Tip Amount by Payment Type",
            xaxis = list(title = "Hour"),
            yaxis = list(title = "Average Tip Amount",type="log"),
            colorway = c("blue", "green"),
            legend = list(title = list(text = "Payment Type")),
            plot_bgcolor = "#ECF0F5",
            paper_bgcolor = "#ECF0F5"
          ) %>%
          config(displayModeBar = FALSE)
      }
    })
    
    
    
    
    #### --------------Weather Server --------------------------------------------------------###
    
    # Convert data frame to long format
    long_weekly_avg <- tidyr::pivot_longer(weather_weekly_avg, starts_with("avg_"), names_to = "Variable", values_to = "Value")
    
    # Create the line plot using plot_ly
    output$avg_weatherPlot <- renderPlotly({
      plot_ly(long_weekly_avg, x = ~week, y = ~Value, color = ~Variable, type = "scatter", mode = "lines") %>%
        layout(
          title = "<b>Weekly Averages</b>",
          xaxis = list(title = "Week"),
          yaxis = list(title = "Average Value",type="log"),
          colorway = c("blue", "green", "red", "orange", "purple", "yellow", "cyan", "magenta"),
          legend = list(title = list(text = "Variable")),
          plot_bgcolor = "#ECF0F5",
          paper_bgcolor = "#ECF0F5"
        ) %>% config(displayModeBar = FALSE)
    })
  
    ## Seasons Barplot 
    
    # Create the bar plot with logarithmic scale
    seasons_bar_plot <- reactive({
      plot_ly(season_count, x = ~pikup_Borough, y = ~count, color = ~seasons, type = "bar") %>%
        layout(
          title = "<b>Number of Seasons for Each Pickup Borough</b>",
          xaxis = list(title = "Pickup Borough"),
          yaxis = list(title = "Count (log10 scale)", type = "log"),
          legend = list(title = list(text = "Seasons")),
          barmode = "stack",
          plot_bgcolor = "#ECF0F5",
          paper_bgcolor = "#ECF0F5"
        )%>% config(displayModeBar = FALSE)
    })
    
    # Render the bar plot
    output$bar_plot_output <- renderPlotly({
      seasons_bar_plot()
    })
    
    ### waether vs Fare and trips 
    
    # Create the line plot using plot_ly
    output$weather_agg_plot <- renderPlotly({
      plot_ly(monthly_agg_data, x = ~month, y = ~total_trips, name = "Number of Trips", type = "scatter", mode = "lines") %>%
        add_trace(x = ~month, y = ~avg_fare_amount, name = "Average Fare Amount", type = "scatter", mode = "lines") %>%
        add_trace(x = ~month, y = ~total_s_depth, name = "Snow Depth", type = "scatter", mode = "lines") %>%
        add_trace(x = ~month, y = ~total_max_temp, name = "Max Temperature", type = "scatter", mode = "lines") %>%
        add_trace(x = ~month, y = ~total_rain, name = "Rain", type = "scatter", mode = "lines") %>%
        layout(
          title = "<b>Number of Trips, Average Fare Amount, and Weather Variables by Month</b>",
          xaxis = list(title = "Month"),
          yaxis = list(title = "Count/Average Fare Amount", type = "log"),
          legend = list(title = list(text = "Variables")),
          plot_bgcolor = "#ECF0F5",
          paper_bgcolor = "#ECF0F5"
        )%>% config(displayModeBar = FALSE)
    })
    
    
  
  # Notification
  
  
  observe({
    if (!is.null(input$dashboard_tabs) &&
        input$dashboard_tabs == "passenger") {
      showNotification(
        "If you don't choose a VendorID, the default will be both.",
        type = "message",
        duration = NULL,
        closeButton = TRUE
      )
    }
  })
  
  # Define the modal dialog
  startup <- modalDialog(
    title = "Important: Please Read!",
    "Hello, before heading into this dashboard, it is important to understand a few terms related to the taxi data. This dataset contains information about taxi trips in New York City, including details like pickup and dropoff locations, trip duration, fare amounts, and more.\n\nFeel free to explore the different tabs and visualizations to gain insights from the data.\n\nNote: The data used in this dashboard is a sample subset and may not represent the entire dataset.",
    easyClose = FALSE,
    footer = modalButton("I Understand")
  )
  
  # Show the modal dialog
  observe({
    showModal(startup)
  })
  
  
  ### User Info 
  
  # Show user guide modal dialog
  observeEvent(input$info_button_over, {
    showModal(
      modalDialog(
        title = "User Guide",
        "Step 1: Use the filters below the chart to select specific data.",
        "Step 2: The chart will update dynamically based on your filter selections.",
        "Step 3: Hover over the data points on the chart to view additional information.",
        easyClose = TRUE
      )
    )
  })
  
  
  
  
  # Track the state of the explanation panel
  bubble_explanation_shown <- FALSE
  
  observeEvent(input$show_bubble_plot_explanation, {
    # Toggle the state of the explanation panel
    bubble_explanation_shown <- !bubble_explanation_shown
    
    # Show or hide the explanation panel
    if (bubble_explanation_shown) {
      shinyjs::show("bubble_explanation")
    } else {
      shinyjs::hide("bubble_explanation")
    }
  })
  
  
  
  output$sessionInfoText <- renderPrint({
    sessionInfo()
  })
  
}


# Run the application
shinyApp(ui = ui, server = server)




#### reference 


#https://stackoverflow.com/questions/57395424/how-to-format-data-for-plotly-sunburst-diagram
#https://plotly.com/r/sunburst-charts/
#https://www.kaggle.com/code/breemen/nyc-taxi-fare-data-exploration
#https://plotly.com/r/sankey-diagram/
#https://nycdatascience.com/blog/student-works/data-analysis-on-nyc-yellow-taxi-data/
#https://medium.com/codex/how-to-automatically-generate-data-structure-for-sankey-diagrams-6082e332139f
#https://www.kaggle.com/code/maheshdadhich/strength-of-visualization-python-visuals-tutorial
#https://plotly.com/r/line-charts/
#https://www.colorhexa.com/ecf0f5

