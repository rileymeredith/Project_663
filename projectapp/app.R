library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(tigris)
library(rlang)
library(htmltools)
library(ggplot2)
library(plotly)
library(tidyr)

# Load spatial data
state_boundaries <- states(cb = TRUE) %>% st_transform(crs = 4326)
county_boundaries <- counties(cb = TRUE) %>% st_transform(crs = 4326)

#Race color mapping
race_colors <- c(
  White = "royalblue",
  Black = "cyan",
  Native = "greenyellow",
  Asian = "orangered",
  PacificIslander = "yellow",
  Mixed = "magenta" 
)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h3("Choose Analysis Type"),
      selectInput("analysis_type", "Select Analysis Type:", choices = c("Map View", "Trend Analysis")),
      
      # Conditional panel for Trend Analysis
      conditionalPanel(
        condition = "input.analysis_type == 'Trend Analysis'",
        selectInput("trend_division", "State or County:", choices = c("State", "County")),
        
        # Dropdown for selecting State if Division is County
        conditionalPanel(
          condition = "input.trend_division == 'County'",
          selectInput("trend_state", "Select State:", choices = unique(county_11_23$STATE_NAME)),
          selectInput("trend_county", "Select County:", choices = NULL),  # Placeholder
          sliderInput("year_pie", "Select Year for Pie Chart:", min = 2011, max = 2023, value = 2021, step = 1)
        ),
        
        # Dropdown for selecting State if Division is State
        conditionalPanel(
          condition = "input.trend_division == 'State'",
          selectInput("trend_state_only", "Select State:", choices = unique(state_11_23_long$NAME)),
          sliderInput("year_pie", "Select Year for Pie Chart:", min = 2011, max = 2023, value = 2021, step = 1)
        )
      ),
      
      # Conditional panel for Map View
      conditionalPanel(
        condition = "input.analysis_type == 'Map View'",
        selectInput("division", "Select Division:", choices = c("State", "County")),
        selectInput("race", "Select Race:", choices = c("White", "Black", "Native", "Asian", "PacificIslander", "Mixed")),
        sliderInput("year_map", "Select Year:", min = 2011, max = 2023, value = 2021, step = 1)
      )
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.analysis_type == 'Map View'",
        h2("Map View"),  # Title only shown for Map View
        leafletOutput("map")
      ),
      conditionalPanel(
        condition = "input.analysis_type == 'Trend Analysis' && input.trend_division == 'State'",
        h2("Trend Analysis - State"),
        plotlyOutput("time_series_plot"),
        plotlyOutput("pie_chart")
      ),
      conditionalPanel(
        condition = "input.analysis_type == 'Trend Analysis' && input.trend_division == 'County'",
        h2("Trend Analysis - County"),
        plotlyOutput("county_time_series_plot"),
        plotlyOutput("county_pie_chart")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Update county choices based on selected state for Trend Analysis
  observeEvent(input$trend_state, {
    counties_in_state <- county_boundaries %>% 
      filter(STATE_NAME == input$trend_state) %>% 
      pull(NAMELSAD)
    
    updateSelectInput(session, "trend_county", choices = counties_in_state)
  })
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      setView(lng = -96, lat = 37.8, zoom = 4)
  })
  
  observeEvent(c(input$analysis_type, input$division, input$race, input$year_map), {
    if (input$analysis_type == "Map View") {
      leafletProxy("map") %>% clearShapes() %>% clearControls()
      
      if (input$division == "State") {
        state_data <- state_11_23_long %>% 
          filter(year == input$year_map) %>% 
          select(NAME, !!sym(input$race)) %>% 
          rename(population = !!sym(input$race))
        
        # Merge with state boundaries with filtered state census data
        state_boundaries_merged <- state_boundaries %>% 
          left_join(state_data, by = "NAME")
        
        labelstate <- sprintf("<strong>%s</strong><br/>%.3f%% %s",
                              state_boundaries_merged$NAME,
                              round(state_boundaries_merged$population * 100, 3),
                              input$race) %>% 
          lapply(htmltools::HTML)
        
        pal_data <- state_boundaries_merged %>% filter(!is.na(population))
        pal <- colorQuantile("BuPu", pal_data$population, n = 5)
        
        # State map
        leafletProxy("map") %>%
          addPolygons(data = state_boundaries_merged,
                      fillColor = ~pal(population),
                      weight = 1.5, color = "black", opacity = 1, fillOpacity = 0.7,
                      highlightOptions = highlightOptions(weight = 5, color = "white", fillOpacity = 0.7),
                      label = labelstate) %>%
          addLegend("bottomright",
                    pal = pal,
                    values = pal_data$population,
                    title = "Population Distribution by Quantiles",
                    opacity = 1)
        
      } else if (input$division == "County") {
        county_data <- county_11_23 %>%
          filter(year == input$year_map) %>%
          select(NAMELSAD, STATE_NAME, !!sym(input$race)) %>%
          rename(population = !!sym(input$race))
        
        # Merge with county boundaries with filtered county census data
        county_boundaries_merged <- county_boundaries %>%
          left_join(county_data, by = c("NAMELSAD" = "NAMELSAD", "STATE_NAME" = "STATE_NAME"))
        
        labelcounty <- sprintf("<strong>%s, %s</strong><br/>%.3f%% %s",
                               county_boundaries_merged$NAMELSAD,
                               county_boundaries_merged$STATE_NAME,
                               round(county_boundaries_merged$population * 100, 3),
                               input$race) %>% 
          lapply(htmltools::HTML)
        
        
        pal_data <- county_boundaries_merged %>% filter(!is.na(population))
        pal <- colorQuantile("BuPu", pal_data$population, n = 5)
        
        # County map
        leafletProxy("map") %>%
          addPolygons(data = county_boundaries_merged,
                      fillColor = ~pal(population),
                      weight = 0.2, color = "black", opacity = 1, fillOpacity = 0.7,
                      highlightOptions = highlightOptions(weight = 5, color = "white", fillOpacity = 0.7),
                      label = labelcounty) %>%
          addLegend("bottomright",
                    pal = pal,
                    values = pal_data$population,
                    title = "Population Distribution by Quantiles",
                    opacity = 1)
      }
    }
  })
  
  # Time series plot for State
  output$time_series_plot <- renderPlotly({
    req(input$trend_state_only)
    time_series_data <- state_11_23_long %>% filter(NAME==input$trend_state_only) %>%
      pivot_longer(cols = c(White, Black, Native, Asian, PacificIslander, Mixed),
                   names_to = "Race",
                   values_to = "Proportion")
    
    plot_ly(time_series_data, x = ~year, y = ~Proportion*Total, color = ~Race, colors=race_colors, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Race Distribution Over Time",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Population",
                          color= "Race"))
  })
  
  # Pie chart for State
  output$pie_chart <- renderPlotly({
    req(input$year_pie)
    pie_data <- state_11_23_long %>%
      filter(year == input$year_pie, NAME == input$trend_state_only) %>%
      select(White, Black, Native, Asian, PacificIslander, Mixed) %>%
      pivot_longer(cols = everything(), names_to = "Race", values_to = "Proportion") %>%
      mutate(Proportion = Proportion * 100)
    
    plot_ly(pie_data, labels = ~Race, values = ~Proportion, marker = list(colors = race_colors), type = 'pie', textinfo = 'label+percent') %>%
      layout(title = paste("Race Distribution for", input$year_pie),
             showlegend = TRUE)
  })
  # Time series plot for County
  output$county_time_series_plot <- renderPlotly({
    req(input$trend_state, input$trend_county)
    county_data <- county_11_23_long %>%
      filter(STATE_NAME == input$trend_state, NAMELSAD == input$trend_county)
    
    plot_ly(data = as.data.frame(county_data),
            x = ~year, y = ~Proportion * TOT_POP, color = ~Race, colors=race_colors, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Race Distribution Over Time - County",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Population", color = "Race"))
  })
  
  # Pie chart for County
  output$county_pie_chart <- renderPlotly({
    req(input$year_pie, input$trend_state, input$trend_county)
    pie_data <- county_11_23_long %>%
      filter(year == input$year_pie, STATE_NAME == input$trend_state, NAMELSAD == input$trend_county)
    
    plot_ly(pie_data, labels = ~Race, values = ~Proportion, marker = list(colors = race_colors), type = 'pie', textinfo = 'label+percent') %>%
      layout(title = paste("Race Distribution for", input$year_pie, "in", input$trend_county),
             showlegend = TRUE)
  })
}

shinyApp(ui = ui, server = server)
