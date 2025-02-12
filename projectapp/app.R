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
library(rmapshaper)
library(profvis)
library(viridis)


# Load spatial data ====
state_boundaries <- states(cb = TRUE) %>% st_transform(crs = 4326)
simplified_boundaries <- ms_simplify(state_boundaries, keep = 0.005) 
state_boundaries <- simplified_boundaries
county_boundaries <- counties(cb = TRUE) %>% st_transform(crs = 4326)
simplified_boundaries <- ms_simplify(county_boundaries, keep = 0.001) 
county_boundaries <- simplified_boundaries

# Race color mapping ====
race_colors <- c(
  White = "darkblue",
  Black = "cyan",
  Native = "chartreuse",
  Asian = "darkmagenta",
  PacificIslander = "yellow",
  Mixed = "magenta" 
)
 #UI ========
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h1("Race Distribution Analysis App"),
      h3("Choose Analysis Type"),
      selectInput("analysis_type", "Select Analysis Type:", choices = c("Geogaphical Analysis", "Distribution Analysis by State and County","Distribution Analysis by Race")),
      
      ## Location Analysis =======
      conditionalPanel(
        condition = "input.analysis_type == 'Distribution Analysis by State and County'",
        selectInput("trend_division", "State or County:", choices = c("State", "County")),
        
        ### Dropdown for selecting State if Division is County =======
        conditionalPanel(
          condition = "input.trend_division == 'County'",
          selectInput("trend_state", "Select State:", choices = unique(county_11_23$STATE_NAME)),
          selectInput("trend_county", "Select County:", choices = NULL),  # Placeholder
          sliderInput("year_pie", "Select Year for Pie Chart:", min = 2011, max = 2023, value = 2011, step = 1, animate = animationOptions(interval = 1500, loop = TRUE)),
          tags$p("This section displays two visualizations: a time series plot and a pie chart. At the top, the time series plot shows the total population of each race in the selected state or county from 2011 to 2023, with a logarithmic scale applied for improved clarity. Below, the pie chart illustrates the racial distribution in the chosen state or county for the selected year. By pressing the play button beneath the year slider, you can visualize how the distribution changes over time.")
        ),
        
        ### Dropdown for selecting State if Division is State ========
        conditionalPanel(
          condition = "input.trend_division == 'State'",
          selectInput("trend_state_only", "Select State:", choices = unique(state_11_23_long$NAME)),
          sliderInput("year_pie", "Select Year for Pie Chart:", min = 2011, max = 2023, value = 2011, step = 1, animate = animationOptions(interval = 1500, loop = TRUE)),
          tags$p("This section displays two visualizations: a time series plot and a pie chart. At the top, the time series plot shows the total population of each race in the selected state or county from 2011 to 2023, with a logarithmic scale applied for improved clarity. Below, the pie chart illustrates the racial distribution in the chosen state or county for the selected year.By pressing the play button beneath the year slider, you can visualize how the distribution changes over time.")
        )
      ),

      ## Map View ==========
      conditionalPanel(
        condition = "input.analysis_type == 'Geogaphical Analysis'",
        selectInput("division", "Select Division:", choices = c("State", "County")),
        selectInput("race", "Select Race:", choices = c("White", "Black", "Native", "Asian", "PacificIslander", "Mixed")),
        sliderInput("year_map", 
                    "Select Year:", 
                    min = 2011, max = 2023, value = 2021, step = 1),
        tags$h3("Welcome to my RShiny dashboard!"),
        tags$p("This app is designed to analyze racial trends across the United States from 2011 to 2023. It aims to inspire further exploration into the events and factors that contribute to significant changes in these populations."),
        tags$div(style = "height: 5px;"),
        tags$p("The panel above allows you to select different methods of analysis. This choropleth map is the first of three visualizations. It displays the racial distribution for the selected race across states or counties. The red areas represent states or counties with the highest proportion of the selected race in relation to their total population, while the yellow areas show those with the lowest proportion of that race.")
        ),
      ## Race Analysis =======
      conditionalPanel(
        condition = "input.analysis_type == 'Distribution Analysis by Race'",
        selectInput("race2","Select Race:", choices = c("White", "Black", "Native", "Asian", "PacificIslander", "Mixed")),
        textOutput("selected_race2"),
        tags$p("This section displays three time series plots. The first plot, to the right of this panel, shows the proportion of the selected race in each state over time."),
        tags$div(style = "height: 5px;"),
        tags$p("The second plot illustrates the percentage change in that race from the previous year across states. The black line represents the linear regression of all states' changes over this period, and the shaded region indicates the 99% predicted interval. This helps identify states and years that may be statistically significant and worth further investigation. It’s important to note that the year 2020 should be excluded from analysis due to potential inaccuracies in the Census Bureau data for that year."),
        tags$div(style = "height: 5px;"),
        tags$p("Finally, the chart at the bottom of this pannel shows the percentage change in total population from the previous year across states. This plot allows for comparison between racial trends and overall population trends."),
        tags$div(style = "height: 5px;"),
        plotlyOutput("tot_pop_change")
        )
      ),
    ##MainPanel ==========
    mainPanel(
      conditionalPanel(
        condition = "input.analysis_type == 'Geogaphical Analysis'",
        h2("Geographic Distribution of Relative Population Proportions"),  # Title only shown for Map View
        leafletOutput("map", height = "700px")
      ),
      conditionalPanel(
        condition = "input.analysis_type == 'Distribution Analysis by State and County' && input.trend_division == 'State'",
        h2("Distribution Analysis by State and County"),
        plotlyOutput("time_series_plot"),
        tags$div(style = "height: 10px;"),
        plotlyOutput("pie_chart")
      ),
      conditionalPanel(
        condition = "input.analysis_type == 'Distribution Analysis by State and County' && input.trend_division == 'County'",
        h2("Distribution Analysis by State and County"),
        plotlyOutput("county_time_series_plot"),
        tags$div(style = "height: 10px;"),
        plotlyOutput("county_pie_chart")
      ),
      conditionalPanel(
        condition = "input.analysis_type == 'Distribution Analysis by Race'",
        h2("Distribution Analysis by Race"),
        tags$div(style = "height: 80px;"),
        plotlyOutput("race_pop_by_state"),
        tags$div(style = "height: 130px;"),
        plotlyOutput("race_change_by_state")
      )
    )
  )
)

#server ============
server <- function(input, output, session) {
  
  ## Update county choices based on selected state for Trend Analysis ======
  observeEvent(input$trend_state, {
    counties_in_state <- county_boundaries %>% 
      filter(STATE_NAME == input$trend_state) %>% 
      pull(NAMELSAD)
    
    updateSelectInput(session, "trend_county", choices = counties_in_state)
  })
  ## Map ========
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      setView(lng = -96, lat = 37.8, zoom = 4)
  })
  
  observeEvent(c(input$analysis_type, input$division, input$race, input$year_map), {
    if (input$analysis_type == "Geogaphical Analysis") {
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
        ##State Map =====        
        ###Palette=======
        pal_data <- state_boundaries_merged %>% filter(!is.na(population))
        pal <- colorQuantile("YlOrRd", pal_data$population, n = 5)
        pal_colors <- unique(pal(sort(pal_data$population))) # hex codes
        pal_labs <- quantile(pal_data$population, seq(0,1,.2))
        pal_labs_vector <- as.vector(pal_labs)
        pal_labs_vector <- round(pal_labs_vector, 3)
        pal_labs <- paste(lag(pal_labs_vector)," - ",pal_labs_vector)[-1]
        
        ###Plot========
        leafletProxy("map") %>%
          addPolygons(data = state_boundaries_merged,
                      fillColor = ~pal(population),
                      weight = 1.5, color = "black", opacity = 1, fillOpacity = 0.7,
                      highlightOptions = highlightOptions(weight = 5, color = "white", fillOpacity = 0.7),
                      label = labelstate) %>%
          addLegend("bottomright",
                    colors=pal_colors,
                    values = pal_data$population,
                    title = paste(input$race,"Race Population Proportion by Quantile"),
                    opacity = 1,
                    labels = pal_labs)
        

      } else if (input$division == "County") {
        ## County map ========
        county_data <- county_11_23 %>%
          filter(year == input$year_map) %>%
          select(STATE,year, NAMELSAD, STATE_NAME, !!sym(input$race)) %>%
          rename(population = !!sym(input$race)) %>%
          ungroup() 
        
        # Merge with county boundaries with filtered county census data
        county_boundaries_merged <- county_boundaries %>%
          left_join(county_data, by = c("NAMELSAD" = "NAMELSAD", "STATE_NAME" = "STATE_NAME"))
        
        
        labelcounty <- sprintf("<strong>%s, %s</strong><br/>%.3f%% %s",
                               county_boundaries_merged$NAMELSAD,
                               county_boundaries_merged$STATE_NAME,
                               round(county_boundaries_merged$population * 100, 3),
                               input$race) %>% 
          lapply(htmltools::HTML)
        
        ##Palette =====
        pal_data <- county_boundaries_merged %>% filter(!is.na(population))
        pal <- colorQuantile("YlOrRd", pal_data$population, n = 5)
        pal_colors <- unique(pal(sort(pal_data$population))) # hex codes
        pal_labs <- quantile(pal_data$population, seq(0,1,.2))
        pal_labs_vector <- as.vector(pal_labs)
        pal_labs_vector <- round(pal_labs_vector, 3)
        pal_labs <- paste(lag(pal_labs_vector)," - ",pal_labs_vector)[-1]
        
        ##Plot ======
        leafletProxy("map") %>%
          addPolygons(data = county_boundaries_merged,
                      fillColor = ~pal(population),
                      weight = 0.2, color = "black", opacity = 1, fillOpacity = 0.7,
                      highlightOptions = highlightOptions(weight = 5, color = "white", fillOpacity = 0.7),
                      label = labelcounty) %>%
          addLegend("bottomright",
                    colors=pal_colors,
                    values = pal_data$population,
                    title = paste(input$race,"Race Population Proportion by Quantile"),
                    opacity = 1,
                    labels = pal_labs)
      }
    }
  })
  
  ##State======
  ###Time series plot for State ========
  output$time_series_plot <- renderPlotly({
    req(input$trend_state_only)
    time_series_data <- state_11_23_long %>% filter(NAME==input$trend_state_only) %>%
      pivot_longer(cols = c(White, Black, Native, Asian, PacificIslander, Mixed),
                   names_to = "Race",
                   values_to = "Proportion")
    
    plot_ly(time_series_data,
            x = ~year,
            y = ~Proportion*Total,
            color = ~Race, colors=race_colors,
            type = 'scatter',
            mode = 'lines+markers') %>%
      layout(title = paste("Race Distribution in",input$trend_state_only,"Over Time"),
             xaxis = list(title = "Year"),
             yaxis = list(title = "Population (Log Scale)",
                          color= "Race",
                          type= "log"))
  })
  
  ### Pie chart for State ========
  output$pie_chart <- renderPlotly({
    req(input$year_pie)
    pie_data <- state_11_23_long %>%
      filter(year == input$year_pie, NAME == input$trend_state_only) %>%
      select(White, Black, Native, Asian, PacificIslander, Mixed) %>%
      pivot_longer(cols = everything(), names_to = "Race", values_to = "Proportion") %>%
      mutate(Proportion = Proportion * 100)
    
    plot_ly(pie_data, labels = ~Race, values = ~Proportion, marker = list(colors = race_colors), type = 'pie', textinfo = 'label+percent') %>%
      layout(title = paste("Race Distribution for", input$year_pie, "in",input$trend_state_only),
             showlegend = TRUE)
  })
  
  ##County ==========
  ### Time series plot for County =======
  output$county_time_series_plot <- renderPlotly({
    req(input$trend_state, input$trend_county)
    county_data <- county_11_23_long %>%
      filter(STATE_NAME == input$trend_state, NAMELSAD == input$trend_county)
    
    plot_ly(data = as.data.frame(county_data),
            x = ~year, y = ~Proportion * TOT_POP, color = ~Race, colors=race_colors, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = paste("Race Distribution in",input$trend_county,"Over Time"),
             xaxis = list(title = "Year"),
             yaxis = list(title = "Population (Log Scale)",
                          color= "Race",
                          type= "log"))
  })
  
  ### Pie chart for County =======
  output$county_pie_chart <- renderPlotly({
    req(input$year_pie, input$trend_state, input$trend_county)
    pie_data <- county_11_23_long %>%
      filter(year == input$year_pie, STATE_NAME == input$trend_state, NAMELSAD == input$trend_county)
    
    plot_ly(pie_data, labels = ~Race, values = ~Proportion, marker = list(colors = race_colors), type = 'pie', textinfo = 'label+percent') %>%
      layout(title = paste("Race Distribution for", input$year_pie, "in", input$trend_county),
             showlegend = TRUE)
  })
  
  ##By race analysis ========
  ####Time series ========
  color_palette <- rainbow(51)
  state_names <- unique(state_11_23_long$NAME)
  state_colors <- setNames(color_palette, state_names)
  
  output$race_pop_by_state <- renderPlotly({
    req(input$race2)
    state_race_prop_filt <- state_race_prop %>% filter(Race == input$race2)

    plot_ly(data=as.data.frame(state_race_prop_filt),
           x = ~year, y = ~Proportion, color = ~NAME, colors=state_colors, type = 'scatter', mode = 'lines+markers') %>%
           layout(title = paste("Proportion of",input$race2,"Race by State Over Time"),
           xaxis = list(title = "Year"),
           yaxis = list(title = paste("Proportion of",input$race2,"Race")))
  })
  ####Yearly Change ==========
  output$race_change_by_state <- renderPlotly({
    req(input$race2)
    state_race_change_filt <- state_race_change %>% filter(Race == input$race2)
    state_race_change_filt$PercentChange <- state_race_change_filt$PercentChange * 100
    
    lm_fit <- lm(PercentChange ~ year, data = state_race_change_filt)
    predictions <- predict(lm_fit, newdata = state_race_change_filt, interval = "prediction", level = 0.99)
    predictions_df <- cbind(state_race_change_filt, predictions)
    
    plot_ly(data=as.data.frame(predictions_df),
            x = predictions_df$year, y = ~PercentChange, color = predictions_df$NAME, colors=state_colors, type = 'scatter', mode = 'lines+markers') %>%
      add_lines(y=fitted(lm_fit),line = list(color = 'black', width = 3), showlegend = FALSE) %>%
      add_trace(data=predictions_df, y = ~lwr, type = 'scatter', mode = 'none',
                fill = 'tonexty',fillcolor = 'rgba(0, 0, 0, 0.05)', showlegend = FALSE) %>%
      add_trace(data=as.data.frame(predictions), y = ~upr, type = 'scatter', mode = 'none',
                fill = 'tonexty',fillcolor = 'rgba(0, 0, 0, 0.05)', showlegend = FALSE) %>% 
      layout(title = paste("Yearly Percent Change in",input$race2,"Race by State"),
             xaxis = list(title = "Year"),
             yaxis = list(title = "Percent Change"))
})
  
  ####Sidebar Total pop=====
  output$tot_pop_change <- renderPlotly(({
    state_totals <- state_11_23_long %>%
      group_by(NAME) %>%
      mutate(Change= (Total-lag(Total))/(lag(Total)),
      ) %>%
      filter(!is.na(Change)) %>%
      ungroup() %>%
      select(year,NAME,Change)
    
    state_totals$Change <- state_totals$Change * 100
    
    lm_fit2 <- lm(Change ~ year, data = state_totals)
    predictions2 <- predict(lm_fit2, newdata = state_totals, interval = "prediction", level = 0.99)
    predictions_df2 <- cbind(state_totals, predictions2)
    
    plot_ly(data=as.data.frame(predictions_df2),
            x = predictions_df2$year, y = ~Change, color = predictions_df2$NAME, colors=state_colors, type = 'scatter', mode = 'lines+markers',visible = "legendonly") %>%
      add_lines(y=fitted(lm_fit2),line = list(color = 'black', width = 3), showlegend = FALSE) %>%
      add_trace(data=predictions_df2, y = ~lwr, type = 'scatter', mode = 'none',
                fill = 'tonexty',fillcolor = 'rgba(0, 0, 0, 0.05)', showlegend = FALSE) %>%
      add_trace(data=as.data.frame(predictions2), y = ~upr, type = 'scatter', mode = 'none',
                fill = 'tonexty',fillcolor = 'rgba(0, 0, 0, 0.05)', showlegend = FALSE) %>%
      layout(title = paste("Yearly Percent Change in Total Population"),
             xaxis = list(title = "Year"),
             yaxis = list(title = "Population"))
  }))
}

shinyApp(ui = ui, server = server)