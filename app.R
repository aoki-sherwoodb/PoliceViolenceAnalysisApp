# final-project
# Ben Aoki-Sherwood and Avery Watts
# June 2022, Intro to Data Science

library(shiny)
library(tidyverse)
library(shinyWidgets)
library(RColorBrewer)
library(lubridate)
library(leaflet)
library(maps)
library(randomForest)
library(ggplot2)
library(ggrepel)
library(caret)

#load data
joined_cities <- read_csv('data/joined_cities.csv')
hate_crimes <- read_csv('data/hate_crimes.csv')

#fit preliminary rf model with all variables
set.seed(220)
rf_data <- joined_cities %>%
  mutate(had_killing = as.factor(ifelse(killings > 0, 'yes', 'no')),
         had_killing = fct_relevel(had_killing, 'yes')) %>%
  select(-c('city', 'state_code', 'state', 'killings')) %>%
  drop_na()
killings_rf_all <- randomForest(had_killing ~ . , data = rf_data, mtry = 14)

#set up data and model for smaller 3-variable rf model fitting
rf_data3 <- select(rf_data, c('had_killing', 'all', 'police_force_size', 'total_population'))
killings_rf_3 <- randomForest(had_killing ~ . , data = rf_data3)

#set ui constants
div_style <- "color:black; background-color:white; 
             margin-bottom:20px; border: 2px solid black; 
             border-radius: 8px; font-size: medium; 
             padding-top: 5px; padding-right: 5px; 
             padding-bottom: 5px; padding-left: 5px;
             margin-top:20px;"

ui <- navbarPage(
  "Police Accountability",
  tabPanel("About", 
           HTML('<center><img src="download.png" width="400"></center>'),
           h3(),
           div("On this site, we provide visualizations of the relationships between
               city and state demographics and police violence, allowing users to investigate
               which factors are correlated with police killings and to predict whether a 
               city with a given set of demographic statistics will have had a police killing.",
               style=div_style),
           div(strong("Data Chronology: "), br(), "The police killings and city demographics
               data are from 2015, while the hate crimes data was collected over the period from 2009 to 2016.",
               style = div_style)
  ),
  tabPanel("Police Residency", 
           tabsetPanel(
             tabPanel("Percentage of Police Living in their Communities", 
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons(
                            "race",
                            "Percentage of Police Living in their Communities by Race",
                            c("All Races" = "all", "White" = "white", "Non-White" = "non_white", "Black" = "black", "Hispanic" = "hispanic"))),
                        mainPanel(leafletOutput("residence_map"), 
                                  div("This graph shows the percentage of police officers that live in the cities that they serve. 
                                      If an officer is policing their own neighborhood, they are more likely to have more personal connections and treat people better.
                                      A user can also subset the data by race to see what percentages of officers of a specific race live in the communities they patrol.",
                                      style=div_style)))),
             tabPanel("Residency and Police Killings",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons(
                            "race2",
                            "Percentage of Police Living in their Communities by Race",
                            c("All Races" = "all", "White" = "white", "Non-White" = "non_white", "Black" = "black", "Hispanic" = "hispanic"))),
                        mainPanel(plotOutput(outputId = "residency_scatterplot"),
                                  div("This graph shows the relationship between the percentage of police living in the communites they patrol and the number of people police have killed in an average year.
                                      There does not seem to be a strong correlation between the two variables. 
                                      However, it would be interesting to look into whether this effects other types of police misconduct.",
                                      style=div_style)))))),
  tabPanel("Demographics and Police Brutality",
           tabsetPanel(
             tabPanel("Mapping Police Killings",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("demographic_info",
                                       "Demographic Information",
                                       c("Median Household Income" = "income",
                                         "Hate Crimes" = "hatecrimes",
                                         "Share of Nonwhite State Residents" = "nonwhite_pop"))),
                        mainPanel(leafletOutput("map_police_killings"),
                                  div("This map shows both police killings in major cities and a demographic variable of each state.
                           ", style=div_style)))),
             tabPanel("Police Killings by Communities' Proportion of Race",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons(
                            "race3",
                            "Proportion of Race in a Given City",
                            c("Native American" = "race_prop_american_indian_and_alaska_native", 
                              "White" = "race_prop_white", 
                              "Black or African American" = "race_prop_black_or_african_american", 
                              "Hispanic" = "race_prop_hispanic_or_latino", 
                              "Asian" = "race_prop_asian"))),
                        mainPanel(plotOutput(outputId = "killings_scatterplot"),
                                  div("This graph displays the relationship between the number of people killed by police in a city 
                        and the proportion of that city's population of the selected race.",
                                      style=div_style)))))),
  tabPanel("Has Your City Had a Police Killing?",
           sidebarLayout(sidebarPanel(div(strong("Enter the police force size, the proportion of police that live
                                                 within the city, and the population for a city, and we will predict whether 
                                                 that city has had a police killing using a random forest model."), style = div_style),
                                      numericInput(inputId = 'force', label = 'Police Force Size: ',
                                                   min = 1, max = max(rf_data$police_force_size), value = round(mean(rf_data$police_force_size))),
                                      numericInput(inputId = 'all', label = 'Proportion of Police Living in City: ',
                                                   min = 0, max = 1, value = round(mean(rf_data$all), 2)),
                                      numericInput(inputId = 'pop', label = 'Population: ',
                                                   min = 1, max = max(rf_data$total_population), value = round(mean(rf_data$total_population))),
                                      actionButton(inputId = 'fit', label = 'Predict!')),
           mainPanel(verticalLayout(
             div(strong('Our prediction based on the selected city stats: '), style = div_style),
             div(strong(textOutput(outputId = 'class')), style = div_style),
             div(strong("Below is a breakdown of which city characteristics (from the entire dataset, not just the 3 used above) our model deemed
                                      most important for determining whether or not a city had a police killing.
                                      Note that many of these characteristics are difficult to explain or 
                                      likely confounded with other characteristics: for example, longitude is
                                      probably highly correlated with population, and so we should take its
                                      seemingly high importance with a grain of salt."), 
                 style = div_style),
             plotOutput(outputId = 'var_imp_plot'))))),
  tabPanel("Citations",
           div(
             p("For data on police residency:", 
               a(href='https://github.com/fivethirtyeight/data/tree/master/police-locals',"538 Github")),
             p("For data on police killings:", 
                 a(href='https://github.com/fivethirtyeight/data/tree/master/police-killings',"538 Github")),
             p("For data on demographics: ",
                 a(href='https://public.opendatasoft.com/explore/dataset/us-cities-demographics/table/', "OpenDataSoft")),
             p("For data on hate crimes: ",
                 a(href="https://github.com/fivethirtyeight/data/tree/master/hate-crimes", "538 Github")),
             style=div_style)),
  #make the background interesting
  setBackgroundColor(
    color = c("#F7FBFF", "#687178"),
    gradient = "radial",
    direction = c("top", "left")))

server <- function(input, output){
  filteredData <- reactive({
    select(joined_cities, input$race)
  })
  
  
  output$residence_map <- renderLeaflet({ 
    residency <- NULL
    if(input$race == "all"){
      residency <- joined_cities$all
    } else if (input$race == "white"){
      residency <- joined_cities$white
    } else if (input$race == "non_white"){
      residency <- joined_cities$non_white
    } else if (input$race == "black"){
      residency <- joined_cities$black
    } else if (input$race == "hispanic"){
      residency <- joined_cities$hispanic
    } else {
      residency <- joined_cities$all
    }
    
    leaflet(data=joined_cities) %>%
      addTiles(data = map("state", fill = TRUE, plot = FALSE)) %>%
      addPolygons(data = map("state", fill = TRUE, plot = FALSE), fillColor = topo.colors(10, alpha = NULL), stroke = FALSE) %>%
      addCircles(lng=joined_cities$lon, lat=joined_cities$lat, radius = ~150000*residency, weight = 1, color = "#777777", 
                 #fillColor = ~colorNumeric(brewer.pal.info["Blues",], joined_cities$all),
                 fillOpacity = 0.7, popup = ~paste(residency))
  })
  
  output$map_police_killings <- renderLeaflet({ 
    bins <- NULL
    fill_by <- NULL
    palette <- NULL
    legend <- NULL
    if(input$demographic_info == "hatecrimes") {
      fill_by <- hate_crimes$avg_hatecrimes_per_100k_fbi
      bins <- c(0, 2, 4, 6, 8, 10, 12)
      palette <- "YlOrRd"
      legend <- "# hatecrimes per 100k"
    } else if (input$demographic_info == "income") {
      fill_by <- hate_crimes$median_household_income
      bins <- c(80000, 70000, 60000, 50000, 40000, 30000)
      palette <- "Blues"
      legend <- "Median Household Income"
    } else if (input$demographic_info == "nonwhite_pop") {
      fill_by <- hate_crimes$share_non_white
      bins <- c(0, .2, .4, .6, .8, 1)
      palette <- "Blues"
      legend <- "Share of Non-White Pop"
    } else {
      
    }
    pal <- colorBin(palette, domain = fill_by, bins = bins)
    leaflet(data=joined_cities) %>%
      addTiles(data = map("state", fill = TRUE, plot = FALSE)) %>%
      addPolygons(data = map("state", fill = TRUE, plot = FALSE), 
                  fillColor = ~pal(fill_by), 
                  stroke = FALSE, 
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,) %>%
      addCircles(lng=joined_cities$lon, lat=joined_cities$lat, radius = ~30000*joined_cities$killings, weight = 1, color = "#777777", 
                 #fillColor = ~colorNumeric(brewer.pal.info["Blues",], joined_cities$all),
                 fillOpacity = 0.7, popup = ~paste(joined_cities$killings)) %>%
      addLegend(pal = pal, values = ~hate_crimes$avg_hatecrimes_per_100k_fbi, opacity = 0.7, title = legend,
                position = "bottomright") %>%
      addControl("<h2 style='padding:0px;margin:0px'>Police Killings</h2>", position="bottomleft")
  })
  
  output$killings_scatterplot <- renderPlot({
    ggplot(joined_cities, aes_string(x=input$race3, y="killings", label="city")) +
      geom_point() +
      geom_label_repel(box.padding   = 0.35, 
                       point.padding = 0.5,
                       segment.color = 'grey50') +
      geom_smooth(method='lm')
  })
  
  output$residency_scatterplot <- renderPlot({
    ggplot(joined_cities, aes_string(x=input$race2, y="killings", label="city")) +
      geom_point() +
      geom_label_repel(box.padding   = 0.35, 
                       point.padding = 0.5,
                       segment.color = 'grey50') +
      geom_smooth(method='lm')
    
  })
  
  output$var_imp_plot <- renderPlot({varImpPlot(killings_rf_all, n.var = 15, main = 'City Stats Most Influential to Classification')})
  
  output$class <- renderText({prediction()})
  
  prediction <- eventReactive(input$fit,
                              {city <- as_tibble(data.frame(all = input$all, 
                                                            police_force_size = input$force, 
                                                            total_population = input$pop))
                              p <- predict(killings_rf_3, newdata = city)
                              ifelse(p == 'yes', 'We predict that your city HAS had a police killing.', 'We predict that your city has NOT had a police killing.')
                              
                              })
  
}
shinyApp(ui = ui, server = server )
