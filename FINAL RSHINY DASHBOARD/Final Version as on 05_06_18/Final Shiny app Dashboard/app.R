# install.packages("rsconnect")
# install.packages("shinythemes")
# install.packages("DT")
# install.packages("shiny")
# install.packages("leaflet")
# install.packages("scales")
# install.packages("lattice")
# install.packages("dplyr")
# install.packages("devtools")
# install.packages("metaplotr")
# install.packages("data.table")
# install.packages("htmltools")
# install.packages("broman")
# install.packages("magrittr")
# install.packages("RColorBrewer")
# install.packages("networkD3")
# install.packages("shinydashboard")
# install.packages("ggplot2")
# install.packages("ggthemes")
# install.packages("plotly")
# install.packages("gplots")
# install.packages("htmlwidgets")
# install.packages("ggExtra")
# install.packages("broom")
# install.packages("highcharter")
# install.packages("tidyverse")
# install.packages("sf")
# install.packages("lubridate")


library(rsconnect)
library(shinythemes)
library(DT)
library(shiny)
library(leaflet)
library(scales)
library(lattice)
library(dplyr)
library(devtools)
library(metaplotr)
library(data.table)
library(htmltools)
library(broman)
library(magrittr)
library(RColorBrewer)
library(networkD3)
library(shinydashboard)
library(ggplot2)
library(ggthemes)
library(plotly)
library(gplots)
library(htmlwidgets)
library(ggExtra)
library(broom)
library(highcharter)
library(tidyverse)
library(sf)
library(lubridate)

# remove(Packages)
setwd("/Users/yinwenjun/Downloads/Final Shiny app Dashboard")

ships<- read.csv("Violencedata_20APRL.csv")
#ships <- subset(ships, Fatalities>5)

dataset <-read.csv("Final_Combined dataset.csv")
#View(dataset)

ships <- ships[order(ships$Fatalities),]
#class(ships)
#str(ships)
#ships <- ships[order(ships$Fatalities),]
#ships <- ships[sample.int(nrow(ships), 10000),]
ui<- navbarPage(strong("ARMED CONFLICT DASHBOARD"), id="nav",
                
                tabPanel("About",img(src='IMG_2155.JPG', align = "left"),  
                         
                         fluidPage( theme = shinytheme("spacelab"), HTML(
                           paste(
                             '<br/>',
                             p("Armed conflicts within the states are generally political conflicts involving citizens fighting for internal change. 
                               It involves a group of people who are armed and ready to fight for the goal of seizing governmental powers. 
                               This group may include militias and armed civilians along with the regular armies. These conflicts result in the 
                               collapse of state institutions mainly the police and judiciary. The fighting in conflicts are usually intermittent and 
                               generally does not occur on well-defined battlefields. The twentieth century saw an upward trend in the fatalities due 
                               to armed conflicts.", align= "justify"),'<br/>',
                             p("This dashboard represents the effect of fatalities caused by armed conflict in middle eastern countries 
                               namely Afghanistan, Iraq, Syria, and Yemen. The basis for visualizations are the violent events recorded in a particular year
                               resulting in the deaths and its effect on the GDP, Mortality, Fertility, Revenue from Natural Resources etc.", align= "justify"),'<br/>',
                             h5("Download data using the Download Data button shown below\n"),
                             h6("Note: ensure to include .csv while saving the file"), '<br/>'
                             #h4("Thank you for using the app!")
                             )),
                           
                           #img(src='Image1.png', height= "100px", width= "50%", align= "bottom right"),
                           #Add a download button for dataset
                           
                           
                           downloadButton("downloadData", "Download Violence Data"),
                           downloadButton("downloadData1", "Download Variable Data")
                           
                           
                           #Add a link to github
                           
                           
                           )
                         
                ),
                
                tabPanel("Data Summary and Resources ",img(src='IMG_2155.JPG', align = "left"),
                         
                         fluidPage( HTML(
                           paste(
                             '<br/>',
                             h3("Data Summary"),'<br/>',
                             p("Data presented in our charts contain detailed information regarding armed conflict events occuring in select regions around the world. 
                               Variables of interest include Location (countries, states, and cities where conflict events are occuring), Event Type (types of conflict 
                               activities or events occuring in various regions), Target (industries and organizations that are being targeted), Nationality of Target,
                               Perpetrators (groups who are carrying out these armed conflict events), Weapon Type (weapons utilized during the events), Number of Fatalities 
                               caused by an event), and Date of specific events. Other variables include GDP, Fertility Rate, Trade Import Rate, and Trade Export Rate. 
                               For more information regarding variable selection and analysis, please refer to ", a("link to white paper"), "."), '<br/>',
                             
                             h3("Data Sources"),'<br/>',
                             p("Click on the links below to be redirected to data sources"),
                             p(a("1. Center for Systemic Peace", href = "http://www.systemicpeace.org/inscrdata.html", target = "_blank")),
                             p(a("2. Global Terrorism Database", href = "https://www.start.umd.edu/gtd/", target = "_blank")),
                             p(a("3. The Peace Research Institute Oslo", href = "https://www.prio.org/Data/Armed-Conflict/GEO-SVAC/", target = "_blank")),
                             p(a("4. The World Bank Group", href = "https://data.worldbank.org/", target = "_blank")),
                             p(a("5. United Nations Data", href = "http://data.un.org/", target = "_blank")),
                             p(a("6. United Nations Development Programme", href = "http://hdr.undp.org/en/content/human-development-index-hdi", target = "_blank")), '<br/>'
                           ))
                           
                           
                           
                )
                
                ),
                
                
                tabPanel("Interactive map",
                         h3("Map shows Lowest to Highest Number of Fatalities and Type of Events by Geographic Location", align="center"),
                         h6("Caption: icons indicate clusters of events and fatalities. click icons to drill down. Drag map in any direction to view region of interest. ", align="center"),
                         div(class="outer",
                             
                             # tags$head(
                             #   # Include our custom CSS
                             #   includeCSS("styles.css"),
                             #   includeScript("gomap.js")
                             # ),
                             
                             
shinyUI(fluidPage(
  fluidRow(
    column(12, leaflet::leafletOutput("map"))),
    fluidRow(column(12, div(style = 'overflow-x: scroll',DT::dataTableOutput("tbl"))))
  )
))),


tabPanel("Data Visualizations",
         tabsetPanel(id = "tabSelected",
                     tabPanel("Trend Graph",
                              h3("Trend Graph Shows Change of Values with Time based on Selected Variables", align="center"),
                              h6("Caption: option available to select country, select measure, and date range. ", align="center"),
                              fluidPage(
                                sidebarLayout(
                                  sidebarPanel(
                                    selectizeInput("filter1",
                                                   "Select Country",
                                                   choices = dataset$Country,
                                                   selected = ""),
                                    selectizeInput("filter2",
                                                   "Select Measure",
                                                   choices = colnames(dataset[,c(8:11, 24:25)]),
                                                   selected = "GDP"),
                                    dateRangeInput("date_range",
                                                   label = "Date Range",
                                                   min = "1960-01-01",
                                                   max = "2016-12-31",
                                                   start = "1960-01-01",
                                                   end = "2016-12-31",
                                                   format = "yyyy",
                                                   startview = "decade")
                                  ),
                                  mainPanel(highchartOutput("trendplot"))
                                )
                              )
                     ),                                  
                                 
                     
                     tabPanel("Density Plot",
                              h3("Density Plot Shows the Distribution of Data based on Selected Variables", align="center"),
                              h6("Caption: option available to select country and measure. ", align="center"),
                              fluidPage(
                                sidebarLayout(
                                  sidebarPanel(
                                    selectInput("filter4",
                                                   "Select Country",
                                                   choices = ships$country,
                                                   selected = "")
                                    
                                  ),
                                  mainPanel(highchartOutput("densityplot"))
                                )
                              )
                     ),
                     
                     tabPanel("Scatter Plot",
                              h3("Scatter Plot Shows Correlation between Number of Fatalities and other Variables of Interest", align="center"),
                              h6("Caption: option available to filter by country, hover on icons to view x and y values, hover on line to view R value. ", align="center"),
                              plotlyOutput('plot1', height= "400px"),
                              hr(),
                              fluidRow(
                                column(3,
                                       h4("Correlation Explorer"),
                                       checkboxGroupInput("Country2", "Select Country", c("Afghanistan", "Iraq", "Syria", "Yemen"), selected = c("Afghanistan", "Iraq", "Syria", "Yemen") )
                                       
                                ),
                                column(4, offset = 1,
                                       selectInput('x', 'X', choice = "Total_Violent_fatalities", names(dataset)[[25]]),
                                       selectInput('y', 'Y', choice= c("GDP", "Trade_Import", "Trade_Export"), names(dataset)[[8]])
                                       
                                )
                              )
                              
                              
                     ),
                     
                     tabPanel("Correlation Matrix",
                              h3("Correlation Matrix Plot Shows Multiple Variables and there Correlation all in one Chart", align="center"),
                              h6("Caption: colors indication whether the correlation is positive, negation, high, medium, or low. hover in each cell to view exact R value.", align="center"),
                              fluidPage(
                                highchartOutput("cormatrix"))
                     ),
                     
                     
                     tabPanel("Box Plot",
                              h3("Box Plot Shows Number of Fatalities Grouped by Event Type", align="center"),
                              h6("Caption: option available to filter by country, use legend icons to select/unselect event type, hover on box plot to view descriptive statistics. ", align="center"),
                              plotlyOutput('plot2', height= "400px"),
                              hr(),
                              fluidRow(
                                column(3,
                                       h4("Box Plot Explorer"),
                                       checkboxGroupInput("Country3", "Select Country", c("Afghanistan", "Iraq", "Syria", "Yemen"), selected = c("Afghanistan", "Iraq", "Syria", "Yemen") )
                                       
                                ),
                                column(4, offset = 1,
                                       selectInput('x1', 'X', choice = "Fatalities", names(ships)[[16]]),
                                       selectInput('y1', 'Y', choice= c("Event_type","Weapon_Type", "Nationality_of_Target"), names(ships)[[6]])
                                       
                                )
                              )
                              
                              
                     ),
                     
                     tabPanel("Sankey Diagram",
                              h3("Chart depicts Frequency from Highest to Lowest Number of Events with Associated link to Geographic Location", align="center"),
                              h6("Caption: for example, the largest node corresponds to bombings/explossions and associated with IRQ, and we can still visualize connections to other major events. ", align="center"),
                              fluidPage(
                                sankeyNetworkOutput("SankeyDiagram", width = "100%", height= "500px" )))
                     

                     )
                   
                     
                     
         ),
tabPanel("Data Table",
         h3("Table Provides View of Entire Dataset for Data Exploration", align="center"),
         h6("Caption: option available to search, sort, filter by country, filter by year, and select n entries. ", align="center"),
         fluidRow(
           column(3,
                  selectInput("country1", label = strong("Country"), c("All Countries"="", list('Afghanistan', 'Iraq', 'Syria', 'Yemen')), multiple=TRUE)
           ),
           column(3,
                  conditionalPanel("input.country1",
                                   selectInput("events1", label = strong("Events"), c("All Events"="", list('Armed Assault', 'Bombing/Explosion', 'Unknown', 'Assassination', 'Hostage Taking (Kidnapping)', 'Facility/Infrastructure Attack', 'Sexual Violence', 'Hostage Taking (Barricade Incident)', 'Hijacking', 'Unarmed Assault')), multiple=TRUE)
                  )
           ),
           column(3,
                  conditionalPanel("input.country1",
                                   selectInput("perpetrators1", label = strong("Perpetrators"), c("All Perpetrators"=""), multiple=TRUE)
                  )
           )
         ),
         fluidRow(
           column(1,
                  numericInput("min", label = strong("Year (From)"), min = 1950, max = 2017, value = 1950)
           ),
           column(1,
                  numericInput("max", label = strong("Year (To)"), min=1950, max=2017, value=2017)
           )
         ),
         hr(),
         DT::dataTableOutput("Violencetable")
         
))



######server########

server <- shinyServer(function(session,input, output) {
  
  ##downloading data
  
  # download button server replace from here 
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("violences-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(ships, file)
    })
  
  
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste("variable-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(dataset, file)
    })
  
  # download button server replace ends here 
  
  
  #### Map####
  
  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet(ships) %>%
      leaflet::addTiles(urlTemplate = "https://api.mapbox.com/styles/v1/ashoksoyal/cjgj3h2u400012rnvm99m8kjb/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiYXNob2tzb3lhbCIsImEiOiJjamdlb3RpMmEwYzA1MnhwNHpzenNrY3J4In0.lAeyiXYmXJpsgQS89ryuLg",
                        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>% 
      #addCircles(~Longitude, ~Latitude, radius= ~sqrt(Fatalities)*3000,
                 #stroke=TRUE, fillOpacity=0.5, fillColor = ~Fatalities)
      addMarkers( ~Longitude, ~Latitude, label= paste("Number of Fatalities:",ships$Fatalities), clusterOptions= markerClusterOptions(), clusterId= "Countrycluster1") %>%
           setView(lng = 44.399648, lat = 32.032916, zoom = 5) #%>%
      #addLegend( values= c(~Longitude, ~Latitude), opacity= 1)
  })
  
  in_bounding_box <- function(data, lat, long, bounds) {
    data %>%
      dplyr::filter(Latitude > bounds$south & Latitude < bounds$north & Longitude < bounds$east & Longitude > bounds$west)
  }
  
  data_map <- reactive({
    if (is.null(input$map_bounds)){
      ships
    } else {
      bounds <- input$map_bounds
      in_bounding_box(ships, lat, long, bounds)
    }
  })
 #hide map table
   #observeEvent(input$btn, hide("tbl"))
  #show map table 
   #observeEvent(input$show, show("tbl"))
  output$tbl <- DT::renderDataTable(({
    DT::datatable(data_map(), extensions = "Scroller", style = "bootstrap", class = "compact", width = "100%",
                  options = list(deferRender = TRUE, scrollY = 300, scrollX= '400px', scroller = TRUE, dom = 'tp'))
  }))
 
  ## Data Explorer server ###########################################
  
  observe({
    EventType <- if (is.null(input$country1)) character(0) else {
      filter(ships, country %in% input$country1) %>%
        `$`('Event_type') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$events1[input$events1 %in% ships$Event_type])
    updateSelectInput(session, "events", choices = ships$Event_type,
                      selected = stillSelected)
  })
  
  observe({
    perpetrators <- if (is.null(input$country1)) character(0) else {
      ships %>%
        filter(country %in% input$country1,
               is.null(input$events1) | ships$Event_type %in% input$events1) %>%
        `$`('Perpetrators') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$perpetrators1[input$perpetrators1 %in% ships$Perpetrators])
    updateSelectInput(session, "perpetrators1", choices = ships$Perpetrators,
                      selected = stillSelected)
  })
  
  
  output$Violencetable <- DT::renderDataTable({
    df <- ships %>%
      filter(
        is.null(input$country1) | country %in% input$country1,
        is.null(input$events1) | Event_type %in% input$events1,
        is.null(input$perpetrators1) | Perpetrators %in% input$perpetrators1,
        Year >= input$min,
        Year <= input$max
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Latitude, '" data-long="', Longitude, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
  
  # ####Sankey Diagram####
  #Sort Data
  ships1 <- ships[order(ships$Country_Code,ships$Event_type),]
  # Create fake data
  nlevels(ships$Event_type)
  lalala <- as.character(ships1$Country_Code[(1)])
  lalala
  dadada <- as.character(ships1$Event_type[1])
  dadada
  
  #src and target from countries to event types
  for (x in 2:47030) { 
    a <- as.character(ships1$Country_Code[x]) 
    b <- as.character(ships1$Event_type[x])
    if(a == as.character(ships1$Country_Code[x-1]) && b == as.character(ships1$Event_type[x-1])){
    }else{
      lalala <- c(lalala , a )
      dadada <- c(dadada, b)}
  }
  
  
  
  networkData <- data.frame(lalala, dadada)
  
  networkData2 <- data.frame(lalala, dadada, stringsAsFactors = FALSE)
  
  nodes <- data.frame(name = unique(c(lalala, dadada)), stringsAsFactors = FALSE)
  nodes$id <- 0:(nrow(nodes) - 1)
  
  
  # create a data frame of the edges that uses id 0:9 instead of their names
  edges <- networkData %>%
    left_join(nodes, by = c("lalala" = "name")) %>%
    select(-lalala) %>%
    rename(source = id) %>%
    left_join(nodes, by = c("dadada" = "name")) %>%
    select(-dadada) %>%
    rename(target = id)
  
  allelements <- c(levels(ships1$Country_Code), levels(ships1$Event_type))
  col1 <- nlevels(ships1$Country_Code)
  col2 <- nlevels(ships1$Event_type)
  col3 <- col1+1
  col4 <- col1+col2
  width <- c()
  
  for (m in 1:col1){
    for (n in col3:col4){
      aa <- sum(ships1$Country_Code == allelements[m] & ships1$Event_type == allelements[n])
      width <- c(width,c(aa))
    }
  }
  
  edges$width <- width
  
  nodes$group <- ifelse(nodes$name %in% lalala, "countries", "Event_Types")
  

  output$SankeyDiagram <- renderSankeyNetwork({
    sankeyNetwork(Links = edges, Nodes = nodes, Source = "source",
                  Target = "target", Value = "width", NodeID = "name",
                  fontSize = 12, nodeWidth = 30)

  })
  
  
  
  
   ##### Correlation Graph#####

  dataset1 <- reactive({
    dataset[(dataset$Country== input$Country2),]
  })


  output$plot1 <- renderPlotly({

    p<- ggplot(dataset1(), aes_string(x=input$x, y=input$y)) +
      geom_count(shape= 25, fill= "Orange")+
      geom_smooth(method = "lm")
    p<- p+ scale_y_log10() + scale_x_log10()
    ggplotly(p)
    #layout(height = input$plotHeight, autosize=TRUE)


  })
  
  # ######### Box Plot########
  dataset3 <- reactive({
    ships[(ships$country== input$Country3),]
  })

  output$plot2 <- renderPlotly({
    g<- ggplot(dataset3(), aes_string(x = input$y1 , y = input$x1)) + geom_boxplot(aes_string(fill= input$y1))
    g<- g+ scale_y_log10() #+ scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
    ggplotly(g)
  })

  # downloading data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("dataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(ships, file)
    })
  
  
  
  ####Density Plot##
  output$densityplot <- renderHighchart({
    df2 <- ships %>%
      filter(ships$country == input$filter4)
    hchart(density(df2$Year), type = "area", color = "#008080", name = input$filter4)
  })
  
  
  ####Trend Graph###
  timeseries <- reactive({
    df1 <- dataset[dataset$Country == input$filter1,]
    df1$Country <- as.character(df1$Country)
    df1$Year <- as.character(df1$Year)
    start_year <- year(as.Date(input$date_range[1]))
    end_year <- year(as.Date(input$date_range[2]))
    
    if (input$filter2 == "GDP") {
      ts(data = df1$GDP, start_year, end_year, frequency = 1)}
    else if (input$filter2 == "Mortalilty") {
      ts(data = df1$Mortalilty, start_year, end_year, frequency = 1)}
    else if (input$filter2 == "Fertility") {
      ts(data = df1$Fertility, start_year, end_year, frequency = 1)}
    else if (input$filter2 == "Population") {
      ts(data = df1$Population, start_year, end_year, frequency = 1)}
    else if (input$filter2 == "Trade_Import") {
      ts(data = df1$Trade_Import, start_year, end_year, frequency = 1)}
    else if (input$filter2 == "Trade_Export") {
      ts(data = df1$Trade_Export, start_year, end_year, frequency = 1)}
    else if (input$filter2 == "Total_Violent_fatalities") {
      ts(data = df1$Total_Violent_fatalities, start_year, end_year, frequency = 1)}
    else {
      paste("Data Unavailable")}
  })
  
  output$trendplot <- renderHighchart({
    timeseries() %>%
      hchart() %>%
      hc_title(text = paste("Trends of ", input$filter2, "in", input$filter1))
  })
  
  
  
  ####Correlation Matrix
  output$cormatrix <- renderHighchart({
       hchart(cor(dataset[,c(8:11, 23:25)]))
     }) 
  
})

shinyApp(ui = ui, server = server)

