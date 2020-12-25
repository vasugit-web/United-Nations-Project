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
setwd("H:/MPS ANALYTICS/Part2/Final Shiny app Dashboard")
ships<- read.csv("Violencedata_20APRL.csv")
ships2<- read.csv("Violencedata_20APRL.csv")
View(ships)
View(ships2)
dataset <-read.csv("Final_Combined dataset.csv")
View(dataset)
set.seed(100)
ships <- ships[order(ships$Fatalities),]
ships2 <- ships2[order(ships2$Fatalities),]
ships <- ships[sample.int(nrow(ships), 10000),]
ui<- navbarPage(strong("ARMED CONFLICT DASHBOARD"), id="nav",
           
                tabPanel("About",img(src='IMG_2155.JPG', align = "left"),

                         fluidPage( HTML(
                           paste(
                             '<br/>',
                             h3("This is my app!\n\n"),'<br/>',
                             h4("Download your data using the choose file button\n\n"),'<br/>',
                             h4("Thank you for using the app!")
                           )),
                           #Add a download button for dataset

                           downloadButton('downloadData', 'Download data')

                           #Add a link to github


                         )

                ),
                 tabPanel("Interactive map",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        

shinyUI(fluidPage(
  fluidRow(
    column(12, leaflet::leafletOutput("map"))),
    fluidRow(column(12, DT::dataTableOutput("tbl")))
  )
))),
tabPanel("Data explorer",
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
         
),
tabPanel("Data Visualization",
                  tabsetPanel(id = "tabSelected",
                              tabPanel("Sankey Diagram",
                                       fluidPage(
                                         sankeyNetworkOutput("SankeyDiagram", width = "100%", height= "900px" ))),
                              tabPanel("Correlation Graph",
                                       fluidRow(
                                         column(3,
                                                h4("Correlation Explorer"),
                                                checkboxGroupInput("Country2", "Select Country", c("Afghanistan", "Iraq", "Syria", "Yemen"), selected = c("Afghanistan", "Iraq", "Syria", "Yemen") )

                                         ),
                                         column(4, offset = 1,
                                                selectInput('x', 'X', choice = "Total_Violent_fatalities", names(dataset)[[25]]),
                                                selectInput('y', 'Y', choice= c("GDP", "Mortalilty", "Fertility", "Total_natural_resources_rents_percentage_of_GDP", "Improved_water_source_percentage_of_population_with_access", "Trade_Import", "Trade_Export"), names(dataset)[[8]])

                                         )
                                       ),
                                       hr(),
                                       plotlyOutput('plot1', height= "500px")
                                       ),
                              tabPanel("Box Pot",
                                        fluidRow(
                                          column(3,
                                                  h4("Box Plot Explorer"),
                                                 checkboxGroupInput("Country2", "Select Country", c("Afghanistan", "Iraq", "Syria", "Yemen"), selected = c("Afghanistan", "Iraq", "Syria", "Yemen") )

                                                  ),
                                                  column(4, offset = 1,
                                                  selectInput('x1', 'X', choice = "Fatalities", names(ships2)[[16]]),
                                                  selectInput('y1', 'Y', choice= c("Event_type","Weapon_Type", "Nationality_of_Target"), names(ships2)[[6]])

                                                         )
                                                          ),
                                                          hr(),
                                                      plotlyOutput('plot2', height= "500px")
                                                 )
                              
                              
                              
                              
                              ))



)
######server########

server <- shinyServer(function(session,input, output) {
  
  ##downloading data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("dataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(ships, file= 'ships.csv')
    })
  
  #### Map####
  
  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet(ships) %>%
      leaflet::addTiles(urlTemplate = "https://api.mapbox.com/styles/v1/ashoksoyal/cjgj3h2u400012rnvm99m8kjb/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiYXNob2tzb3lhbCIsImEiOiJjamdlb3RpMmEwYzA1MnhwNHpzenNrY3J4In0.lAeyiXYmXJpsgQS89ryuLg",
                        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>% 
      #addCircles(~Longitude, ~Latitude, radius= ~sqrt(Fatalities)*3000,
                 #stroke=TRUE, fillOpacity=0.5, fillColor = ~Fatalities)
      addMarkers( ~Longitude, ~Latitude, label= ~as.character(Fatalities), clusterOptions= markerClusterOptions(), clusterId= "Countrycluster1") %>%
           setView(lng = 44.399648, lat = 32.032916, zoom = 5)
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
  
  output$tbl <- DT::renderDataTable({
    DT::datatable(data_map(), extensions = "Scroller", style = "bootstrap", class = "compact", width = "100%",
                  options = list(deferRender = TRUE, scrollY = 300, scrollX= '400px', scroller = TRUE, dom = 'tp'))
  })
 
  ## Data Explorer server ###########################################
  
  observe({
    EventType <- if (is.null(input$country1)) character(0) else {
      filter(ships, country %in% input$country1) %>%
        `$`('Event_type') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$events1[input$events1 %in% ships$Event_type])
    updateSelectInput(session, "events1", choices = ships$Event_type,
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
  levels(ships$Event_type)
  clo1 <- nlevels(ships$Country_Code)
  clo2 <- nlevels(ships$Target)
  clo3 <- nlevels(ships$Event_type)
  clo4 <- clo1 + 1
  clo5 <- clo1 + clo2
  clo6 <- clo5 + 1
  clo7 <- clo5 + clo3
  allelement <- c(levels(ships$Country_Code),
                  levels(ships$Target),
                  levels(ships$Event_type))

  builtlink <- c()
  for (j in 1:clo1){
    for (k in clo4:clo5){
      aa <- sum(ships$Country_Code == allelement[j]  & ships$Target == allelement[k])
      builtlink <- c(builtlink, c(j-1, k-1, aa))
    }
  }
  for (m in clo4:clo5){
    for (n in clo6: clo7){
      zz <- sum(ships$Event_type == allelement[n]  & ships$Target == allelement[m])
      builtlink <- c(builtlink, c(m-1, n-1, zz))
    }
  }



  matrixlink <-matrix(builtlink, byrow = TRUE, ncol = 3)

  nodes <- data.frame("name" = c(levels(ships$Country_Code),
                                 levels(ships$Target),
                                 levels(ships$Event_type)))

  links <- as.data.frame(matrixlink)
  names(links) <- c("source", "target", "value")

  output$SankeyDiagram <- renderSankeyNetwork({
    sankeyNetwork(Links = links, Nodes = nodes,
                  Source = "source", Target = "target",
                  Value = "value", NodeID = "name",
                  fontSize= 14, nodeWidth = 30)

  })
  
  
  
  
   ##### Correlation Graph#####

  dataset1 <- reactive({
    dataset[(dataset$Country== input$Country2),]
  })


  output$plot1 <- renderPlotly({

    p<- ggplot(dataset1(), aes_string(x=input$x, y=input$y)) +
      geom_count(shape= 25)+
      geom_smooth(method = "lm")
    p<- p+ scale_y_log10() + scale_x_log10()
    ggplotly(p)
    #layout(height = input$plotHeight, autosize=TRUE)


  })
  
  # ######### Box Plot########
  dataset3 <- reactive({
    ships2[(ships2$country== input$Country2),]
  })

  output$plot2 <- renderPlotly({
    g<- ggplot(dataset3(), aes_string(x = input$y1 , y = input$x1)) + geom_boxplot()
    g<- g+ scale_y_log10()
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
  
  
})

shinyApp(ui = ui, server = server)

