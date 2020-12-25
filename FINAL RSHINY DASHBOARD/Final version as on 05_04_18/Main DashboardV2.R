
Packages<- c("shiny", "shinyjs", "leaflet", "scales", "lattice", "dplyr", "devtools", "metaplotr", "data.table", "htmltools",
             "broman", "magrittr", "RColorBrewer", "networkD3", "shinydashboard", "ggplot2", "ggthemes", "plotly",
             "gplots", "htmlwidgets", "ggExtra", "broom", "highcharter", "lubridate", "tidyverse", "sf", "geojsonio")
lapply(Packages, library, character.only = TRUE)

setwd("H:/MPS ANALYTICS/Part2")
ships<- read.csv("Violencedata_20APRL.csv")
#ships <- ships[sample.int(nrow(ships), 10000),]
ships1<- read.csv("Violencedata_20APRL.csv")
#Sort Data
#ships1 <- ships1[sample.int(nrow(ships1), 10000),]
#ships1 <- ships[order(ships$Country_Code,ships$Event_type),]
ships2<- read.csv("Violencedata_20APRL.csv")
#ships2 <- ships2[sample.int(nrow(ships2), 10000),]
#View(ships)
#View(ships2)
dataset <-read.csv("Final_Combined dataset.csv")
#View(dataset)
#set.seed(100)
ships <- ships[order(ships$Fatalities),]
ships2 <- ships2[order(ships2$Fatalities),]
#ships <- ships[sample.int(nrow(ships), 10000),]
ui<- navbarPage(strong("ARMED CONFLICT DASHBOARD"), id="nav",
           
                tabPanel("About",img(src='IMG_2155.JPG', align = "left"),  

                         fluidPage( HTML(
                           paste(
                             '<br/>',
                             h4("Armed conflicts within the states are generally political conflicts involving citizens fighting for internal change. 
                                 It involves a group of people who are armed and ready to fight for the goal of seizing governmental powers. 
                                 This group may include militias and armed civilians along with the regular armies. These conflicts result in the 
                                 collapse of state institutions mainly the police and judiciary. The fighting in conflicts are usually intermittent and 
                                 generally does not occur on well-defined battlefields. The twentieth century saw an upward trend in the fatalities due 
                                 to armed conflicts.", align= "justify"),'<br/>',
                            h4("This dashboard represents the effect of fatalities caused by armed conflict in middle eastern countries 
                                 namely Afghanistan, Iraq, Syria, and Yemen. The basis for visualizations are the violent events recorded in a particular year
                                 resulting in the deaths and its effect on the GDP, Mortality, Fertility, Revenue from Natural Resources etc.", align= "justify"),'<br/>',
                             h4("Download your data using the Download data button\n\n"),'<br/>'
                             #h4("Thank you for using the app!")
                           )),
                           
                           #img(src='Image1.png', height= "100px", width= "50%", align= "bottom right"),
                           #Add a download button for dataset

                           downloadButton("downloadData", "Download data")

                           #Add a link to github


                         )

                ),
                
                tabPanel("Data Summary and Resources ",img(src='brand.JPG', align = "left"),
                         
                         fluidPage( HTML(
                           paste(
                             '<br/>',
                             h3("This is the data summary and resources page, put headers here \n\n"),'<br/>',
                             
                             
                             p("write down the data summary , put paragraph here "),
                             p(a("Data Source1", href = "https://www.prio.org/Data/Armed-Conflict/GEO-SVAC/", target = "_blank")),
                             p(a("Data Source2", href = "https://www.start.umd.edu/gtd/", target = "_blank")),
                             p(a("Data Source3", href = "https://www.start.umd.edu/gtd/", target = "_blank")),
                             p(a("Data Source4", href = "https://www.prio.org/Data/Armed-Conflict/GEO-SVAC/", target = "_blank")),
                             p(a("Data Source5", href = "http://hdr.undp.org/en/content/human-development-index-hdi", target = "_blank")),
                             p(a("Data Source6", href = "http://hdr.undp.org/en/humandev", target = "_blank")),
                             p(a("Data Source7", href = "https://data.worldbank.org/", target = "_blank")),
                             p(a("Data Source8", href = "http://data.un.org/", target = "_blank")),
                             p(a("Data Source9", href = "http://www.systemicpeace.org/inscrdata.html", target = "_blank"))
                            ))
                           
                           
                           
                         )
                         
                ),
                
                
                 tabPanel("Interactive map",
                    div(class="outer",
                        
                        # tags$head(
                        #   # Include our custom CSS
                        #   includeCSS("styles.css"),
                        #   includeScript("gomap.js")
                        # ),
                        

shinyUI(fluidPage(
  fluidRow(useShinyjs(),
    column(12, leaflet::leafletOutput("map"))),
   # actionButton("btn", "Hide Table"),
   # actionButton("show", "Show Table")),
    fluidRow(useShinyjs(), column(12, div(style = 'overflow-x: scroll',DT::dataTableOutput("tbl"))))
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
                                         sankeyNetworkOutput("SankeyDiagram", width = "100%", height= "800px" ))),
                              tabPanel("Correlation Graph",
                                       plotlyOutput('plot1', height= "500px"),
                                       hr(),
                                       fluidRow(
                                         column(3,
                                                h4("Correlation Explorer"),
                                                checkboxGroupInput("Country2", "Select Country", c("Afghanistan", "Iraq", "Syria", "Yemen"), selected = c("Afghanistan", "Iraq", "Syria", "Yemen") )

                                         ),
                                         column(4, offset = 1,
                                                selectInput('x', 'X', choice = "Total_Violent_fatalities", names(dataset)[[25]]),
                                                selectInput('y', 'Y', choice= c("GDP", "Mortalilty", "Fertility", "Total_natural_resources_rents_percentage_of_GDP", "Improved_water_source_percentage_of_population_with_access", "Trade_Import", "Trade_Export"), names(dataset)[[8]])

                                         )
                                       )
                                       
                                       
                                       ),
                              tabPanel("Box Pot",
                                       plotlyOutput('plot2', height= "500px"),
                                       hr(),
                                       fluidRow(
                                          column(3,
                                                  h4("Box Plot Explorer"),
                                                 checkboxGroupInput("Country3", "Select Country", c("Afghanistan", "Iraq", "Syria", "Yemen"), selected = c("Afghanistan", "Iraq", "Syria", "Yemen") )

                                                  ),
                                                  column(4, offset = 1,
                                                  selectInput('x1', 'X', choice = "Fatalities", names(ships2)[[16]]),
                                                  selectInput('y1', 'Y', choice= c("Event_type","Weapon_Type", "Nationality_of_Target"), names(ships2)[[6]])

                                                         )
                                                          )
                                                          
                                                      
                                                 )
                              
                              
                              
                              
                              ))



)
######server########

server <- shinyServer(function(session,input, output) {
  
  ##downloading data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("ships-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(ships, file)
    })
  
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
  nlevels(ships1$Event_type)
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
    ships2[(ships2$country== input$Country3),]
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
  
  
})

shinyApp(ui = ui, server = server)

