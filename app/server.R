## Load Packages -----
library("shiny")
library("leaflet")
library("dplyr")
library("RColorBrewer")
library("stringr")
library("parcoords")
library("ggplot2")
library("reshape2")
library("geosphere")
library("ggthemes")
library("formattable")
library("base64enc")
library("plotly")
#devtools::install_github("timelyportfolio/parcoords")


##Import Data -----

#Map Data
data <- read.csv("Final_Data.csv",as.is = TRUE)
data <- data[,-1]
#Delay Data
data_delay <- read.csv("delay_combination.csv")
#Customer Review Data
data_satisfy <- read.csv("consumer_satisfaction.csv")

#Customer Reviews Image
image1 <- sprintf("data:image/png;base64,%s", base64encode("star1.png"))
image2 <- sprintf("data:image/png;base64,%s", base64encode("star2.png"))
image3 <- sprintf("data:image/png;base64,%s", base64encode("star3.png"))
image4 <- sprintf("data:image/png;base64,%s", base64encode("star4.png"))

carr_select <- function(x= c()){
  y <- c()
  for(i in 1:5){
    if(x[i] == 1){y[i] <- image1}
    if(x[i] == 2){y[i] <- image2}
    if(x[i] == 3){y[i] <- image3}
    if(x[i] == 4){y[i] <- image4}
  }
  return(y)
  
}
#Luggage Data
luggage <- read.csv("luggage.csv")

## Safety Data
safety <- read.csv("security.csv",header=T,sep=',')

#Recommendation Data
combined_data<-read.csv('delay_rating_fare_safety_RS.csv',
                        header=T,sep=',')
names(combined_data)<-c('X','ORIGIN_CITY','DEST_CITY','QUARTER','Carrier','Average Delay Time (Minutes)','Accidents','Average Price($)',
                        'Food & Drink','Entertainment','Seat Comfort','Stuff Service',
                        'Value for Money')


#Here create star for the selection
StarCreater <- function(num){
  return(paste(strrep("<image class=\"frqstar\" src=\"https://upload.wikimedia.org/wikipedia/commons/2/29/Gold_Star.svg\" >", num)))
}



server <- function(input, output,session) {
  
  #input: 1.company, 2. origin city, 3. popularity
  sub.data <- reactive({
    
    return(data[data$OriginCity == input$Departure & data$Carrier == input$Company & data$Starsnum %in% input$Popularity,])
  })
  
  
  ##For display  of Map in "Map"
  
  
  output$AirMap <- renderLeaflet({
    
    #
    sub_data <- sub.data()
    
    
    if(nrow(sub_data) == 0){
      
      #load map
      map = leaflet(sub_data) %>% setView(-98.35  , 39.48, zoom = 4) %>% 
        #addTiles()%>% addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
        #                      attribution = paste(
        #                       '&copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors',
        #                      '&copy; <a href="http://cartodb.com/attributions">CartoDB</a>'
        #                   ))
        addProviderTiles(providers$Esri.WorldStreetMap)
      
      
      
    }
    
    else{
      
      #data = head(arrange(airdata(),desc(frequency)), input$NumberofDestination)
      
      #load map
      map = leaflet(sub_data) %>% setView(-98.35  , 39.48, zoom = 4) %>% 
        #addTiles()%>% addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
        #                      attribution = paste(
        #                       '&copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors',
        #                      '&copy; <a href="http://cartodb.com/attributions">CartoDB</a>'
        #                   ))
        addProviderTiles(providers$Esri.WorldStreetMap)
      
      #load icon
      icons <- awesomeIcons(
        icon = 'ion-plane',
        iconColor = 'white',
        library = 'ion',
        markerColor = "blue")
      
      #add marker for dest in the map
      map <- map %>% addAwesomeMarkers(lng = ~DestLon, lat = ~DestLat, icon=icons, layerId = ~DestCity) 
      #load lon and lat 
      flows <- gcIntermediate(select(sub_data, OriginLon, OriginLat), select(sub_data, DestLon, DestLat), n=200 , sp = TRUE, addStartEnd = TRUE) 
      #add polylines connecting origin and destination 
      map <- map %>% 
        addPolylines(data = flows,color = "grey",weight = 2,
                     dashArray = "5, 5") #color= colors,fillOpacity = 1 weight = data$frequency/5) 
      
      
      
      
      
      
      
      #map %>% addLegend("bottomleft", 
      #                  colors =c("lightskyblue", "yellow", "orange", "red"),
      #                  labels= c("0-20", "20-40","40-60","60+"),
      #                  title= "Frequency",
      #                  opacity = 1)
    }
    
    
    
    
    
  })
  
  #marker disappear  
  observeEvent(input$AirMap_marker_click, {
    
    #get selected subset of data
    sub_data <- sub.data()
    
    #find origin city
    OriginCityID <- sub_data$OriginCity[1]
    
    #find orgin city lon & lat
    OriginCityLon <- sub_data$OriginLon[1]
    
    OriginCityLat <- sub_data$OriginLat[1]
    
    #add ID for click 
    DestCityID <- input$AirMap_marker_click$id
    
    #find dest lon &lat
    DestCityLon <- sub_data[sub_data$DestCity == DestCityID, "DestLon"]
    
    DestCityLat <- sub_data[sub_data$DestCity == DestCityID, "DestLat"]
    
    #load lon & lat
    flows <- gcIntermediate(c(OriginCityLon, OriginCityLat) , c(DestCityLon, DestCityLat), n=200 , sp = TRUE, addStartEnd = TRUE) 
    
    #content pop-up in origin city marker
    OriginContent <- paste(
      OriginCityID
    )
    
    #content pop-up in origin city marker: 1. Dest city 2 "Popularity", 3: level of star
    DestContent <- paste(
      sep = "<br />",
      DestCityID,
      "Popularity:",
      sub_data[sub_data$DestCity == DestCityID, "Stars"]
    )
    
    #load icon for dest marker 
    icons <- awesomeIcons(
      icon = 'ion-plane',
      iconColor = 'white',
      library = 'ion',
      markerColor = "blue")
    
    #load icon for origin marker 
    OriginIcon <- awesomeIcons(
      icon = 'ion-plane',
      iconColor = 'white',
      library = 'ion',
      markerColor = "red")
    
    leafletProxy("AirMap", session) %>%
      #clear dash line   clear markers      add polylines between remaining city and origin 
      clearShapes() %>% clearMarkers() %>% addPolylines(data = flows, color = "grey",weight = 2, dashArray = "5, 5") %>%
      # add markers
      addAwesomeMarkers(lng = OriginCityLon, lat = OriginCityLat, icon=OriginIcon, layerId = OriginCityID, label = htmltools::HTML(OriginContent), labelOptions = labelOptions(noHide = TRUE)) %>%
      addAwesomeMarkers(lng = DestCityLon, lat = DestCityLat, icon=icons, layerId = DestCityID, label = htmltools::HTML(DestContent), labelOptions = labelOptions(noHide = TRUE))
    
  })
  
  #get all markers back by clicking map 
  observeEvent(input$AirMap_click, {
    
    sub_data <- sub.data()
    
    if(nrow(sub_data) ==0){
      leafletProxy("AirMap", session)
    }
    
    else{
      icons <- awesomeIcons(
        icon = 'ion-plane',
        iconColor = 'white',
        library = 'ion',
        markerColor = "blue")
      
      flows <- gcIntermediate(select(sub_data, OriginLon, OriginLat), select(sub_data, DestLon, DestLat), n=200 , sp = TRUE, addStartEnd = TRUE) 
      
      
      
      leafletProxy("AirMap", session) %>%
        clearShapes() %>% clearMarkers() %>% addAwesomeMarkers(lng = sub_data$DestLon, lat = sub_data$DestLat, icon=icons, layerId = sub_data$DestCity) %>%
        addPolylines(data = flows,color = "grey",weight = 2,
                     dashArray = "5, 5") #color= colors,fillOpacity = 1 weight = data$frequency/5) 
    }
    
    
    
    
  })
  
  ##For display of Delay in Tab2
  output$carrierPlot <- renderPlot({
    if(length(input$carrier == 0)){
      plot(0,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
    }
    if(length(input$carrier > 0)){
      # generate an rnorm distribution and plot it
      carr_name <- input$carrier
      carr_row <- data_delay$CARRIER %in% carr_name
      carr_data <- data_delay[carr_row, c(2,4,6,8,10,12)]
      colnames(carr_data)[2:6] <- c("-15~0", "0~15", "15~30", "30~60", ">60")
      data_melt <- reshape2::melt(carr_data, id = "CARRIER")
      ggplot(data=data_melt, aes(x = variable, y= value , fill = CARRIER)) +
        geom_bar(stat="identity", position=position_dodge()) + 
        theme_wsj()+
        guides(fill=guide_legend(title=NULL))+
        theme(axis.title = element_blank(),legend.position = 'top') + 
        labs(x = "Delay Time in Minutes",y = "Percentage")
    }
    
  })
  
  
  ##For display of Customer Review
  output$carrierPlot2 <- renderFormattable({
    data_rate <- data_satisfy[data_satisfy$CARRIER == input$carrier2, ]
    df <- data.frame(
      'Rating_Type' = c("Food and Drink", "Entertainment", "Seat Comfort",
                        "Stuff Service", "Value For Money"),
      'Rating_Scores' = as.numeric(data_rate[,3:7])
    )
    names(df) <- c("Rating Type", "Rating Scores")
    
    image_tile <- formatter("img",
                            src = x ~ carr_select(x),
                            NA)
    
    formattable(df, list('Rating Scores' = image_tile))
  })
  ##For display of Safety Data
  output$carrierPlot3 <- renderPlot({
    safety$`company name` <- safety$X 
    safety$`Normalized frequency` <- round((safety$n - mean(safety$n))/sd(safety$n), 2)  # compute normalized mpg
    safety$nn_type <- ifelse(safety$`Normalized frequency` < 0, "below", "above")  # above / below avg flag
    safety <- safety[order(safety$`Normalized frequency`), ]  # sort
    safety$`company name` <- factor(safety$`company name`, levels = safety$`company name`)  # convert to factor to retain sorted order in plot.
    
    
    ##For display of Luggage 
    output$carrierPlot4 <- renderPlotly({
      plot_ly(luggage,labels = ~Company,values=~freq) %>%
        add_pie(hole = 0.6) %>%
        layout( showlegend = F,
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
    
    
    # Diverging Barcharts
    ggplot(safety, aes(x=`company name`, y=`Normalized frequency`, label=`Normalized frequency`)) + 
      geom_bar(stat='identity', aes(fill=nn_type), width=.5)  +
      geom_point(stat='identity', fill="black", size=6)+
      geom_text(aes(label=n),color="white",size=3)+
      theme_wsj()+
      guides(fill=guide_legend(title=NULL))+
      theme(axis.title = element_blank(),legend.position = 'top')+
      scale_fill_manual(name="Mileage", 
                        labels = c("Above Average", "Below Average"), 
                        values = c("above"="#FE9F2C", "below"="#A52A2A")) + 
      theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) + 
      coord_flip()
  })
  
  ##For display  of Recommendation
  sub_data<-reactive({list(data=combined_data[combined_data$ORIGIN_CITY==as.character(str_to_title(input$from)) & 
                                                combined_data$DEST_CITY==as.character(str_to_title(input$to)) & 
                                                combined_data$QUARTER==as.numeric(input$quarter),
                                              -c(1,2,3,4)])})
  output$par <- renderParcoords({
    parcoords(sub_data()$data, rownames = F
              , brushMode = "1d-axes"
              , reorderable = T
              , queue = F
              , color = list(
                colorBy = "Carrier"
                , colorScale = htmlwidgets::JS("d3.scale.category10()"))
    )
  }) 
  
  
  
  
  
}


