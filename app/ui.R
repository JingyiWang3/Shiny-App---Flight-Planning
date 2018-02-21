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



##1.Map -----
tab1 <- tabPanel("Interactive map",
                 
                 #CSS file for page style
                 includeCSS("theme.css"),
                 
                 tags$div(class= "City_Carrier", 
                          
                          #leaflet out 
                          leafletOutput("AirMap", width="100%", height= "700px"),
                          absolutePanel(id = "City_Carrier", class = "City_Carrier_panel panel panel-default",
                                        
                                        #selection input 
                                        selectInput("Company", label = "Company:", choices = as.character(unique(data$Carrier))),
                                        selectInput("Departure", label = "Departure City", choices = as.character(unique(data$OriginCity)))
                                        
                          ),
                          
                          #add class for CSS
                          absolutePanel(id = "Pop_Panel", class = "Pop_Panel panel panel-default",
                                        
                                        #check box for multi choice box input   
                                        checkboxGroupInput("Popularity", label = "Popularity:", choiceNames = list(HTML(StarCreater(1)), HTML(StarCreater(2)), HTML(StarCreater(3)), HTML(StarCreater(4)), HTML(StarCreater(5))), choiceValues = list(1,2,3,4,5), selected = list(1,2,3,4,5))
                                        
                          )
                          
                 ),
                 
                 
                 
                 
                 #absolutePanel(id = "Controls", class = "panel panel-default", fixed = TRUE,
                 #                           draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                 #                           width = 320, height = "auto",style = "opacity: 0.8",
                 #                           
                 #                           titlePanel("Airline Map"),
                 #                           
                 #                           selectInput("Company", 
                 #                                       label = "Select Company:", 
                 #                                       choices = as.character(unique(data$Carrier))),
                 #                           radioButtons("Departure", "Departure City",
                 #                                                          choices = as.character(unique(data$OriginCity)) ,selected = as.character(unique(data$OriginCity))[1]),
                 #                           sliderInput("NumberofDestination", "Hottest Destinations", min=1, 72, value=1, step=1),
                 #                           hr(),
                 #                           plotOutput("hist", height = 200),
                 
                 
                 tags$a(href = 
                          "https://www.transtats.bts.gov/","Data Source:https://www.transtats.bts.gov/"),
                 p("Frequency between 0-37 is one star city, 38-75 is two star city, 76-113 is three star city, 114-151 is four star city and 152-188 is five star city",style='color:white'))

# wellPanel(# h3("Select Cities"),

##2.Delay & Custoner Reviews -----
tab2 <- navbarMenu("Airline Statistics",
                   
                   #tags$style("body {background: url(https://images.trvl-media.com/media/content/expus/graphics/launch/home/tvly/150324_flights-hero-image_1330x742.jpg) no-repeat center center fixed; 
                   #            background-size: cover;}"),         
                   tabPanel(title = "On-Time Performance",
                            h3("Percentage of Delayed Flights by Length of Time Delayed (in Minutes)",style="color:	white",align="center",offset = -1,astyle ="font-family:helvetica;"),
                            fluidRow( wellPanel(style = "overflow-y:scroll;  height: 500px; opacity: 0.9; background-color: #ffffff;",
                                                column(width = 9, plotOutput("carrierPlot"),
                                                       tags$a(href = "https://www.transtats.bts.gov/","2015-2017 Data,Source:https://www.transtats.bts.gov/")
                                                       
                                                ),
                                                column(width = 3, checkboxGroupInput("carrier", "Choose a Airline:",
                                                                                     choices = c("American Airlines", "Alaska Airlines", "JetBlue Airways","Delta Air Lines" ,"Atlantic Southeast Airlines" ,"Frontier Airlines" ,"Hawaiian Airlines","American Eagle Airlines","Spirit Air Lines","SkyWest Airlines","United Air Lines","US Airways","Virgin America","Southwest Airlines"),
                                                                                     selected = "American Airlines"))
                                                
                                                
                            ))),
                   
                   
                   
                   tabPanel("Constomer Satisfiction Reviews",
                            h3("Constomer Satisfiction Reviews",style="color:	white",align="center",astyle="font-family:helvetica;"),
                            fluidRow( wellPanel(style = "overflow-y:scroll; height: 420px; opacity: 0.9; background-color: #ffffff;",
                                                column(width = 9,formattableOutput("carrierPlot2"),
                                                       tags$a(href = "http://www.airlinequality.com/","Data Source:http://www.airlinequality.com/")
                                                ),
                                                column(width = 3, selectInput("carrier2", "Choose a carrier:", 
                                                                              choices = data_satisfy$CARRIER),
                                                       helpText("The number of  blue planes stand for the rating score"))))), 
                   
                   tabPanel("Luggage Mishandling",
                            h3("Airline Luggage Mishandling Rate",align="center",astyle="font-family:helvetica;",style="color:	white"),
                            fluidRow( wellPanel(style = "overflow-y:scroll; height: 480px; opacity: 0.9; background-color: #ffffff;",
                                                plotlyOutput("carrierPlot4"),
                                                tags$a(href = "https://www.transtats.bts.gov/","Data Source:https://www.transtats.bts.gov/")
                            ))),
                   tabPanel("Airline Accident Record",
                            h3("Airline Accident Records",style="color:	white",align="center",astyle="font-family:helvetica;"),
                            fluidRow( wellPanel(style = "overflow-y:scroll; height: 480px; opacity: 0.9; background-color: #ffffff;",
                                                plotOutput("carrierPlot3"),
                                                p("Footnote: Number in the circle represents the number of accident for airlines"),
                                                
                                                tags$a(href = "https://www.transtats.bts.gov/","2000-2017 Data, Source:https://www.transtats.bts.gov/")
                            ))))


##3.Recommendation -----
tab3 <-tabPanel("Find the Perfect Airline",
                fluidRow(
                  column(4,selectInput('from', 'From',combined_data$ORIGIN_CITY,selected =htmlOutput(''))),
                  column(4,selectInput('to', 'To',combined_data$DEST_CITY,selected =htmlOutput(''))),
                  column(4,selectInput('quarter', 'Quarter',c(1,2,3,4),selected=1))),
                fluidRow(
                  wellPanel(style = "overflow-y:scroll; height: 550px; opacity: 0.9; background-color: #ffffff;",
                            parcoordsOutput('par',width = '1200px',height='500px'),
                            tags$a(href = "https://www.transtats.bts.gov/","Data Source:https://www.transtats.bts.gov/")
                  )))


## UI 
ui <- shinyUI(navbarPage(title = strong("AirPlan"),
                         tab1,
                         tab2,
                         tab3
))
