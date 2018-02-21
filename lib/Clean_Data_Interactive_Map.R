
#input data
seventeen <- read.csv("2017.csv",as.is = TRUE)
sixteen <- read.csv("2016.csv",as.is = TRUE)
fifteen <- read.csv("2015.csv",as.is = TRUE)

#input carrier
carrier <- c("AA","AS","B6","DL", "EV", "F9","HA","MQ", "NK", "OO", "UA" ,"US", "VX", "WN")

#select factor
seventeen1 <- select(seventeen,FREIGHT,UNIQUE_CARRIER,UNIQUE_CARRIER_NAME,ORIGIN,ORIGIN_CITY_NAME,DEST,DEST_CITY_NAME, YEAR,QUARTER,MONTH)
sixteen1 <- select(sixteen,FREIGHT,UNIQUE_CARRIER,UNIQUE_CARRIER_NAME,ORIGIN,ORIGIN_CITY_NAME,DEST,DEST_CITY_NAME, YEAR,QUARTER,MONTH)
fifteen1 <- select(fifteen,FREIGHT,UNIQUE_CARRIER,UNIQUE_CARRIER_NAME,ORIGIN,ORIGIN_CITY_NAME,DEST,DEST_CITY_NAME, YEAR,QUARTER,MONTH)
citydf <- rbind(fifteen1, sixteen1, seventeen1)

#citydf <- read.csv("citydf.csv")

fourteen <- citydf %>% filter( citydf$UNIQUE_CARRIER %in% carrier)


city.origin  <- fourteen %>% filter( fourteen$ORIGIN_CITY_NAME == "New York, NY" |fourteen$ORIGIN_CITY_NAME == "Chicago, IL" |fourteen$ORIGIN_CITY_NAME == "Los Angeles, CA" )

#unique(city.origin$ORIGIN_CITY_NAME) #check ori city

city.origin$frequency <- rep(1, nrow(city.origin))

nodate <- city.origin[,c(2,3,5,7,11)]
temp <- nodate  %>%  group_by(UNIQUE_CARRIER,DEST_CITY_NAME ) %>% mutate(sumfrequency = sum(frequency))    %>%  select (UNIQUE_CARRIER,UNIQUE_CARRIER_NAME,ORIGIN_CITY_NAME,DEST_CITY_NAME ,sumfrequency ) %>% arrange(UNIQUE_CARRIER)
temp <- temp[,c(1,3,4,5)]
colnames(temp) <-c("Carrier", "Origin", "Dest","Frequency")

write.csv(temp,"Map_DF.csv")



library(ggmap)

coordinate <- read.csv("uscitiesv.csv", as.is = T)
coordinate$city.states <- paste(coordinate$city, coordinate$state_id, sep = ", ")


data <- read.csv("Map_DF.csv", as.is = TRUE)
data <- data[, -1]
city <-as.character(unique(data$Dest))
city <- city[-131]


get_code <- function(address){
  code <- geocode(address)
  while (is.na(code$lon) == FALSE | is.na(code$lat) == FALSE) {
    code <- geocode(address)
  }
  
  return(code)
  
}
a <- "New York, NY"

get_code(a)


#First, we need to get the from and dest for air map

From.Dest <- unique.array(data[,c(1:4)])

From.Dest <- From.Dest[From.Dest$Dest != "Guam, TT",]

#Second, get cord for each Dest and original

#This helper function is extract the cord for each city
find.c <- function(vec) {

  position <- which(vec == coordinate$city.states)
  
  if(length(position) == 0){
    
    coor <- geocode(vec)
    while (is.na(coor$lon) == TRUE | is.na(coor$lat) == TRUE) {
      coor <- geocode(vec)
    }
  }
  else{
    coor <- coordinate[position, c("lng", "lat")]
  }
  
  return(coor)
  
}


#Here apply the funciton to each city.
Coord <- matrix(unlist(lapply(city, find.c)), byrow = T, ncol = 2)
Coord <- cbind(city, Coord)

Coord <- as.data.frame(Coord)

colnames(Coord) <- c("City", "lon","lat")


#Here merge two Coord and From.dest df

From.Dest.Coord <- merge(Coord, From.Dest, by.x = "City", by.y = "Dest")

From.Dest.Coord <- cbind(From.Dest.Coord, OR.LON = NA, OR.LAT = NA)

#Get the lon and lat for NY, CHI, LAX

NY.LON.LAT <- find.c("New York, NY")

CHI.LON.LAT <- find.c("Chicago, IL")

LAX.LON.LAT <- find.c("Los Angeles, CA")

#Then input the data

From.Dest.Coord[From.Dest.Coord$Origin == "New York, NY", c("OR.LON", "OR.LAT")] <- NY.LON.LAT

From.Dest.Coord[From.Dest.Coord$Origin == "Chicago, IL", c("OR.LON", "OR.LAT")] <- CHI.LON.LAT

From.Dest.Coord[From.Dest.Coord$Origin == "Los Angeles, CA", c("OR.LON", "OR.LAT")] <- LAX.LON.LAT

#Then change the name

Final.Data <- data.frame(Carrier = From.Dest.Coord$Carrier, OriginCity = From.Dest.Coord$Origin, OriginLon = From.Dest.Coord$OR.LON, OriginLat = From.Dest.Coord$OR.LAT, DestCity = From.Dest.Coord$City, DestLon = From.Dest.Coord$lon, DestLat = From.Dest.Coord$lat, Freq = From.Dest.Coord$Frequency)

creatstar <- function(num){
  
  star <- ifelse(num >= 2, ifelse(num >= 37, ifelse( num >= 74, ifelse( num >= 111, ifelse( num >= 148 , 5, 4) , 4 ) , 3) , 2), 1)
  
  return(paste(strrep("<image class=\"frqstar\" src=\"https://upload.wikimedia.org/wikipedia/commons/2/29/Gold_Star.svg\" >", star)))
  
}

creatstarnum <- function(num){
  
  star <- ifelse(num >= 2, ifelse(num >= 37, ifelse( num >= 74, ifelse( num >= 111, ifelse( num >= 148 , 5, 4) , 4 ) , 3) , 2), 1)
  
  return(star)
  
}

Final.Data$Stars <- unlist(lapply(Final.Data$Freq, creatstar))

Final.Data$Starsnum <- unlist(lapply(Final.Data$Freq, creatstarnum))

name <- read.csv("airlines name.csv", as.is = T)

for (i in 1:length(name$ID)){
  
  position <- which(name$ID[i] == Final.Data$Carrier)
  
  Final.Data$Carrier[position] <- rep(name[i,2], length(position))
  
}



#Then write out the datafram to csv file
write.csv(Final.Data, "Final_Data.csv")


