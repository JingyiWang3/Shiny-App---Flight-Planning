delay_data<-read.csv('C:/Users/rolco/desktop/delay_quaterly_data_for_RS.csv',header=TRUE,sep=',')
consumer_data<-read.csv('C:/Users/rolco/desktop/consumer_satisfaction.csv',header=TRUE,sep=',')
fare_data<-read.csv('C:/Users/rolco/desktop/fare_quaterly_data_for_RS.csv',header=TRUE,sep=',')
safety_data<-read.csv('C:/Users/rolco/desktop/security.csv',header=TRUE,sep=',')
delay_data<-delay_data[,-1]
fare_data<-fare_data[,-1]

#combine delay data, consumer satisfaction and accident data
join_delay_rating_accident<-function(carrier){
  library(plyr)
  len<-sum(delay_data$UNIQUE_CARRIER==carrier)
  delay<-delay_data[delay_data$UNIQUE_CARRIER==carrier,]
  rating<-as.matrix.data.frame(consumer_data[consumer_data$CARRIER==carrier,-1])
  rating_mat<-matrix(rep(rating,len),ncol=5,byrow = T)
  colnames(rating_mat)<-names(consumer_data)[-1]
  safety<-safety_data[safety_data$CARRIER==carrier,-1]
  safety_mat<-matrix(rep(safety,len),ncol=1)
  colnames(safety_mat)<-'ACCIDENT_SUM'
  join_df<-data.frame(delay,data.frame(rating_mat))
  join_df<-data.frame(join_df,data.frame(safety_mat))
  return(join_df)
}

library(plyr)
delay_rating_accident<-ldply(consumer_data$CARRIER,join_delay_rating_accident)

#combine delay_rating_accident and fare_data
delay_rating_accident$ID<-paste(delay_rating_accident$ORIGIN_AIRPORT_ID,
                                delay_rating_accident$DEST_AIRPORT_ID,
                                delay_rating_accident$QUATER,
                                delay_rating_accident$UNIQUE_CARRIER,sep='')
fare_data$ID<-paste(fare_data$ORIGIN_AIRPORT_ID,fare_data$DEST_AIRPORT_ID,
                    fare_data$QUARTER,fare_data$TICKET_CARRIER,sep='')
combined_data<-merge(delay_rating_accident,fare_data,by='ID')

#Clear combined data
library(dplyr)
combined_data <- combined_data %>% 
  select(UNIQUE_CARRIER,ORIGIN_AIRPORT_ID=ORIGIN_AIRPORT_ID.x,
         DEST_AIRPORT_ID=DEST_AIRPORT_ID.x,QUARTER=QUATER,AVG_DELAY,
         FOOD_DRINK,ENTERTAINMENT,SEAT_COMFORT,STUFF_SERVICE,VALUE_FOR_MONEY,
         AVG_FARE,ACCIDENT_SUM) %>%
  filter(AVG_FARE != 0)


#combine airports' name and complete carrier name into the data.frame
airlines_name<-read.csv('C:/Users/rolco/desktop/airlines name.csv',header=TRUE,sep=',')
find_complete_name<-function(abv){
  name<-airlines_name[airlines_name$ID==abv,2]
  return(name)
}
combined_data$CARRIER_NAME<-sapply(combined_data$UNIQUE_CARRIER,find_complete_name)

airport_info<-read.csv('C:/Users/rolco/desktop/airport_info.csv',header=T,sep=',',as.is=T)
airport_info<-airport_info %>% filter(nchar(STATE)==3)
find_city_name<-function(code){
  name<-airport_info[airport_info==code,'CITY']
  return(name)
}
combined_data$ORIGIN_CITY<-sapply(combined_data$ORIGIN_AIRPORT_ID,find_city_name)
combined_data$DEST_CITY<-sapply(combined_data$DEST_AIRPORT_ID,find_city_name)


#clean data
combined_data.RS<- combined_data %>%
  dplyr::group_by(UNIQUE_CARRIER,ORIGIN_CITY,DEST_CITY,QUARTER,CARRIER_NAME) %>%
  dplyr::summarise(AVG_DELAY=mean(AVG_DELAY),SUM_ACCIDENT=mean(ACCIDENT_SUM),
                   AVG_FARE=mean(AVG_FARE),
                   FOOD_DRINK=mean(FOOD_DRINK),ENTERTAINMENT=mean(ENTERTAINMENT),
                   SEAT_COMFORT=mean(SEAT_COMFORT),STUFF_SERVICE=mean(STUFF_SERVICE),
                   VALUE_FOR_MONEY=mean(VALUE_FOR_MONEY))

write.csv(combined_data,'C:/Users/rolco/Desktop/delay_rating_fare_safety_complete.csv')
write.csv(combined_data.RS,'C:/Users/rolco/Desktop/delay_rating_fare_safet_RS.csv')
