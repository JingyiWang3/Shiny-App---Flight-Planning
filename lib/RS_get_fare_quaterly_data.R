#get fare quaterly data
RS_get_fare_data<-function(data_date){
  library(dplyr)
  filepath=paste('C:/Users/rolco/Desktop/airfare/',data_date,'.csv',sep='')
  fare_q<-read.csv(filepath,header=T,sep=',')
  fare_q<-fare_q %>% 
    filter(TICKET_CARRIER !='99' & TICKET_CARRIER !='--' & 
             ORIGIN_COUNTRY == 'US' & DEST_COUNTRY =='US') %>%
    select(QUARTER,ORIGIN_AIRPORT_ID,DEST_AIRPORT_ID,TICKET_CARRIER,MARKET_FARE) %>%
    group_by(ORIGIN_AIRPORT_ID,DEST_AIRPORT_ID,QUARTER,TICKET_CARRIER) %>%
    summarise(FARE_SUM=sum(MARKET_FARE),LINE_SUM=length(TICKET_CARRIER))
  return(fare_q)
}

#get data list including quaterly data
data_date<-c('2015Q1','2015Q2','2015Q3','2015Q4',
             '2016Q1','2016Q2','2016Q3','2016Q4',
             '2017Q1','2017Q2','2017Q3','2017Q4')
quater_data<-lapply(data_date,RS_get_fare_data)

#transform the data list to data.frame
fare_data<-NULL
for (i in 1:length(data_date)){
  fare_data<-rbind.data.frame(fare_data,quater_data[[i]])
}

#clear data.frame and get average fare
fare_data<-fare_data %>%
  group_by(ORIGIN_AIRPORT_ID,DEST_AIRPORT_ID,QUARTER,TICKET_CARRIER) %>%
  summarise(FARE_SUM=sum(FARE_SUM),LINE_SUM=sum(LINE_SUM))

fare_data<-fare_data %>%
  group_by(ORIGIN_AIRPORT_ID,DEST_AIRPORT_ID,QUARTER,TICKET_CARRIER) %>%
  summarise(AVG_FARE=(FARE_SUM/LINE_SUM))

write.csv(fare_data,'C:/Users/rolco/Desktop/fare_quaterly_data_for_RS.csv')



  
