#Summarize quaterly data
RS_get_delay_data<-function(data_date){
  library(dplyr)
  filepath=paste('C:/Users/rolco/Desktop/delay_data/',data_date,'.csv',sep='')
  delay_q<-read.csv(filepath,header=T,sep=',')
  delay_q<-delay_q %>% 
    filter(!is.na(DEP_DELAY_NEW)) %>%
    group_by(UNIQUE_CARRIER,AIRLINE_ID,ORIGIN_AIRPORT_ID, DEST_AIRPORT_ID) %>%
    summarise(DELAY_TIME_SUM=sum(DEP_DELAY_NEW),LINE_SUM=length(YEAR))
  month<-as.numeric(substr(data_date,3,4))
  if (month==1|month==2|month==3){
    delay_q$QUATER<-1
  }
  if (month==4|month==5|month==6){
      delay_q$QUATER<-2
  }
  if (month==7|month==8|month==9){
    delay_q$QUATER<-3
  }
  if (month==10|month==11|month==12){
    delay_q$QUATER<-4
  }
  return(delay_q)
}

#get data list including quaterly data
data_date<-c('1501','1502','1503','1504','1505','1506','1507','1508','1509',
             '1510','1511','1512','1601','1602','1603','1604','1605','1606',
             '1607','1608','1609','1610','1611','1612','1701','1702','1703',
             '1704','1705','1706','1707','1708','1709','1710','1711','1712')
quater_data<-lapply(data_date,RS_get_delay_data)

#transform the data list to data.frame
delay_data<-NULL
for (i in 1:length(data_date)){
  delay_data<-rbind.data.frame(delay_data,quater_data[[i]])
}

#clear data.frame and get average delay time (in minutes)
delay_data<-delay_data %>%
  group_by(UNIQUE_CARRIER,AIRLINE_ID,ORIGIN_AIRPORT_ID, DEST_AIRPORT_ID,QUATER) %>%
  summarise(DELAY_TIME_SUM=sum(DELAY_TIME_SUM),LINE_SUM=sum(LINE_SUM))

delay_data<-delay_data %>%
  group_by(UNIQUE_CARRIER,AIRLINE_ID,ORIGIN_AIRPORT_ID, DEST_AIRPORT_ID,QUATER) %>%
  summarise(AVG_DELAY=(DELAY_TIME_SUM/LINE_SUM))

write.csv(delay_data,'C:/Users/rolco/Desktop/delay_quaterly_data_for_RS.csv')
