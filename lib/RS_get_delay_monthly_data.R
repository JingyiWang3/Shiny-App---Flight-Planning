#Summarize monthly data
RS_get_delay_data<-function(data_date){
  library(dplyr)
  filepath=paste('C:/Users/rolco/Desktop/delay_data/',data_date,'.csv',sep='')
  delay_month<-read.csv(filepath,header=T,sep=',')
  delay_month<-delay_month %>% 
    filter(!is.na(DEP_DELAY_NEW)) %>%
    group_by(UNIQUE_CARRIER,AIRLINE_ID,ORIGIN_AIRPORT_ID, DEST_AIRPORT_ID) %>%
    summarise(DELAY_TIME_SUM=sum(DEP_DELAY_NEW),LINE_SUM=length(YEAR))
  return(delay_month)
}

#get data list including monthly data
data_date<-c('1501','1502','1503','1504','1505','1506','1507','1508','1509',
             '1510','1511','1512','1601','1602','1603','1604','1605','1606',
             '1607','1608','1609','1610','1611','1612','1701','1702','1703',
             '1704','1705','1706','1707','1708','1709','1710','1711','1712')
month_data<-lapply(data_date,RS_get_delay_data)

#transform the data list to data.frame
delay_data<-NULL
for (i in 1:length(data_date)){
  delay_data<-rbind.data.frame(delay_data,month_data[[i]])
}

#clear data.frame and get average delay time (in minutes)
delay_data<-delay_data %>%
  group_by(UNIQUE_CARRIER,AIRLINE_ID,ORIGIN_AIRPORT_ID, DEST_AIRPORT_ID) %>%
  summarise(DELAY_TIME_SUM=sum(DELAY_TIME_SUM),LINE_SUM=sum(LINE_SUM))

delay_data<-delay_data %>%
  group_by(UNIQUE_CARRIER,AIRLINE_ID,ORIGIN_AIRPORT_ID, DEST_AIRPORT_ID) %>%
  summarise(AVG_DELAY=(DELAY_TIME_SUM/LINE_SUM))

write.csv(delay_data,'C:/Users/rolco/Desktop/delay_data_for_RS.csv')
