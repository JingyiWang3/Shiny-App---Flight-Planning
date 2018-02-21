count_rate<-function(vec){
  count<-length(strsplit(vec,'fill')[[1]])-1
  return(count)
}

rate_table<-function(name){
  url<-paste("http://www.airlinequality.com/airline-reviews/",
             name,'/?sortby=post_date%3ADesc&pagesize=10',sep='')
  surl<-url(url,'r')
  page<-readLines(surl,warn=FALSE,encoding='UTF-8')
  rate_pattern<-"<span class=\"star.*"
  rate<-page[grepl(rate_pattern,page)][1:5]
  rate<-sapply(rate,count_rate)
  names(rate)<-NULL
  return(rate)
}

airlines<-c('American Airlines','Alaska Airlines','JetBlue Airways',
            'Delta Air Lines','Frontier Airlines','Hawaiian Airlines',
            'SkyWest Airlines','United AirLines','US Airways','Virgin America',
            'Southwest Airlines','Spirit AirLines')
#'ExpressJet Airlines', 'AirTran Airways', 'Envoy Air','American Eagle Airlines',
#'Atlantic Southeast Airlines'

library(stringr)
airlines<-str_to_lower(airlines)
airlines<-gsub(' ','-',airlines)

library(plyr)
review_data<-ldply(airlines,latest_review100)

rate_data<-ldply(airlines,rate_table)
rownames(rate_data)<-airlines
names(rate_data)<-c('food&beverages','entertainment','seat comfort',
                    'stuff-service','value for money')

write.csv(rate_data,'C:/Users/rolco/desktop/consumer_satisfaction.csv')
