flight <- read.csv("/Users/jiangyiran/Downloads/comb14_17.csv")

colnames(flight)

max(flight$DEP_DELAY_GROUP,na.rm=TRUE)
head(flight)
dim(flight)
library(dplyr)
flighttest_1 <- flight %>%
  group_by(UNIQUE_CARRIER) %>%
  filter(DEP_DELAY_GROUP == -1) %>%
  summarise(n = n())
head(flighttest_1)

flighttest_2 <- flight %>%
  group_by(UNIQUE_CARRIER) %>%
  filter(DEP_DELAY_GROUP == 0) %>%
  summarise(n = n())
head(flighttest_2)

flighttest_3 <- flight %>%
  group_by(UNIQUE_CARRIER) %>%
  filter(DEP_DELAY_GROUP == 1) %>%
  summarise(n = n())
head(flighttest_3)

flighttest_4 <- flight %>%
  group_by(UNIQUE_CARRIER) %>%
  filter(DEP_DELAY_GROUP %in% 2:3) %>%
  summarise(n = n())
head(flighttest_4)

flighttest_5 <- flight %>%
  group_by(UNIQUE_CARRIER) %>%
  filter(DEP_DELAY_GROUP %in% 4:12) %>%
  summarise(n = n())
data.frame(flighttest)
flighttest$UNIQUE_CARRIER

flighttotal <- flight %>%
  filter(!is.na(DEP_DELAY_GROUP)) %>%
  filter(!is.na(UNIQUE_CARRIER)) %>%
  group_by(UNIQUE_CARRIER) %>%
  summarise(n = n())

flighttest_1$n <- flighttest_1$n/flighttotal$n
flighttest_2$n <- flighttest_2$n/flighttotal$n
flighttest_3$n <- flighttest_3$n/flighttotal$n
flighttest_4$n <- flighttest_4$n/flighttotal$n
flighttest_5$n <- flighttest_5$n/flighttotal$n

flight_data <- data.frame(flighttest_1)
flight_data<-cbind(flight_data,flighttest_1$n,flighttest_2$n,flighttest_3$n,flighttest_4$n,flighttest_5$n)
row.names(flight_data) <- flight_data$UNIQUE_CARRIER
flight_data <- flight_data[,3:7]
colnames(flight_data) <- c("-15-0","0-15","15-30","30-60",">60")
View(flight_data)
write.csv("delay_combined")


airname <- read.csv("/Users/jiangyiran/Downloads/airlines\ name.csv")
accident <- read.csv("/Users/jiangyiran/Desktop/Stat\ 2nd/ADS/accident.csv")
head(accident)
matchname <- unique(accident$Air.Carrier)
name <- airname[,2]
name <- toupper(name)
matchname <- toupper(matchname)
strsplit(name," ")
name[1]
q <- grep("SOUTHWEST",matchname)
matchname[q]
reg <- "ALASKA"
p <- regexpr(reg, matchname)
regmatches(matchname,p)
list <- c("AMERICAN AIRLINES","ALASKA AIRLINES","JETBLUE","DELTA AIR","ATLANTIC SOUTHEAST",
          "FRONTIER AIRLINES","HAWAIIAN AIRLINES","AMERICAN EAGLE","SPIRIT AIRLINES",
          "SKYWEST AIRLINES","US AIRWAYS","UNITED AIRLINES","SOUTHWEST AIRLINES")

new<-NULL
for (i in 1:length(list)){
  new2 <- accident %>%
    filter(str_detect(Air.Carrier,list[i])) %>%
    summarise(n = n())
  rownames(new2) <- list[i]
  new <- rbind(new,new2)
  print(new)
}
airname
colnames(accident)
head(accident)
setwd("/Users/jiangyiran/Desktop/Stat\ 2nd/ADS/")
write.csv(new, file = "security.csv", row.names = TRUE)
