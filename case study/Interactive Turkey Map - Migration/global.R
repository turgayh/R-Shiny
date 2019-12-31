library(readxl)
data <- read_excel("./data/2018-2019_data.xlsx")
graphData <- data
## 2019 Data
data2019 <- data[data$Year == "2019",]
BoatsArrived2019 <- sum(as.numeric(data2019$`Boats Arrived`), na.rm = TRUE)
TotalArrivals2019 <- sum(as.numeric(data2019$`Total Arrivals`), na.rm = TRUE)
TransfersMainland2019 <- sum(as.numeric(data2019$`Transfers to mainland`), na.rm = TRUE)
TotalPopulation2019 <- sum(as.numeric(data2019$`Total population`), na.rm = TRUE)

## 2018 Data
data2018 <- data[data$Year == "2018",]
BoatsArrived2018 <- sum(as.numeric(data2018$`Boats Arrived`), na.rm = TRUE)
TotalArrivals2018 <- sum(as.numeric(data2018$`Total Arrivals`), na.rm = TRUE)
TransfersMainland2018 <- sum(as.numeric(data2018$`Transfers to mainland`), na.rm = TRUE)
TotalPopulation2018 <- sum(as.numeric(data2018$`Total population`), na.rm = TRUE)