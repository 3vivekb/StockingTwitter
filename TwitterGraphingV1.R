# Twitter Stock Grapher

#My old design was to have a list of lists of stocks and keywords.
#Each stock data frame

dropbox.path <- "/Users/victorbergelin/Dropbox/SJSU ALLT/Kurser SJSU/Data mining SJSU/Shared Bus 192/Twitter"
dropbox.path <- "/Users/Vivek/Dropbox/Bus 192/Shared Bus 192/Twitter/"
setwd(dropbox.path)
getwd()

library(MASS)
library(Quandl)
library(ggplot2)
require(plyr)
?laply

stock1 <- list("IBM", "@IBM")
stock2 <- list("CSCO","@CSCO","Cisco", "CiscoSystems", "JohnChambers") #"John Chambers", "Gary Moore")
stock3 <- list("google", "larrypage", "GOOG")
stock4 <- list("MSFT", "@MSFT", "Microsoft", "Bill Gates")

# set to run:
to.date <- Sys.Date()
from.date <- as.Date(to.date, origin = "1900-01-01") - 16
time.days <- as.Date(to.date, origin = "1900-01-01") - as.Date(from.date, origin = "1900-01-01")

stock1.sent.full <-numeric()
stock2.sent.full <-numeric()
stock3.sent.full <-numeric()
stock4.sent.full <-numeric()

stock1.sent.full <- read.table(header = TRUE, file = "stock1sent.csv", stringsAsFactors = FALSE, sep = ",")
stock2.sent.full <- read.table(header = TRUE, file = "stock2sent.csv", stringsAsFactors = FALSE, sep = ",")
stock3.sent.full <- read.table(header = TRUE, file = "stock3sent.csv", stringsAsFactors = FALSE, sep = ",")
stock4.sent.full <- read.table(header = TRUE, file = "stock4sent.csv", stringsAsFactors = FALSE, sep = ",")

library(devtools)
install_github('R-package','quandl')
library(Quandl)
Quandl.auth("VtpWqKPmh8jqsfhykhbo")
QDayChange <- function(q.stock)
{
  change<-(q.stock$Close-q.stock$Open)/q.stock$Open
  return(change)
}

stock1.limited = Quandl("GOOG/NASDAQ_MSFT", start_date=from.date,end_date=to.date)
stock2.limited = Quandl("GOOG/NASDAQ_CSCO", start_date=from.date,end_date=to.date)
stock3.limited = Quandl("GOOG/NASDAQ_GOOG", start_date=from.date,end_date=to.date)
stock4.limited = Quandl("GOOG/NYSE_CVX", start_date=from.date,end_date=to.date)

stock1.data <-data.frame(as.character(stock1.limited$Date), QDayChange(stock1.limited), stringsAsFactors = FALSE)
stock2.data <-data.frame(as.character(stock2.limited$Date), QDayChange(stock2.limited), stringsAsFactors = FALSE)
stock3.data <-data.frame(as.character(stock3.limited$Date), QDayChange(stock3.limited), stringsAsFactors = FALSE)
stock4.data <-data.frame(as.character(stock4.limited$Date), QDayChange(stock4.limited), stringsAsFactors = FALSE)

colnames(stock1.data) <- c("Date", "DailyChange")
colnames(stock2.data) <- c("Date", "DailyChange")
colnames(stock3.data) <- c("Date", "DailyChange")
colnames(stock4.data) <- c("Date", "DailyChange")

stock1.sort <- stock1.data[order(stock1.data$Date) , ]
stock2.sort <- stock2.data[order(stock2.data$Date) , ]
stock3.sort <- stock1.data[order(stock3.data$Date) , ]
stock4.sort <- stock2.data[order(stock4.data$Date) , ]

stock1final <-merge(stock1.sort, stock1.sent.full, by.x = "Date", by.y = "Date")
stock2final <-merge(stock2.sort, stock2.sent.full, by.x = "Date", by.y = "Date")
stock3final <-merge(stock3.sort, stock3.sent.full, by.x = "Date", by.y = "Date")
stock4final <-merge(stock4.sort, stock4.sent.full, by.x = "Date", by.y = "Date")


str(stock1final)
stock1final <- within(stock1final, {
  Sentiment <- as.numeric(Sentiment)
})
stock2final <- within(stock2final, {
  Sentiment <- as.numeric(Sentiment)
})
stock3final <- within(stock3final, {
  Sentiment <- as.numeric(Sentiment)
})
stock4final <- within(stock4final, {
  Sentiment <- as.numeric(Sentiment)
})

stock1final$Sentiment[5] <- (stock1final$Sentiment[4] + stock1final$Sentiment[6])/2
#stock3final$Sentiment[5] <- (stock1final$Sentiment[4] + stock1final$Sentiment[6])/2
stock4final$Sentiment[5] <- (stock4final$Sentiment[4] + stock4final$Sentiment[6])/2

# Write to file
# IBM
x = as.Date(stock1final$Date)
y1 = stock1final$DailyChange
y2 = stock1final$Sentiment

# CISCO
x = as.Date(stock2final$Date)
y1 = stock2final$DailyChange
y2 = stock2final$Sentiment

# GOOGLE
x = as.Date(stock3final$Date)
y1 = stock3final$DailyChange
y2 = stock3final$Sentiment

# Microsoft
x = as.Date(stock4final$Date)
y1 = stock4final$DailyChange
y2 = stock4final$Sentiment

df <- data.frame(x,y1,y2)
colnames(df) <- c("Dates last 10 days", "Stock daily change", "Sentiment score")

# Line
ggplot(df, aes(x)) + # basic graphical object
  geom_line(aes(y=y1*10), colour="orange", size = 2) +  # first layer
  geom_line(aes(y=y2), colour="blue", size = 2)  + 
  #scale_size_area() + 
  xlab("Dates last 10 days") +
  ylab("Stock daily change and sentiment score") +
  ggtitle("Stock / Twitter analysis for Google")

# Point
ggplot(df, aes(x)) + # basic graphical object
  geom_point(aes(y=y1*10), colour="blue", size = 5) +  # first layer
  geom_point(aes(y=y2), colour="yellow", size = 5)  + 
  #scale_size_area() + 
  xlab("Dates last 10 days") +
  ylab("Stock daily change and sentiment score") +
  ggtitle("Stock / Twitter analysis for Microsoft")
