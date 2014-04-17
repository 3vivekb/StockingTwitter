# Twitter Stock Predictor

#My old design was to have a list of lists of stocks and keywords.
#Each stock data frame

#dropbox.path <- "/Users/victorbergelin/Dropbox/SJSU ALLT/Kurser SJSU/Data mining SJSU/Shared Bus 192/Twitter"
#setwd(dropbox.path)
dropbox.path <- "/Users/Vivek/Dropbox/Bus 192/Shared Bus 192/Twitter/"
setwd(dropbox.path)


getwd()

library(MASS)
library(Quandl)
require(plyr)
library(ggplot2)
?laply

source("TwitterGetScoresFunction.R") # Twitter analysis:

#https://dev.twitter.com/docs/api/1.1/get/search/tweets
#for details on searching tweets!
#TwitterSentimentData <- function(my.stocks, from.date, to.date, by.day = TRUE, path.select = 0)


stock1 <- list("IBM", "@IBM")
stock2 <- list("CSCO","@CSCO","Cisco", "CiscoSystems", "JohnChambers")
stock3 <- list("google", "larrypage", "GOOG")
stock4 <- list("MSFT", "@MSFT", "Microsoft", "Bill Gates")

# Dates
to.date <- Sys.Date()
from.date <- as.Date(to.date, origin = "1900-01-01") - 8
time.days <- as.Date(to.date, origin = "1900-01-01") - as.Date(from.date, origin = "1900-01-01")

# Set working directory
dropbox.path <- "/Users/Vivek/Dropbox/Bus 192/Shared Bus 192/Twitter/"
setwd(dropbox.path)

# Read saved data from CSV
stock1sentold <- read.csv(file = "stock1.csv", stringsAsFactors = FALSE)
stock2sentold <- read.csv(file = "stock2.csv", stringsAsFactors = FALSE)
stock3sentold <- read.csv(file = "stock3.csv", stringsAsFactors = FALSE)
stock4sentold <- read.csv(file = "stock4.csv", stringsAsFactors = FALSE)

colnames(stock1sentold) <- c("Sentiment", "Date", "Number of Tweets")
colnames(stock2sentold) <- c("Sentiment", "Date", "Number of Tweets")
colnames(stock3sentold) <- c("Sentiment", "Date", "Number of Tweets")
colnames(stock4sentold) <- c("Sentiment", "Date", "Number of Tweets")

# Load twitter data:
  stock1sent <- TwitterSentimentData(stock1, from.date, to.date)  
  stock2sent <- TwitterSentimentData(stock2, from.date, to.date)  
  stock3sent <- TwitterSentimentData(stock3, from.date, to.date)  
  stock4sent <- TwitterSentimentData(stock4, from.date, to.date)  
  stock5sent <- TwitterSentimentData(stock5, from.date, to.date)

# Merge data:
stock1.sent.full <- rbind(stock1sentold, stock1sent)
stock2.sent.full <- rbind(stock2sentold, stock2sent)
stock3.sent.full <- rbind(stock3sentold, stock3sent)
stock4.sent.full <- rbind(stock4sentold, stock4sent)

# Save merged files:
write.csv(stock1.sent.full,file = "stock1sent.csv", row.names = FALSE)
write.csv(stock2.sent.full,file = "stock2sent.csv", row.names = FALSE)
write.csv(stock3.sent.full,file = "stock3sent.csv", row.names = FALSE)
write.csv(stock4.sent.full,file = "stock4sent.csv", row.names = FALSE)

# Read saved files
stock1.sent.full <- read.table(header = TRUE, file = "stock1sent.csv", stringsAsFactors = FALSE, sep = ",")
stock2.sent.full <- read.table(header = TRUE, file = "stock2sent.csv", stringsAsFactors = FALSE, sep = ",")
stock3.sent.full <- read.table(header = TRUE, file = "stock3sent.csv", stringsAsFactors = FALSE, sep = ",")
stock4.sent.full <- read.table(header = TRUE, file = "stock4sent.csv", stringsAsFactors = FALSE, sep = ",")

# This part is unnecessary!
colnames(stock1.sent.full) <- c("Sentiment", "Date", "Number of Tweets")
colnames(stock2.sent.full) <- c("Sentiment", "Date", "Number of Tweets")
colnames(stock3.sent.full) <- c("Sentiment", "Date", "Number of Tweets")
colnames(stock4.sent.full) <- c("Sentiment", "Date", "Number of Tweets")


##########################################################

library(devtools)
install_github('R-package','quandl')
library(Quandl)

Quandl.auth("VtpWqKPmh8jqsfhykhbo")

QDayChange <- function(q.stock)
{  
  
  change<-(q.stock$Close-q.stock$Open)/q.stock$Open
  return(change)
}

# Search twitter stocks:
stock1.limited = Quandl("GOOG/NASDAQ_MSFT", start_date=from.date,end_date=to.date)
stock2.limited = Quandl("GOOG/NASDAQ_CSCO", start_date=from.date,end_date=to.date)
stock3.limited = Quandl("GOOG/NASDAQ_GOOG", start_date=from.date,end_date=to.date)
stock4.limited = Quandl("GOOG/NYSE_CVX", start_date=from.date,end_date=to.date)

# Create data frame:
stock1.data <-data.frame(as.character(stock1.limited$Date), QDayChange(stock1.limited), stringsAsFactors = FALSE)
stock2.data <-data.frame(as.character(stock2.limited$Date), QDayChange(stock2.limited), stringsAsFactors = FALSE)
stock3.data <-data.frame(as.character(stock3.limited$Date), QDayChange(stock3.limited), stringsAsFactors = FALSE)
stock4.data <-data.frame(as.character(stock4.limited$Date), QDayChange(stock4.limited), stringsAsFactors = FALSE)

# Name:
colnames(stock1.data) <- c("Date", "DailyChange")
colnames(stock2.data) <- c("Date", "DailyChange")
colnames(stock3.data) <- c("Date", "DailyChange")
colnames(stock4.data) <- c("Date", "DailyChange")

# Sort and merge:
stock1.sort <- stock1.data[order(stock1.data$Date) , ]
stock2.sort <- stock2.data[order(stock2.data$Date) , ]
stock3.sort <- stock1.data[order(stock3.data$Date) , ]
stock4.sort <- stock2.data[order(stock4.data$Date) , ]
stock1final <-merge(stock1.sort, stock1sent, by.x = "Date", by.y = "Date")
stock2final <-merge(stock2.sort, stock2sent, by.x = "Date", by.y = "Date")
stock3final <-merge(stock3.sort, stock3sent, by.x = "Date", by.y = "Date")
stock4final <-merge(stock4.sort, stock4sent, by.x = "Date", by.y = "Date")

# Prepare for plot
x = as.Date(stock1final$Date)
y1 = stock1final$DailyChange
y2 = stock1final$Sentiment

x = as.Date(stock2final$Date)
y1 = stock2final$DailyChange
y2 = stock2final$Sentiment

x = as.Date(stock3final$Date)
y1 = stock3final$DailyChange
y2 = stock3final$Sentiment

x = as.Date(stock4final$Date)
y1 = stock4final$DailyChange
y2 = stock4final$Sentiment

# Create data frame:
df <- data.frame(x,y1,y2)
colnames(df) <- c("Dates last 10 days", "Stock daily change", "Sentiment score")

# Plot data:
ggplot(df, aes(x)) + # basic graphical object
  geom_line(aes(y=y1*10), colour="red", size = 2) +  # first layer
  geom_line(aes(y=y2), colour="green", size = 2)  + 
  xlab("Dates last 10 days") +
  ylab("Stock daily change and sentiment score") +
  ggtitle("Stock / Twitter analysis for Microsoft")

# Sava data frame:
s <- paste(stock1[1], ".Rda")
save(df,file=as.character(s))







library(lmtest)
library(TTS)
data(ChickEgg)
rm(days)
days<-stock1final$Date
DC<-stock1final$DailyChange
sent<-stock1final$Sentiment


days<-ts(days) 
DC<-ts(DC) 
sent<-ts(sent) 
days
DC
sent

hey<-ts(stock1final)

granger(cbind(DC,sent), L=1) 

grangertest(DailyChange ~ Sentiment, order = 1, data = hey)
?grangertest
data(ChickEgg)
ChickEgg


stock1.data
stock1sent
stock2.data


#stock1final$Sentiment[5] <- (stock1final$Seniment[2] + stock1final$Sentiment[4])/2
#stock3final$Sentiment[5] <- (stock3final$Sentiment[4] + stock3final$Sentiment[6])/2
#stock4final$Sentiment[5] <- (stock4final$Sentiment[4] + stock4final$Sentiment[6])/2

stock1final
stock2final
stock3final
stock4final

getwd()
dropbox.path <- "/Users/Vivek/Dropbox/Bus 192/Shared Bus 192/Twitter/finaldata/"
setwd(dropbox.path)

write.csv(stock1final,file = "stock1final.csv", row.names = FALSE)
write.csv(stock2final,file = "stock2final.csv", row.names = FALSE)
write.csv(stock3final,file = "stock3final.csv", row.names = FALSE)
write.csv(stock4final,file = "stock4final.csv", row.names = FALSE)
