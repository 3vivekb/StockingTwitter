TwitterSentimentData <- function(my.stocks, from.date, to.date, by.day = TRUE, path.select = 1, save.tweets = 1)
{

time.days <- as.Date(to.date, origin = "1900-01-01") - as.Date(from.date, origin = "1900-01-01")
time.numdays <- as.numeric(time.days)


time.from <- as.Date(from.date, origin = "1900-01-01")
time.to <- as.Date(time.from, origin = "1900-01-01")  + 1

source("score.sentiment.R") # Twitter analysis:

# Twitter API access:
CUSTOMER_KEY <- "*"
CUSTOMER_SECRET <- "*"
ACCESS_TOKEN <- "*"
ACCESS_secret<- "*"
library(twitteR)
twitCred<- setup_twitter_oauth(CUSTOMER_KEY, CUSTOMER_SECRET, ACCESS_TOKEN, ACCESS_secret, credentials_file=NULL)

#you should still be in the dropbox right now.
hu.liu.pos = scan('positive-words.txt',
                  what='character', comment.char=';')
hu.liu.neg = scan('negative-words.txt',
                  what='character', comment.char=';')
pos.words = c(hu.liu.pos, 'upgrade')
neg.words = c(hu.liu.neg, 'wtf', 'wait', 'waiting',
              'epicfail', 'mechanical')
pos.words = c(hu.liu.pos, 'long')
neg.words = c(hu.liu.neg, 'short')

# Set up variables:
days<-numeric()
stocks.scores <- numeric()
stocks.query <- numeric()
stocks.tweets <- numeric()
stocks.text <-numeric()
stocks.mean <-numeric()
dates <- character()
dates <-list()
dates<- character()
sentiment<- numeric()
tweet.count <- numeric()


# Loop though all days:
t<-1
for(t in 1:time.numdays)
{
      rm(running.total)
      running.total<-numeric()
      i<-1
      search.terms <- length(my.stocks)
      
          # Loop through:
          for(i in 1:search.terms)
          {
                stock.query <- my.stocks[[i]]
            
                rm(stock.tweets)
                stock.tweets = searchTwitter(stock.query, n=1500,
                                         since=as.character(time.from), until=as.character(time.to), lang = 'en',)
                stock.text = laply(stock.tweets, function(t) t$getText() )
            
                if(save.tweets){
                    title <- paste("tweetbox/", time.from, time.to, Sys.Date(), stock.query, ".txt")
                    write.csv(stock.text, file = as.character(title))
                }
                stock.scores = score.sentiment(stock.text, pos.words, neg.words, .progress='text')

                running.total<- c(running.total, stock.scores$score)    
          }
      
      #count of tweets
      num1 <- length(running.total)
      tweet.count <- c(tweet.count, num1)
      
      sent1 <- mean(running.total)
      sentiment<- c(sentiment, sent1)
      day1 <- time.from
      dates <- c(dates, as.character(day1))      

      # Increase date:
      time.from  <- as.Date(time.from, origin = "1900-01-01") + 1
      time.to <- as.Date(time.from, origin = "1900-01-01") + 1
}
  
  stock.sentiment.data <- data.frame(sentiment, dates, tweet.count)
  colnames(stock.sentiment.data) <- c("Sentiment","Date", "Number of Tweets")
  return(stock.sentiment.data)
  
}
