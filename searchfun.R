# Searchfun R

library(twitteR)
stock.tweets = searchTwitter("zbuechler", n=1500)

                             since=as.character(time.from), until=as.character(time.to))
stock.tweets = searchTwitter("julieako", n=1500)


time.from
time.to
rm(puppies)

puppies <- character()

str(puppies)

as.character(time.from)



puppies <- c(puppies, as.character(time.from))

puppies

str(puppies[1])

puppies[1]

kk <- data.frame(puppies)
kk
str(puppies[1])


stock.query <- my.stocks[[i]]

stock.query <- "$MSFT"
stock.query <- "MSFT"
stock.query <- "Microsoft"

time.from
time.to

stock4 <- list("MSFT", "@MSFT", "Microsoft", "Bill Gates")

stock5 <- list("XOM")
my.stocks<- stock5
stock.query

time.from
time.to

rm(stock.tweets)
stock.tweets = searchTwitter(stock.query, n=1500, lang = 'en',
                             since=as.character(time.from), until=as.character(time.to))
time.from  <- as.Date(time.from, origin = "1900-01-01") + 1
time.to <- as.Date(time.from, origin = "1900-01-01") + 1

time.from  <- as.Date(time.from, origin = "1900-01-01") - 1
time.to <- as.Date(time.from, origin = "1900-01-01") + 1
stock.tweets = searchTwitter(stock.query, n=1500,
                             since=as.character(time.from), until=as.character(time.to))


stock.query <- "ibm"
stock.tweets = searchTwitter(stock.query, n=1500, lang = 'en')
                             

> length(stock.tweets)
#[1] 599
> length(stock.tweets2)
#[1] 899

stock.tweets = searchTwitter("Gates", n=1500, lang = 'en')
stock.tweets2b = searchTwitter("Bill Gates", n=1500, lang = 'en')

str(stock.tweets2b)
st2<- stock.tweets2b

length(st2)
kh2 <- character()
for(i in 1:length(st2))
{
  
kh2[[i]] <- st2[[i]]$text
}

kh2
kh2[1]
kh2[2]

kh2
library(plyr)
stock.text = laply(st2, function(t) t$getText() )


getwd()

#FUCK YEA THIS WORKS
write.csv(stock.text, file = "data8.txt")

laa2<- read.csv("data8.txt", stringsAsFactors = FALSE)

stjwjw <- as.character(laa2$x)

stjwjw == stock.text


#ahuh <- paste(stock.query, time.from, time.to, ".txt")

str(Sys.Date())
ahuh2 <- paste(time.from, time.to, Sys.Date(), stock.query, ".txt")

ahuh2
str(ahuh2)

str(ahuh)
write.csv(stock.text, file = ahuh)


laa2b<- read.csv(ahuh, stringsAsFactors = FALSE)

stjwjwa <- as.character(laa2b$x)

stjwjwa == stock.text


#Use these two
str(time.from)
str(time.to)

str(as.character(time.from))
str(as.character(time.to))

as.character(time.from)

str(stock.query)
paste(stock.query, time.from, time.to)

stock.scores = score.sentiment(stock.text, pos.words, neg.words, .progress='text')



str(st2[[2]])

str(st2[[1]])

st2[[1]]$text

st2[[1]]$

write.


stock.tweets3 = searchTwitter("XOM", n=1500)
stock.tweets3



for(i in 1:search.terms)
{
  #For searching multiple stocks + multiple stock terms    
  #stock.query <- my.stocks[[a]][[i]]
  
  #For searching one stock's terms
  stock.query <- my.stocks[[i]]
  
  rm(stock.tweets)
  stock.tweets = searchTwitter(stock.query, n=1500,
                               since=as.character(time.from), until=as.character(time.to))
  
  #            stock.tweets = searchTwitter(stock.query, n=1500,
  #                                         since=as.character("2013-11-05"), until=as.character(time.to))
  
  rm(stock.text)
  stock.text = laply(stock.tweets, function(t) t$getText() )
  stock.scores = score.sentiment(stock.text, pos.words, neg.words, .progress='text')
  
  #Assumption: we assume all tweets get equally exposed at all times
  #In the future we can see how many followers a tweet has.
  #then we can factor a score in to each sentiment, to see how real exposed it is.
  running.total<- c(running.total, stock.scores$score)
  
  # *** Add progress meter for long load times
}

#count of tweets?
num1 <- length(running.total)
tweet.count <- c(tweet.count, num1)

sent1 <- mean(running.total)
sentiment<- c(sentiment, sent1)

day1 <- time.from
dates <- c(dates, as.character(day1))      

# *** later we can store every keyword mean for each day as a list too
time.from  <- as.Date(time.from, origin = "1900-01-01") + 1
time.to <- as.Date(time.from, origin = "1900-01-01") + 1
# *** Do we care about weekends? Implement weekend separator to skipp weekends 
# and do three day analysis: Fr-Su!
}

stock.sentiment.data <- data.frame(sentiment, dates, tweet.count)
colnames(stock.sentiment.data) <- c("Sentiment","Date", "Number of Tweets")


getwd()

write.matrix(stock1final,file = "stock1finalold.csv", sep = ",")
dropbox.path <- "/Users/Vivek/Dropbox/Bus 192/Shared Bus 192/Twitter/"
setwd(dropbox.path)


stocktest <- read.csv(file = "stock1finalold.csv", stringsAsFactors = FALSE)
#stock1sent <- 


rm(Thurman)
rm(Thurman2)
rm(canada.cities)
rm(laa, laa2b)