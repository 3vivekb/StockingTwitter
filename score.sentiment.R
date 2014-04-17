## 
# Mostly used Jeffrey Breen's code however 
# added unicode support for words and right now l
# in the process of adding multi-language support.
##
#https://gist.github.com/davidcoallier/5496106#file-processing-r

library(stringr)
tryTolower = function(x)
{
  # create missing value
  # this is where the returned value will be
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}


score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    #sentence = iconv(sentence, 'UTF-16LE', 'ASCII')
    sentence = iconv(sentence, 'UTF-8', 'ASCII')
    
    
    sentence <- gsub('[[:punct:]]', '', sentence)
    sentence <- gsub('[[:cntrl:]]', '', sentence)
    sentence <- gsub('\\d+', '', sentence)
    
    #sentence <- str_replace_all("[^[:alnum:]]", " ", sentence)
    
    #x <- "a1~!@#$%^&*(){}_+:\"<>?,./;'[]-=" #or whatever
    #str_replace_all(x, "[[:punct:]]", " ")
    
    sentence <- sapply(sentence, function(x) tryTolower(x))
    
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # They are not nothing.
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}