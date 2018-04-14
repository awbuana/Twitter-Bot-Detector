# identifikasi
link <- "LINK"
retweet <- "RETWEET"
mention <- "MENTION"
hashtag <- "#"
hour <- "HOUR"


# opinion words
lexicon_positive = scan('positive.txt', what='character', comment.char=';')
lexicon_negative = scan('negative.txt', what='character', comment.char=';')


extractFeature <- function(username) {
  print(username)
  
  userTweets <- userTimeline(username,n=100, includeRts = TRUE)
  userTweets.df <- do.call(rbind, lapply(userTweets, as.data.frame))
  
  n <- nrow(userTweets.df)
  
  # tweet range time
  time <- max(userTweets.df$created) - min(userTweets.df$created)
  days <- as.numeric(time, units="days")  
  
  tweet_per_days <- n/days
  
  # mark link
  userTweets.df$text <- gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", link, userTweets.df$text)
  # mark retweet
  userTweets.df$text <- gsub("RT @\\w+:", retweet, userTweets.df$text)
  # mark mention
  userTweets.df$text <- gsub("@\\w+", mention, userTweets.df$text)
  # mark hour
  userTweets.df$text <- gsub("^[0-9]{1,2}[:.,-][0-9]{1,2}", hour, userTweets.df$text)
  
  mention_per_tweet <- mean(str_count(userTweets.df$text, mention))
  retweet_per_tweet <- mean(str_count(userTweets.df$text, retweet))
  link_per_tweet <- mean(str_count(userTweets.df$text, link))
  hashtag_per_tweet <- mean(str_count(userTweets.df$text, hashtag))
  
  avg_sentiment <- mean(sapply(userTweets.df$text, FUN=tweetSentiment))
  
  avg_distance <- tweetsDistance(userTweets.df$text)
  
  df <- data.frame(tweet_per_days,mention_per_tweet,retweet_per_tweet,link_per_tweet,hashtag_per_tweet,avg_sentiment, avg_distance)
  
  return(df)
}

tweetSentiment <- function(tweet) {

  #remove control character
  tweet <- gsub('[[:cntrl:]]', '', tweet)
  #remove punctuation
  tweet <- gsub('[[:punct:]]', '', tweet)
  #remove digit
  tweet <- gsub('\\d+', '', tweet)
  
  #lowercase
  error_catch = FALSE
  tryCatch(tweet <- tolower(tweet), error=function(e) error_catch=TRUE)
  if(error_catch) return(0)

  #split sentence to words vector
  words <- unlist(str_split(tweet, '\\s+'))
  #count sentiment match
  pos.matches <- !is.na(match(words, lexicon_positive))
  neg.matches <- !is.na(match(words, lexicon_negative))
  #sentiment score
  score <- sum(pos.matches) - sum(neg.matches)
  
  return(score)
}

tweetsDistance <- function(tweets) {
  distance <- 0
  n = length(tweets)
  for (i in 1:(n-1)){
    newdist = 0
    tryCatch(newdist<-stringdist(tweets[i],tweets[i+1],method="dl"),error=function(e)newdist<-0)
    distance <- distance + newdist
  }
  return(distance/n)
}
