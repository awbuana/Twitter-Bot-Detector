setwd(c("C:/Users/ASUS/Desktop/Proyek Akhir"))
load("C:/Users/ASUS/Desktop/Proyek Akhir/data.RData")

# install all packages
install.packages("twitteR")
install.packages("httpuv")
install.packages("stringr")
install.packages("stringdist")
install.packages("caret")
install.packages("Rweka")
install.packages("kernlab")
install.packages("klaR")
install.packages("FSelector")

#connect all libraries
library(twitteR)
library(httpuv)
library(stringr)
library(stringdist)
library(caret)
library(RWeka)
library(kernlab)
library(klaR)
library(FSelector)

#connect to API
consumer_key <- '#' #put the Consumer Key from Twitter Application
consumer_secret <- '#'  #put the Consumer Secret from Twitter Application
access_token <- '#'
access_token_secret <- '#'
setup_twitter_oauth(consumer_key, consumer_secret)


# load training data
bot_data <- read.csv("bot.csv", stringsAsFactors=FALSE, header=TRUE)


# feature extraction
source("extractFeature.R")
feature_tmp <- t(sapply(bot_data$username, FUN=extractFeature))
feature <- cbind(feature_tmp,bot_data[,-1])

feature$tweet_per_days<- as.numeric(feature$tweet_per_days)
feature$mention_per_tweet<- as.numeric(feature$mention_per_tweet)
feature$retweet_per_tweet<- as.numeric(feature$retweet_per_tweet)
feature$link_per_tweet<- as.numeric(feature$link_per_tweet)
feature$hashtag_per_tweet<- as.numeric(feature$hashtag_per_tweet)
feature$avg_sentiment<- as.numeric(feature$avg_sentiment)
feature$avg_distance<- as.numeric(feature$avg_distance)


# attr imporatnce
label <- feature$bot
feature_non_label <- feature[-10]
weights <- chi.squared(label ~., feature_non_label)
weight_order <- rev(order(weights))
weights$attr_importance[weight_order]
barplot(weights$attr_importance[weight_order], 
        main="Feature Importance", 
        col = 1:9)
legend("topright", 
       legend = rownames(weights)[weight_order],
       fill = 1:9,
       cex = 0.6)
feature_raw <- feature
feature <- cbind(feature[,rownames(weights)[weight_order][1:7]],feature$bot)
colnames(feature)[8] <- "bot"


# define training control
train_control <- trainControl(method="cv", number=10)


# model experiment 
model_j48 <- train(bot~., data=feature, trControl=train_control, method="J48")
model_svm1 <- train(bot~., data=feature, trControl=train_control, method="svmLinear")
model_svm2 <- train(bot~., data=feature, trControl=train_control, method="svmPoly")
model_svm3 <- train(bot~., data=feature, trControl=train_control, method="svmRadial")
model_nb <- train(bot~., data=feature, trControl=train_control, method="nb")

feature_scale <- data.frame(scale(feature[-8]),feature[model_svm2 <- train(bot~., data=feature, trControl=train_control, method="svmPolynomial")])
model_knn <- train(bot~., data=feature, trControl=train_control, method="knn")
model_knn_scale <- train(bot~., data=feature_scale, trControl=train_control, method="knn")

# choosing model
model <- model_svm2
print(model)
confusionMatrix(model)

# training model
model_svm2 <- train(bot~., data=feature, method="svmPoly")
model <- model_svm2

# apakah rata-rata tweet yang dipublikasi oleh bot berbeda dengan manusia?
hasil_Ttest <- t.test(feature$tweet_per_days~as.factor(feature$bot))


# buat test
test_username <- "TimmyTimeKids"
test_following <- getUser(test_username)$friendsCount
test_follower <- getUser(test_username)$followersCount
data_test <- t(sapply(test_username, FUN=extractFeature))
fol <- data.frame(c(test_following),c(test_follower))
colnames(fol) <- c('following','follower')
data_test <- cbind(data_test,fol)
data_test$tweet_per_days<- as.numeric(data_test$tweet_per_days)
data_test$mention_per_tweet<- as.numeric(data_test$mention_per_tweet)
data_test$retweet_per_tweet<- as.numeric(data_test$retweet_per_tweet)
data_test$link_per_tweet<- as.numeric(data_test$link_per_tweet)
data_test$hashtag_per_tweet<- as.numeric(data_test$hashtag_per_tweet)
data_test$avg_sentiment<- as.numeric(data_test$avg_sentiment)
data_test$avg_distance<- as.numeric(data_test$avg_distance)

data_test <- data_test[,colnames(feature[1:(ncol(feature)-1)])]

predict(model,data_test)


######################### PROPORSI dari hastag ramadhan lokasi jakarta
tweets_ramadhan <- searchTwitter("#ramadhan", n=1000, geocode ='-6.1751100,106.8650390,50mi')
tweets_ramadhan_df <- twListToDF(tweets_ramadhan)
tweets_ramadhan_user <- data.frame(table(tweets_ramadhan_df$screenName))
colnames(tweets_ramadhan_user) <- c('user','freq')
tweets_ramadhan_user <- tweets_ramadhan_user[1:500,1]
feature_ramadhan <- t(sapply(as.factor(tweets_ramadhan_user), FUN=extractFeature))
ramadhan_following <-  t(sapply(as.factor(tweets_ramadhan_user), FUN=followingfun))
ramadhan_follownig_t <- t(ramadhan_following)
ramadhan_follower <-  (sapply(as.factor(tweets_ramadhan_user), FUN=followerfun))
ramadhan_follower[201:500] <- 0
ramadhan_follower <- data.frame(ramadhan_follower)
ramadhan_follower <- t(ramadhan_follower)
colnames(ramadhan_follower) <- c('follower')
colnames(ramadhan_follownig_t) <- c('following')
ramadhan_df <- cbind(feature_ramadhan,ramadhan_follownig_t,ramadhan_follower)
ramadhan_df <- data.frame(ramadhan_df)
ramadhan_df$tweet_per_days<- as.numeric(ramadhan_df$tweet_per_days)
ramadhan_df$mention_per_tweet<- as.numeric(ramadhan_df$mention_per_tweet)
ramadhan_df$retweet_per_tweet<- as.numeric(ramadhan_df$retweet_per_tweet)
ramadhan_df$link_per_tweet<- as.numeric(ramadhan_df$link_per_tweet)
ramadhan_df$hashtag_per_tweet<- as.numeric(ramadhan_df$hashtag_per_tweet)
ramadhan_df$avg_sentiment<- as.numeric(ramadhan_df$avg_sentiment)
ramadhan_df$avg_distance<- as.numeric(ramadhan_df$avg_distance)
ramadhan_df$following<- as.numeric(ramadhan_df$following)
ramadhan_df$follower<- as.numeric(ramadhan_df$follower)


followingfun <- function(username){
  test_following <- getUser(username)$friendsCount
  return(data.frame(test_following))
}

followerfun <- function(username){
  test_follower <- getUser(username)$followersCount
  return(data.frame(test_follower))
}

proporsi <- predict(model_svm2_scale,ramadhan_df)
table(proporsi) #porporsi tweeter jakarta pada hashtag ramadhan

save.image("C:/Users/ASUS/Desktop/Proyek Akhir/data.RData")
