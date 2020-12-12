# Extract tweets for any user 
# Perform Sentimental Analysis
# Loading Library
library("twitteR")
#install.packages("ROAuth")
library("ROAuth")

cred <- OAuthFactory$new(consumerKey='ExLkvybQJPDBlMAZyoS4hueCi', # Consumer Key (API Key)
                         consumerSecret='iVHmRFUoOR5ee3NtUlPpY418w8a9QmN3Bn2zYJTa8wL8KApkoA', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

save(cred, file="twitter authentication.Rdata")

# load("twitter authentication.Rdata")

#install.packages("base64enc")
library(base64enc)

#install.packages("httpuv")
library(httpuv)

setup_twitter_oauth("ExLkvybQJPDBlMAZyoS4hueCi", # Consumer Key (API Key)
                    "iVHmRFUoOR5ee3NtUlPpY418w8a9QmN3Bn2zYJTa8wL8KApkoA", #Consumer Secret (API Secret)
                    "1318850690437971971-GgDY3vnLTf4YJEHGXeoIFukMelQOeu",  # Access Token
                    "KxHdySMC4AdKKPmj0qzJI8NNcvTmae3sF18m2ymeK5KTW")  #Access Token Secret
  
#registerTwitterOAuth(cred)

Tweets <- userTimeline('@imVkohli', n = 15,includeRts = T)
TweetsDF <- twListToDF(Tweets)
dim(TweetsDF)
View(TweetsDF)

write.csv(TweetsDF, "Tweets.csv",row.names = F)

getwd()
# Sentiment Alanysis
# Loading packages
install.packages("syuzhet")
library("syuzhet")
library(lubridate,ggplot2)
library(ggplot2)
library(scales)
library(dplyr)
library(reshape2)
# Loading Tweets of Viral Kohli - Captain of India CricKet Team
Kohli <- readLines("D:\\Data\\IN102385\\My Documents\\Tweets.csv")
View(Kohli)
Kohli<-iconv(Kohli,"UTF-8")
#each sentence  by eight
example<-get_sentences(Kohli)
nrc_data<-get_nrc_sentiment(Kohli)
head(nrc_data)
# Bar plot emotion mining
windows()
barplot(colSums(nrc_data),las=1,col=rainbow(10),ylab='count', main='Emotion Plot')
# Mostly there are positive sentiments in Virat Kohli's tweets.
sentiment_syuzhet<- get_sentiment(example,method="syuzhet")
sentiment_vector <- get_sentiment(example,method="bing")
sentiment_afinn <- get_sentiment(example, method="afinn")
sentiment_nrc <- get_sentiment(example, method="nrc")
sentiments <- data.frame(sentiment_syuzhet,sentiment_vector,sentiment_afinn,sentiment_nrc)

sum(sentiment_afinn)
mean(sentiment_afinn)
summary(sentiment_afinn)                                                                                                                                                                                                                                                                                                                                                                                                                              
windows()
plot(sentiment_vector,type='l',main='plot trajectory',xlab="Narrative time",ylab="Emotional Variance")
# Eight emotional attributes are 
# anger", "anticipation", "disgust", "fear", "joy", "sadness", 
# "surprise", "trust", "negative", "positive."

# To extract the sentence with the most negative emotional valence
negative <- example[which.min(sentiment_syuzhet)]
negative

# and to extract the most positive sentence
positive <- example[which.max(sentiment_syuzhet)]

### CONCLUSION
# Tweets of Virat Kohli was analysed and found to have more positive emotions than negatives
# Trajectory Plot indicates that positive emotions are very strong
# One example of postive and negative emotion were extracted from his tweets.
