# Extract reviews of any product from ecommerce website like amazon
# Perform emotion mining

# Loading the required packaged 
install.packages(c("rvest","XML","magrittr"))
library(rvest)
library(XML)
library(magrittr)

# Extracting Amazon Reviews for book " The Tower of Nero"

aurl <- "https://www.amazon.in/Tower-Nero-Trials-Apollo-Book/product-reviews/0141364084/ref=cm_cr_getr_d_paging_btm_prev_2?ie=UTF8&reviewerType=all_reviews"
amazon_reviews <- NULL
# Loop to read and club the reviews from pages
for (i in 1:10){
 murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>% html_nodes(".review-text") %>% html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}

length(amazon_reviews)
amazon_reviews

#creating the .txt file 
write.table(amazon_reviews,"towerofnero.txt",row.names = F)
getwd()
# Install package for pre-processiong the data
install.packages("tm")  # for text mining
install.packages(c("SnowballC","textstem")) # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes

library('tm')
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library('textstem')
# Importing book reviews data
tower<-read.csv("D:\\Data\\IN102385\\My Documents\\towerofnero.txt")
x <- as.character(tower$x)
x <- iconv(x, "UTF-8") #Unicode Transformation Format. The '8' means 
                       #it uses 8-bit blocks to represent a character
# Load the data as a corpus
x <- Corpus(VectorSource(x))
inspect(x[1:3])

# Removing unnecessary symbols like -,;,: etc

toSpace <- content_transformer(function(y,pattern) { return (gsub(pattern, " ",y))})

x1 <- tm_map(x, toSpace, "-")
inspect(x1[1])
x1 <- tm_map(x1, toSpace, "!")
inspect(x1[1])
x1 <- tm_map(x1, toSpace, "'")
inspect(x1[1])

# Convert the text to lower case
x1 <- tm_map(x1, tolower)
inspect(x1[1])
# Remove numbers
x1 <- tm_map(x1, removeNumbers)
# Remove punctuations
x1 <- tm_map(x1, removePunctuation)
inspect(x1[1])
# Remove english common stopwords
x1 <- tm_map(x1, removeWords, stopwords('english'))
# Remove your own stop word
# specify your stopwords as a character vector
inspect(x1[1])
x1 <- tm_map(x1, removeWords, c("the","will","im")) 
inspect(x1[1])
#striping white spaces
x1 <- tm_map(x1, stripWhitespace)
inspect(x1[1])
# Text stemming
x1<-lemmatize_words(x1)
                                                                                                                                                                                                                                           inspect(x1[1])
# Term document matrix(TDM)
# converting unstructured data to structured format using TDM

tdm <- TermDocumentMatrix(x1)
tdm <- as.matrix(tdm)
tdm
#Frequency of words
v <- sort(rowSums(tdm),decreasing=TRUE)
v
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Bar plot
w <- rowSums(tdm)
w_sub <- subset(w, w >= 20)
barplot(w_sub, las=3, col = rainbow(20))
# Term watch repeats in all most all documents
x1 <- tm_map(x1, removeWords, c('besides','just','also'))
x1 <- tm_map(x1, stripWhitespace)
tdm <- TermDocumentMatrix(x1)
tdm <- as.matrix(tdm)
tdm
w1 <- rowSums(tdm)
# Word cloud
#with all the words
wordcloud(words = names(w1), freq = w1, random.order = F, colors = rainbow(20), scale=c(2,.2), rot.per = 0.3)
# Loading positive and negative dictionaries
pos.words = scan(file.choose(), what="character", comment.char=";")	# read-in positive-words.txt
neg.words = scan(file.choose(), what="character", comment.char=";") 	# read-in negative-words.txt
pos.words = c(pos.words,"wow", "kudos", "hurray") # including own positive words to the existing list

# Positive wordcloud
pos.matches = match(names(w), c(pos.words))
pos.matches = !is.na(pos.matches)
freq_pos <- w[pos.matches]
p_names <- names(freq_pos)

wordcloud(p_names,freq_pos,scale=c(3.5,.2),random.order = F,colors = rainbow(20))

# Negative wordcloud
neg.matches = match(names(w), c(neg.words))
neg.matches = !is.na(neg.matches)
freq_neg <- w[neg.matches]
n_names <- names(freq_neg)

wordcloud(n_names,freq_neg,scale=c(3.5,.2),random.order=F,colors = brewer.pal(8,"Dark2"))

# We can check associated words with some key words
# Association between words
tdm<-TermDocumentMatrix(x1)
findAssocs(tdm,c("tyrant"),corlimit=0.6)

# Emotion Mining of the Review
# Sentiment Alanysis
install.packages("syuzhet")
library("syuzhet")
library(lubridate,ggplot2)
library(ggplot2)
library(scales)
library(dplyr)
library(reshape2)
tower <- readLines("D:\\Data\\IN102385\\My Documents\\towerofnero.txt")
tower<-iconv(tower,"UTF-8")
#each sentence  by eight
emotion<-get_sentences(tower)
nrc_data<-get_nrc_sentiment(emotion)
head(nrc_data)
# Bar plot emotion mining
windows()
barplot(colSums(nrc_data),las=1,col=rainbow(10),ylab='count', main='Emotion Plot')
sentiment_syuzhet<- get_sentiment(emotion,method="syuzhet")
sentiment_vector <- get_sentiment(emotion,method="bing")
sentiment_afinn <- get_sentiment(emotion, method="afinn")
sentiment_nrc <- get_sentiment(emotion, method="nrc")
sentiments <- data.frame(sentiment_syuzhet,sentiment_vector,sentiment_afinn,sentiment_nrc)

sum(sentiment_afinn)
mean(sentiment_afinn)
summary(sentiment_afinn)
windows()
plot(sentiment_vector,type='l',main='plot trajectory',xlab="Narrative time",ylab="Emotional Variance")

# To extract the sentence with the most negative emotional valence
negative <- emotion[which.min(sentiment_syuzhet)]
negative

# and to extract the most positive sentence
positive <- emotion[which.max(sentiment_syuzhet)]
positive
## CONCLUSION 
# All the main attributes (Overall, Positive and Negative)were plotted in Word Cloud
# Sentimental Analysis indicates negatives and positives w.r.t 8 parameters were classified 
# The book " The Tower of Nero" was found to have more positive motions than negative.
# Sentimental analysis indicates that the book is reading worth as it balances 
# both positive and negative emotions which is plotted in Trajectory plot
