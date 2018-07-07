#Twitter 1 - Configure Tweets and Download them
#@dupadhyaya
#Working using my Keys
library("curl")
library("twitteR")
library("ROAuth")
library("sqldf")
library("tidytext")
library(SentimentAnalysis)
library('syuzhet')

download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")

#https://apps.twitter.com/
#different for each account
consumerKey="rdq2ZpHjZuvUTSbq64gd7rP6O"
consumerSecret="d1F2WYXT81FKGQw2QLJ9SowBq3FYdp0oZIDx8rtOGRiTItu1cL"
AccessToken="311935827-9PRiOqHU5F0d3hzl6jL7uzk1dt6lg9bnBd4LCp3b"
AccessTokenSecret="Ui0NmPTsPbXUnP7Kf9d7ssJZ684VvqUUj0GlzqJ76wUmG"

#Common for all accounts except the keys

cred <- OAuthFactory$new(consumerKey=consumerKey, consumerSecret=consumerSecret,
                         requestURL='https://api.twitter.com/oauth/request_token', 
                         accessURL='https://api.twitter.com/oauth/access_token', 
                         authURL='https://api.twitter.com/oauth/authorize')

cred$handshake(cainfo="cacert.pem") # it will take you to browser: authorise, copy key

save(cred, file="twitter authentication.Rdata") # store this to avoid asking again


#Load saved authentication cert
load("twitter authentication.Rdata")
#registerTwitterOAuth(cred)

setup_twitter_oauth(consumerKey, consumerSecret, AccessToken, AccessTokenSecret)

search.string <- "#Belgium"
no.of.tweets <- 1200

tweets <- searchTwitter(search.string, n=no.of.tweets,lang="en", resultType = 'recent')

tweets <- searchTwitter(search.string, n=no.of.tweets,lang="en",
                        since='2017-01-01')

?searchTwitter

tweets.text <- sapply(tweets, function(x) x$getText())


# Replace retweet enteries (“rt”)
tweets.text <- gsub("rt", "", tweets.text)

# Replace @UserName
tweets.text <- gsub("@\\w+", "", tweets.text)

# Remove punctuation
tweets.text <- gsub("[[:punct:]]", "", tweets.text)

# Remove links
tweets.text <- gsub("http\\w+", "", tweets.text)

# Remove tabs
tweets.text <- gsub("[ |\t]{2,}", "", tweets.text)

# Remove blank spaces at the beginning
tweets.text <- gsub("^ ", "", tweets.text)

# Remove blank spaces at the end
tweets.text <- gsub(" $", "", tweets.text)



#  REMOVE STOP WORDS like "the"

#install tm – if not already installed
install.packages("tm")
library("tm")

#create corpus
tweets.text.corpus <- Corpus(VectorSource(tweets.text))

#clean up by removing stop words
tweets.text.corpus <- tm_map(tweets.text.corpus, function(x)removeWords(x,stopwords()))

toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern," ",
                                                                  x))})
tweets.text.corpus<- tm_map(tweets.text.corpus,toSpace,"[^[:graph:]]")

 

# GENERATE WORD CLOUD

#install wordcloud if not already installed
install.packages("wordcloud")
library("wordcloud")

#generate wordcloud
wordcloud(tweets.text.corpus,min.freq = 2, scale=c(7,0.5),colors=brewer.pal(8, "Dark2"), 
          random.color= TRUE, random.order = FALSE, max.words = 150)

?wordcloud

# To find wrong tweet

which(grepl("belgium woke up with a ", tweets.text))

tweets.text <- tweets.text[-1196] 

# Find Sentiments

sentiment <- analyzeSentiment(tweets.text.corpus)

dummy<-cbind(tweets.text,sentiment)


dummy$sent_type<-convertToBinaryResponse(sentiment)$SentimentGI

dummy <- dummy[c(1,16)]

table(dummy$sent_type)


#NRC

d<-get_nrc_sentiment(tweets.text)
td<-data.frame(t(d))

td_new <- data.frame(rowSums(td))
#The function rowSums computes column sums across rows for each level of a grouping variable.

#Transformation and  cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL

#Visualisation

qplot(sentiment, data=td_new, weight=count, geom="bar",fill=sentiment)+
  ggtitle("Feedback sentiments")+  coord_flip()

output1<-data.frame(colnames(t(td[1:8,]))[apply(t(td[1:8,]),1,which.max)])




?get_sentiments


par(mfrow= c(1,1))


class(tweets)

TextPreprocessing <- lapply(tweets, function(x) {
  
  x = gsub('http\\S+\\s*', '', x) ## Remove URLs
  
  x = gsub('\\b+RT', '', x) ## Remove RT
  
  x = gsub('#\\S+', '', x) ## Remove Hashtags
  
  x = gsub('@\\S+', '', x) ## Remove Mentions
  
  x = gsub('[[:cntrl:]]', '', x) ## Remove Controls and special characters
  
  x = gsub("\\d", '', x) ## Remove Controls and special characters
  
  x = gsub('[[:punct:]]', '', x) ## Remove Punctuations
  
  x = gsub("^[[:space:]]*","",x) ## Remove leading whitespaces
  
  x = gsub("[[:space:]]*$","",x) ## Remove trailing whitespaces
  
  x = gsub(' +',' ',x) ## Remove extra whitespaces
  
})


amerh_diab: RT @Namjoon51415854:
  