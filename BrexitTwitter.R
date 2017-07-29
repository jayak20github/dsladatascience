install.packages("lubridate")
library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)
library(wordcloud)


#Setup Oauth for Twitter
api_key <- "hlAjtdkJOB54w2LC4x6ftsRvB"
api_secret <- "4sScLwshSWEoiBKBjxubyMM4e6ld1EWCCBJrhAXqJzPCkFAMYn"
access_token <- "46191603-3hCgFGfjbzGwyvx5etEbCYsjcwMaODDnd9d5ZFnLP"
access_token_secret <- "X5xjJnSAIF5ABo0V8vACRCacb1cwHB2gmnI5OIA7kV6pE"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

#Extract tweets on brexit
brexit_tweets <- userTimeline("brexit", n = 1000)

# Convert the list of tweets into a data frame

brexit_tweetDF<-twListToDF(brexit_tweets)
head(brexit_tweetDF)

brexit_tweetDF<-as.tbl(brexit_tweetDF)
brexit_tweetDF


# Intial look at tweetng patterns

# Get source of tweets from statusSource
library(tidyr)

head(brexit_tweetDF$statusSource,20)

# Extract iPhone and Android updates

brexit_tweets_tbl <- brexit_tweetDF %>%
  select(id, statusSource, text, created) %>%
  tidyr::extract(statusSource, "source", "Twitter for (.*?)<")%>%
  filter(source %in% c("iPhone", "Android","iPad"))

brexit_tweets_tbl

# Transform the text of tweets into Document Term Matrix
# First clean the text
library(tm)

# Create a collection of documents with each tweet text is a row
# Remove some special characters like smilies to help cleaning


brexit_tweetDF$text<-sapply(brexit_tweetDF$text,function(x) iconv(x ,to="UTF-8-MAC",sub = "" ))
brexit_tweetCorpus <- Corpus(VectorSource(brexit_tweetDF$text))

# Usual transformations for cleaning the test
brexit_tweetCorpus <- tm_map(brexit_tweetCorpus, tolower)
brexit_tweetCorpus <- tm_map(brexit_tweetCorpus, removePunctuation)
brexit_tweetCorpus <- tm_map(brexit_tweetCorpus, removeNumbers)
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x) # remove URLs
brexit_tweetCorpus <- tm_map(brexit_tweetCorpus, removeURL)
twtrStopWords <- c(stopwords("english"),'with','the','an')
brexit_tweetCorpus <- tm_map(brexit_tweetCorpus, removeWords, twtrStopWords) # remove stop words


inspect(brexit_tweetCorpus)
brexit_tweetCorpus
brexit_tweets
# Create a Document Term Matrix
brexit_tweetDTM<-DocumentTermMatrix(brexit_tweetCorpus,list(termFreq=1))
inspect(brexit_tweetDTM)
saveRDS(brexit_tweetDTM,"/Users/jaya_kuppuswamy/Documents/Jaya/datasciencegit/twitter/brexit_tweetDTM")

# Find frequent terms
brexit_freqTerms<-findFreqTerms(brexit_tweetDTM,lowfreq = 10)

brexit_freqTerms
# Find their frequencies
brexit_term.freq<-colSums(as.matrix(brexit_tweetDTM))
brexit_term.freq
brexit_term.freq.df<-data.frame(term = names(brexit_term.freq),freq=brexit_term.freq)
brexit_term.freq.df


# Wordcloud
wordcloud(words = brexit_freqTerms,
          freq = brexit_term.freq.df[brexit_term.freq.df$term %in% brexit_freqTerms,2],
          max.words = 100,
          colors = T,random.color = T)

names(brexit_term.freq)

get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

nrcfear <- get_sentiments("nrc") %>% 
  filter(sentiment == "fear")

nrcfear

brexit_term.freq

brexit_term_count <- brexit_term.freq.df %>%
  inner_join(nrcfear, by=c("term"="sentiment")) %>%
  count(word, sort = TRUE)

head(brexit_term_count)

barplot(brexit_term_count)
