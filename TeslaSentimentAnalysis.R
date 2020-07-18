
# Project:Real Time Sentiment Analysis of Tweets
#Darrel Donald (dld180001), Joseph Bao (jxb110430),Sheetal Kadam ( sak170006)


#install and load libraries

load.libraries <-
  c(
    'rtweet',
    'dplyr',
    'tm',
    'SentimentAnalysis',
    'dplyr',
    'tidyverse',
    'stringi',
    'wordcloud',
    'ggplot2'
  )

libraries_to_install <-
  load.libraries[!load.libraries %in% installed.packages()]
for (lib in libraries_to_install)
  install.packages(lib, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)




########### Data Collection #######################################

query <- "recession"

#read tweets

recession_tweets <- search_tweets(
  q = query,
  n = 10000,
  lang = "en",
  include_rts = FALSE
)
recession_tweets = x


recession_tweets <- dplyr::select(recession_tweets,
                                  created_at,
                                  text,
                                  place_name,
                                  place_type,
                                  country ,
                                  location)

#data cleaning

# latin to ASCII conversion

recession_tweets$text <-
  sapply(recession_tweets$text, function(row)
    iconv(row, "latin1", "ASCII", sub = ""))

# remove special characters

recession_tweets$text <-
  gsub("@[[:alpha:]]*", "", recession_tweets$text)
recession_tweets$text = gsub("http\\w+", "", recession_tweets$text)

recession_tweets$text = gsub("&amp", "", recession_tweets$text)
recession_tweets$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", recession_tweets$text)
recession_tweets$text = gsub("@\\w+", "", recession_tweets$text)
recession_tweets$text = gsub("[[:digit:]]", "", recession_tweets$text)
recession_tweets$text = gsub("[ \t]{2,}", "", recession_tweets$text)
recession_tweets$text = gsub("^\\s+|\\s+$", "", recession_tweets$text)


# remove emoticons
finalText = str_replace_all(recession_tweets$text, "[^[:graph:]]", " ")
finalText = gsub("&amp", "", finalText)
finalText = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", finalText)

#create corpus and use tm package

text_corpus <- Corpus(VectorSource(finalText))

text_corpus <- tm_map(text_corpus, removeWords,
                      c("rt", "re", "amp"))

text_corpus <- tm_map(text_corpus, removeWords,
                      stopwords("english"))

text_corpus <- tm_map(text_corpus, removePunctuation)


text_df <- data.frame(text_clean = get("content", text_corpus),
                      stringsAsFactors = FALSE)

recession_tweets <- cbind.data.frame(recession_tweets, text_df)


######################### Data Exploration ##############################

# generate word cloud
dtm <- TermDocumentMatrix(text_corpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)


set.seed(1234)

wordcloud(
  words = d$word,
  freq = d$freq,
  min.freq = 2,
  max.words = 200,
  random.order = FALSE,
  rot.per = 0.35,
  colors = brewer.pal(8, "Dark2")
)


###################Sentiment Extraction ##############################################

recession_sentiment <- analyzeSentiment(recession_tweets$text_clean)

# get average sentiment from different dictionaries
recession_sentiment <- dplyr::mutate(recession_sentiment,
                                     average_sentiment = rowMeans(recession_sentiment[, 1:4]))

recession_sentiment <- dplyr::mutate(recession_sentiment,
                                     direction = convertToDirection(recession_sentiment$SentimentQDAP))


recession_tweets <-
  cbind.data.frame(recession_tweets, recession_sentiment)

###################Sentiment Plots ##############################################


# Count positive and negative categories
table(convertToBinaryResponse(recession_tweets$SentimentLM))

# Data Distribution of sentiment
summary(recession_tweets$SentimentLM)

# Histogram
hist(recession_sentiment$SentimentLM,
     probability = TRUE,
     main = "Density of Distribution for Standardized Sentiment")
lines(density(recession_sentiment$SentimentLM))

# scales sentiment
hist(scale(recession_tweets$average_sentiment), main = "Density of Distribution for Scaled Average Sentiment")

#correlation of different dictionaries
cor(recession_sentiment[, c("SentimentLM", "SentimentHE", "SentimentQDAP")])

#sentiment plots
plotSentiment(recession_sentiment$SentimentLM, xlab = "Tweets about recession (LM)")
plotSentiment(recession_sentiment$SentimentHE, xlab = "Tweets about recession (HE)")

# positive vs negative
plot(recession_sentiment$direction, xlab = "Sentiment")

#plot average sentiment
plot(recession_tweets$average_sentiment,
     ylab = "Average sentiment",
     xlab = "Tweets")


plotSentiment(recession_sentiment, xlab = "Tweets")

plotSentiment(recession_sentiment, cumsum = TRUE, xlab = "Tweets")

plotSentiment(recession_sentiment) + ggtitle("Evolving sentiment")
plotSentiment(recession_sentiment) + theme_void()


# sentiment across tweets by time
plotSentiment(
  recession_tweets$SentimentLM,
  x = recession_tweets$created_at,
  xlab = "Time",
  cumsum = TRUE
)

# plot of sentiment over time
ggplot(recession_tweets, aes(x = created_at, y = average_sentiment)) +
  geom_point(aes(color = country)) 
