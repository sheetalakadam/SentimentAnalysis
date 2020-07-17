
#install and load libraries

load.libraries <-
  c('rtweet',
    'dplyr',
    'tm',
    'SentimentAnalysis',
    'dplyr'
    )

libraries_to_install <-
  load.libraries[!load.libraries %in% installed.packages()]
for (lib in libraries_to_install)
  install.packages(lib, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)

install.packages("SentimentAnalysis")
q <- "Tesla"
filename = "out.txt"

tesla_tweets <- search_tweets(q = "Tesla", n = 10000,
                               lang = "en",
                               include_rts = FALSE)

tesla_tweets$text <- gsub("http.*","",  tesla_tweets$text)
tesla_tweets$text <- gsub("https.*","", tesla_tweets$text)

text_corpus <- Corpus(VectorSource(tesla_tweets$text))
text_corpus <- tm_map(text_corpus, tolower)
text_corpus <- tm_map(text_corpus, removeWords, 
                      c("rt", "re", "amp"))


text_corpus <- tm_map(text_corpus, removeWords, 
                      stopwords("english"))


text_corpus <- tm_map(text_corpus, removePunctuation)

text_df <- data.frame(text_clean = get("content", text_corpus), 
                      stringsAsFactors = FALSE)

tesla_tweets <- cbind.data.frame(tesla_tweets, text_df)






tesla_sentiment <- analyzeSentiment(tesla_tweets$text_clean)

