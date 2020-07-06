
consumer_key <- "CB39CGwzb3TKeJJ7gYBPdV0Yy"
consumer_secret <- "U35sL55LWm0tnW5NpWg2cvXuGfFlWndDV236JFy4hWqjQtgZLa"
access_token <- "267378572-2NOmMbqLcQaizJLHLVfyXf8hjiGS0sfrPb2llT9y"
access_secret <- "FzJeLSGpORc4MbkVhWjA2sIE5hSa6PCQpHGojOg3QNfk7"

setup_twitter_oauth(consumer_key, consumer_secret, 
                    access_token, access_secret)

ncdc_list <- searchTwitter("@ncdc", n=189)

library(dplyr)

ncdc_df <- bind_rows(lapply(ncdc_list, as.data.frame))


library(lubridate)

ncdc_df$date <- day(ncdc_df$created)
ncdc_df$hour <- hour(ncdc_df$created)


library(ggplot2)

ggplot(ncdc_df, aes(x = date)) + 
  geom_density()

ggplot(ncdc_df, aes(x = hour)) + 
  geom_density()

# Clean the data
ncdc_df$text <- gsub("@[[:alpha:]]*","", ncdc_df$text)

library(tm)

text_corpus <- Corpus(VectorSource(ncdc_df$text))

#  conver text to lowercase
text_corpus <- tm_map(text_corpus, tolower)


# Remove all NCDC and re etc

text_corpus <- tm_map(text_corpus, removeWords, 
c("ncdc", "rt", "re", "amp"))

# Remove stop words
text_corpus <- tm_map(text_corpus, removeWords, 
                      stopwords("english"))

# Remove punctuations



text_corpus <- tm_map(text_corpus, removePunctuation)


# Turn the clean data into data frame
text_df <- data.frame(text_clean = get("content", text_corpus), 
                      stringsAsFactors = FALSE)

head(text_df)

# bind text_df to ncdc_df

ncdc_df <- cbind.data.frame(ncdc_df, text_df)

library(SentimentAnalysis)
library(SnowballC)


ncdc_sentiment <- analyzeSentiment(ncdc_df$text_clean)

ncdc_sentiment <- dplyr::select(ncdc_sentiment, 
                                 SentimentGI, SentimentHE,
                                 SentimentLM, SentimentQDAP, 
                                 WordCount)
ncdc_sentiment <- dplyr::mutate(ncdc_sentiment, 
                                 mean_sentiment = rowMeans(ncdc_sentiment[,-5]))



ncdc_sentiment <- dplyr::select(ncdc_sentiment, 
                                 WordCount, 
                                 mean_sentiment)

ncdc_df <- cbind.data.frame(ncdc_df, ncdc_sentiment)

ncdc_df_negative <- filter(ncdc_df, mean_sentiment < 0)

nrow(ncdc_df_negative)

library(quanteda)

ncdc_tokenized_list <- tokens(ncdc_df_negative$text_clean)

ncdc_dfm <- dfm(ncdc_tokenized_list)

word_sums <- colSums(ncdc_dfm)
length(word_sums)

head(word_sums)

freq_data <- data.frame(word = names(word_sums), 
                        freq = word_sums, 
                        row.names = NULL,
                        stringsAsFactors = FALSE)


sorted_freq_data <- freq_data[order(freq_data$freq, decreasing = TRUE),]
                              
                              ]

ncdc_corpus_tm <- Corpus(VectorSource(ncdc_df_negative[,19]))

ncdc_dtm <- DocumentTermMatrix(ncdc_corpus_tm)


ncdc_dtm <- removeSparseTerms(ncdc_dtm, 0.98)

ncdc_df_cluster <- as.data.frame(as.matrix(ncdc_dtm))

library(devtools)

library(EGA)

library(mvtnorm)


#ega_ncdc <- EGA(ncdc_df_cluster)


ega_ncdc$dim.variables








sessionInfo()









