library(rtweet)
library(quanteda)
library(tidyverse)
library(ggplot2)
library(topicmodels)
library(tidytext)
library(dplyr)
library(sentimentr)
library(widyr)
library(ggraph)
library(igraph)
library(ggpubr)
library(textdata)

tweets_vaccine.df <- tweets_covid.df
query <- "vaccine OR VACCINE OR Vaccine" 

tweets_vaccine.df <- search_tweets(
  query,
  n = 200000,
  type = "recent",
  include_rts = TRUE,
  geocode = NULL,
  max_id = NULL,
  parse = TRUE,
  token = NULL,
  retryonratelimit = TRUE,
  verbose = TRUE,
  lang = "en",
  tweet_mode = "extended"
)

print(nrow(tweets_vaccine.df))

selected_columns <- c(1,3:6,12:14,17,67,68)
print(colnames(tweets_vaccine.df[,selected_columns]))

corp_twitter <- quanteda::corpus(tweets_vaccine.df[,selected_columns], text_field = "text")
summary(corp_twitter, n=5)
texts(corp_twitter)[1:5]
tokens(texts(corp_twitter[1:5]))
summary.corpus <- summary(corp_twitter, n=nrow(tweets_vaccine.df))

first_tweet <- head(tweets_vaccine.df[order(tweets_vaccine.df$created_at),], n=1)
first_tweet$created_at

last_tweet <- tail(tweets_vaccine.df[order(tweets_vaccine.df$created_at),], n=1)
last_tweet$created_at

docvars(corp_twitter, "Time") <- as.POSIXct(docvars(corp_twitter, "created_at"))

ggplot(summary.corpus, aes(x=created_at)) +
  geom_histogram(aes(y=..count..), 
                 binwidth=3600,
                 colour="blue",
                 fill="blue", 
                 alpha=0.8) +
  ggtitle(paste0("Activity ",nrow(tweets_vaccine.df)," tweets")) +
  scale_y_continuous(name="Number of tweets") + 
  scale_x_datetime(date_labels = "%H:%M:%S", breaks = scales::pretty_breaks(8))

word_tweets <- tokens(corp_twitter,
                     what="word",
                     remove_numbers=TRUE,
                     remove_punct=TRUE,
                     remove_symbols=TRUE,
                     remove_separators=TRUE,
                     remove_url=TRUE)
head(word_tweets,n=2)
word_tweets <- tokens_remove(word_tweets,stopwords(language = "en"))
head(word_tweets,n=2)
words_to_remove <- c('vaccine','vaccin','vaccines')
dfmat_corp_twitter <- dfm(word_tweets,remove = words_to_remove,
                          stem = FALSE, remove_punct = TRUE, remove_url=TRUE)
dfFreq <- textstat_frequency(dfmat_corp_twitter)

ggplot(dfFreq[1:40,], aes(x=feature, y=frequency)) + 
  geom_col() +
  coord_flip() +
  labs(x = "Word", y = "Count") +
  theme_minimal()

ggplot(dfFreq[1:40,], aes(x=reorder(feature, -rank), y=frequency)) + 
  geom_col() +
  coord_flip() +
  labs(x = "Word", y = "Count") +
  theme_minimal()

textplot_wordcloud(dfmat_corp_twitter, min_count = 6, random_order = FALSE,
                   rotation = .25,
                   color = RColorBrewer::brewer.pal(8, "Dark2"))

dfFreq_long_top <- tidyr::gather(
  dfFreq[1:40,],
  key = "key",
  value = "value",
  c("frequency","docfreq"),
  na.rm = FALSE,
  convert = FALSE,
  factor_key = FALSE
)
ggplot(dfFreq_long_top, aes(x=reorder(feature, -rank), y=value, fill = key)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_x_discrete(breaks = dfFreq_long_top[1:40,"feature"]) + 
  labs(x = "Words", y = "Count") +
  coord_flip() +
  theme_minimal()

dfm2 <- dfm(tokens_ngrams(word_tweets,n=2))
dfFreq2 <- textstat_frequency(dfm2)
ggplot(dfFreq2[1:40,], aes(x=reorder(feature, frequency), y=frequency)) + 
  geom_col() +
  labs(x = "Words", y = "Count") +
  coord_flip() +
  theme_minimal()


set.seed(1234)
dtm <- convert(dfmat_corp_twitter, to = "topicmodels")
lda2 <- LDA(dtm, k = 2, control = list(seed=1234))
terms(lda2, 10)

topics<-tidy(lda2, matrix = "beta")
topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  ggplot(aes(reorder(term, beta),beta,fill=factor(topic)))+
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_fill_viridis_d() +
  coord_flip() +
  labs(x = "Topic",
       y = "beta score",
       title = "Topic modeling")

# 
# tweet_top_terms <- tweet_topics %>%
#   group_by(topic) %>%
#   top_n(10, beta) %>%
#   ungroup() %>%
#   arrange(topic, -beta)
# 
# tweet_top_terms %>%
#   mutate(term = reorder_within(term, beta, topic)) %>%
#   ggplot(aes(term, beta, fill = factor(topic))) +
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~ topic, scales = "free") +
#   coord_flip() + 
#   scale_x_reordered() +
#   theme_minimal()

tweets_sentiment.df <- tweets_vaccine.df[,selected_columns]
sentiment_by_sentence <- sentiment(get_sentences(tweets_sentiment.df$text))
sentiment_by_sentence
average_sentiment_by_sentence <- mean(sentiment_by_sentence$sentiment)
standard_deviation_sentiment_by_sentence <- sd(sentiment_by_sentence$sentiment)

tweets_sentiment.df$created_at <-  tweets_sentiment.df$created_at %>% as.POSIXct
tweets_sentiment.df$roundTime <- as.POSIXct(cut(tweets_sentiment.df$created_at, breaks = "30 mins"))
tweets_sentiment_group <- tweets_sentiment.df %>% group_by(roundTime) %>% group_split()
sentiment_by_time <- data.frame('time'=1:length(tweets_sentiment_group),
                                'polarity_score'=rep(0,length(tweets_sentiment_group)))
for (i in 1:length(tweets_sentiment_group)) {
  sentiment_by_time$polarity_score[i] <- 
    sentiment(tweets_sentiment_group[[i]]$text) %>% 
    select("sentiment") %>% unlist %>% mean
}
sentiment_by_time$time <- as.POSIXct(levels(factor(tweets_sentiment.df$roundTime)))
ggplot(sentiment_by_time, aes(x = time, y = polarity_score)) +
  geom_line() +
labs(x = "Time", y = "Polarity score", title = "Sentiment score over time")

tweets_vaccine_with_country.df <- subset(tweets_vaccine.df,!is.na(country),selected_columns)
tweets_vaccine_with_country.df <- subset(tweets_vaccine.df,country!="",selected_columns)
nrow(tweets_vaccine_with_country.df)
unique(tweets_vaccine_with_country.df$country)

library(dplyr)
tweets_vaccine_with_country_count.df <- tweets_vaccine_with_country.df %>% count(country, sort=TRUE)  %>% filter(n>7)
tweets_vaccine_with_country.df <- tweets_vaccine_with_country.df %>% filter(country %in% tweets_vaccine_with_country_count.df$country)

tweets_vaccine_with_country_count.df %>% na.omit() %>% 
mutate(country=reorder(country,n)) %>%ggplot(aes(x=country,y=n))+
geom_bar(stat="identity")+geom_col()+coord_flip() +
labs(x = "Country", y = "Count",
title = "Tweets per country") +
theme_light()

sentiment_country_by_sentence <- sentiment(get_sentences(tweets_vaccine_with_country.df$text))
sentiment_country_by_sentence
tweets_sentiment_country_group <- tweets_vaccine_with_country.df %>% group_by(country) %>% group_split()
sentiment_by_country <- data.frame('country'=unique(tweets_vaccine_with_country.df$country),
                                'polarity_score'=rep(0,length(tweets_sentiment_country_group)))
for (i in 1:length(tweets_sentiment_country_group)) {
  sentiment_by_country$polarity_score[i] <- 
    sentiment(tweets_sentiment_country_group[[i]]$text) %>% 
    select("sentiment") %>% unlist %>% mean
}
ggplot(sentiment_by_country, aes(x = reorder(country, -polarity_score), y = polarity_score)) +
  geom_col() +
  coord_flip() +
  labs(x = "Country", y = "Polarity score", title = "Sentiment score per country")

library(plyr)
word_tweets_df <- ldply (word_tweets, data.frame)
colnames(word_tweets_df)=c("tweet","word")
word_tweets_df <- distinct(word_tweets_df)

users_who_mention_word <- word_tweets_df %>%
  dplyr::count(word) %>%
  filter(n >= 10000) 

word_correlations <- word_tweets_df %>%
  semi_join(users_who_mention_word, by = "word") %>%
  pairwise_cor(item = word, feature = tweet) %>%
  filter(correlation >= 0.96)

graph_from_data_frame(d = word_correlations,
                      vertices = users_who_mention_word %>%
                        semi_join(word_correlations, by = c("word" = "item2"))) %>%
  ggraph(layout = 'fr') +
  geom_edge_link(aes(color = correlation)) +
  scale_edge_colour_gradient(low = "#F0B83C", high = "#BC1024")+
  geom_node_point() +
  geom_node_text(aes(size = n, label = name), repel = TRUE) +
  scale_size_continuous( name = "number of mentions")

tidy_tweets <- tweets_vaccine.df %>%  
  select(status_id, 
         text)%>% 
  unnest_tokens(word, text)
View(tidy_tweets)

my_stop_words <- tibble( 
  word = c(
    "vaccine",
    "vaccin",
    "vaccines"
  ),
  lexicon = "twitter"
)

all_stop_words <- stop_words %>%
  bind_rows(my_stop_words)
View(all_stop_words)

no_numbers <- tidy_tweets %>%
  filter(is.na(as.numeric(word)))

no_stop_words <- no_numbers %>%
  anti_join(all_stop_words, by = "word")

nrc <- get_sentiments("nrc")

View(nrc)

nrc_words <- no_stop_words %>%
  inner_join(nrc, by="word")
View(nrc_words)

pie_words<- nrc_words %>%
  group_by(sentiment) %>%
  tally %>% 
  arrange(desc(n)) 

ggpubr::ggpie(pie_words, "n", label = "sentiment", 
              fill = "sentiment", color = "white", 
              palette = "Spectral")
