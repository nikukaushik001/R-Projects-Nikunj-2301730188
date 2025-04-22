# Load libraries
if (!require(rtweet)) install.packages("rtweet")
if (!require(tidytext)) install.packages("tidytext")
if (!require(dplyr)) install.packages("dplyr")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(stringr)) install.packages("stringr")
if (!require(tidyr)) install.packages("tidyr")

library(rtweet)
library(tidytext)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

# Fetch tweets about "Artificial Intelligence"
tweets <- search_tweets("Artificial Intelligence", n = 300, lang = "en", include_rts = FALSE)

# Tokenize and clean tweets
tweet_words <- tweets %>%
  select(status_id, text) %>%
  unnest_tokens(word, text)

data("stop_words")
cleaned_tweets <- tweet_words %>%
  anti_join(stop_words, by = "word")

# Perform sentiment analysis using Bing lexicon
bing_sentiments <- cleaned_tweets %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)

# Visualize sentiment distribution
sentiment_count <- bing_sentiments %>%
  count(sentiment)

ggplot(sentiment_count, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Sentiment Distribution of Tweets", x = "Sentiment", y = "Count")

# Visualize top words by sentiment
top_words <- bing_sentiments %>%
  group_by(sentiment) %>%
  top_n(8, n) %>%
  ungroup() %>%
  arrange(sentiment, -n)

ggplot(top_words, aes(x = reorder(word, n), y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +
  coord_flip() +
  labs(title = "Top Words by Sentiment", x = "Words", y = "Frequency")
