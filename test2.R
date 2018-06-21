install.packages("tidyquant")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("tm")
install.packages("igraph")
install.packages("ggraph")
install.packages("topicmodels")

library(rtweet)
library(tidyverse)
library(tidyquant)
library(ggplot2)
library(SnowballC)

library(wordcloud)
library(tm)

library(igraph)
library(ggraph)

## web browser method: create token and save it as an environment variable
create_token(
  app = "sociolegaltech_R_analysis",
  consumer_key = "rpRXPSSHDwBejBMy2SDI8ikLJ",
  consumer_secret = "4VnZ6KorOEGi7mKc8UupIYFe8UVrkOgKt15OlidHzcJ9VTjyXK")

## Get Tweets of A User

my_name <- get_timeline("sociolegaltech", includeRts=TRUE)

my_mentions <- get_mentions("sociolegaltech")

my_retweets <- get_retweeters(962470541058703360)

my_friends <- get_friends("sociolegaltech")
friends_data <- lookup_users(my_friends$user_id)

df <- users_data(sociolegaltech)

usr_df <- lookup_users("sociolegaltech")


rt <- search_tweets(
  "#LSA2018", include_rts = FALSE)

rt2 <- search_tweets(
  "#LSACA2018", include_rts = FALSE)

friends_data %>%
  count(lang) %>%
  droplevels() %>%
  ggplot(aes(x = reorder(lang, desc(n)), y = n)) +
    geom_bar(stat = "identity", color = palette_light()[1], fill = palette_light()[1], alpha = 0.8) +
    theme_tq() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    labs(x = "language ISO 639-1 code",
         y = "number of followers")

friends_data %>%
  ggplot(aes(x = log2(followers_count))) +
    geom_density(color = palette_light()[1], fill = palette_light()[1], alpha = 0.8) +
    theme_tq() +
    labs(x = "log2 of number of followers",
         y = "density")

tidy_descr <- friends_data %>%
  unnest_tokens(word, description) %>%
  mutate(word_stem = wordStem(word)) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!grepl("\\.|http", word))

tidy_descr %>%
  count(word_stem, sort = TRUE) %>%
  filter(n > 20) %>%
  ggplot(aes(x = reorder(word_stem, n), y = n)) +
    geom_col(color = palette_light()[1], fill = palette_light()[1], alpha = 0.8) +
    coord_flip() +
    theme_tq() +
    labs(x = "",
         y = "count of word stem in all followers' descriptions")

tidy_descr %>%
  count(word_stem) %>%
  mutate(word_stem = removeNumbers(word_stem)) %>%
  with(wordcloud(word_stem, n, max.words = 100, colors = palette_light()))

friends_data_NA <- friends_data %>%
  filter(description != "")

tidy_descr_ngrams <- friends_data %>%
  unnest_tokens(bigram, description, token = "ngrams", n = 2, collapse = FALSE) %>%
  filter(!grepl("\\.|http", bigram)) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- tidy_descr_ngrams %>%
  count(word1, word2, sort = TRUE)

bigram_graph <- bigram_counts %>%
  filter(n > 5) %>%
  graph_from_data_frame()

set.seed(1)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color =  palette_light()[1], size = 5, alpha = 0.8) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 0.5) +
  theme_void()

bigrams_separated <- friends_data %>%
  unnest_tokens(bigram, description, token = "ngrams", n = 2, collapse = FALSE) %>%
  filter(!grepl("\\.|http", bigram)) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(word1 == "not" | word1 == "no") %>%
  filter(!word2 %in% stop_words$word)

tidy_descr_sentiment <- tidy_descr %>%
  left_join(select(bigrams_separated, word1, word2), by = c("word" = "word2")) %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  rename(nrc = sentiment.x, bing = sentiment.y) %>%
  mutate(nrc = ifelse(!is.na(word1), NA, nrc),
         bing = ifelse(!is.na(word1) & bing == "positive", "negative",
                       ifelse(!is.na(word1) & bing == "negative", "positive", bing)))

not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()

not_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
    geom_col(show.legend = FALSE) +
    scale_fill_manual(values = palette_light()) +
    labs(x = "",
         y = "Sentiment score * number of occurrences",
         title = "Words preceded by \"not\"") +
    coord_flip() +
    theme_tq()

tidy_descr_sentiment <- tidy_descr %>%
  left_join(select(bigrams_separated, word1, word2), by = c("word" = "word2")) %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  rename(nrc = sentiment.x, bing = sentiment.y) %>%
  mutate(nrc = ifelse(!is.na(word1), NA, nrc),
         bing = ifelse(!is.na(word1) & bing == "positive", "negative",
                       ifelse(!is.na(word1) & bing == "negative", "positive", bing)))

tidy_descr_sentiment %>%
  filter(nrc != "positive") %>%
  filter(nrc != "negative") %>%
  gather(x, y, nrc, bing) %>%
  count(x, y, sort = TRUE) %>%
  filter(n > 10) %>%
  ggplot(aes(x = reorder(y, n), y = n)) +
    facet_wrap(~ x, scales = "free") +
    geom_col(color = palette_light()[1], fill = palette_light()[1], alpha = 0.8) +
    coord_flip() +
    theme_tq() +
    labs(x = "",
         y = "count of sentiment in followers' descriptions")


tidy_descr_sentiment %>%
  count(screen_name, word, bing) %>%
  group_by(screen_name, bing) %>%
  summarise(sum = sum(n)) %>%
  spread(bing, sum, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  ggplot(aes(x = sentiment)) +
    geom_density(color = palette_light()[1], fill = palette_light()[1], alpha = 0.8) +
    theme_tq()

library(reshape2)
tidy_descr_sentiment %>%
  count(word, bing, sort = TRUE) %>%
  acast(word ~ bing, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = palette_light()[1:2],
                   max.words = 100)

library(topicmodels)

dtm_words_count <- tidy_descr %>%
  mutate(word_stem = removeNumbers(word_stem)) %>%
  count(screen_name, word_stem, sort = TRUE) %>%
  ungroup() %>%
  filter(word_stem != "") %>%
  cast_dtm(screen_name, word_stem, n)

# set a seed so that the output of the model is predictable
dtm_lda <- LDA(dtm_words_count, k = 5, control = list(seed = 1234))

topics_beta <- tidy(dtm_lda, matrix = "beta")

p1 <- topics_beta %>%
  filter(grepl("[a-z]+", term)) %>% # some words are Chinese, etc. I don't want these because ggplot doesn't plot them correctly
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, color = factor(topic), fill = factor(topic))) +
    geom_col(show.legend = FALSE, alpha = 0.8) +
    scale_color_manual(values = palette_light()) +
    scale_fill_manual(values = palette_light()) +
    facet_wrap(~ topic, ncol = 5) +
    coord_flip() +
    theme_tq() +
    labs(x = "",
         y = "beta (~ occurrence in topics 1-5)",
         title = "The top 10 most characteristic words describe topic categories.")

user_topic <- tidy(dtm_lda, matrix = "gamma") %>%
  arrange(desc(gamma)) %>%
  group_by(document) %>%
  top_n(1, gamma)

p2 <- user_topic %>%
  group_by(topic) %>%
  top_n(10, gamma) %>%
  ggplot(aes(x = reorder(document, -gamma), y = gamma, color = factor(topic))) +
    facet_wrap(~ topic, scales = "free", ncol = 5) +
    geom_point(show.legend = FALSE, size = 4, alpha = 0.8) +
    scale_color_manual(values = palette_light()) +
    scale_fill_manual(values = palette_light()) +
    theme_tq() +
    coord_flip() +
    labs(x = "",
         y = "gamma\n(~ affiliation with topics 1-5)")

library(grid)
library(gridExtra)
grid.arrange(p1, p2, ncol = 1, heights = c(0.7, 0.3))


