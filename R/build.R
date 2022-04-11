library(tidytext)
library(stopwords)
library(tm)
library(wordcloud2)
tweets <- read_csv(here("Data/cyberbullying_tweets.csv"))
data("stop_words")


tweets$tweet_text <- tolower(tweets$tweet_text)
tweets$cleaned_tweet <- gsub("https?://.+", "", tweets$tweet_text)
tweets$cleaned_tweet <- gsub("@\\S*", "", tweets$cleaned_tweet) 
tweets$cleaned_tweet <- gsub("amp", "", tweets$cleaned_tweet) 
tweets$cleaned_tweet <- gsub("[\r\n]", "", tweets$cleaned_tweet)
tweets$cleaned_tweet <- gsub("[[:punct:]]", "", tweets$cleaned_tweet)
tweets$cleaned_tweet <- gsub("[^\x01-\x7F]", "", tweets$cleaned_tweet)
tweets$cleaned_tweet <- gsub("rt", "", tweets$cleaned_tweet)
tweets$cleaned_tweet <- gsub("\n", " ", tweets$cleaned_tweet)
tweets$cleaned_tweet <- gsub("^\\s+", "", tweets$cleaned_tweet)
tweets$cleaned_tweet <- gsub("\\s+$", "", tweets$cleaned_tweet)
tweets$cleaned_tweet <- gsub("[ |\t]+", " ", tweets$cleaned_tweet)


tweets$stripped_tweets <- removeWords(tweets$cleaned_tweet, stop_words$word)

words_r <- tweets %>% 
        unnest_tokens(word,stripped_tweets) %>% 
        anti_join(stop_words)
words_ethnicity <- words %>% filter(cyberbullying_type == "ethnicity") %>% count(word,sort = TRUE)

set.seed(1234)

wordcloud(words = test$word, freq = test$n, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,  
          colors=brewer.pal(8, "Dark2"))


wordcloud2(data = words_ethnicity, size = 1, color = 'random-dark')

wordcloud(words = words_ethnicity$word, freq = words_ethnicity$n, min.freq = 20,
          max.words=200, random.order=FALSE,  
          colors=brewer.pal(8, "Dark2"))

# 
# tweets$test <- text
# 
# 
# tweets$stripped_tweets <- gsub("http.*","",  tweets$tweet_text)
# tweets$stripped_tweets <- gsub("https.*","",tweets$stripped_tweets)
# 
# cleaned_tweets <- tweets %>% 
#   dplyr::select(stripped_tweets) %>% 
#   unnest_tokens(word,stripped_tweets) %>% 
#   anti_join(stop_words)
# 
# test <- cleaned_tweets %>% 
#   group_by(cyberbullying_type,word) %>% 
#   summarise(
#     num_appear = count(word)
#   )











