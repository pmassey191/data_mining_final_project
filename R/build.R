#load in data
tweets <- read_csv(here("Data/cyberbullying_tweets.csv"))
data("stop_words")


#categorizing cyberbullying type
tweets$cb_type <- sapply(as.factor(tweets$cyberbullying_type),unclass)

#cleaning tweets
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

tweets$stripped_tweets <- removeWords(tweets$cleaned_tweet, c(stop_words$word,"im","ur","isnt","dont","youre","doesnt","lol","lmfao"))


ggplot(data = tweets, aes(x=cyberbullying_type,fill = cyberbullying_type))+
  geom_bar()+
  theme_minimal()

tweets <- tweets %>% 
  filter(stripped_tweets!="")

ggplot(data = tweets, aes(x=cyberbullying_type,fill = cyberbullying_type))+
  geom_bar()+
  theme_minimal()

#creating word clouds

words <- tweets %>% 
  unnest_tokens(word,stripped_tweets)

words_count <- words %>% count(word,sort = TRUE) %>% filter(n>10)

words_notcb <- words %>% filter(cyberbullying_type == "not_cyberbullying") %>% count(word,sort = TRUE)
words_gender <- words %>% filter(cyberbullying_type == "gender") %>% count(word,sort = TRUE)
words_religion <- words %>% filter(cyberbullying_type == "religion") %>% count(word,sort = TRUE)
words_age <- words %>% filter(cyberbullying_type == "age") %>% count(word,sort = TRUE)
words_ethnicity <- words %>% filter(cyberbullying_type == "ethnicity") %>% count(word,sort = TRUE)
words_othercb <- words %>% filter(cyberbullying_type == "other_cyberbullying") %>% count(word,sort = TRUE)


wordcloud2(data = words_notcb, size = 1, color = 'random-dark')
wordcloud2(data = words_gender, size = 1, color = 'random-dark')
wordcloud2(data = words_religion, size = 1, color = 'random-dark')
wordcloud2(data = words_age, size = 1, color = 'random-dark')
wordcloud2(data = words_ethnicity, size = 1, color = 'random-dark')
wordcloud2(data = words_othercb, size = 1, color = 'random-dark')













