#load in data
library(textstem)
library(naivebayes)
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
tweets$stripped_tweets <- lemmatize_strings(tweets$stripped_tweets)

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

words_count <- words %>% count(word,sort = TRUE)

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

test <- Corpus(VectorSource(tweets$stripped_tweets))

test_2 <- DocumentTermMatrix(test)
inspect(test_2)
test_2 <- removeSparseTerms(test_2,.05)
inspect(test_2)

test_3 <- as(as.matrix(test_2),"sparseMatrix")

X_NB = test_3  # feature matrix
y_NB = factor(tweets$cyberbullying_type)

N = length(y_NB)
train_frac = 0.8
train_set = sample.int(N, floor(train_frac*N)) %>% sort
test_set = setdiff(1:N, train_set)

X_train = X_NB[train_set,]
X_test = X_NB[test_set,]

y_train = y_NB[train_set]
y_test = y_NB[test_set]

# train the model: this function is in the naivebayes package.
# Check out "congress109_bayes_detailed" if you want to see a 
# version where we step through these calculations "by hand", i.e.
# not relying on a package to build the classifier.
nb_model = multinomial_naive_bayes(x = X_train, y = y_train)

y_test_pred = predict(nb_model, X_test)

# look at the confusion matrix
table(y_test, y_test_pred)

# overall test-set accuracy
sum(diag(table(y_test, y_test_pred)))/length(y_test)











