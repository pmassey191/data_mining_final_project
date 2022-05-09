#load in data
library(textstem)
library(randomForest)
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

tweets$stripped_tweets <- removeWords(tweets$cleaned_tweet, c(stop_words$word, "yup","youve", "im","ur","isnt","dont","youre","doesnt","lol","lmfao"))
tweets$stripped_tweets <- lemmatize_strings(tweets$stripped_tweets)

ggplot(data = tweets, aes(x=cyberbullying_type,fill = cyberbullying_type))+
  geom_bar()+
  theme_minimal()+
  labs(title = "Number of Tweets by Category",x= "Cyber Bullying Type",y="Count")+
  scale_fill_viridis_d(name = "Cyber Bullying Type")

tweets <- tweets %>% 
  filter(stripped_tweets!="") %>% 
  rownames_to_column("id")


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

pal = viridis(n=20)
wordcloud(words = words_ethnicity$word,freq=words_ethnicity$n)
wordcloud(words = words_ethnicity$word, freq = words_ethnicity$n, min.freq = 1,          
          max.words=200, random.order=FALSE, rot.per=0.35,random.color = TRUE,           
          colors=pal)


wordcloud2(data = words_notcb, size = 1, color = 'random-dark')
wordcloud2(data = words_gender, size = 1, color = 'random-dark')
wordcloud2(data = words_religion, size = 1, color = 'random-dark')
wordcloud2(data = words_age, size = 1, color = 'random-dark')
wordcloud2(data = words_ethnicity, size = 1, color = 'random-dark')
wordcloud2(data = words_othercb, size = 1, color = 'random-dark')

stripped_tweets <- Corpus(VectorSource(tweets$stripped_tweets))

stripped_tweets <- DocumentTermMatrix(stripped_tweets)
inspect(stripped_tweets)
stripped_tweets <- removeSparseTerms(stripped_tweets,sparse = .999)

stripped_tweets <- stripped_tweets %>% as.matrix() %>% as.data.frame() %>% rownames_to_column("id")
stripped_tweets <- stripped_tweets %>%  filter(rowSums(stripped_tweets %>% select(-id))!=0)
stripped_tweets <- stripped_tweets %>% inner_join(tweets %>% select(id,cyberbullying_type),stripped_tweets,by="id")

stripped_tweets_mat <- as.matrix(stripped_tweets %>% select(-id,-cyberbullying_type))

colnames(stripped_tweets) <- make.names(colnames(stripped_tweets))




rownames(stripped_tweets) <- stripped_tweets$id

X_NB = stripped_tweets_mat  # feature matrix
y_NB = factor(stripped_tweets$cyberbullying_type)

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
confusion_matrix_bayes <- table(y_test, y_test_pred)

# overall test-set accuracy
sum(diag(table(y_test, y_test_pred)))/length(y_test)

Z = stripped_tweets %>% select(-id,-cyberbullying_type)/rowSums(stripped_tweets %>% select(-id,-cyberbullying_type))

pc5 = prcomp(Z, scale=TRUE,rank = 5)
tweets <- tweets %>% rownames_to_column("index")
scores <- pc5$x %>% as.data.frame() %>%  rownames_to_column("index")
tweets_combined <- inner_join(tweets, scores)

training = tweets_combined[train_set,]
testing = tweets_combined[test_set,]
training_test = stripped_tweets[train_set,]
testing_test = stripped_tweets[test_set,]

variance <- pc5$sdev

fviz_eig(pc5,ncp = )
fviz_pca_ind(pc5)
get_eig(pc5)

qplot(c(1:25),variance)+
  geom_line()


forest_tweets <- randomForest(as.factor(cyberbullying_type) ~ .-id,data = training_test)



plot(forest_tweets)
varImpPlot(forest_tweets)

qplot(scores[,2], scores[,3], color=as.factor(tweets_combined$cyberbullying_type), xlab='Component 1', ylab='Component 2')
qplot(scores[,4], scores[,5], color=as.factor(tweets_combined$cyberbullying_type), xlab='Component 3', ylab='Component 4')
qplot(scores[,6], scores[,7], color=as.factor(tweets_combined$cyberbullying_type), xlab='Component 5', ylab='Component 6')
qplot(scores[,8], scores[,9], color=as.factor(tweets_combined$cyberbullying_type), xlab='Component 7', ylab='Component 8')
qplot(scores[,10], scores[,11], color=as.factor(tweets_combined$cyberbullying_type), xlab='Component 9', ylab='Component 10')



y_hat_forest <- predict(forest_tweets,testing)

cm_forest <- table(testing$cyberbullying_type, y_hat_forest)


cmatrix <-  confusionMatrix(confusion_matrix_bayes)
cm <- conf_mat(confusion_matrix_bayes)
autoplot(cm,type = "heatmap")+
  scale_fill_gradient(high = "red",low="white")+
  labs(x="Predicted",y="Actual")

cm <- conf_mat(cm_forest)
cmatrix <-  confusionMatrix(test_1)
autoplot(cm,type = "heatmap")+
  scale_fill_gradient(high = "red",low="white")

kbl(cmatrix$byClass,digits = 3) %>% kable_styling(latex_options = "scale_down")

kbl(cmatrix$overall,digits = 3) %>% kable_styling(latex_options = "scale_down")

cmatrix$overall[1]





