# loading dataset into R
ham_or_spam <- read.csv("~/datasets/text_msgs.csv")
head(ham_or_spam,5)

str(ham_or_spam)
#finding proportion of Spam $ ham msgs
prop.table(table(ham_or_spam$type)) *100

# data cleaning and standardizing text data

require(tm)
require(SnowballC)

sms_corpus <- VCorpus(VectorSource(ham_or_spam$text))
print(sms_corpus)

sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords,stopwords())
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)

# Spliting text Document

sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
sms_dtm

# creating a word cloud

spam = which(ham_or_spam$type == "spam")
ham  = which(ham_or_spam$type == "ham")
require(wordcloud)

wordcloud(sms_corpus_clean[spam], max.words = 40 )
wordcloud(sms_corpus_clean[ham], max.words = 40)

# creating train & test sets

sms_dtm_train <- sms_dtm[1:4169,]
sms_dtm_test <- sms_dtm[4170:5572,]


sms_train_labels <- ham_or_spam[1:4169,]$type
sms_test_labels <- ham_or_spam[4170:5572,]$type

# creating frequent words
sms_freq_words <-findFreqTerms(sms_dtm_train,5)
str(sms_freq_words)

sms_dtm_freq_train <-sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[, sms_freq_words]


con_counts <- function(x){
  x <- ifelse(x > 0, "Yes","No")
} 

sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, con_counts)
s

# modeling

require(e1071)
sms_classfier <- naiveBayes(sms_train, sms_train_labels)
summary(sms_classfier)
sms_test_predict <- predict(sms_classfier, sms_test)
# Evaluating model

library(gmodels)

CrossTable(sms_test_predict,sms_test_labels, prop.chisq = FALSE,prop.t = FALSE,
           dnn = c('predict','actual'))


# improving Model 
#sms_classfier2 <- naiveBayes(sms_train, sms_train_labels, laplace = 1)
#sms_test_predict2 <- predict(sms_classfier2, sms_test)
#CrossTable(sms_test_predict2,sms_test_labels, prop.chisq = FALSE,prop.t = FALSE,
 #          dnn = c('predict','actual'))
