#preprocessing
tweets = read.csv("tweets.csv", stringsAsFactors = FALSE)
str(tweets)
tweets$Negative = as.factor(tweets$Avg <= -1)
table(tweets$Negative)
install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)
corpus = Corpus(VectorSource(tweets$Tweet))
corpus
corpus[[1]]
corpus = tm_map(corpus, tolower)
corpus[[1]]
corpus = tm_map(corpus, removePunctuation)
stopwords("english")[1:10]
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus[[1]]
corpus = tm_map(corpus, stemDocument)
corpus[[1]]
#bag of words
frequencies = DocumentTermMatrix(corpus)
frequencies
inspect(frequencies[1000:1005, 505:515])
findFreqTerms(frequencies, lowfreq = 20)
sparse = removeSparseTerms(frequencies, 0.995)
tweetsSparse = as.data.frame(as.matrix(sparse))
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))
tweetsSparse$Negative = tweets$Negative
library(caTools)
set.seed(123)
split = sample.split(tweetsSparse$Negative, SplitRatio=0.7)
trainSparse = subset(tweetsSparse, split==TRUE)
testSparse = subset(tweetsSparse, split==FALSE)
#cart
library(rpart)
library(rpart.plot)
tweetCART = rpart(Negative ~ ., data=trainSparse, method="class")
prp(tweetCART)
predictCART = predict(tweetCART, newdata=testSparse, type="class")
table(testSparse$Negative, predictCART)
#compute accuracy
(294+18)/(294+6+37+18)
table(testSparse$Negative)
300/(300+55)
#random forest
install.packages("randomForest")
library(randomForest)
set.seed(123)
tweetRF = randomForest(Negative ~ ., data=trainSparse)
predictRF = predict(tweetRF, newdata=testSparse)
table(testSparse$Negative, predictRF)
(293+21)/(293+7+34+21)
