install.packages("e1071")
install.packages("readxl")
install.packages("tm")
install.packages("dplyr")
install.packages("doMC")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("RMariaDB")
library(e1071)
library(readxl)
library(tm)
library(dplyr)
library(caret)
library(doMC)
library(tm)
library(readxl)
library(doMC)
registerDoMC(cores=detectCores())  # Use all available cores
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(RMariaDB)

storiesDb <- dbConnect(RMariaDB::MariaDB(), user='root', password='', dbname='isupapua', host='localhost')
dbListTables(storiesDb)
query<-paste("SELECT label,tweet from twitter " ,sep="")
print(query)
rs = dbSendQuery(storiesDb,query)
dbRows<-dbFetch(rs)    
dbRows
df <-data.frame(dbRows)
stopwordID <- "stoplist.txt"
cStopwordID<-readLines(stopwordID)
#####################


glimpse(df)

set.seed(1234)
df <- df[sample(nrow(df)), ]
df <- df[sample(nrow(df)), ]
glimpse(df)

df$class<-as.factor(df$label)
corpus <- Corpus(VectorSource(df$tweet))
# Inspect the corpus
corpus
inspect(corpus[1:3])

removeURL<-function(x) gsub ("http[^[[:space:]]*","",x)
corpus<-tm_map(corpus,content_transformer(removeURL))
#menghapus karakter selain huruf dan spasi
removeNumFuct<-function(x) gsub("[^[:alpha:][:space:]]*","",x)
corpus<-tm_map(corpus,content_transformer(removeNumFuct))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
removeURL<-function(x) gsub ("http[^[[:space:]]*","",x)
corpus<-tm_map(corpus,content_transformer(removeURL))

#menghapus karakter selain huruf dan spasi
removeNumFuct<-function(x) gsub("[^[:alpha:][:space:]]*","",x)
corpus<-tm_map(corpus,content_transformer(removeNumFuct))
corpus <- tm_map(corpus, removeWords,cStopwordID)
corpus <- tm_map(corpus, removeWords, c("isu", stopwords("english")))
corpus <- tm_map(corpus, removeWords, c("papua", stopwords("english")))
corpus.clean <- tm_map(corpus, stripWhitespace)

#############

dtm <- DocumentTermMatrix(corpus.clean)


# Frequency
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
wf <- data.frame(word=names(freq), freq=freq)
# Plot Histogram
subset(wf, freq>75)    %>%
  ggplot(aes(word, freq)) +
  geom_bar(stat="identity", fill="darkred", colour="darkgreen") +
  theme(axis.tweet.x=element_tweet(angle=45, hjust=1))

##################
tdm <-TermDocumentMatrix(corpus.clean)

m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
wordcloud(words = d$word, freq = d$freq, min.freq = 2,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
#############

#freqplot
findFreqTerms(dtm, lowfreq = 2)
findAssocs(dtm, terms = "freedom", corlimit = 0.3)
head(d, 10)

PLOT
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word, col ="lightblue", main ="Most frequent words", ylab = "Word frequencies")



############

df.train <- df[1:140,]
df.test <- df[141:200,]

dtm.train <- dtm[1:140,]
dtm.test <- dtm[141:200,]
inspect(dtm.train)
inspect(dtm.test)

corpus.clean.train <- corpus.clean[1:140]
corpus.clean.test <- corpus.clean[141:200]

fivefreq <- findFreqTerms(dtm.train, 2)
length((fivefreq))
## [1] 12144

# Use only 5 most frequent words (fivefreq) to build the DTM

dtm.train.nb <- DocumentTermMatrix(corpus.clean.train, control=list(dictionary = fivefreq))

dim(dtm.train.nb)
dtm.train.nb
## [1]  11400 12144

dtm.test.nb <- DocumentTermMatrix(corpus.clean.test, control=list(dictionary = fivefreq))

dim(dtm.train.nb)
## [1]  11400 12144

# Function to convert the word frequencies to yes (presence) and no (absence) labels
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

##################
library(class)
mat.df <- as.data.frame(data.matrix(dtm), stringsAsfactors = FALSE)
mat.df <- cbind(mat.df, df$label, row.names = NULL)
colnames(mat.df)[ncol(mat.df)] <- "type"


train <- sample(nrow(mat.df), ceiling(nrow(mat.df) * .30))
test <- (1:nrow(mat.df))[- train]
train<-mat.df[1:140,]
test <-mat.df[141:200,]

###############

#################
# prep the data

dtm.train.nb <- as.data.frame(as.matrix(dtm.train.nb))
dtm.test.nb <- as.data.frame(as.matrix(dtm.test.nb))

sms_train1 <- cbind(cat=factor(df.train$label), dtm.train.nb)
sms_test1 <- cbind(cat=factor(df.test$label), dtm.test.nb)

# sms_train1[,-1]<-apply(sms_train1[,-1],MARGIN=2,as.numeric)
# sms_test1<-apply(sms_test, MARGIN=2, as.numeric)

sms_train1<-as.data.frame(sms_train1)
sms_test1<-as.data.frame(sms_test1)

# model specification
fit1 <- svm(cat~., data=sms_train1)

# print a summary
fit1

fit1.pred <- predict(fit1, na.omit(sms_test1))
fit1.pred

fit1.perf <- table(na.omit(sms_test1$cat), fit1.pred, dnn=c("Actual", "Predicted"))

fit1.perf

confMatrix1 <- confusionMatrix(fit1.pred, sms_test1$cat)
confMatrix1



conf.mat <- confusionMatrix(fit1.pred, df.test$class)

conf.mat

