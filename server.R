library(e1071)
library(readxl)
library(tm)
library(dplyr)
library(caret)
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(RMariaDB)

storiesDb <- dbConnect(RMariaDB::MariaDB(), user='xxxx', password='xxxx', 
                       dbname='xxxx', host='xxxxx')
dbListTables(storiesDb)
query<-paste("SELECT id,label, tweet, alasan_kata from twitter ORDER by id" ,sep="")
print(query)
rs = dbSendQuery(storiesDb,query)
dbRows<-dbFetch(rs)    
dbRows
df <-data.frame(dbRows)

stopwordID <- "stoplist.txt"
cStopwordID<-readLines(stopwordID)
#########################
set.seed(1234)
df <- df[sample(nrow(df)), ]

glimpse(df)

df$class <- as.factor(df$label)
corpus <- Corpus(VectorSource(df$tweet))

# Use dplyr's  %>% (pipe) utility to do this neatly.
# Use dplyr's  %>% (pipe) utility to do this neatly.
removeURL<-function(x) gsub ("http[^[[:space:]]*","",x)
corpus<-tm_map(corpus,content_transformer(removeURL))

#menghapus karakter selain huruf dan spasi
removeNumFuct<-function(x) gsub("[^[:alpha:][:space:]]*","",x)
corpus<-tm_map(corpus,content_transformer(removeNumFuct))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
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
tdm <-TermDocumentMatrix(corpus.clean)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

###########
fivefreq <- findFreqTerms(dtm, 4)
length((fivefreq))
#############
function(input, output){
  
  output$dataasli <- renderTable( {
    
    
    dfdata <- data.frame(df)
    dfdata
    #inspect(head(sort(income_rules, by='confidence'),10))
    
    
  })
  output$datatest <- renderTable( {
    dfdata <- data.frame(df)
    dimbantu<-as.numeric(dim(dfdata[1]))
    
    # luaran<-as.numeric(input$prosentase)+1
    luaran<-round(dimbantu*input$prosentase)
    bantuan<-luaran[1]
    
    # bantuan
    batasbawah<-dimbantu-bantuan
    df[ batasbawah:dimbantu,]
    #dfdata[dimbantu-10:dimbantu,]
  })
  
  
  output$histoplot <- renderPlot(
    
    
    barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word, col ="brown",
            main ="Most frequent words", ylab = "Word frequencies")
    
  )
  
  output$wordcloudplot <- renderPlot(
    
    wordcloud(words = d$word, freq = d$freq, min.freq = 2,
              max.words=100, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
    
    
  )
  output$matrix <-renderPrint({
    #input$prosentase
    # conf.mat
    
    dfdata <- data.frame(df)
    dimbantu<-as.numeric(dim(dfdata[1]))
    
    # luaran<-as.numeric(input$prosentase)+1
    luaran<-round(dimbantu*input$prosentase)
    bantuan<-luaran[1]+1
    
    # bantuan
    batasbawah<-dimbantu-bantuan
    batasbawahb<-batasbawah+1
    #df[ batasbawah:dimbantu,]
    ###########
    df.train <- df[1:batasbawah,]
    df.test <- df[batasbawahb:dimbantu,]
    
    dtm.train <- dtm[1:batasbawah,]
    dtm.test <- dtm[batasbawahb:dimbantu,]
    
    
    corpus.clean.train <- corpus.clean[1:batasbawah]
    corpus.clean.test <- corpus.clean[batasbawahb:dimbantu]
    
    
    
    # Use only 5 most frequent words (fivefreq) to build the DTM
    dtm.train.nb <- DocumentTermMatrix(corpus.clean.train, control=list(dictionary = fivefreq))
    
    dim(dtm.train.nb)
    dtm.train.nb
    ## [1]  1500 12144
    
    dtm.test.nb <- DocumentTermMatrix(corpus.clean.test, control=list(dictionary = fivefreq))
    
    dim(dtm.train.nb)
    # Function to convert the word frequencies to yes (presence) and no (absence) labels
    convert_count <- function(x) {
      y <- ifelse(x > 0, 1,0)
      y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
      y
    }
    
    dtm.train.nb <- as.data.frame(as.matrix(dtm.train.nb))
    dtm.test.nb <- as.data.frame(as.matrix(dtm.test.nb))
    
    sms_train1 <- cbind(cat=factor(df.train$label), dtm.train.nb)
    sms_test1 <- cbind(cat=factor(df.test$label), dtm.test.nb)
    
    
    sms_train1<-as.data.frame(sms_train1)
    sms_test1<-as.data.frame(sms_test1)
    
    # model specification
    fit1 <- svmfit1 <- svm(cat~., data=sms_train1, kernel ="linear", cost =1, scale =FALSE)
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
  })
  
  
}








