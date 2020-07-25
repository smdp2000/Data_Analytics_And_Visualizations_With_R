library(ggplot2)
library(readr)
library(tm)
library(wordcloud)
library(plyr)
library(lubridate)
library(syuzhet)


#Import the twitter data set
tweetsdata=read.csv('/home/samroadie/Desktop/DA_Lab/LAB5',stringsAsFactors = FALSE)

options(warn=-1)
summary(tweetsdata)


tweetsdata$created_date=as.Date(tweetsdata$created,format='%Y-%m-%d %H:%M:%S')#convert created to date format
tweetsdata$hour = format(as.POSIXct(tweetsdata$created,format="%Y-%m-%d %H:%M:%S"),"%H")#Extract Hour from the date
tweetsdata$isRetweetNum=ifelse(tweetsdata$isRetweet==FALSE,0,1)#Numerical variable to indicate whether a tweet was retweet
tweetsdata$retweetedNum=ifelse(tweetsdata$retweeted==FALSE,0,1)#Total number of times a tweet was tetweeted
tweetsdata$tweet=c(1)#Additional column that will help us in summing up total tweets

#preprocessing

some_txt<-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweetsdata$text)
some_txt<-gsub("http[^[:blank:]]+","",some_txt)
some_txt<-gsub("@\\w+","",some_txt)
some_txt<-gsub("[[:punct:]]"," ",some_txt)
some_txt<-gsub("[^[:alnum:]]"," ",some_txt)


text_corpus <- Corpus(VectorSource(some_txt))
text_corpus <- tm_map(text_corpus, removePunctuation)
text_corpus <- tm_map(text_corpus, content_transformer(tolower))
text_corpus <- tm_map(text_corpus, tm::removeWords, tm::stopwords('english'))
text_corpus <- tm_map(text_corpus, removeWords, c("00bd","will","00a0","amp","00b8","looking","for?"))
corpus <- TermDocumentMatrix(text_corpus)
corpus <- as.matrix(corpus)
corpus <- sort(rowSums(corpus),decreasing=TRUE)

df <- data.frame(word = names(corpus),freq=corpus)

head(df, 20)

#most popular users

y=ddply(tweetsdata, .(screenName), numcolwise(sum))
popularUsers=y[,c("screenName","retweetCount","tweet")]
popularUsers=popularUsers[order(-popularUsers$retweetCount),]
popularUsers=head(popularUsers,n=10)
popularUsers

#Most Replies
Replies=tweetsdata[is.na(tweetsdata$replyToSN)==FALSE,]
y=ddply(Replies, .(replyToSN), numcolwise(sum))
Replies=y[,c("replyToSN","tweet")]
Replies=Replies[order(-Replies$tweet),]
Replies=head(Replies,n=20)
colnames(Replies)=c("User","RepliesReceived")
Replies

#sentiment analysis

mysentiment<-get_nrc_sentiment((some_txt))
####used to classify sentiment scores
Sentimentscores<-data.frame(colSums(mysentiment[,]))
names(Sentimentscores)<-"Score"
SentimentScores<-cbind("sentiment"=rownames(Sentimentscores),Sentimentscores)
rownames(SentimentScores)<-NULL
ggplot(data=SentimentScores,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Total sentiment based on scores")

#plots

library(RColorBrewer)
coul <- brewer.pal(5, "Set2") 
barplot(height=df[1:10,]$freq, names=df[1:10,]$word, col=coul ,horiz=T, las=1,xlab = "Words",ylab ="Frequency")

wordcloud(text_corpus, min.freq = 50,
          max.words=1500, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"), scale = c(3,0.5))  
#
Clustering
corpus = tm::Corpus(tm::VectorSource(some_txt)) 
corpus.cleaned <- tm::tm_map(corpus, function(x) iconv(x, to='UTF-8', sub='byte')) 
corpus.cleaned <- tm::tm_map(corpus.cleaned, tm::removeWords, tm::stopwords('english'))
corpus.cleaned <- tm::tm_map(corpus, tm::stemDocument, language = "english")
corpus.cleaned <- tm::tm_map(corpus.cleaned, tm::stripWhitespace)


#DOING CLUSTER ANALYSIS ON ONLY 9000 CORPUSES
tdm <- tm::DocumentTermMatrix(corpus.cleaned[1:9000]) 
tdm.tfidf <- tm::weightTfIdf(tdm)
tdm.tfidf <- tm::removeSparseTerms(tdm.tfidf, 0.999) 
tfidf.matrix <- as.matrix(tdm.tfidf) 

dist.matrix = proxy::dist(tfidf.matrix, method = "cosine")

clustering.kmeans <- kmeans(tfidf.matrix,2,nstart=100)
clustering.hierarchical <- hclust(dist.matrix, method = "ward.D2") 
clustering.dbscan <- dbscan::hdbscan(dist.matrix, minPts = 10)

master.cluster <- clustering.kmeans$cluster 
slave.hierarchical <- cutree(clustering.hierarchical, k = 4) 
slave.dbscan <- clustering.dbscan$cluster 
stacked.clustering <- rep(NA, length(master.cluster))  
names(stacked.clustering) <- 1:length(master.cluster) 
for (cluster in unique(master.cluster)) { 
  indexes = which(master.cluster == cluster, arr.ind = TRUE) 
  slave1.votes <- table(slave.hierarchical[indexes]) 
  slave1.maxcount <- names(slave1.votes)[which.max(slave1.votes)]   
  slave1.indexes = which(slave.hierarchical == slave1.maxcount, arr.ind = TRUE) 
  slave2.votes <- table(slave.dbscan[indexes]) 
  slave2.maxcount <- names(slave2.votes)[which.max(slave2.votes)]   
  stacked.clustering[indexes] <- slave2.maxcount 
}



points <- cmdscale(dist.matrix, k = 2) 

palette <- colorspace::diverge_hcl(2) # Creating a color palette 
previous.par <- par(mfrow=c(2,2), mar = rep(1.5, 4)) 

plot(points, main = 'K-Means clustering', col = as.factor(master.cluster), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
plot(points, main = 'Hierarchical clustering', col = as.factor(slave.hierarchical), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),  
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
plot(points, main = 'Density-based clustering', col = as.factor(slave.dbscan), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
plot(points, main = 'Stacked clustering', col = as.factor(stacked.clustering), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
par(previous.par) # recovering the original plot space parameters


#HYPOTHESIS TESTING
install.packages("ggpubr")
library(BSDA)

positive_mean = mean(mysentiment$positive)
sigmap = sd(mysentiment$positive)
negative_mean = mean(mysentiment$negative)
sigman = sd(mysentiment$negative)

countp = length(mysentiment$positive)
countn = length(mysentiment$negative)


z_calcs1 = zsum.test(mean.x = positive_mean,sigma.x = sigmap,n.x = countp,mean.y = negative_mean,sigma.y =sigman,n.y = countn )


#Null hypothesis: H0: There is no significant difference between the means

print(z_calcs1)
#henceforth our null hypothesis rejected that there is no significant difference between the means. Hence positive and negative belongs to different sample

#
#
#
#

