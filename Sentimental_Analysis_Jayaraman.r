install.packages('syuzhet')
install.packages('ggplot2')
install.packages('RTextTools')
library('syuzhet')
library('ggplot2')
library('RTextTools')
sentimemttext<-read.csv("C:/Users/Jayaraman/Desktop/Master of Business Analytics-Australia/Semester 3/Customer Analytics (CA)/Assignment-1/Sentiment_test.csv")
sentimemttext$Content <-  as.character(sentimemttext$Content)
sentimemttext$ContentSentiment1=get_sentiment(sentimemttext$Content,method = 'syuzhet')
sentimemttext$ContentSentiment2=get_sentiment(sentimemttext$Content,method = 'afinn')
sentimemttext$ContentSentiment3=get_sentiment(sentimemttext$Content,method = 'bing')
sentimemttext$ContentSentiment4=get_sentiment(sentimemttext$Content,method = 'nrc')
sentimemttext$Sentiment1Prediction1 <- ifelse(sentimemttext$ContentSentiment1 > 0,"Pos","Neg")
sentimemttext$Sentiment1Prediction2 <- ifelse(sentimemttext$ContentSentiment2 > 0,"Pos","Neg")
sentimemttext$Sentiment1Prediction3 <- ifelse(sentimemttext$ContentSentiment3 > 0,"Pos","Neg")
sentimemttext$Sentiment1Prediction4 <- ifelse(sentimemttext$ContentSentiment4 > 0,"Pos","Neg")
RecallAccuracy1<-recall_accuracy(sentimemttext$ï..Sent,sentimemttext$Sentiment1Prediction1)
RecallAccuracy2<-recall_accuracy(sentimemttext$ï..Sent,sentimemttext$Sentiment1Prediction2)
RecallAccuracy3<-recall_accuracy(sentimemttext$ï..Sent,sentimemttext$Sentiment1Prediction3)
RecallAccuracy4<-recall_accuracy(sentimemttext$ï..Sent,sentimemttext$Sentiment1Prediction4)
print("Accuracy using Syuzhet")
RecallAccuracy1
print("Accuracy using Afinn")
RecallAccuracy2
print("Accuracy using Bing")
RecallAccuracy3
print("Accuracy using Nrc")
RecallAccuracy4
par(mfrow=c(2,2))
plot(
  sentimemttext$ContentSentiment1,
  type = "l",
  main = "Change of sentiments",
  xlab = "Narrative Time",
  ylab = "Sentiment",
  col="blue")
plot(
  sentimemttext$ContentSentiment2,
  type = "l",
  main = "Change of sentiments",
  xlab = "Narrative Time",
  ylab = "Sentiment",
  col="red")
plot(
  sentimemttext$ContentSentiment3,
  type = "l",
  main = "Change of sentiments",
  xlab = "Narrative Time",
  ylab = "Sentiment",
  col="red")
plot(
  sentimemttext$ContentSentiment4,
  type = "l",
  main = "Change of sentiments",
  xlab = "Narrative Time",
  ylab = "Sentiment",
  col="blue")

nrcData = get_nrc_sentiment(sentimemttext$Content)
sentiment <- get_sentiment(sentimemttext$Content)
angrySentences = which(nrcData$anger > 0)
sentimemttext$Content[angrySentences]
td = data.frame(t(nrcData))
tdSum = data.frame(rowSums(td[1:78]))
names(tdSum)[1] = "Count"
tdSum = cbind("sentiment" = rownames(tdSum), tdSum)
rownames(tdSum) = NULL
tdSum = tdSum[1:8,]
qplot(sentiment, data=tdSum, weight=Count, geom="bar",fill=sentiment) + 
  ggtitle("Tweets Emotions")


