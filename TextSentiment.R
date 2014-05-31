
#Set messages to profile. Accepts character strings: "in" "out" or "all"
MSGS<-"all"


##This script takes standard SMS output and attaches a sentiment scores to each text message
##Use the App "SMS To Text", found in the Google Play store to convert SMS to text
##The primary sentiment file used in v.1 is the AFINN.txt file found here: http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=6010

require(tm)||{install.packages("tm", dependencies = c("Depends","Suggests"))} 
require(wordcloud)||{install.packages("wordcloud", dependencies = c("Depends","Suggests"))} 
require(tau)||{install.packages("tau", dependencies = c("Depends","Suggests"))} 
require(dplyr)||{install.packages("dplyr", dependencies = c("Depends","Suggests"))} 

##Reading in AFINN file and processing
AFINN<-read.table("C:/R/AFINN-111.txt", sep="\t", stringsAsFactors=F)
AFINN[1,1]<-as.character("abandon")
AFINN<-rbind(AFINN[AFINN[,2]>=0,],
             cbind(AFINN[AFINN[,2]<0,1],AFINN[AFINN[,2]<0,2]*2)) #doubling effect of negative words

#Reading in the SMS file. Have included a few different options, for testing
aFile <- readLines("C:/R/catsms_20131212.txt")
#aFile <- readLines("C:/R/testIggysms_20131212.txt")
#aFile <- readLines("C:/R/luposms_20131218.txt")

##parse the text file
aDf <- as.data.frame(t(as.data.frame(strsplit(aFile,"\t"))))
names(aDf) <- c("Date","Time","Type","Num","Name","MSG")

##by setting MSGS above, subset by message types "in", "out" or "all"
if(MSGS=="in"){
  aDf <- aDf[aDf[,"Type"]=="in",]
  } else {
    if(MSGS=="out"){
      aDf <- aDf[aDf[,"Type"]=="out",]
    } else {
      if(MSGS=="all"){
        aDf <-aDf
      }
    }
  }

##Scoring each SMS
##convert each SMS to a document-term frequency matrix
fin<-data.frame()
for(i in 1:nrow(aDf)){ #for each SMS
  options(warn=-1)
  x<-as.character(aDf[i,6])
  WordCount <- length(strsplit(x," ")[[1]])
  mCorpus <- Corpus(VectorSource(x))
  mCorpus <- tm_map(mCorpus, tolower)
  mCorpus <- tm_map(mCorpus, removePunctuation)
  mCorpus <- tm_map(mCorpus, removeNumbers)
  mCorpus <- tm_map(mCorpus, removeWords, stopwords("english"))
  if(is.null(DocumentTermMatrix(mCorpus)$dimnames$Terms)){ #flow control for SMS's with no scoreable words
    Score<-0
    out<-cbind(WordCount,Score)
    } else {
      mDTM <- DocumentTermMatrix(mCorpus) #create dfTf Matrix
      m2 <- t(as.matrix(mDTM))
      m2df <- data.frame(as.vector(dimnames(m2)[1]),m2)
    
      #merge the SMS document term frequency matrix with AFINN file
      mer <- merge(m2df,AFINN,by.x="Terms",by.y="V1",all.x=T)
      mer[is.na(mer$V2),"V2"]<-0 #convert NA's to zeros (neutral)
      Score<-sum(as.numeric(mer$V2)) #sum the score for the SMS
      out<-cbind(WordCount,Score)
    }
  fin<-rbind(fin,out)
}

###Create the output table: 
##bind the original SMS dataframe with newly generated sentiment scores
##word counts
##full count
##Cumulative Score for total
##Date-Time concatenation

Count<-1:nrow(aDf)
CumScore<-cumsum(fin$Score)
DateTime<-paste(aDf$Date,aDf$Time)

fin.df<-data.frame(Count,DateTime,aDf,fin,CumScore)
names(fin.df)<-c("Count","DateTime",colnames(aDf),"WordCount","Score","CumScore")

## use df.plot from here on
df.plot<-fin.df

## converting classes from factors 
df.plot$Date <- as.Date(df.plot$Date)
df.plot$Time <- format(as.POSIXct(df.plot$Time,format='%H:%M:%S'),'%H')
df.plot$DateTime <- as.POSIXct(df.plot$DateTime)
df.plot$Type <- as.factor(df.plot$Type)
df.plot$Num <- as.integer(df.plot$Num)
df.plot$Name <- as.factor(df.plot$Name)
df.plot$MSG <- as.character(df.plot$MSG)
df.plot$WordCount <-as.numeric(df.plot$WordCount)
df.plot$Score <- as.numeric(df.plot$Score) 
df.plot$CumScore <- as.numeric(df.plot$CumScore)


#===========PLOTTING===========
require(ggplot2)||{install.packages("ggplot2", dependencies = c("Depends","Suggests"))} 


###PLOT 0) Line Chart Static
xrange <- range(df.plot$Date)
yrange <- range(df.plot$CumScore) 
nnames <- length(levels(df.plot$Type))

plot(df.plot$Date, df.plot$CumScore, type="n", xlab="Date",
     ylab="Sentiment Score" )
abline(h=0,col=1,lty=2)
colors <- heat.colors(3) 
linetype <- 1
plotchar <- seq(18,18+nnames,1)

for (p in 1:nnames) { 
  df.type <- df.plot[df.plot$Type==levels(df.plot$Type)[p],] 
  lines(df.type$Date, cumsum(df.type$Score), lwd=2.0,
        lty=1, col=colors[p], pch=plotchar[p])
}

title("Texting Sentiment",paste0(range(df.plot$Date)[1]," to ",range(df.plot$Date)[2]))

legend(xrange[1], yrange[2], levels(df.plot$Type), cex=0.8, col=colors,
       pch=plotchar, lty=linetype)

###PLOT 0.1) Plot Over Full Date Range
seq(from=range(df.plot$Date)[1],to=range(df.plot$Date)[2],by=0.1)


###PLOT 1) Spline Chart Static

xrange <- range(1:max(table(df.plot$Type)))
yrange <- range(df.plot$Score) 
nnames <- length(levels(df.plot$Type))

plot(xrange, yrange, type="n", xlab="Sequential Texts",
     ylab="Sentiment Score" ) 
abline(h=0,col=1,lty=2)
colors <- rainbow(3) 
linetype <- 1
plotchar <- seq(18,18+nnames,1)

for (p in 1:nnames) { 
  df.type <- df.plot[df.plot$Type==levels(df.plot$Type)[p],] 
  scorespline<-spline(df.type$Score)
  lines(scorespline$x, scorespline$y, lwd=2.0,
        lty=1, col=colors[p], pch=plotchar[p])
}

title("Texting Sentiment",paste0(range(df.plot$Date)[1]," to ",range(df.plot$Date)[2]))

legend(xrange[1], yrange[2], levels(df.plot$Type), cex=0.8, col=colors,
       pch=plotchar, lty=linetype)


###PLOT 2) Spline Chart Cumulative

xrange <- range(1:max(table(df.plot$Type)))
yrange <- range(df.plot$CumScore) 
nnames <- length(levels(df.plot$Type))

plot(xrange, yrange, type="n", xlab="",
     ylab="Sentiment" ) 
abline(h=0,col=1,lty=2)
colors <- heat.colors(3:4) 
linetype <- 1
plotchar <- seq(18,18+nnames,1)

for (p in 1:nnames) { 
  df.type <- df.plot[df.plot$Type==levels(df.plot$Type)[p],]
  scorespline<-spline(cumsum(df.type$Score))
  lines(scorespline$x, scorespline$y, lwd=2.0,
        lty=1, col=colors[p], pch=plotchar[p])
}

title("Cumulative Sentiment",paste0(range(df.plot$Date)[1]," to ",range(df.plot$Date)[2]))

legend(xrange[1], yrange[1]+30, levels(df.plot$Type), cex=0.8, col=colors,
       pch=plotchar, lty=linetype)


### PLOT 3) ## Scatter: Score by Date
qplot(Date,Score, data=df.plot, colour=Type)

### Plot 4) Bubble: Time of day vs. Sentiment vs. WordCount
qplot(Time, Score, data=df.plot, size=WordCount, colour=Type,xlab="Time of Day", ylab="Sentiment Score")

### Plot 5) 
