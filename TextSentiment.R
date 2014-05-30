
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
aDf <- t(as.data.frame(strsplit(aFile,"\t")))
colnames(aDf) <- c("Date","Time","Type","Num","Name","MSG")

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
  x<-aDf[i,6]
  AllWordCount <- length(strsplit(x," ")[[1]])
  mCorpus <- Corpus(VectorSource(x))
  mCorpus <- tm_map(mCorpus, tolower)
  mCorpus <- tm_map(mCorpus, removePunctuation)
  mCorpus <- tm_map(mCorpus, removeNumbers)
  mCorpus <- tm_map(mCorpus, removeWords, stopwords("english"))
  if(is.null(DocumentTermMatrix(mCorpus)$dimnames$Terms)){ #flow control for SMS's with no scoreable words
    Score<-0
    } else {
      mDTM <- DocumentTermMatrix(mCorpus) #create dfTf Matrix
      m2 <- t(as.matrix(mDTM))
      m2df <- data.frame(as.vector(dimnames(m2)[1]),m2)
    
      #merge the SMS document term frequency matrix with AFINN file
      mer <- merge(m2df,AFINN,by.x="Terms",by.y="V1",all.x=T)
      mer[is.na(mer$V2),"V2"]<-0 #convert NA's to zeros (neutral)
      Score<-sum(as.numeric(mer$V2)) #sum the score for the SMS
      out<-cbind(AllWordCount,Score)
    }
  fin<-rbind(fin,out)
}

##Create output table: bind the original SMS dataframe with newly generated  
##sentiment scores and word counts  
fin.df<-data.frame(1:nrow(aDf),aDf,fin)
names(fin.df)<-c("Count",colnames(aDf),"AllWordCount","Score")

## use df.plot from here on
df.plot<-fin.df

## convert classes from factors 
df.plot$Date <- as.Date(df.plot$Date)
#df.plot$Time <- Will deal  with time later
df.plot$Type <- as.factor(df.plot$Type)
df.plot$Num <- as.integer(df.plot$Num)
df.plot$Name <- as.factor(df.plot$Name)
df.plot$MSG <- as.character(df.plot$MSG)
df.plot$AllWordCount <-as.numeric(df.plot$AllWordCount)
df.plot$Score <- as.numeric(df.plot$Score) 



#===========PLOTTING===========


###PLOT 1) Standard package, using spline
## range values 
xrange <- range(1:max(table(df.plot$Type)))
yrange <- range(df.plot$Score) 
nnames <- length(levels(df.plot$Type))

## set up plot 
plot(xrange, yrange, type="n", xlab="Sequential Texts",
     ylab="Sentiment Score" ) 
abline(h=0,col=1,lty=2)
colors <- rainbow(3) 
linetype <- 1
plotchar <- seq(18,18+nnames,1)
# add lines 
for (p in 1:nnames) { 
  df.type <- df.plot[df.plot$Type==levels(df.plot$Type)[p],] 
  scorespline<-spline(df.type$Score)
  lines(scorespline$x, scorespline$y, lwd=2.0,
        lty=1, col=colors[p], pch=plotchar[p])
}
# add title 
title("SMS Sentiment")
# add legend 
legend(xrange[1], yrange[2], levels(df.plot$Type), cex=0.8, col=colors,
       pch=plotchar, lty=linetype)



### PLOT 2) Line Chart using ggplot





