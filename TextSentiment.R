##This script takes standard SMS output and attaches a sentiment scores to each text message
##Use the App "SMS To Text", found in the Google Play store to convert SMS to text
##The primary sentiment file used in v.1 is the AFINN.txt file found here: http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=6010

require(tm)||{install.packages("tm", dependencies = c("Depends","Suggests"))} 
require(wordcloud)||{install.packages("wordcloud", dependencies = c("Depends","Suggests"))} 
require(tau)||{install.packages("tau", dependencies = c("Depends","Suggests"))} 
require(dplyr)||{install.packages("dplyr", dependencies = c("Depends","Suggests"))} 

#Reading in AFINN file
AFINN<-read.table("C:/R/AFINN-111.txt", sep="\t", stringsAsFactors=F)
AFINN[1,1]<-as.character("abandon")

#Processing the AFINN file to double effects of negative words. 
#Can add controls to increase positive/negative effect
AFINN<-rbind(AFINN[AFINN[,2]>=0,],
             cbind(AFINN[AFINN[,2]<0,1],AFINN[AFINN[,2]<0,2]*2))

#Reading in the SMS file. Have included a few different options, for testing
aFile <- readLines("C:/R/catsms_20131212.txt")
#aFile <- readLines("C:/R/testIggysms_20131212.txt")
#aFile <- readLines("C:/R/luposms_20131218.txt")

#parse the text file according to SMS format into a dataframe
aDf <- t(as.data.frame(strsplit(aFile,"\t")))
colnames(aDf) <- c("Date","Time","Type","Num","Name","MSG")

#subset by "in" msgs, if necessary
aDf <- aDf[aDf[,"Type"]=="in",]
cloudFile <- data.frame(MSG=as.character(aDf[,6]))

#convert each SMS to a document-term frequency matrix
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
      mDTM <- DocumentTermMatrix(mCorpus) #create dTf Matrix
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

#Create output table: original SMS dataframe with sentiment scores for each SMS  
fin.df<-data.frame(aDf,fin)
names(fin.df)<-c(colnames(aDf),"AllWordCount","Score")

#Create date sequence using min and max dates
mn<-min(as.Date(fin.df$Date))
mx<-max(as.Date(fin.df$Date))
date.seq<-data.frame("Date"=seq.Date(from=min(as.Date(fin.df$Date)),to=max(as.Date(fin.df$Date)),by=1))
date.df<-merge(date.seq,fin.df,by.x="Date",by.y="Date",all.y=T)

df.plot<-date.df

# Create Line Chart

#### convert factor to numeric for convenience 
df.plot$Date <- as.Date(df.plot$Date)
#df.plot$Time <- Will fix and play with time later
df.plot$Type <- as.factor(df.plot$Type)
df.plot$Num <- as.integer(df.plot$Num)
df.plot$Name <- as.factor(df.plot$Name)
df.plot$MSG <- as.character(df.plot$MSG)
df.plot$AllWordCount
df.plot$Score <- as.numeric(df.plot$Score) 

nscores <- max(df.plot$Score)

# get the range for the x and y axis 
xrange <- range(df.plot$Date) 
yrange <- range(df.plot$circumference) 

# set up the plot 
plot(xrange, yrange, type="n", xlab="Age (days)",
     ylab="Circumference (mm)" ) 
colors <- rainbow(nscores) 
linetype <- c(1:nscores) 
plotchar <- seq(18,18+nscores,1)

# add lines 
for (i in 1:nscores) { 
  tree <- subset(df.plot, Tree==i) 
  lines(tree$age, tree$circumference, type="b", lwd=1.5,
        lty=linetype[i], col=colors[i], pch=plotchar[i]) 
} 

# add a title and subtitle 
title("Tree Growth", "example of line plot")

# add a legend 
legend(xrange[1], yrange[2], 1:nscores, cex=0.8, col=colors,
       pch=plotchar, lty=linetype, title="Tree")


