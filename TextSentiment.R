##This script takes standard SMS output and attaches a sentiment scores to each text message
##The primary sentiment file used in v.1 is the AFINN.txt file found here: http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=6010

require(tm)||{install.packages("tm", dependencies = c("Depends","Suggests"))} 
require(wordcloud)||{install.packages("wordcloud", dependencies = c("Depends","Suggests"))} 
require(tau)||{install.packages("tau", dependencies = c("Depends","Suggests"))} 
require(dplyr)||{install.packages("dplyr", dependencies = c("Depends","Suggests"))} 

AFINN<-read.table("C:/R/AFINN-111.txt", sep="\t", stringsAsFactors=F)
AFINN[1,1]<-as.character("abandon")

aFile <- readLines("C:/R/catsms_20131212.txt")
#aFile <- readLines("C:/R/testIggysms_20131212.txt")
#aFile <- readLines("C:/R/luposms_20131218.txt")

aDf <- t(as.data.frame(strsplit(aFile,"\t")))
names <- c("Date","Time","Type","Num","Name","MSG")
colnames(aDf) <- names

#subset by "in" msgs, if necessary
aDf <- aDf[aDf[,"Type"]=="in",]
cloudFile <- data.frame(MSG=as.character(aDf[,6]))

fin<-data.frame()

for(i in 1:nrow(aDf)){
  options(warn=-1)
  x<-aDf[i,6]
  mCorpus <- Corpus(VectorSource(x))
  mCorpus <- tm_map(mCorpus, tolower)
  mCorpus <- tm_map(mCorpus, removePunctuation)
  mCorpus <- tm_map(mCorpus, removeNumbers)
  mCorpus <- tm_map(mCorpus, removeWords, stopwords("english"))
  if(is.null(DocumentTermMatrix(mCorpus)$dimnames$Terms)){
    Score<-0
  } else{
    mDTM <- DocumentTermMatrix(mCorpus)
    m2 <- t(as.matrix(mDTM))
    m2df <- data.frame(as.vector(dimnames(m2)[1]),m2)
    mer <- merge(m2df,AFINN,by.x="Terms",by.y="V1",all.x=T)
    mer[is.na(mer$V2),"V2"]<-0
    Score<-sum(mer$V2)
  }
fin<-rbind(fin,Score)
}
fin.df<-data.frame(aDf,fin)
names(fin.df)<-c(colnames(aDf),"Score")



