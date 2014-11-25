library(RCurl)
library(XML)
library(tm)

#Now get them all
doc<-htmlParse(getURL("http://www.presidentialrhetoric.com/historicspeeches/index.html"))
vec<-xpathSApply(doc, "//a/@href")
vecList<-vec[grep("inaugural",vec)]

presList<-list()
for (vvv in vecList){print(vvv)
                     url<-paste("http://www.presidentialrhetoric.com/historicspeeches/",vvv,sep="")
                     a<-getURL(url)
                     b<-htmlParse(url)
                     x<-readHTMLTable(b)
                     text<-levels(x[[6]]$V1)
                     text<-gsub("\n","",text)
                     text<-gsub("[,.]","",text)
                     presList[[vvv]]<-PlainTextDocument(text)
}
presCorpus <- Corpus(VectorSource(presList))
#presCorpus<-tm_map(presCorpus,tolower)
presCorpus <- tm_map(presCorpus, content_transformer(tolower))
presCorpus<-tm_map(presCorpus,removeWords,stopwords("english"))
presCorpus<-tm_map(presCorpus,removePunctuation)
presCorpus<-tm_map(presCorpus,removeNumbers)
presCorpus<-tm_map(presCorpus,stripWhitespace)
presTDM<-TermDocumentMatrix(presCorpus)
presTDM$dimnames$Docs<-substring(vecList,1,5)
#save(presTDM,file="/Users/gregorymatthews/Dropbox/presTDM.RData")

findFreqTerms(presTDM,300)
findAssocs(presTDM,"government",0.7)

m <- as.matrix(t(presTDM))
rownames(m)<-presTDM$dimnames$Docs
library(proxy)
#What is the "cosine" method here?  Why am I using it?
#http://en.wikipedia.org/wiki/Cosine_similarity
#Wikipedia answer everything.  
d <- dist((m),method="cosine")
hc <- hclust(d, method="average")
plot(hc)


weighted<-weightTfIdf(presTDM)
inspect(weighted[1:10,1:10])
m <- as.matrix(weighted)
rownames(m) <- 1:nrow(m)
cl <- kmeans(t(m), 10)
cl
table(cl$cluster)

#Some other fun stuff
m <- as.matrix(t(presTDM))
rownames(m)<-presTDM$dimnames$Docs
group<-factor(c(rep(1,31),rep(2,24)))
colnames(m)[1]

t.test(m[,1]~group)$p.value

goat<-function(x){
  out<-t.test(x~group)$p.value
  out
}
results<-apply(m,2,goat)
interesting<-data.frame(word=colnames(m),pval=results)
((interesting[order(interesting$pval),])[1:100,])[-c(1,4,10),]
t.test((m[,colnames(m)=="duties"])~group)
t.test((m[,colnames(m)=="world"])~group)
t.test((m[,colnames(m)=="public"])~group)
t.test((m[,colnames(m)=="god"])~group)
t.test((m[,colnames(m)=="poverty"])~group)

m[,colnames(m)=="work"]

temp<-strsplit(presList[[15]]," ")
cbind(temp[[1]][which(temp[[1]]=="duty")+1],
      temp[[1]][which(temp[[1]]=="duty")+2],
      temp[[1]][which(temp[[1]]=="duty")+3])
thing<-cbind(temp[[1]][which(temp[[1]]=="duty")-3],
             temp[[1]][which(temp[[1]]=="duty")-2],
             temp[[1]][which(temp[[1]]=="duty")-1])



#Examples from here: http://michael.hahsler.net/SMU/7337/install/tm.R