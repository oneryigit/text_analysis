
library(tidyverse)
library(stm)
library(stmCorrViz)
library(patchwork)

setwd("~/Desktop/htb")
data=read.csv('stm_hutbe_01.csv')


myvars <- c("content0","content", "content2","date","operation", "election", "surec", "tokened","lematized", "lematized_v2")
data <- data[myvars]

data$date= as.POSIXct( data$date, format="%d.%m.%Y" )

print(class(data$date))

sw=read.csv("https://raw.githubusercontent.com/InJuxSanct/turkish-stopwords/master/src/lib/stopwords/raw-stopwords.txt", sep=' ', header = FALSE)
#sw=stopwords::stopwords("tr", source = "stopwords-iso")


V1=c("allah",'kardeşlerim',"müslüman","müslümanlar", "efendimiz","okudugum", "okuduğum","mümin", "müminler", "in", "ın","peygamberimiz", 
     "peygamber", "aziz","rabbimiz", "rabbimize","rabbimizin","dua", "muhterem", "mübarek","yüce","cuma", "an", "ic", "namaz","ayet", "kerim","biz", 
     "bizlere", "bugünkü","kıymetli","türlü", "bugün","kur", "kuran", "res", "lullah","islam", 'kurban',
     "lerim", "kardes","yle", "slu","zel", "din","nku", "bugu", "erifte", "ini","etmek", "ını","i̇slam", "yi", "yu","aynı","hz", 
     "nun", "lü", "go", "gu", "eb", "iman", "gün","rlu", "muhammed", "insan")
sw2 <- data.frame(V1)

sw_all <- rbind(sw, sw2)


a=sw_all[['V1']]

shortdoc=substring(data$content0, first=200L,last = 1200L)


processed =textProcessor(
  data$content2,
  metadata = data,
  lowercase = TRUE,
  removestopwords = FALSE,
  removenumbers = TRUE,
  removepunctuation = TRUE,
  ucp = FALSE,
  stem = FALSE,
  wordLengths = c(3, Inf),
  sparselevel = 1,
  language = "tur",
  verbose = TRUE,
  onlycharacter = FALSE,
  striphtml = FALSE,
  customstopwords = a,
  custompunctuation = NULL,
  v1 = FALSE
)


out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 5)

docs <- out$documents
vocab <- out$vocab
meta <-out$meta


#Estimate 
hutbe <- stm(documents = out$documents, vocab = out$vocab, K = 20, max.em.its = 75, data = out$meta, init.type = "Spectral", seed=18)

#evaluate
labelTopics(hutbe)

# Display Expected Topic Proportions
topicNames<-c("Topic 1: Family and Ties ","Topic 2: Brotherhood, Solidarity and Sympaty","Topic 3: Charity and Kindness, ","Topic 4: Holy Events and Nights","Topic 5: Lifespan, Time","Topic 6: Moral Values I (Honesty)","Topic 7: Eid Celebration","Topic 8: Moral Values I (Neigborhood, Kindness)","Topic 9: Life and Afterlife","Topic 10: Moral Values III (Halal and Haram)","Topic 11 The Battle of Karbala: ","Topic 12: Feelingness","Topic 13: Islam (General)","Topic 14: Children and Education ","Topic 15: Nation-State Discourse I","Topic 16: Forgiveness (Heart)","Topic 17: Patience ","Topic 18: Mosques and Education","Topic 19: Nation-State Discourse II","Topic 20: Honour and Decency")

topics=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18, 19,20)



#################
pdf("top_topic.pdf", width=6, height=8)
par(bty="n",col="black",lwd=3)
plot.STM(hutbe,type="summary",ylim=c(1,20))
dev.off()

################
par(bty="n",col="black",lwd=2)
pdf("topics_detailed1.pdf", width=8, height=10)
plot(hutbe, type = "labels", topics=c(1,2,3,4,5,6,7,8,9,10))
dev.off()
######
par(bty="n",col="black",lwd=2)
pdf("topics_detailed2.pdf", width=8, height=10)
plot(hutbe, type = "labels", topics=c(11,12,13,14,15,16, 17,18, 19,20))
dev.off()



par(bty="n",col="black",lwd=1)
pdf("doc_topic_prop.pdf", width=18, height=10)
plot(hutbe, type="hist",topics=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16, 17,18, 19,20))
dev.off()

######
par(bty="n",col="black",lwd=1)
pdf("comperison.pdf", width=8, height=6)
plot(hutbe, type="perspectives", topics=c(9,19),ylim=c(-1,0), xlim = c(-1,5))
dev.off()

#####
par(bty="n",col="black",lwd=1)
pdf("comperison2.pdf", width=7, height=5)
plot(hutbe, type="perspectives", topics=c(2,19),ylim=c(-1,0), xlim = c(-1,5))
dev.off()

#evaluate
hutbe2 <- selectModel(out$documents, out$vocab, K=20,max.em.its=75, data=meta, runs=20, seed=19)
plotModels(hutbe2)
topicQuality(model=hutbe, documents=docs)

#evaluate
labelTopics(hutbe)


#find NUMBER OF TOPIC 
#findingk <- searchK(out$documents, out$vocab, K = c(10:30), data = meta, verbose=FALSE)
#plot(findingk)
#####
par(bty="n",col="black",lwd=1)
pdf("thought1.pdf", width=9, height=9)
thoughts19 <- findThoughts(hutbe, texts=shortdoc, n=2, topics = 19)$docs[[1]]
plotQuote(thoughts19, width = 70, main = "Topic 19")
dev.off()

thoughts19

######################################

out$meta$operation <- as.factor(out$meta$operation)
out$meta$election <- as.factor(out$meta$election)
out$meta$surec <- as.factor(out$meta$surec)

## operation
#Estimate 
hutbe <- stm(documents = out$documents, vocab = out$vocab, K = 20,  prevalence=~operation, max.em.its = 75, data = out$meta, init.type = "Spectral", seed=18)

prep <- estimateEffect(1:20 ~ operation, hutbe, meta = out$meta, uncertainty = "Global")

summary(prep, topics=19)

pdf("regression.pdf", width=8, height=9)
par(bty="n",col="black",lwd=2)
plot(prep, covariate = "operation", topics = topics,model = hutbe, method = "difference",cov.value1 = "1", cov.value2 = "0",xlab = "No Military Operation ... Military Operation",main = "Effect of Military Operation", xlim = c(-.3, .3),labeltype = "custom",custom.labels = topicNames)
dev.off()


############### surec
#Estimate 
hutbe <- stm(documents = out$documents, vocab = out$vocab, K = 20, prevalence= ~surec, max.em.its = 75, data = out$meta, init.type = "Spectral", seed=18)

prep <- estimateEffect(1:20 ~ surec, hutbe, meta = out$meta, uncertainty = "Global")

summary(prep, topics=2)

pdf("regression2.pdf", width=8, height=9)
par(bty="n",col="black",lwd=2)
plot(prep, covariate = "surec", topics = topics,model = hutbe, method = "difference",cov.value1 = "1", cov.value2 = "0",xlab = "No Peace Process ... Peace Process",main = "Effect of Peace Process", xlim = c(-.3, .3),labeltype = "custom",custom.labels = topicNames)
dev.off()


#################election
hutbe <- stm(documents = out$documents, vocab = out$vocab, K = 20, prevalence= ~election, max.em.its = 75, data = out$meta, init.type = "Spectral", seed=18)

prep <- estimateEffect(1:20 ~ election, hutbe, meta = out$meta, uncertainty = "Global")

pdf("regression3.pdf", width=8, height=9)
par(bty="n",col="black",lwd=2)
plot(prep, covariate = "election", topics = topics,model = hutbe, method = "difference",cov.value1 = "1", cov.value2 = "0",xlab = "No Election ... Election",main = "Effect of Election", xlim = c(-.3, .3),labeltype = "custom",custom.labels = topicNames)
dev.off()



library('igraph')
### topic correlations
pdf("corr.pdf", width=7, height=7)
par(bty="n",col="black",lwd=5)
mod.out.corr <- topicCorr(hutbe)
plot(mod.out.corr,edge.arrow.size=.9, vertex.color="gray70", vertex.size=12, vertex.frame.color="gray", vertex.label.color="black", vertex.label.cex=1, vertex.label.dist=2, edge.curved=0.5, main= "Positive Corelations between Topics",edge.width=2, edge.color="black", edge.lty=1)
dev.off()



#cloud
library(wordcloud)
library(wordcloud2)

library(RColorBrewer)

#milliyetci
cloud(hutbe, topic=19,min.freq = 2,  max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

#kardes

cloud(hutbe, topic=2,min.freq = 2,  max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))



##### time series
hutbe <- stm(documents = out$documents, vocab = out$vocab, K = 20, prevalence= ~ s(date), max.em.its = 75, data = out$meta, init.type = "Spectral", seed=18)

date <- as.numeric(out$meta$date)
out$meta$date <- as.numeric(out$meta$date)

op_date=data[data$operation==1,]$date
pp_date=data[data$surec==1,]$date

prep <- estimateEffect(1:20 ~ s(date), hutbe, meta = out$meta, uncertainty = "Global")

plot(prep, 'date', method = "continuous", topics =19, model = hutbe, printlegend = FALSE, xaxt = "n")
# Add dates to x-axis
axis(1, data$date, format(data$date, "%Y"))
abline(v=as.numeric(op_date), lwd=.5, col='blue',lty = "dashed")

#########
hutbe <- stm(documents = out$documents, vocab = out$vocab, K = 20, prevalence= ~s(date), max.em.its = 75, data = out$meta, init.type = "Spectral", seed=18)

out$meta$date <- as.numeric(out$meta$date)


pp_date=data[data$surec==1,]$date

prep <- estimateEffect(1:20 ~ s(date), hutbe, meta = out$meta, uncertainty = "Global")

plot(prep, "date", method = "continuous", topics =2, model = hutbe, printlegend = FALSE, xaxt = "n")
# Add dates to x-axis
axis(1, data$date, format(data$date, "%Y"))
abline(v=as.numeric(pp_date), lwd=.5, col='blue',lty = "dashed")


#######

