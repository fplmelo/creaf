# Conceptual graoh-------------------------------------------------------------------------
## Word cloud-----

library(ggtern)
# Install
# install.packages("tm")  # for text mining
# install.packages("SnowballC") # for text stemming
# install.packages("wordcloud") # word-cloud generator 
# install.packages("RColorBrewer") # color palettes
# install.packages("wordcloud2") # word-cloud generator 

# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("wordcloud2")
library("RColorBrewer")

wc_comp <- read.delim("wc.csv", sep = ",")
text<-wc_comp
text<- as.data.frame(text)
docs <- Corpus(VectorSource(text$Abstract))
docs.t<-Corpus(VectorSource(text$Title))
# inspect(docs)
# class(docs)
##Data Cleaning and Wrangling
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
docs.t <- tm_map(docs.t, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
docs.t <- tm_map(docs.t, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
docs.t <- tm_map(docs.t, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("and","the","our","that", "for","are","also",",more","has","with","can", "of"))
docs.t <- tm_map(docs.t, removeWords, c("and","the","our","that", "for","are","also",",more","has","with","can", "of"))
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
docs.t <- tm_map(docs.t, removePunctuation)
# Eliminate extra white spaces
# docs <- tm_map(docs, stripWhitespace)
# Text stemming
#docs <- tm_map(docs, stemDocument)

# Step 4 : Build a term-document matrix
# Document matrix is a table containing the frequency of the words. Column names are words and row names are documents. The function TermDocumentMatrix() from text mining package can be used as follow :
# FOR ABSTRACTS
dtm.ab <- TermDocumentMatrix(docs)
m.ab <- as.matrix(dtm.ab)
v.ab <- sort(rowSums(m.ab),decreasing=TRUE)
d.ab <- data.frame(word = names(v.ab),freq=v.ab)
head(d.ab, 20)
subset(d.ab, word == "restoration")
d2.ab<-d.ab[-c(1:2),]
head (d2.ab, 20)

# Step 5 : Generate the Word cloud
# The importance of words can be illustrated as a word cloud as follow :

par(mfrow=c(1,1))
dev.off()
set.seed(5)
wordcloud(words = d2.ab$word, freq = d.ab$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

set.seed(5)
wordcloud2(d2.ab,color=brewer.pal(8,"Dark2"))


## FOR TITLES
dtm.t <- TermDocumentMatrix(docs.t)
m.t <- as.matrix(dtm.t)
v.t <- sort(rowSums(m.t),decreasing=TRUE)
d.t <- data.frame(word = names(v.t),freq=v.t)
head(d.t, 20)
subset(d.t, word == "restoration")
d2.t<-d.t[-c(1:4),]
head (d2.t, 20)



par(mfrow=c(1,1))
dev.off()
set.seed(5)
wordcloud(words = d.t$word, freq = d.t$freq, min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

set.seed(5)
wordcloud2(d2.t[c(5:104),],color = colfunc(100))


## FOR TITLES and ABSTRACTS

d.tab=as.data.frame(merge(d.t,d2.ab, by="word"), sort = TRUE)
d.tab<-d.tab[order(-d.tab$freq.y),]
d.tab$sum<-rowSums(d.tab[,c(2,3)])
d.tab<-d.tab[order(-d.tab$sum),]
d.tab<-d.tab[, c(1,4)]
rownames(d.tab)<-d.tab$word

d.tab



library(viridis)
library(ggplot2)
colfunc<-colorRampPalette(c("red","yellow","springgreen","royalblue"))
set.seed(5)
wordcloud2(d.all.nexus,color = colfunc(100))
dev.off()
wordcloud(words = d.tab$word, freq = d.tab$sum, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.25,
          colors=brewer.pal(8, "Dark2"))

## Word Cloud for Restoration Tropical Forest-----

wc_comp <- read.delim("~/Dropbox/Artigos em Escrita/Nexus Caatinga/wc_rest_comp.csv", stringsAsFactors=FALSE)
text<-wc_comp
text<- as.data.frame(text)
docs <- Corpus(VectorSource(text$AB))
docs.t<-Corpus(VectorSource(text$TI))
# inspect(docs)
# class(docs)
##Data Cleaning and Wrangling
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
docs.t <- tm_map(docs.t, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
docs.t <- tm_map(docs.t, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
docs.t <- tm_map(docs.t, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("and","the","our","that", "for","are","also",",more","has","with","can", "of"))
docs.t <- tm_map(docs.t, removeWords, c("and","the","our","that", "for","are","also",",more","has","with","can", "of"))
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
docs.t <- tm_map(docs.t, removePunctuation)
# Eliminate extra white spaces
# docs <- tm_map(docs, stripWhitespace)
# Text stemming
#docs <- tm_map(docs, stemDocument)

# FOR ABSTRACTS
dtm.ab <- TermDocumentMatrix(docs)
m.ab <- as.matrix(dtm.ab)
v.ab <- sort(rowSums(m.ab),decreasing=TRUE)
d.ab <- data.frame(word = names(v.ab),freq=v.ab)
head(d.ab, 20)
subset(d.ab, word == "restoration")
d2.ab<-d.ab[-c(1,3:4,6),]
head (d2.ab, 20)

dtm.t <- TermDocumentMatrix(docs.t)
m.t <- as.matrix(dtm.t)
v.t <- sort(rowSums(m.t),decreasing=TRUE)
d.t <- data.frame(word = names(v.t),freq=v.t)
head(d.t, 20)
subset(d.t, word == "restoration")
d2.t<-d.t[-c(1:3,6),]
head (d2.t, 20)


d.all.rest=as.data.frame(merge(d.t,d.ab, by="word"), sort = TRUE)
d.all.rest<-d.all.rest[order(-d.all.rest$freq.y),]
d.all.rest$sum<-rowSums(d.all.rest[,c(2,3)])
d.all.rest<-d.all.rest[,c(1,4)]
colnames(d.all.rest)<-c("word", "freq")
subset(d.all.rest, word == "restoration")
d.all.rest<-d.all.rest[-c(1,3,4,6),]



par(mfrow=c(1,1))
dev.off()
set.seed(5)
library(viridis)
library(wesanderson)
library(ggsci)
library(ggplot2)
colfunc<-colorRampPalette(c("red","yellow","springgreen","royalblue"))
set.seed(5)
wordcloud2(d.all.rest[c(1:100),],color = colfunc(100))

comp.cloud<-merge(d.all.nexus, d.all.rest, by = "word", all = TRUE)
comp.cloud[is.na(comp.cloud)]<-0
comp.cloud    
colnames(comp.cloud)<-c("word","Nexus","Restoration")
rownames(comp.cloud)<-comp.cloud$word


comparison.cloud(comp.cloud[,c(2,3)], random.order = F, max.words=200, 
                 colors=wes_palette("Darjeeling1", n = 2),
                 title.bg.colors = NULL)
commonality.cloud(comp.cloud[,c(2,3)], random.order = F, max.words=100)

#Usando proporções, em vez de absolutos
comp.cloud2 = mutate(comp.cloud, 
                     nexus_prop = Nexus / sum(Nexus),
                     Rest_prop = Restoration / sum(Restoration))
comp.cloud2<-column_to_rownames(comp.cloud2, 'word')

comparison.cloud(comp.cloud2[,c(3,4)], random.order = F, max.words=200, 
                 colors=wes_palette("Darjeeling1", n = 2),
                 title.bg.colors = NULL) # Fica exatamente igual

## Usando GGPLOT + ggwordcloud
library(ggwordcloud)

comp.cloud %>%
  gather("Nexus", "Restoration", key= type, value = freq) %>% 
  arrange(desc(freq))->comp.cloud3
ggplot(comp.cloud3[1:100,],aes(label = word, size = freq, color = type)) + geom_text_wordcloud_area() +
  scale_size_area(max_size = 12) +
  theme_minimal() +
  facet_wrap(~type) # O número de palavras não é igual para cada pesquisa

comp.cloud %>% 
  select(word, Nexus) %>% 
  arrange(desc(Nexus))->WC.Nexus
comp.cloud %>% 
  select(word, Restoration) %>% 
  arrange(desc(Restoration))->WC.Rest

set.seed(42)
ggplot(WC.Nexus [1:100,], aes(label = word, size = Nexus)) +
  geom_text_wordcloud(rm_outside = F,area_corr = TRUE, color = "#F8766D") +
  scale_size_area(max_size = 20) +
  theme_minimal()-> WC.Nexus.graf

set.seed(300)
ggplot(WC.Rest [1:100,], aes(label = word, size = Restoration)) +
  geom_text_wordcloud(rm_outside = F, area_corr = TRUE, color = "#7CAE00") +
  scale_size_area(max_size = 20) +
  theme_minimal()-> WC.rest.graf

library(cowplot)
plot_grid(WC.Nexus.graf, WC.rest.graf, nrow  = 2, labels = c("Nexus", "Retoration"))


