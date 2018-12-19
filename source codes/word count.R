library(ggplot2)
library(clusterSim)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(tm)
library(wordcloud)
library(slam)
library(sentimentr)
library(xlsx)

# setting up the directory
setwd("C:/Users/ThunderDom/Documents/R/R code/analytics project/")

data=read.csv("word_count.csv")
postorpus=data$Var1
postCorpus=Corpus(VectorSource(data$Var1))

#applyong the tolower function to corpus
postCorpus=tm_map(postCorpus,tolower)

writeLines(as.character(postCorpus[[14]]))

#applying the remove punctation function to corpus
postCorpus=tm_map(postCorpus,removePunctuation)

#applying the remove numbers function to corpus
postCorpus=tm_map(postCorpus,removeNumbers)

#stripping the white spaces 
postCorpus=tm_map(postCorpus,stripWhitespace)

#stemming the corpus
postCorpus=tm_map(postCorpus,stemDocument)

#converting into white document
postCorpus=tm_map(postCorpus,PlainTextDocument)

#recreate corpus
postCorpus = Corpus(VectorSource(postCorpus))

#applying the stop words with custom stopwords 
postCorpus = tm_map(postCorpus, removeWords, c('i','its','it','us','use','used','using','will','yes','say','can','take','one',
                                               stopwords('english')))

#converting into term document matrix
tdm=TermDocumentMatrix(postCorpus)

inspect(tdm)
words_freq = rollup(tdm, 2, na.rm=TRUE, FUN = sum)
words_freq = as.matrix(words_freq)
words_freq = data.frame(words_freq)
words_freq$words = row.names(words_freq)
row.names(words_freq) = NULL
words_freq = words_freq[,c(2,1)]
names(words_freq) = c("Words", "Frequency")
#wordcloud(words_freq$Words, words_freq$Frequency)
write.csv(words_freq,file="wordud.csv")
#Most frequent terms which appears in atleast 300 times
findFreqTerms(tdm, 2000)

#creating the word cloud 
graphics.off()
wordcloud(postCorpus, max.words=1000, random.order=FALSE, rot.per=90, scale=c(2.2,.15), colors=brewer.pal(7, "Dark2"))  
