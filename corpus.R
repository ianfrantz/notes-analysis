#Create text corpuses from the clusters
library(tm)
library(reshape)
library (SnowballC)

#Create a Noble notes.corpus
notes.corpus <- Corpus(VectorSource(notes$V2))

#Convert notes.corpus to lowercase and remove punctuation
notes.corpus <- tm_map  (notes.corpus, removePunctuation)

#Build a notes term-document matrix
notes.dtm <- TermDocumentMatrix(notes.corpus, control = list(stopwords=TRUE, wordLengths = c(1,30)))

#Find words that appear between a lower and upper frequency bound
findFreqTerms(notes.dtm, 500, 1000)

#Count words
freqwrds <- sort (rowSums(as.matrix(notes.dtm)), decreasing = TRUE)

#Return top 25 words
melt (freqwrds [1:25])

#Correlation of word with other words
findAssocs(notes.dtm, 'adv', 0.10)

#Find multiple associated words
matchwords <- c("delivery", "couch")
x <- sapply(notes.corpus, `[[`, 1)
output <- unique (grep(paste(matchwords, collapse = "|"), x, value= TRUE))
output.corpus <- Corpus(VectorSource(output))
output.dtm <- TermDocumentMatrix(output.corpus, control = list(stopwords = TRUE, wordLengths = c(1,30)))
freqoutput <- sort (rowSums(as.matrix(output.dtm)), decreasing = TRUE)
melt (freqoutput [1:20])
