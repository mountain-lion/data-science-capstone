source("util.R")
#source("load.R")
# Preload necessary R librabires

library(dplyr); 

#library(SnowballC)
library(stringi); 
library(stringr)
library(shiny)
# loading data 


library(doParallel); 

dfm.Cache <- list("unigramDFM.rds","bigramDFM.rds","trigramDFM.rds","quadgramDFM.rds")
#find the top term grams with a minimum of occurrence in the corpus

getTopTermsFreqTable <- function(dfm.matrix, lowfreq = 50)
{
  
  ngram.sorted <- sort(rowSums(dfm.matrix), decreasing=TRUE)
  ngram.FreqTable <- data.frame(Words=names(ngram.sorted), Frequency = ngram.sorted)
  return (ngram.FreqTable)
}

getTermFrequency <- function(sampleData, xgrams, lowfreq = 50)
{
  print(xgrams)
  ngram.dfm <- dfm(sampleData, ngrams = xgrams, verbose = TRUE, concatenator = " ", stopwords=TRUE, remove_punct = TRUE)
  
  ngram.matrix <- as.data.frame(as.matrix(docfreq(ngram.dfm)))
  return (getTopTermsFreqTable(ngram.matrix, lowfreq))
}

loadTermFrequency <- function(corpusData, ngrams = 1, lowfreq = 50) {
  #print("loadTermFrequency")
  #print(ngrams)
  termFrequencyTable <- data.frame("Words", "Frequency")
  if(!file.exists(dfm.Cache[[ngrams]]))
  {
    #create document-feature matrix tokenized on n-grams
    message("Cache is clean. Rebuilding cache of RDS ...")
    termFrequencyTable <- getTermFrequency(corpusData, xgrams = ngrams, lowfreq = 50)
    save(termFrequencyTable,file=dfm.Cache[[ngrams]])
  }
  else
  {
    message("Loading DFM Frequency Table from RDS cache ...")
    load(dfm.Cache[[ngrams]], verbose = TRUE)
  }
  return (termFrequencyTable)
}

if(!exists("ft.Data"))
{

library(doParallel); 
cl <- makeCluster(detectCores() - 2)
registerDoParallel(cl)

library(quanteda)

intervalStart <- Sys.time()
ft.Data <- list(4)

for (i in 1:4) {
  ft.Data[[i]] <- loadTermFrequency(sample_data, ngrams = i, lowfreq = 50)
  gc()
}
intervalEnd <- Sys.time()
stopCluster(cl)

# paste("Getting Term Matrices for N-Grams [1:3] took: ",intervalEnd - intervalStart,
#       attr(intervalEnd - intervalStart,"units"))

}





