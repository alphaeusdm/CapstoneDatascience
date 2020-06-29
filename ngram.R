library(ggplot2)
library(reshape2)
library(tm)
library(slam)
library(data.table)
library(RWeka)


con <- file("final/en_US/en_US.blogs.txt",open = "r")
blogs <- readLines(con)
close(con)

con <- file("final/en_US/en_US.news.txt",open = "r")
news <- readLines(con)
close(con)

con <- file("final/en_US/en_US.twitter.txt",open = "r")
twitter <- readLines(con)
close(con)

prefix <- "final/en_US/en_US"
ext <- "txt"
suffix <- "sample"
rate <- 1

build.sample <- function(name, enc = "UTF-8") {  
  filename <- paste(prefix, name, ext, sep = '.')
  data <- readLines(filename, encoding = enc)  
  line.count <- length(data)
  data.sample <- sample(data, 2 * line.count / 100)  
  rm(data)
  sample.fn <- paste(prefix, name, suffix, ext, sep = '.')
  writeLines(data.sample, sample.fn)
}

build.sample("blogs")
build.sample("news")
build.sample("twitter")

options(mc.cores = 1)

unigram <- function(x) NGramTokenizer(x,Weka_control(min = 1, max = 1))
bigram <- function(x) NGramTokenizer(x,Weka_control(min = 2, max = 2))
trigram <- function(x) NGramTokenizer(x,Weka_control(min = 3, max = 3))
quadgram <- function(x) NGramTokenizer(x,Weka_control(min = 4, max = 4))

corp <- VCorpus(DirSource("final/en_US", pattern = "sample", encoding = "UTF-8"),readerControl = list(reader = readPlain,language = "en",load = TRUE))
corp <- tm_map(corp, content_transformer(tolower))
corp <- tm_map(corp,removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, stripWhitespace)

gram.build <- function(tokenizer) {
  tdm <- TermDocumentMatrix(corp, control = list(tokenize = tokenizer))
  
  freqs <- rowSums(as.matrix(tdm))
  freqs <- freqs[order(freqs, decreasing = T)] 
  
  ngram.list <- strsplit(names(freqs), "\\s")
  n <- length(ngram.list[[1]])
  tokens <- unlist(ngram.list)
  rm(ngram.list)
  
  m <- matrix(tokens, ncol = n, byrow = T)
  df <- as.data.frame(m, stringsAsFactors = F)
  result <- cbind(freqs, df)
  
  print(head(result))
  
  result
  
}

uni.gram <- gram.build(unigram)
bi.gram <- gram.build(bigram)
tri.gram <- gram.build(trigram)
quad.gram <- gram.build(quadgram)


saveRDS(uni.gram, file = "unigram.RData")
saveRDS(bi.gram, file = "bigram.RData")
saveRDS(tri.gram, file = "trigram.RData")
saveRDS(quad.gram, file = "quadgram.RData")