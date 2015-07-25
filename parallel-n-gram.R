source("utilities.R")

require.or.install("tm")
require.or.install("slam")
require.or.install("RSQLite")
require.or.install("doSNOW")
Sys.setenv(JAVA_HOME="")
require.or.install("RWeka")

require.or.install("ggplot2")

num.cores <- 4
cluster <- makeCluster(num.cores, type="SOCK")
registerDoSNOW(cluster)

clusterEvalQ(cluster, library(tm))
clusterEvalQ(cluster, library(slam))
clusterEvalQ(cluster, library(RSQLite))
clusterEvalQ(cluster, Sys.setenv(JAVA_HOME=""))
clusterEvalQ(cluster, library(RWeka))

# This function splits any data set uniformly into num.cores chunks
split.by.processors <- function(data.set) {
  g <- factor(round((num.cores - 1) * runif(length(data.set))))
  split(data.set, g)
}

clean.corpus <- function(corpus) {
  transforms <- list(stripWhitespace, removeNumbers, removePunctuation, content_transformer(tolower))
  tm_map(corpus, tm_reduce, tmFuns=transforms)
}

create.corpus <- function(training.set) {
  Corpus(VectorSource(training.set))
}

create.corpus.parallel <- function(training.set) {
  corpi <- parLapply(cluster, split.by.processors(training.set), create.corpus)
  cleaned.corpi <- parLapply(cluster, corpi, clean.corpus)
  Reduce(c, cleaned.corpi)
}

create.ngram.dtm <- function(corpus, gram) {
  DocumentTermMatrix(corpus, control = list(tokenize=function(x) NGramTokenizer(x, Weka_control(min=gram, max=gram))))
}

create.dtm.parallel <- function(corpus, gram) {
  dtms <- parLapply(cluster, split.by.processors(corpus), create.ngram.dtm, gram)
  Reduce(c, dtms)
}

create.word.freq <- function(dtm, n, dataset) {
  #freq <- sort(col_sums(dtm), decreasing=TRUE)
  freq <- col_sums(dtm)
  word.freq <- data.frame(word=names(freq), freq=freq, row.names=NULL, stringsAsFactors=FALSE)
  word.freq$dataset <- as.integer(dataset)
  word.freq$n <- n
  if (n > 1) {
    word.splits <- strsplit(word.freq$word, " ")
    word.freq$gram <- sapply(word.splits, function(x) paste(x[1:n-1], collapse=' '))
    word.freq$word <- sapply(word.splits, function(x) x[n])
  } else {
    word.freq$gram <- word.freq$word
  }
  word.freq
}

create.ngrams.db <- function(db) {
  db.create.statement <- "
    CREATE TABLE ngrams (
      n INTEGER NOT NULL,
      gram TEXT NOT NULL,
      word TEXT NOT NULL,
      frequency INTEGER NOT NULL,
      dataset INTEGER NOT NULL,
      PRIMARY KEY (gram, dataset)
    );
  "
  dbSendQuery(conn=db, db.create.statement)
}

bulk.insert.ngrams <- function(db, word.freq) {
  db.insert.statement <- '
    INSERT OR REPLACE INTO ngrams (n, gram, word, frequency, dataset) VALUES (
      $n,
      $gram,
      $word,
      COALESCE(
        (SELECT frequency FROM ngrams
          WHERE gram=$gram AND dataset=$dataset
        ) + $freq,
        $freq
      ),
      $dataset
    )
  '
  dbBegin(db)
  dbSendPreparedQuery(db, db.insert.statement, bind.data=word.freq)  
  dbCommit(db)
}

ngrams <- data.frame()

bulk.combine.ngrams <- function(word.freq) {
  ngrams <<- rbind(ngrams, word.freq)
  aggregate(freq ~ ., data=word.freq, sum)
  print(paste(date(), "Aggregated ngrams are", object.size(ngrams)/1024^2, "MB"))
}

count.and.store.ngrams <- function(n, db, corpus, dataset) {
  print(paste(date(), "Generating DocumentTermMatrix for gram", n))
  dtm <- create.dtm.parallel(corpus, n)
  print(paste(date(), "Creating word frequency data frame for gram", n))
  word.freq <- create.word.freq(dtm, n, dataset)
  print(paste(date(), "Inserting into the DB for gram", n))
  bulk.insert.ngrams(db, word.freq)
  #print("Combining into ngrams")
  #bulk.combine.ngrams(word.freq)
}

batch.word.count.file <- function(file.name, db, dataset) {
  file.con <- file(file.name)
  batch.size <- 200000
  batch.num <- 1
  print(paste(date(), "Starting file", file.name))
  repeat {
    print(paste(date(), "Starting batch", batch.num))
    train <- readLines(file.con, batch.size)
    
    if (length(train) == 0 | batch.num > 10) {
      break
    }
    
    corpus <- create.corpus.parallel(train)
    lapply(1:4, count.and.store.ngrams, db, corpus, dataset)
    
    print(paste(date(), "Ending batch", batch.num))
    batch.num <- batch.num + 1
  }
  print(paste(date(), "Completed file", file.name))
  close(file.con)
}


twitter.file.name <- "corpus/final/en_US/en_US.twitter.train.txt"
blogs.file.name <- "corpus/final/en_US/en_US.blogs.train.txt"
news.file.name <- "corpus/final/en_US/en_US.news.train.txt"
file.names <- c(twitter.file.name, blogs.file.name, news.file.name)
datasets <- as.factor(c("twitter", "blogs", "news"))

ngrams.db <- dbConnect(SQLite(), dbname="ngrams.db")
create.ngrams.db(ngrams.db)
batch.word.count.file(twitter.file.name, ngrams.db, datasets[1])
batch.word.count.file(blogs.file.name, ngrams.db, datasets[2])
batch.word.count.file(news.file.name, ngrams.db, datasets[3])

dbDisconnect(ngrams.db)
stopCluster(cluster)