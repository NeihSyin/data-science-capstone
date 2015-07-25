library(doSNOW)
library(hash)
cluster <- makeCluster(4, type="SOCK")
registerDoSNOW(cluster)

document.to.words <- function(document) {
  Filter(function(word) word != "", sapply(strsplit(gsub("[^a-zA-Z ]", "", tolower(document)), " "), trimws))
}


blogs.words <- parLapply(cluster, blogs.train[1:100000], document.to.words)
twitter.words <- parLapply(cluster, twitter.train[1:100000], document.to.words)
news.words <- parLapply(cluster, news.train[1:100000], document.to.words)

word.counts <- function(words) {
  counts <- hash()
  for (i in 1:length(words)) {
    if (is.null(counts[[ words[[i]] ]])) {
      counts[[ words[[i]] ]] <- 1
    } else {
      counts[[ words[[i]] ]] <- counts[[ words[[i]] ]] + 1
    }
  }
  counts
}

combine.word.counts <- function(words, words2) {
   k <- keys(words2)
   for (i in 1:length(k)) {
     if (is.null(words[[ k[[i]] ]])) {
       words[[ k[[i]] ]] <- words2[[ k[[i]] ]]
     } else {
       words[[ k[[i]] ]] <- words2[[ k[[i]] ]] + words[[ k[[i]] ]]
     }
   }
   words
}

word.counts.list <- function(words) {
  counts <- list()
  for (i in 1:length(words)) {
    if (is.null(counts[[ words[[i]] ]])) {
      counts[[ words[[i]] ]] <- 1
    } else {
      counts[[ words[[i]] ]] <- counts[[ words[[i]] ]] + 1
    }
  }
  counts
}

combine.word.counts.list <- function(words, words2) {
   for (i in 1:length(words2)) {
     if (is.null(words[[ words2[[i]] ]])) {
       words[[ words2[[i]] ]] <- words2[[ i ]]
     } else {
       words[[ words2[[i]] ]] <- words2[[ i ]] + words[[ words2[[i]] ]]
     }
   }
   words
}

clusterEvalQ(cluster, library(hash))

twitter.word.counts <- parLapply(cluster, twitter.words[1:10000], word.counts.list)
twitter.reduced.counts <- Reduce(combine.word.counts.list, twitter.word.counts)
rm(twitter.word.counts)

blogs.word.counts <- parLapply(cluster, blogs.words[1:10000], word.counts.list)
blogs.reduced.counts <- Reduce(combine.word.counts.list, blogs.word.counts)
rm(blogs.word.counts)

news.word.counts <- parLapply(cluster, news.words[1:10000], word.counts)
news.reduced.counts <- Reduce(combine.word.counts, news.word.counts)
rm(news.word.counts)

overall.counts <- Reduce(combine.word.counts, c(twitter.reduced.counts, blogs.reduced.counts, news.reduced.counts))
