library(tm)
library(SnowballC)
library(wordcloud)
library(magrittr)
library(rvest)
spark_url="http://spark.apache.org/"
title_li = html(spark_url) %>% html_nodes("li a") %>% html_text()
title_p  = html(spark_url) %>% html_nodes("p") %>% html_text()
title_all <-  c(title_li,title_p)
my_spark = data.frame(title = title_all)
spark_corpus <- VCorpus(VectorSource(my_spark$title))
spark_corpus_clean <- tm_map(spark_corpus, content_transformer(tolower))
spark_corpus_clean <- tm_map(spark_corpus_clean, removeNumbers) # remove numbers
spark_corpus_clean <- tm_map(spark_corpus_clean, removeWords, stopwords()) # remove stop words
spark_corpus_clean <- tm_map(spark_corpus_clean, removePunctuation)
spark_corpus_clean <- tm_map(spark_corpus_clean, stemDocument)

dtm <- TermDocumentMatrix(spark_corpus_clean)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
d
set.seed(1234)
wordcloud(spark_corpus_clean, min.freq = 2, random.order = FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
d$word[d$freq>1]
