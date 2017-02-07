# RBA Tidy.R
# sentiment Analysis of RBA Statements
# https://github.com/juliasilge/tidytext/blob/master/vignettes/tidying_casting.Rmd
# Created by Aja Manu 09/08/16

# clear environment
rm(list=ls())

# Load libraries
library(tm)
library(dplyr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(zoo)
library(topicmodels)
library(slam)

# Set work directory
setwd("C:/Users/amanu.CPE/Documents/R/Text Analytics")

# Load files--------------------------------------------------------------------
getReaders()
corpus <- VCorpus(DirSource("C:/Users/amanu.CPE/Documents/R/Text Analytics/Data"), 
                  list(reader = readPlain))

# Create document term matrix
dtm <- DocumentTermMatrix(corpus) # create document term matrix

# Create tidytext
data <- tidy(dtm)

# Tidy data---------------------------------------------------------------------
# remove puncutation and clean up corpus
data$term <- gsub('[[:punct:]]', '', data$term)
#data$term <- gsub('declines | declining', 'decline', data$term)

# Import and remove stop_words from data
data("stop_words")
data <- data %>%
      anti_join(stop_words, by = c('term' = 'word'))

# Sentiment analysis------------------------------------------------------------
# perform sentiment analysis
# Get Bing +ve/-ve dictionary
bing <- sentiments %>%
      filter(lexicon == "bing") %>%
      dplyr::select(word, sentiment)

# bing <- bing[-1012,] # consistent is a word that the RBA usees alot so may want to think about taking it out

# Match data to +ve/-ve
rba_sentiments <- data %>%
      inner_join(bing, by = c(term = "word"))

rba_sentiments

# Find the most Negative Documents
rba_sentiments %>%
      count(document, sentiment, wt = count) %>%
      ungroup() %>%
      spread(sentiment, n, fill = 0) %>%
      mutate(sentiment = positive - negative) %>%
      arrange(sentiment)

# Find the most Positive Documents
rba_sentiments %>%
      count(document, sentiment, wt = count) %>%
      ungroup() %>%
      spread(sentiment, n, fill = 0) %>%
      mutate(sentiment = positive - negative) %>%
      arrange(desc(sentiment))

# Visualise wortd that contributed to positive or negative sentiment
rba_word_counts <- data %>%
      inner_join(bing, by = c(term = "word")) %>%
      count(term, sentiment, sort = TRUE) %>%
      ungroup()

rba_word_counts %>%
      filter(n > 25) %>%
      mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
      mutate(term = reorder(term, n)) %>%
      ggplot(aes(term, n, fill = sentiment)) +
      geom_bar(stat = "identity") +
      ggtitle("RBA Most Positive/Negative Words Used") +
      theme(plot.title = element_text(size=20, face="bold", vjust=2)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ylab("Contribution to sentiment")

# Get Sentiment for documents
rba_sentiments1 <- rba_sentiments %>%
      count(document, sentiment, wt = count) %>%
      ungroup() %>%
      spread(sentiment, n, fill = 0) %>%
      mutate(sentiment = positive - negative)

# Get R data
rba_sentiments1$document <- gsub(".txt", "", rba_sentiments1$document)
rba_sentiments1$date <- as.Date(as.yearmon(rba_sentiments1$document, "%b%y"))

# Re-order
rba_sentiments1 <- rba_sentiments1[order(rba_sentiments1$date), ]
rba_sentiments1$rebased <- rba_sentiments1$sentiment/mean(rba_sentiments1$sentiment)

# Plot net-sentiment polarity
ggplot(rba_sentiments1, aes(date, rebased*100)) +
      geom_line(size = 1, colour = '#2F6165') +
      geom_smooth(se = FALSE, col = 'black') +
      ggtitle("Net Economic Sentiment of RBA Statements") +
      theme(plot.title = element_text(size=20, face="bold", vjust=2)) +
      xlab("Date") +
      ylab("Net Sentiment Score (Average = 100)") +
      scale_color_discrete(name="Below \nGuideline") +
      geom_hline(yintercept = 100, size = 1, col = "#CCA36E") +
      ggplot2::annotate("text", label = "GFC", x = as.Date("2008-08-01"),
               y = -130, col = '#00AAA1') +
      ggplot2::annotate("text", label = "Eurozone \n Concerns", x = as.Date("2012-12-01"),
               y = -110, col = '#00AAA1')
      
# Rolling-arerage for sentiment
rba_sentiments1$average <- rollapply(rba_sentiments1$rebased*100, width = 7, 
                                     FUN = mean, align = "center", fill = NA)

ggplot(rba_sentiments1, aes(x = date, y = rebased*100)) +
      geom_line(size = 1, colour = '#2F6165') +
      geom_line(data = rba_sentiments1, aes(y = average), size = 1, colour = 'red') +
      ggtitle("Net Economic Sentiment of RBA Statements") +
      theme(plot.title = element_text(size=20, face="bold", vjust=2)) +
      xlab("Date") +
      ylab("Net Sentiment Score (Average = 100)") +
      scale_color_discrete(name="Below \nGuideline") +
      geom_hline(yintercept = 100, size = 1, col = "#CCA36E") +
      ggplot2::annotate("text", label = "GFC", x = as.Date("2008-08-01"),
               y = -250, col = '#00AAA1') +
      ggplot2::annotate("text", label = "Eurozone \n Concerns", x = as.Date("2012-12-01"),
               y = -110, col = '#00AAA1')

# Write csv file for data
#write.csv(rba_sentiments1, "Sentiment.csv", row.names = FALSE)

# 

# Topic Modelling---------------------------------------------------------------
# http://tidytextmining.com/topicmodeling.html
word_count <- data %>%
      count(document, term, sort = TRUE) %>%
      ungroup()

word_count

# Cast a document term matrix
rba_dtm <- word_count %>%
      cast_dtm(document, term, n)

rba_dtm # get stats on Dccument term matrix

# Calculate the term frequency-inverse document frequency
summary(col_sums(rba_dtm))

term_tfidf <- tapply(rba_dtm$v/row_sums(rba_dtm)[rba_dtm$i], rba_dtm$j, mean) *
      log2(nDocs(rba_dtm)/col_sums(rba_dtm > 0))
summary(term_tfidf)

# We only include terms which have a tf-idf value of greater than the median
rba_dtm <- rba_dtm[,term_tfidf >= summary(term_tfidf)[3]]
rba_dtm <- rba_dtm[row_sums(rba_dtm) > 0,]
rba_dtm
summary(col_sums(rba_dtm))
dim(rba_dtm)

# Create topic models using LDA and set number of topics 
rba_lda <- LDA(rba_dtm, k = 10, control = list(seed = 1234))
rba_lda

rba_lda_td <- tidytext:::tidy.LDA(rba_lda)
rba_lda_td

# Get topics
top_terms <- rba_lda_td %>%
      group_by(topic) %>%
      top_n(5, beta) %>%
      ungroup() %>%
      arrange(topic, -beta)

top_terms

# plot top terms in topics
top_terms %>%
      mutate(term = reorder(term, beta)) %>%
      ggplot(aes(term, beta, fill = factor(topic))) +
      geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free")

##############################################################################
# create a k topic LDA model
k <- 5
SEED <- 2010
rba_TM <- list(VEM = LDA(rba_dtm, k = k, control = list(seed = SEED)),
               VEM_fixed = LDA(rba_dtm, k = k,
                               control = list(estimate.alpha = FALSE, seed = SEED)),
               Gibbs = LDA(rba_dtm, k = k, method = "Gibbs",
                           control = list(seed = SEED, burnin = 1000,
                                          thin = 100, iter = 1000)),
              CTM = CTM(rba_dtm, k = k,
                        control = list(seed = SEED,
                                       var = list(tol = 10^-4), em = list(tol = 10^-3))))

sapply(rba_TM[1:2], slot, "alpha")

Topic <- topics(rba_TM[["VEM"]], 1)
Terms <- terms(rba_TM[["VEM"]], 5)
Terms
