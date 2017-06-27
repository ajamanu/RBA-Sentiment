# RBA Loughran.R
# Text analysis of the RBA Monetary policy decisions based on
# https://juliasilge.com/blog/tidytext-0-1-3/
# Created by Aja Manu 27/06/17

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

# Clean Data
data$document <- gsub(".txt", "", data$document)
data$date <- as.Date(as.yearmon(data$document, "%b%y"))

# Convert to tidy format
tidy_data <- data %>%
      unnest_tokens(word, term) %>%
      add_count(date) %>%
      rename(month_total = n)

# Apply loughran lexicon to rba sentiment
rba_sentiment <- tidy_data %>%
      inner_join(get_sentiments("loughran"))

# Look at relative sentiment
rba_sentiment %>%
      count(date, month_total, sentiment) %>%
      filter(sentiment %in% c("positive", "negative", 
                              "uncertainty")) %>%
      mutate(sentiment = factor(sentiment, levels = c("negative",
                                                      "positive",
                                                      "uncertainty"))) %>%
      ggplot(aes(date, n / month_total, fill = sentiment)) +
      geom_area(position = "identity", alpha = 0.5) +
      labs(y = "Relative frequency", x = NULL,
           title = "Sentiment analysis of RBA's Monetary Policy Decision",
           subtitle = "Using the Loughran-McDonald lexicon")

# See which words are driving sentiment
rba_sentiment %>%
      count(sentiment, word) %>%
      filter(sentiment %in% c("positive", "negative", 
                              "uncertainty")) %>%
      group_by(sentiment) %>%
      top_n(15) %>%
      ungroup %>%
      mutate(word = reorder(word, n)) %>%
      mutate(sentiment = factor(sentiment, levels = c("negative",
                                                      "positive",
                                                      "uncertainty"))) %>%
      ggplot(aes(word, n, fill = sentiment)) +
      geom_col(alpha = 0.8, show.legend = FALSE) +
      coord_flip() +
      scale_y_continuous(expand = c(0,0)) +
      facet_wrap(~sentiment, scales = "free") +
      labs(x = NULL, y = "Total number of occurrences",
           title = "Words driving sentiment scores in RBA's Monerary Policy Decisions",
           subtitle = "From the Loughran-McDonald lexicon")

