install.packages("tidytext")
library(tidytext)
library(janeaustenr)
library(stringr)
library(tidytext)
library(dplyr)


## Sentiment Analysis with the Inner Join
# Using "janeaustenr", "stringr" and "tidytext".
# 'janeaustenr'package can offer us with textual data in form of books authored by Jane Austen.


neat_data <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = T)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

# I have even performed thetidy operation on our text such that each row contains one word.
# we can now make use of the "bing" lexicon to implement filter() over the words that correspond to joy.

pos_senti <- get_sentiments("bing") %>%
  filter(sentiment == "positive")

neat_data %>%
  filter(book == "sensesensibility") %>%
  semi_join(pos_senti) %>%
  count(word, sort = T)

## From our result here, we can see many positive words like "good", "happy", "love" etc.
# the next step, we can use spread() function to segregate the data into separate columns
#of positive and negative sentiments.
# I will then be able to use the mutate() function to calculate the net sentiment, that is,
#the difference between positive and negative sentiment.

library(tidyr)
bing <- get_sentiments("bing")
sensesensibility_sentiment <- neat_data %>%
  inner_join(bing) %>%
  count(book = "sensesensibility", index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

## In this step we isualise the words present in book "sensesensibility" based on their
# positive & negetive scores
library(ggplot2)
ggplot(sensesensibility_sentiment, aes(index, sentiment, fill = book)) +
  geom_bar(stat = "identity", show.legend = T) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

## Next we proceed towards tallying the most common positive and negative words that are in the novel.

counting_words <- neat_data %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = T)
head(counting_words)

## during this last step, I will performe visualization of sentiment score. using ggplot()
#  to visualize the data based on their score.

counting_words %>%
  filter(n > 150) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
           geom_col() +
           coord_flip() +
           labs(y = "Sentiment Score")


library(reshape2)
install.packages("wordcloud")
library(wordcloud)
neat_data %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = T) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "cyan"),
                   max.words = 100)
