library(tidytext)
library(tidyverse)
library(janeaustenr)
library(dplyr)
library(stringr)

#Load Text

tidy_books<-austen_books()%>%
  group_by(book)%>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(
      str_detect(text,
                 regex("^chapter [\\divxlc]",
                       ignore_case = TRUE)))
    )%%
  
  ungroup()%%
  unnest_tokens(word, text)

get_sentiments("nrc")
nrc<-get_sentiments("nrc")

library(textdata)

nrc_joy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")

tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrc)%>%
  count(sentiment, sort = TRUE)

bing<-get_sentiments("bing")