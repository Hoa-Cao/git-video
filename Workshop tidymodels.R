install.packages("tidymodels")
devtools::install_github("EmilHvitfeldt/scotus")
#https://github.com/EmilHvitfeldt/scotus

library(tidyverse)
library(scotus)
library(tidymodels)

scotus_filtered %>%
  as_tibble()

scotus_filtered %>%
  mutate(year=as.numeric(year),
         year=10*(year%/%10))%>%
  count(year)%>%
  ggplot(aes(year,n))+
  geom_col()+
  labs(x="Year", y="Number of opinions per decade")

#data split
set.seed(1234)
scotus_split<-scotus_filtered%>%
  mutate(
    year=as.numeric(year),
    text=str_remove_all(text,"'")
  )%>%
  initial_split()

scotus_train<-training(scotus_split)
scotus_test<-testing(scotus_split)

#Note: Use above step before tokenizing and unnestting

#Recipe

install.packages("textrecipes")

library(textrecipes)

scotus_rec<-recipe(year~text, data=scotus_train)%>%
  step_tokenize(text)%>%
  step_tokenfilter(text,max_tokens = 1e3)%>%
  step_tfidf(text)%>%
  step_normalize(all_predictors())

scotus_prep<-prep(scotus_rec)
scotus_bake<-bake(scotus_prep, new_data=NULL)

#Model

install.packages("ranger")
library(ranger)

rf_spec<-rand_forest(trees=1000)%>%
  set_engine("ranger")%>%
  set_mode("regression")

#Workflow

scotus_wf<-workflow()%>%
  add_recipe(scotus_rec)%>%
  add_model(rf_spec)%>%
  fit(data=scotus_train)


