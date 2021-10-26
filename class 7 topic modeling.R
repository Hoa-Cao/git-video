install.packages("topicmodels")

library(tidyverse)
library(topicmodels)
library(tidytext)

data("AssociatedPress")
#Set a seed so that the output of the model is predictable
lda_results<-LDA(AssociatedPress, k=2, control = list(seed=1234))
lda_results

ap_topics<-tidy(lda_results, matrix="beta")

install.packages("reshape2")
library(reshape2)

lda_results_5<-LDA(AssociatedPress, k=5, control = list(seed=1234))
ap_topics<-tidy(lda_results_5, matrix="gamma")

ap_top_terms<-ap_topics%>%
  group_by(topic)%>%
  slice_max(beta, n=10)%>%
  ungroup()
  arrange(topic, -beta)

ap_top_terms%>%
  mutate(term=reorder_within(term, beta, topic))%>%
  ggplot(aes(beta, term, fill=factor(topic)))+
  geom_col()+
  facet_wrap(~topic)


