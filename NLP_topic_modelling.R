
# https://www.tidytextmining.com/preface.html
# Text mining with R

library(topicmodels)
data("AssociatedPress")
AssociatedPress

# LDA (Latent Dirichlet Allocation)
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
ap_lda

# Get "topics"
library(tidytext)
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

library(ggplot2)
library(dplyr)

# Plot topics
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# (words with) Greatest difference between categories
library(tidyr)

beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread

list(beta_spread$term)

# Select 10 largest and smallest values of "log_ratio")
df1 <- subset(beta_spread, beta_spread$log_ratio<=max(tail(sort(beta_spread$log_ratio, decreasing = T),10)))
df2 <- subset(beta_spread, beta_spread$log_ratio>=min(tail(sort(beta_spread$log_ratio, decreasing = F),10)))
df = rbind(df1,df2)
df

# Plot
barplot(sort(df$log_ratio), names.arg=df$term[order(df$log_ratio)], las=2)

# Gamma contains the probability of document X to belong to topic Y
ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents
# the model estimates that only about 24.8% of the words in document 1 were generated from topic 1
