library(dplyr)
library(tidytext)
library(fuzzyjoin)
library(tokenizers)

##############################
# Compare text sequences (words/ngrams)

text1=as.character("Hi my name is Bixi and I like cycling a lot. It is just great!")
mytext1=data_frame(text1)

text2=as.character("Hi my name is Lissi and I'm good in swimming. It is just great!")
mytext2=data_frame(text2)

ngram1 = unnest_tokens(mytext1, ngram, text1, token = "ngrams", n = 4)
ngram2 = unnest_tokens(mytext2, ngram, text2, token = "ngrams", n = 4)

# Find matching sequence(s)
semi_join(ngram1,ngram2)

##############################
# Compare sequences of single letters

ngram3=tokenize_character_shingles(mytext1$text1, n = 10, n_min = 10, strip_non_alphanum = FALSE)
ngram4=tokenize_character_shingles(mytext2$text2, n = 10, n_min = 10, strip_non_alphanum = FALSE)

ngram3=as.data.frame(ngram3)
ngram4=as.data.frame(ngram4)

# Find matching sequences of single letters
semi_join(ngram3,ngram4)

##############################
# Compare a "long" text sequence and keywords over a certain string sequence

longtext = data.frame(text=c("This is my very long text which will be searched for partial string matches"))
keywords = data.frame(keywords=c("texts", "match", "foo", "bar"))

ngram1 = unnest_tokens(longtext, characters, text, token = "character_shingles", n = 4)
ngram2 = unnest_tokens(keywords, characters, keywords, token = "character_shingles", n = 4)

# Find matching sequence(s)
semi_join(ngram1,ngram2)

##############################
# Fuzzy matching of strings

###########
# A) agrep:
# https://stackoverflow.com/questions/27360908/merging-data-frame-rows-based-on-similar-strings-in-r
x <- c("Company A Pty.","BigData GMBH","Company A Pty. Ltd.","Red Pants Warsaw", "Company A Georgia", "Red Pants Ltd", "BlueSocks House")

# Find matches
agrep(x[1],x, max=0.1, value=TRUE)

###########
# B) Fuzzyjoin:
# https://stackoverflow.com/questions/26405895/how-can-i-match-fuzzy-match-strings-from-two-datasets

a <- data.frame(name = c('Ace Co', 'Bayes', 'asd', 'Bcy', 'Baes', 'Bays'),
                price = c(10, 13, 2, 1, 15, 1))
b <- data.frame(name = c('Ace Co.', 'Bayes Inc.', 'asdf'),
                qty = c(9, 99, 10))

# Find matches
stringdist_join(a, b, 
                by = "name",
                mode = "left",
                ignore_case = FALSE, 
                method = "jw", 
                max_dist = 99, 
                distance_col = "dist"
) %>%
  group_by(name.x) %>%
  top_n(1, -dist)
