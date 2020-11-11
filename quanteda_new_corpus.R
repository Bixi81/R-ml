library(quanteda)

### New text corpus (test set) on some fixed document term matrix (train set)

mytext <- c(oldtext = "This is my old text")
dtm_old <- dfm(mytext)
dtm_old

newtext <- c(newtext = "This is my new text")
dtm_new <- dfm(newtext)
dtm_new

dtm_matched <- dfm_match(dtm_new, featnames(dtm_old))
dtm_matched
