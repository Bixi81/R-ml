# Minimal example -> regression on "bag of words"

# Dummy data
df=data.frame(
  text = 
    c(
    "Hello where are you",
    "Hello how are you",
    "Holla where have you been",
    "Hello my name is James",
    "Hello where do you come from",
    "I think this is great",
    "I think we need to go",
    "I believe it is good to do that",
    "I guess we all want this",
    "I don't think this works"),
  value = 
    c(1,2,3,2,4, 100,120,90,130,110)
)

# Make sure text is character
df$text = as.character(df$text)

library(tidytext)
library(dplyr)

# Check most frequent words
fwords=tibble(text = df$text) %>% 
  tidytext::unnest_tokens(word, text) %>%    
  dplyr::count(word, sort = TRUE)  
fwords

library(tm)
library(stringr)

# Preprocess text
df$text = str_replace_all(df$text, "[[:punct:]]", "")
df$text = tolower(df$text)
df$text = removePunctuation(df$text)
#df$text = removeNumbers(df$text)
df$text = stripWhitespace(df$text)

# Generate corpus
textsource <- VectorSource(df$text)
corpus = VCorpus(textsource)

# Look at a single line (second line)
corpus[[2]][1]

# Set up a document-term matrix
dtm = DocumentTermMatrix(corpus)
dtm = as.matrix(dtm)
dim(dtm)

# DTM to data frame (cbind the "value" column from the data frame)
dd=data.frame(cbind(value=df$value,dtm))

# Prepare data for regression
y = as.matrix(as.numeric(dd$value))
x = as.matrix(dd[,2:ncol(dd)])

# Estimate Lasso/Ridge/Elastic Net (alpha is the relevant parameter)
# See: https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
library(glmnet)
cvfit = cv.glmnet(x, y, type.measure="mse", nfolds=3, alpha=0)
coef(cvfit, s = "lambda.min")
pred = predict(cvfit, newx = as.matrix(dd[2:ncol(dd)]), s = "lambda.min")

cbind(pred=round(pred), truth=df$value, df$text)
# Well, not a great prediction (no wonder given the few observations)
# However, results go in the right direction
