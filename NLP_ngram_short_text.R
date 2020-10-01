# Dummy data
# Note the little differences in the strings, however there is a clear pattern
df = data.frame(text=c("ab13ab12ab16","ag16ag16fg16","ab12ab12ab12","fg12fg16fg16","ab16ab12af12","fg16fg16fg16"),target=c(1,0,1,0,1,0))

head(df)

# Set up lists to post results from loop
ngrams = list()
targets = list()
observation = list()

# Loop over rows of DF
counter = 1
for (row in 1:nrow(df)){
  # Loop over strings in first colum per row
  for (s in 1:nchar(as.character(df$text[[row]]))){
    # Get "ngram" (sequence of two letters/digits)
    substring=substring(as.character(df$text[[row]]), s, s+1)
    # Append if >1, also post row and target
    if (nchar(substring)>1){
      ngrams[[counter]]<-substring
      targets[[counter]]<-df$target[[row]]
      observation[[counter]]<-row
      counter = counter+1
    }
  }
}

# Lists to DFs
ngramdf=data.frame(ngram=matrix(unlist(ngrams), nrow=length(ngrams), byrow=T))
targets=data.frame(ngram=matrix(unlist(targets), nrow=length(targets), byrow=T))
obs=data.frame(obs=matrix(unlist(observation), nrow=length(observation), byrow=T))

# Get dummy encoding ("one hot") from all the ngrams
dummies = model.matrix(~ . -1 , data=ngramdf)

# Bind target and dummies to train DF
train = cbind(targets,dummies)

# Now apply Lasso to predict the classes
# https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
library(glmnet)
cvfit = cv.glmnet(as.matrix(train[,-1]), as.matrix(train[,1]), family = "binomial", type.measure = "class")

# Predict class per ngram
classes = predict(cvfit, newx = as.matrix(train[,-1]), s = "lambda.min", type = "class")

# predicted probability per ngram -> needs (type="response")
probs = predict(cvfit, newx = as.matrix(train[,-1]), s = "lambda.min", type = "response")

# Calculate average prob that a sequence (so ngrams per row) belongs to some class
result = cbind(probs,obs)
# Get mean per row
result = aggregate(. ~ result$obs, result[1], mean)
# Bind original text and target
result = cbind(result,df)
colnames(result)<-c("observation","estimated_prob", "text", "original_target")

result
