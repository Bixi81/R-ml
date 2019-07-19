# Skript to analyse "similarity" of sequences in natural language settings
# October 2018
#
# Problem:
# two documents, one (ttext) possibly containing sequences from another document (torig)
# both texts are tokenised (into ngrams)
# each ngram "ttext" is compared to each ngram of "torig"

###########################################################################
# 0) Load packages:

library(dplyr)
library(tidytext)
library(fuzzyjoin)
library(tokenizers)
library(stringdist)
library(pdftools)
library(parallel)

# Global parameter settings
# Length of word-sequences to be considered 
# Ngram too long (danger of missing equal sequences)
# Ngram too small (will find many matches)
ngramlength = 7
# Regulation of string comparison (higher = less accurate)
maxd = 12
# Method of string comparison (see ?amatch) / Options: "osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex"
matchmethod = "osa"

###########################################################################
# 1) Read content:

# Read text
# Note: One sentecne in both examples below is very similar (text2, last sentence)

# IN TEXT 1:
# The EU ETS was launched in 2005 and is the first - and still by far the largest - international 
# system for trading greenhouse gas emission allowances covering over three-quarters of the allowances 
# traded on the international carbon market.

# IN TEXT2:
# The EU Eemissions Trading System has been launched in 2005 and is the first international 
# system covering over three-quarters of the allowances traded on the carbon market.


text1=as.character(
  "European Union Emissions Trading System (EU ETS) is the cornerstone of the European Union's policy to tackle climate change 
and its key tool for cost-effective reduction of emissions of carbon dioxide (CO2) and other greenhouse gases (GHG) in the power, 
aviation and industrial sectors. The EU ETS was launched in 2005 and is the first - and still by far the largest - international 
system for trading greenhouse gas emission allowances covering over three-quarters of the allowances traded on the international carbon market.
The EU ETS operates in the 31 countries of the European Economic Area (EEA). It limits emissions from nearly 11,000 power plants and manufacturing 
installations as well as slightly over 500 aircraft operators flying between EEA's airports (Report from the Commission to the 
European Parliament and to the Council, Report on the functioning of the European carbon market, 23 November 2017 (COM(2017) 693 final, p. 7)."
)

text2=as.character(
  "The primary intended audience of this package is scholars and professionals in fields where the impact of news on society is a prime factor, 
such as journalism, political communication and public relations (Baum and Groeling 2008; Boczkowski and De Santos 2007; Ragas 2014). 
To what extent the content of certain sources is homogeneous or diverse has implications for central theories of media effects, such as 
agenda-setting and the spiral of silence (Bennett and Iyengar 2008; Blumler and Kavanagh 1999). Identifying patterns in how news travels 
from the initial source to the eventual audience is important for understanding who the most influential gatekeepers are (Shoemaker and Vos 2009). 
Furthermore, the document similarity data enables one to study news values (Galtung and Ruge 1965) by analyzing what elements of news 
predict their diffusion rate and patterns. The EU Eemissions Trading System has been launched in 2005 and is the first international 
system covering over three-quarters of the allowances traded on the carbon market."
)

mytext1=data_frame(text1)
mytext2=data_frame(text2)

###########################################################################
# 2) Generate ngrams:

ttext = unnest_tokens(mytext1, ngram, text1, token = "ngrams", n = ngramlength)
torig = unnest_tokens(mytext2, ngram, text2, token = "ngrams", n = ngramlength)

# Drop every second row (we might not need all ngrams) #toDelete <- seq(0, length(dat), 2)
# ATTENTION: ngrams should not be too long!
ttext <-  ttext[-seq(0, length(ttext$ngram), 2), ]
torig <-  torig[-seq(0, length(torig$ngram), 2), ]

###########################################################################
# 3) Compare each ngram in ttext to each in torig

# 3.1) Use the "stringdist" package
# https://cran.r-project.org/web/packages/stringdist/stringdist.pdf

# With "generous" distance allowed, similarities arround 151-153 are detected
amatch(ttext$ngram,torig$ngram,maxDist=10, method = matchmethod)

# Let's store results in a way that allows interpretation
results1 = cbind(ttext$ngram, torig$ngram[amatch(ttext$ngram,torig$ngram,maxDist=maxd, method = matchmethod)])
# Remove "nas" 
results1 = results1[complete.cases(results1), ]
results1

# 3.2) "Join" similar ngrams (uses stringdist)
# THIS METHOD YIELDS SAME RESULT AS METHOD ABOVE
results2 = stringdist_join(ttext, torig, 
                           by = "ngram",
                           mode = "left",
                           ignore_case = T, 
                           method = matchmethod, 
                           max_dist = maxd, 
                           distance_col = "dist"
) %>%
  group_by(ngram.x) %>%
  top_n(1, -dist)

results2

###########################################################################
# 4) Application to Jane Austen Books
library(janeaustenr)
txt1 <- austen_books() %>% 
  filter(book == "Pride & Prejudice") %>%
  pull(text) 
txt2 <- austen_books() %>% 
  filter(book == "Sense & Sensibility") %>%
  pull(text) 

# Text very long for whole book (take only part of a book)
length(txt1)
txt1 = txt1[10:1000]
length(txt2)
txt2 = txt2[10:1000]

# Set parameter
ngramlength = 8
maxd = 10
matchmethod = "osa"

# Get time
start_time = Sys.time()

# Combine all pages in one text line if needed / bit of cleening
txt1 = paste(txt1, collapse = " ")
txt1 = gsub("\\r"," ",txt1)
txt1 = gsub("\\n"," ",txt1)
txt2 = paste(txt2, collapse = " ")
txt2 = gsub("\\r"," ",txt2)
txt2 = gsub("\\n"," ",txt2)

# Date frame and tokens
mytext1=data_frame(txt1)
mytext2=data_frame(txt2)
ttext = unnest_tokens(mytext1, ngram, txt1, token = "ngrams", n = ngramlength)
torig = unnest_tokens(mytext2, ngram, txt2, token = "ngrams", n = ngramlength)
# Drop every second row (we might not need all ngrams)#toDelete <- seq(0, length(dat), 2)
ttext <-  ttext[-seq(0, length(ttext$ngram), 2), ]
torig <-  torig[-seq(0, length(torig$ngram), 2), ]

# Get results and store them
jane = cbind(ttext$ngram, torig$ngram[amatch(ttext$ngram,torig$ngram,maxDist=maxd, method = matchmethod)])
jane = jane[complete.cases(jane), ]

cat("Time needed", Sys.time() - start_time)
jane
