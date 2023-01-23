# LIBRARY AND PACKAGES ---------------------------------------------------------
.libPaths("C:/R library")

if(!require("tidyverse")) {install.packages("tidyverse")}
library(tidyverse)
if(!require("readxl")) {install.packages("readxl")}
library(readxl)
if(!require("stm")) {install.packages("stm")}
library(stm)

# Published paper to assist:
# Roberts, Margaret E., Brandon M. Stewart, and Dustin Tingley. "Stm: An R package for structural topic models." Journal of Statistical Software 91.1 (2019): 1-40.


# IMPORT DATA ------------------------------------------------------------------
<import spreadsheet>
# spreasheet$textcolumn will be text data.


# INGEST DATA ------------------------------------------------------------------
processed <- textProcessor(<df$textdata>, 
                           metadata = <df>, 
                           removenumbers = FALSE)
#numbers retained as 101 and 999 may appear as terms. 
out <- prepDocuments(processed$documents, 
                     processed$vocab, 
                     processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta
# results in a corpus of n docs. - re-indexed.


# PREPARE DATA -----------------------------------------------------------------
# can plot how many documents, words and tokens would be removed when setting
# minimum threshold for number of infrequent words
plotRemoved(processed$documents, 
            lower.thresh = seq(1, 100, by = 1))

# set minimum threshold of word frequency
out <- prepDocuments(processed$documents, 
                     processed$vocab, 
                     processed$meta, 
                     lower.thresh = <choose cutoff>)
# remove 17 documents and 2842 of 3099 words (9406 of 26223 tokens).


# SEARCH FOR 'BEST' K ----------------------------------------------------------
Ksearch <- searchK(out$documents, 
                   out$vocab, 
                   K = c(2:50), # choose range
                   prevalence =~ #<add variables from metadata if wanted>, 
                   data = out$meta)
Ksearch
plot(Ksearch) #learn how to interpret this properly


# ESTIMATE MODEL ---------------------------------------------------------------
# K = established by Ksearch
stm_Spectral_Fit <- stm(documents = out$documents, 
                        vocab = out$vocab, 
                        K = <k search result>, # add result from ksearch
                        max.em.its = 75, 
                        data = out$meta, 
                        init.type = "Spectral")


# UNDERSTAND -------------------------------------------------------------------
# provide words in each topic
labelTopics(stm_Spectral_Fit, n = 10)

# can plot these too
plot.STM(stm_Spectral_Fit, type = "labels", n = 10, width = 100)

# drilling down into individual quote
# prep document data
x <- as.data.frame(out$meta)
thoughts1 <- findThoughts(stm_Spectral_Fit, 
                          text = <insert df$textdata>, 
                          n = 20, 
                          topics = 1) # choose topic no.
plotQuote(thoughts1, width = 100, main = "Specific Police Issues")


# VISUALISE --------------------------------------------------------------------
# visualise correlation between all of the topics
topic_correlates <- topicCorr(stm_Spectral_Fit)
plot(topic_correlates)

# proportion of each topic across sample:
plot.STM(stm_Spectral_Fit, type = "summary")

# wordclouds for individual topics:
cloud(stm_Spectral_Fit, topic = 1)

# example quotes:
quotes <- findThoughts(stm_Spectral_Fit, text = <df$textdata>, n = 20, topics = 1)
plotQuote(quotes, width = 100, main = "topic 1")

# compare words across two topics
plot(stm_Spectral_Fit, type = "perspectives", topics = c(1, 2))


# INTERPRET AND REPORT ---------------------------------------------------------
# update later?


# TOPIC EXPORT -----------------------------------------------------------------
# possible to export theta score - i.e., how much each document related to each theme
# read more into theta scores from this analysis to be exact
export_topics <- make.dt(stm_Spectral_Fit, meta = out$meta)
write_excel_csv(export_topics, "<enter file path>")
