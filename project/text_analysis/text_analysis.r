

#install.packages(c("stm","quanteda","tidyverse","tidytext", "conText"))
#"Rtsne", "rsvd", "geometry", "dplyr"))
library(stm)
library(tm)
library(conText)
library(tidytext)
library(quanteda)
library(tidyverse)

# getwd()
setwd("/Users/xiaosun/Documents/website/project/text_analysis")

# read data
pres_df <- read.delim("pres_speeches.txt", sep = ",")

str(pres_df)

# Check presidents in this dataset
table(pres_df$president)

# time frame: 1789-2023
range(pres_df$date)

# check the first document to see what is a word
pres_df$transcript[1]

# The first word is "My", let's isolate that.
substr(pres_df$transcript[1], 1, 2)

# check if certain terms are in a given document
# "fixed" means it must match the pattern exactly, so no regular expressions
unlist(
  strsplit(pres_df$transcript[1], " ", fixed = TRUE)
)

# Get the unique words in the document (ie. the "vocabulary")
unique(
  unlist(
    strsplit(pres_df$transcript[1], " ", fixed = TRUE)
    )
  )

# Is the word "My" in the list of unique words?
"My" %in% unique(unlist(strsplit(pres_df$transcript[1], " ", fixed = TRUE)))

# Capitalization matters!!
"my" %in% unique(unlist(strsplit(pres_df$transcript[1], " ", fixed = TRUE)))

"my" %in% unique(
  unlist(
    strsplit(
      tolower(
        pres_df$transcript[1]), " ", fixed = TRUE)
    )
  )

# Note: checking text for this sort of stuff will not change the text
# You have to save over it! This is why you'll often see terms like "raw text"
# to refer to text that has not been processed. Let's create a new column for
# this. This step is not advisable if you're working with a very large dataset.

pres_df$Raw_Text <- pres_df$transcript

# Whenever we want to start from scratch, we can set the original "transcript"
# column to be the same as the "Raw_Text" column, effectively undoing whatever
# processing/editing we've done.

# Let's use the tidytext library to do some word counting. Looks like there are
# a little over 4 million words (going off row count) in the presidential speech
# corpus.
pres_df %>%
  unnest_tokens(word, transcript)

# But how many times does each word appear?
pres_df %>%
  unnest_tokens(word, transcript) %>%
  count(word, sort = TRUE) %>%
  slice(1:25)

# Most text/linguistic data follow these sort of highly skewed distributions,
# eg. the Zipf distribution. Sometimes including these words in an analysis is
# helpful, sometimes it's not. Luckily there are lots of easy ways to get rid 
# of these types of high frequency function words (usually called "stopwords").
data("stop_words")
stop_words

pres_df %>%
  unnest_tokens(word, transcript) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  slice(1:25)

# For today we just want the words, the source is not important.
stop_words$lexicon <- NULL

# Let's say you wanted to add some custom stopwords. 
# Easy!
stop_words <- rbind(stop_words, 
                    "government",
                    "people")  

pres_df %>%
  unnest_tokens(word, transcript) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>% 
  filter(n > 5000) %>%
  slice(1:9) # only look words appear over 5000 times

# Most frequent words by president:
pres_df %>%
  unnest_tokens(word, transcript) %>%
  anti_join(stop_words) %>%
  count(president, word, sort = TRUE) %>% 
  group_by(president)
  # filter(n > 1000) %>%

# Still a lot to parse, let's get the top word associated with each president
pres_df %>%
  unnest_tokens(word, transcript) %>%
  anti_join(stop_words) %>%
  count(president, word, sort = TRUE) %>% 
  group_by(president) %>%
  filter(president == "Donald Trump") %>%
  slice_max(n, n = 15) # most frequent words by DT

# But presumably some of those "united" words are for "United States", right? 
# Probably other things along those same lines?
# Let's filter out bigrams that have stop words

## wanna see two words together, bigrams
pres_df %>%
  unnest_tokens(bigram, transcript,
                token = "ngrams", n = 2) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
    #mutate(word1_stop = if_else(word1 == "curfew",1,0),
    #       word2_stop = if_else(word2 == "curfew",1,0),
    #       stop_counts = word1_stop + word2_stop) %>%
    #filter(stop_counts != 0) %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  unite(bigram, c(word1,word2), sep = " ") %>%
  count(president, bigram, sort = TRUE) %>% 
  group_by(president) %>%
  slice_max(n, n = 5)

# We can also
bigram_df <- pres_df %>%
  unnest_tokens(bigram, transcript,
                token = "ngrams", n = 2) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  #mutate(word1_stop = if_else(word1 %in% stop_words$word,1,0),
  #       word2_stop = if_else(word2 %in% stop_words$word,1,0),
  #       stop_counts = word1_stop + word2_stop) %>%
  #filter(stop_counts != 2) %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  unite(bigram, c(word1,word2), sep = " ") %>%
  count(president, bigram, sort = TRUE) %>% 
  group_by(president) %>%
  slice_max(n, n = 5)
  
bigram_df %>%
  filter(president == "Donald Trump" |
         president == "Barack Obama") %>%
ggplot(., aes(x = n, y = reorder(bigram, n), fill = president)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~president, scales = "free_y") +
             #nrow = 2)
  labs(x = "Bigram Frequency",
       y = "Bigram")

# Additional code/instruction: https://www.tidytextmining.com/ 

################################################################################
# Topic modeling
# Today we will use the structural topic modeling (STM) package. The general 
# workflow is usually something like:

# 1. Preprocess data
# 2. Decide a number of topics
# 3. Generate the model
# 4. Analyze the results

# There are lots of different ways to accomplish the first three, but we're
# going to use the STM library's built in tools.

# If you're having memory issues, you should delete everything and reload the
# pres_df. 

# Cleaning the data

# If you want to generate a model that includes bigrams/trigrams/etc., look 
# into the quanteda library.

# But it will take much longer for such a model to converge, so here we just
# focus on unigrams with some small adjustments

# For example, lets convert some common phrases/terms in the text to turn them 
# into unigrams, like United States. 

# First, let's lowercase everything to make the data easier to work with.

cleaned_text <- tolower(pres_df$transcript)

cleaned_text <- gsub("united states of america",
                                          "unitedstatesofamerica", 
                     cleaned_text,
                                                fixed = TRUE)

# Order matters!!
cleaned_text <- gsub("united states",
                                          "unitedstatesofamerica", cleaned_text,
                                          fixed = TRUE)

pres_df$clean_text <- cleaned_text

# There are many other types of things to consider. For example, maybe it makes 
# sense to handle every instance of "President _____". 

# eg. paste("president", 
#unique(
#  stringr::word(
#    pres_df$president, -1)
#  )
#)

# Let's convert the date variable to a decade metric and start building the 
# model. There are lots of fancy things to do with dates/times in R, but in a 
# pinch this will also work for some analyses

pres_df$date_clean <- as.numeric(substr(pres_df$date, 1, 4))

# Now that our extra preprocessing is done, let's build an actual model
# textProcessor is a function in the STM library that feeds into another STM
# function (prepDocuments). The data is split into documents, metadata, and
# vocabulary. 
# As noted above, this model will use unigrams, and you can understand the 
# modeling process as "figuring out which words tend to co-occur" (as opposed
# to pairs of words, etc.)

# For speed, let's only look at transcripts from more recent presidents. 
pres_df_stm <- pres_df %>%
  filter(president == "Joe Biden" |
           president == "Donald Trump" |
           president == "Barack Obama" |
           president == "George W. Bush" |
           president == "Bill Clinton" |
           president == "George H. W. Bush")

pres_proc <- textProcessor(documents = pres_df_stm$clean_text,
                    metadata = pres_df_stm,
                    lowercase = TRUE,
                    stem = TRUE,
                    removestopwords = TRUE,
                    removenumbers = TRUE,
                    removepunctuation = TRUE)

meta <- pres_proc$meta
vocab <-pres_proc$vocab
docs <- pres_proc$documents

pres_out <- prepDocuments(docs, vocab, meta)
docs <- pres_out$documents
vocab <- pres_out$vocab
meta <- pres_out$meta

# Normally, deciding the number of topics is a big decision. But here, we're 
# going to use a built-in method in STM (see Mimno & Lee 2014)

set.seed(123)

## Idatuning

pres_stm <- stm(docs, vocab, K = 0,
               prevalence = ~ s(date_clean), 
               data = meta,
               init.type =  "Spectral")
## Rtsne, rsvd and geometry
#install.packages("Rtsne", "rsvd", "geometry")
library(Rtsne)
library(rsvd)
library(geometry)


# Create a text file to look at the topics
sink("pres_stm_terms.txt")
labelTopics(pres_stm, n = 10)
sink()

# Create a text file to estimate the time effect on the topics. When I was 
# testing the code, I ended up generating 81 topics based on the built-in method
# I mentioned. 
pres_stm_effects <- estimateEffect(1:73 ~ s(date_clean),
                                   stmobj = pres_stm,
                                   metadata = meta,
                                   uncertainty = "None")

sink("pres_stm_effects.txt")
summary(pres_stm_effects)
sink()

# Create matrix of topic scores/values
pres_gamma <- tidy(pres_stm, matrix = "gamma")

pres_gamma <- pres_gamma %>%
  spread(key = topic, value = gamma)

# Last thing: let's visualize! Replace i with a topic of interest

plot(pres_stm_effects, "date_clean",
     method = "continuous",
     topics = 3, 
     model = pres_stm, 
     printlegend = TRUE, 
     xlab = "Year")

pres_reg <- cbind(pres_df_stm$date_clean, 
                  pres_gamma[,3:74])

summary(lm(`pres_df_stm$date_clean` ~ ., data = pres_reg))

################################################################################
# Word embeddings with covariates
# A newish method that leverages word embeddings covariates with a similar
# regression type framework (Rodriguez, Spirling, & Stewart 2022)

# Let's use the same df as before to create a new variable by political party
pres_df_stm <- pres_df_stm %>%
  mutate(party = case_when(president == "Joe Biden" ~ "D",
           president == "Donald Trump" ~ "R",
           president == "Barack Obama" ~ "D",
           president == "George W. Bush" ~ "R",
           president == "Bill Clinton" ~ "D",
           president == "George H. W. Bush" ~ "R"))

pres_df_stm$ID <- seq(1:nrow(pres_df_stm))

toks <- tokens(pres_df_stm$clean_text, 
               remove_punct = T, 
               remove_symbols = T, 
               remove_numbers = T, 
               remove_separators = T)

# Clean out stopwords and words with 2 or fewer characters
toks_nostop <- tokens_select(toks, pattern = stopwords("en"), 
                             selection = "remove", 
                             min_nchar = 3)

# Only use features that appear at least 5 times in the corpus
feats <- dfm(toks_nostop, tolower = T, verbose = TRUE) %>% 
  dfm_trim(min_termfreq = 5) %>% 
  featnames()

# Leave the pads so that non-adjacent words will not become adjacent (in the
# word window)
toks <- tokens_select(toks_nostop, feats, padding = TRUE)

docvars(toks, "doc_id") <- pres_df_stm$ID
docvars(toks, "Party") <- factor(pres_df_stm$party)

immig_toks <- tokens_context(x = toks, pattern = c("immigration",
                                                   "immigrants",
                                                   "immigrant",
                                                   "immigrate"), 
                            window = 8L)

head(docvars(immig_toks),3)

immig_dfm <- dfm(immig_toks)

# Create Transformation Matrix
# construct feature-co-occurrence matrix
toks_fcm <- quanteda::fcm(toks, context = "window", window = 8,
                          count = "weighted", weights = 1 / (1:8), tri = FALSE)

# Estimate transformation
local_transform <- compute_transform(x = toks_fcm,
                                     pre_trained = cr_glove_subset, 
                                     weighting = 'log')
# Document-embedding matrix
immig_dem <- dem(x = immig_dfm, pre_trained = cr_glove_subset,
              transform = TRUE, 
              transform_matrix = local_transform, 
              verbose = TRUE)

immig_party <- dem_group(immig_dem, groups = immig_dem@docvars$Party)

# Now that we have the embeddings prepped, we can see how the meaning of the
# immigration words differ across political parties
immig_cos_sims <- cos_sim(immig_party, pre_trained = cr_glove_subset, 
                       features = immig_party@features, 
                       as_list = FALSE) 

# This will find the most different cosine similarities and with a 95% threshold
spread_cos_sims <- spread(immig_cos_sims, key = "target", value = "value")
spread_cos_sims$diff <- spread_cos_sims$D- spread_cos_sims$R

quantile(spread_cos_sims$diff, c(.025, .975))

five_perc_cos <- spread_cos_sims %>%
  filter(diff < -0.08954314 | diff > 0.23022252) %>%
  select(feature, diff)

# If you want to see what the most distinctive words are in a spreadsheet: 
write.csv(five_perc_cos, "immig_words.csv", row.names = FALSE)

# And finally a simple visualization
five_perc_cos %>%
  mutate(party = if_else(diff > 0, "D", "R")) %>%
ggplot(., aes(y = reorder(feature, diff), x = diff,
              color = party)) +
  geom_point() +
  scale_color_manual(values = c("blue", "red")) +
  labs(x = "Difference in cosine similarity",
       y = "Word",
       color = "Party") +
  xlim(c(-0.4,0.4)) +
  theme_minimal()


# This is all meant to be an introduction, there are so many other things to try
# and analyses to perform. Hopefully this gets you started analyzing text!