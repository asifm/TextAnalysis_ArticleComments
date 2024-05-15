## flowchart TD
##   input([Input: text data]) --> A(Pre-process text data)
##   A --> B(Create document-term matrix) --> C(Decide on model parameters)
##   C --> D(Generate topic model)
##   D --> output([Output: N lists of terms, each list representing a topic])
##   output -.-> E(Evaluate coherence of topics) -.-> C

## ----------------------------------------------------------------------------------------
library(tidyverse)
library(tm)
library(topicmodels)
library(slam)
library(topicdoc)
library(stopwords)


## ----------------------------------------------------------------------------------------
posts <- read_csv("data/posts.csv")


## ----------------------------------------------------------------------------------------
glimpse(posts)


## ----------------------------------------------------------------------------------------
{{< include merge_datasets.R >}}


## ----------------------------------------------------------------------------------------
slice_sample(posts, n = 5)


## ----------------------------------------------------------------------------------------
# Rename relevant columns according to `tm` package requirements
textdata <- posts |>
  rename(doc_id = post_id, text = post_text) |>
  select(doc_id, text) |>
  as.data.frame()


## ----------------------------------------------------------------------------------------
#| eval: false

## en_stopwords <- stopwords::stopwords(
##     language = "en", source = "snowball"
## )
## corpus <- tm::Corpus(DataframeSource(textdata)) |>
##     tm_map(content_transformer(tolower)) |>
##     tm_map(removeWords, en_stopwords) |>
##     tm_map(removePunctuation, preserve_intra_word_dashes = TRUE) |>
##     tm_map(removeNumbers) |>
##     tm_map(stemDocument, language = "en") |>
##     tm_map(stripWhitespace)


## ----------------------------------------------------------------------------------------
min_doc <- 10

## ----------------------------------------------------------------------------------------
#| eval: false

## dtm <- tm::DocumentTermMatrix(corpus,
##     control = list(bounds = list(global = c(min_doc, Inf)))
## )
## 
## # Vocabulary pruning may create empty rows in the dtm.
## # Such rows need to be removed before running LDA.
## idx <- slam::row_sums(dtm) > 0
## dtm <- dtm[idx, ]
## textdata <- textdata[idx, ]
## 
## # Save for later use
## save(dtm, textdata, file = "r_objects/dtm.RData")


## ----------------------------------------------------------------------------------------
load("r_objects/dtm.RData")
tm::inspect(dtm)


## ----------------------------------------------------------------------------------------
#| code-fold: true

# Split the dtm into two parts
dtm_wsj <- dtm[grepl("^wsj", dtm$dimnames$Docs), ]
dtm_nyt <- dtm[grepl("^nyt", dtm$dimnames$Docs), ]

# Calculate term frequencies for wsj
term_freq_wsj <- base::colSums(as.matrix(dtm_wsj))

# Sort term frequencies in descending order for wsj
sorted_term_freq_wsj <- sort(term_freq_wsj, decreasing = TRUE)

# Find the most occurring 20 terms in wsj
top_terms_wsj <- sorted_term_freq_wsj[1:20]

# Calculate term frequencies for nyt
term_freq_nyt <- base::colSums(as.matrix(dtm_nyt))

# Sort term frequencies in descending order for nyt
sorted_term_freq_nyt <- sort(term_freq_nyt, decreasing = TRUE)

# Find the most occurring 20 terms in nyt
top_terms_nyt <- sorted_term_freq_nyt[1:20]


## ----------------------------------------------------------------------------------------
print(top_terms_nyt)


## ----------------------------------------------------------------------------------------
print(top_terms_wsj)


## ----------------------------------------------------------------------------------------
#| output: false
#| eval: false

## set.seed(123)
## 
## # Number of iterations for the LDA algorithm
## iter <- 1000
## 
## for (k in 2:6) {
##     # Create the LDA model
##     lda <- topicmodels::LDA(dtm,
##         k = k, method = "Gibbs",
##         control = list(iter = iter, verbose = iter / 10)
##     )
## 
##     # Save the model
##     saveRDS(lda, paste0("models/lda_", k, ".rds"))
## }


## ----------------------------------------------------------------------------------------
#| column: page

lda2 <- readRDS("models/lda_2.rds")
top_terms2 <- terms(lda2, 15)
print(top_terms2)

topicdoc::topic_diagnostics(topic_model = lda2, dtm_data = dtm)


## ----------------------------------------------------------------------------------------
#| column: page
lda3 <- readRDS("models/lda_3.rds")
top_terms3 <- terms(lda3, 15)
print(top_terms3)

topicdoc::topic_diagnostics(topic_model = lda3, dtm_data = dtm)


## ----------------------------------------------------------------------------------------
#| column: page
lda4 <- readRDS("models/lda_4.rds")
top_terms4 <- terms(lda4, 15)
print(top_terms4)

topicdoc::topic_diagnostics(topic_model = lda4, dtm_data = dtm)


## ----------------------------------------------------------------------------------------
#| column: page
lda5 <- readRDS("models/lda_5.rds")
top_terms5 <- terms(lda5, 15)
print(top_terms5)

topicdoc::topic_diagnostics(topic_model = lda5, dtm_data = dtm)


## ----------------------------------------------------------------------------------------
#| column: page
lda6 <- readRDS("models/lda_6.rds")
top_terms6 <- terms(lda6, 15)
print(top_terms6)

topicdoc::topic_diagnostics(topic_model = lda6, dtm_data = dtm)

