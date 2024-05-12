# nolint: object_name_linter

library(tidyverse)
library(tm)
library(topicmodels)
library(ldatuning) # https://nikita-moor.r-universe.dev/ldatuning
library(wordcloud)
# library(pals) # color palettes https://kwstat.r-universe.dev/pals
# library(DT) # tabular reports
# library(flextable) # https://davidgohel.r-universe.dev/flextable
# library(reshape2) # Hadley recommends tidyr
# library(SnowballC) # stemming https://nalimilan.r-universe.dev/SnowballC
# library(stm)
# library(lda) # Gibbs sampling LDA https://solivella.r-universe.dev/lda

# Load data
posts <- read_csv("data/posts.csv")

textdata <- posts |>
  rename(doc_id = post_id, text = post_text) |>
  select(doc_id, text) |>
  as.data.frame()

# # Work on a small sample
# textdata <- textdata[1:100, ]

# english_stopwords <- tm::stopwords("en") # nolint: commented_code_linter.
en_stopwords <- readLines("https://ladal.edu.au/resources/stopwords_en.txt", encoding = "UTF-8") # nolint
# create corpus object
corpus <- tm::Corpus(DataframeSource(textdata)) |>
  tm_map(content_transformer(tolower)) |>
  tm_map(removeWords, en_stopwords) |>
  tm_map(removePunctuation,
    preserve_intra_word_dashes = TRUE
  ) |>
  tm_map(removeNumbers) |>
  tm_map(stemDocument, language = "en") |>
  tm_map(stripWhitespace)


# Document-term matrix
# compute document term matrix with terms >= minimum_frequency
minimum_frequency <- 5 # not used. was having problems.
dtm <- DocumentTermMatrix(processed_corpus)
dim(dtm)


# due to vocabulary pruning, we have empty rows in our dtm
# LDA does not like this. So we remove those docs from the
# dtm and the metadata
sel_idx <- slam::row_sums(dtm) > 0
dtm <- dtm[sel_idx, ]
textdata <- textdata[sel_idx, ]

# # create models with different number of topics
# result <- ldatuning::FindTopicsNumber(
#   dtm,
#   topics = seq(from = 2, to = 6, by = 2),
#   metrics = c("CaoJuan2009", "Deveaud2014"),
#   method = "Gibbs",
#   control = list(seed = 77),
#   verbose = TRUE
# )

# FindTopicsNumber_plot(result)

# number of topics
K <- 3
# set random number generator seed
set.seed(1234)
# compute the LDA model, inference via 1000 iterations of Gibbs sampling
topic_model <- LDA(dtm, K,
  method = "Gibbs",
  control = list(iter = 500, verbose = 25)
)

# have a look a some of the results (posterior distributions)
tm_result <- posterior(topic_model)
# format of the resulting object
attributes(tm_result)


nTerms(dtm) # lengthOfVocab
# topics are probability distributions over the entire vocabulary
beta <- tm_result$terms # get beta from results
dim(beta) # K distributions over nTerms(dtm) terms

nDocs(dtm) # size of collection
# for every document we have a probability distribution of its contained topics
theta <- tm_result$topics
dim(theta) # nDocs(dtm) distributions over K topics



terms(topic_model, 30)

# concatenate the five most likely terms of each topic to a string that represents a pseudo-name for each topic.
top_5terms_per_topic <- terms(topic_model, 5)
topic_names <- apply(top_5terms_per_topic, 2, paste, collapse = " ")


# visualize topics as word cloud
topic_to_viz <- 2 # change for your own topic of interest
# topicToViz <- grep('action', topicNames)[1] # Or select a topic by a term contained in its name
# select to 40 most probable terms from the topic by sorting the term-topic-probability vector in decreasing order
topterms <- sort(tm_result$terms[topic_to_viz, ], decreasing = TRUE)[1:40]
words <- names(topterms)
# extract the probabilites of each of the top terms
probabilities <- sort(tm_result$terms[topic_to_viz, ], decreasing = TRUE)[1:40]
# visualize the terms as wordcloud
mycolors <- brewer.pal(8, "Dark2")
wordcloud(words, probabilities, random.order = FALSE, color = mycolors)

sample_ids <- c(2, 100, 200)
lapply(corpus[sample_ids], as.character)

N <- length(sample_ids)
# get topic proportions form example documents
topic_proportion_examples <- theta[sample_ids, ]
colnames(topic_proportion_examples) <- topic_names
viz_df <- melt(cbind(data.frame(topic_proportion_examples), document = factor(1:N)), variable.name = "topic", id.vars = "document")

ggplot(data = viz_df, aes(topic, value, fill = document), ylab = "proportion") +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip() +
  facet_wrap(~document, ncol = N)
