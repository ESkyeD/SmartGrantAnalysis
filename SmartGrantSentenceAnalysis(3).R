#install.packages("text2vec")
library(tm)
library(quanteda)
library(text2vec)
library(dplyr)
library(tidyverse)

text_files_dir <- "./SMART GRANT"

# List all text files in the directory
text_files <- list.files(text_files_dir, pattern = "\\.txt$", full.names = TRUE)

# Read the text files into a Corpus
corpus <- Corpus(DirSource(text_files_dir, pattern = "\\.txt$"))

# Convert Corpus to a data frame
text_data <- data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE)

# Convert text to lower case
text_data$text <- tolower(text_data$text)
text_data$text <- removeWords(text_data$text, stopwords("english"))
text_data$text<-removeWords(text_data$text,top_terms$Processing)
# Remove punctuation
text_data$text <- removePunctuation(text_data$text)

# Remove numbers
text_data$text <- removeNumbers(text_data$text)

# Remove stopwords

# Strip whitespace
text_data$text <- stripWhitespace(text_data$text)

text_data_sentences <- text_data %>%
  mutate(sentences = strsplit(text, "(?<!\\w\\.\\w.)(?<![A-Z][a-z]\\.)(?<=\\.|\\?)\\s", perl=TRUE)) %>%
  unnest(sentences)
quanteda_corpus <- corpus(text_data_sentences$sentences)
### ok

# Create an iterator over tokens
tokens <- word_tokenizer(text_data_sentences$sentences)

# Create a vocabulary
vocab <- create_vocabulary(itoken(tokens, progressbar = FALSE))
it<-itoken(tokens,progressbar = FALSE)
# Create a vectorizer
vectorizer <- vocab_vectorizer(vocab)
#Create a TCM
tcm <- create_tcm(it, vectorizer, skip_grams_window = 6)

# Train a GloVe model
glove <- GlobalVectors$new(rank = 50, x_max = 10)
wv_main <- glove$fit_transform(tcm, n_iter = 10, convergence_tol = 0.01)
wv_context <- glove$components
word_vectors <- wv_main + t(wv_context)


compute_sentence_embedding <- function(sentence, word_vectors, vocab) {
  tokens <- unlist(strsplit(sentence, " "))
  valid_tokens <- tokens[tokens %in% rownames(word_vectors)]
  if (length(valid_tokens) == 0) return(rep(0, ncol(word_vectors)))
  colMeans(word_vectors[valid_tokens, , drop = FALSE])
}
sentence_embeddings <- t(sapply(text_data_sentences$sentences, compute_sentence_embedding, word_vectors = word_vectors, vocab = vocab))
sentence_embeddings <- as.matrix(sentence_embeddings)
# Create sentence embeddings by averaging word vectors

cosine_similarities <- sim2(sentence_embeddings, method = "cosine", norm = "l2")
# Calculate cosine similarity between sentence embeddings
  


