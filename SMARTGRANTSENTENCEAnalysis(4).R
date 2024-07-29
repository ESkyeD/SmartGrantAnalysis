# Install and load required packages
# install.packages("stringr")
# install.packages("dplyr")
# install.packages("text2vec")
library(stringr)
library(dplyr)
library(text2vec)

# Split the documents into individual data frames
Draft <- documents[1,]
Bellevue <- documents[2,]
Broward <- documents[3,]
Cleveland <- documents[4,]
Colorado <- documents[5,]
Louisville <- documents[6,]
Phoenix <- documents[7,]
Vegas <- documents[8,]
Fontana <- documents[9,]

MacroIndividualList <- list(Draft, Bellevue, Broward, Cleveland, Colorado, Louisville, Phoenix, Vegas, Fontana)

process_paragraphs_to_sentences <- function(df) {
  sentences <- str_split(df$grant, "(?<=[.!?])\\s+")
  sentences <- unlist(sentences)
  
  sentence_df <- data.frame(Sentences = sentences, stringsAsFactors = FALSE)
  name_df <- data.frame(name = rep(df$name, length(sentences)), stringsAsFactors = FALSE)
  
  final_df <- bind_cols(name_df, sentence_df)
  return(final_df)
}

PostProcessList <- lapply(MacroIndividualList, process_paragraphs_to_sentences)
FinalDataFrame <- bind_rows(PostProcessList)

# Keep the original sentences for later use
FinalDataFrame2 <- bind_rows(PostProcessList)

# Preprocess the sentences
FinalDataFrame$Sentences <- tolower(FinalDataFrame$Sentences)
FinalDataFrame$Sentences <- removeWords(FinalDataFrame$Sentences, stopwords("english"))
FinalDataFrame$Sentences <- removePunctuation(FinalDataFrame$Sentences)
FinalDataFrame$Sentences <- removeNumbers(FinalDataFrame$Sentences)
FinalDataFrame <- FinalDataFrame %>%
  mutate(Sentences = ifelse(Sentences == "" | Sentences == " ", NA, Sentences))

# Replace town names
town_names <- c("bellevue", "fontana", "las vegas", "vegas", "cleveland", "phoenix", "broward", "vegas", "colorado", "raleigh", "louisville")
replacement_vector <- setNames(rep("townname", length(town_names)), town_names)
FinalDataFrame <- FinalDataFrame %>%
  mutate(Sentences = str_replace_all(Sentences, replacement_vector))

# Replace state names
State_names <- c(
  "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", 
  "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", 
  "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", 
  "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
  "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", 
  "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", 
  "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", 
  "Wisconsin", "Wyoming"
)
replacement_vector <- setNames(rep("statename", length(State_names)), State_names)
FinalDataFrame <- FinalDataFrame %>%
  mutate(Sentences = str_replace_all(Sentences, replacement_vector))

# Create text data and preprocess
text_data <- data.frame(name = FinalDataFrame$name, text = FinalDataFrame$Sentences)
text_data$text <- stripWhitespace(text_data$text)

text_data_sentences <- text_data %>%
  mutate(sentences = strsplit(text, "(?<!\\w\\.\\w.)(?<![A-Z][a-z]\\.)(?<=\\.|\\?)\\s", perl=TRUE)) %>%
  unnest(sentences)

quanteda_corpus <- corpus(text_data_sentences$sentences)

# Create an iterator over tokens
tokens <- word_tokenizer(text_data_sentences$sentences)
vocab <- create_vocabulary(itoken(tokens, progressbar = FALSE))
it <- itoken(tokens, progressbar = FALSE)
vectorizer <- vocab_vectorizer(vocab)
tcm <- create_tcm(it, vectorizer, skip_grams_window = 4)

# Assuming `word_vectors` is previously trained
# Train a GloVe model (optional)
# glove <- GlobalVectors$new(rank = 50, x_max = 10)
# wv_main <- glove$fit_transform(tcm, n_iter = 10, convergence_tol = 0.01)
# wv_context <- glove$components
# word_vectors <- wv_main + t(wv_context)

compute_sentence_embedding <- function(sentence, word_vectors, vocab) {
  tokens <- unlist(strsplit(sentence, " "))
  valid_tokens <- tokens[tokens %in% rownames(word_vectors)]
  if (length(valid_tokens) == 0) return(rep(0, ncol(word_vectors)))
  colMeans(word_vectors[valid_tokens, , drop = FALSE])
}

sentence_embeddings <- t(sapply(text_data_sentences$sentences, compute_sentence_embedding, word_vectors = word_vectors, vocab = vocab))
sentence_embeddings <- as.matrix(sentence_embeddings)

cosine_similaritiesSentence <- sim2(sentence_embeddings, method = "cosine", norm = "l2")
filtered_matrix <- ifelse(cosine_similaritiesSentence >= 0.8 & cosine_similaritiesSentence <= 0.99, cosine_similaritiesSentence, NA)
indices <- which(!is.na(filtered_matrix), arr.ind = TRUE)

# Create a data frame with the indices, their corresponding values, and grant names
result <- data.frame(
  Sentence1 = FinalDataFrame$Sentences[indices[, 1]],
  Grant1 = FinalDataFrame$name[indices[, 1]],
  Sentence2 = FinalDataFrame$Sentences[indices[, 2]],
  Grant2 = FinalDataFrame$name[indices[, 2]],
  Similarity = filtered_matrix[indices]
)

result <- result %>%
  mutate(Similarity = ifelse(Grant1 == Grant2, NA, Similarity)) %>%
  na.omit()

NoPracticeresult <- result %>%
  mutate(Similarity = ifelse(Grant1 == "A Practice Grant", NA, Similarity)) %>%
  mutate(Similarity = ifelse(Grant2 == "A Practice Grant", NA, Similarity)) %>%
  na.omit()

# Count the occurrences of each sentence
sentence_counts <- table(indices[, 1])
count_df <- data.frame(Sentence = FinalDataFrame$Sentences[as.numeric(names(sentence_counts))], Count = as.vector(sentence_counts))

DoubleFilter <- count_df %>%
  mutate(Count = ifelse(Count < 3, NA, Count)) %>%
  na.omit()

# Mapping back to original sentences
original_sentences <- FinalDataFrame2$Sentences
preprocessed_sentences <- FinalDataFrame$Sentences

# Create a data frame with original and preprocessed sentences
sentence_df <- data.frame(
  Original = original_sentences,
  Preprocessed = preprocessed_sentences,
  stringsAsFactors = FALSE
)



### Do Jaccard Similarity on this grant stuff
# Function to compute Jaccard similarity between two sets of words
jaccard_similarity <- function(a, b) {
  intersection <- length(intersect(a, b))
  union <- length(union(a, b))
  return(intersection / union)
}

# Function to map filtered sentences back to original sentences using Jaccard similarity
map_to_original_jaccard <- function(filtered_sentence, original_sentences) {
  filtered_tokens <- unlist(strsplit(filtered_sentence, " "))
  best_match <- NULL
  best_similarity <- 0
  
  for (original_sentence in original_sentences) {
    original_tokens <- unlist(strsplit(original_sentence, " "))
    similarity <- jaccard_similarity(filtered_tokens, original_tokens)
    
    if (similarity > best_similarity) {
      best_similarity <- similarity
      best_match <- original_sentence
    }
  }
  
  return(best_match)
}

# Map each filtered sentence to the best matching original sentence
mapped_sentences <- sapply(DoubleFilter$Sentence, map_to_original_jaccard, original_sentences = FinalDataFrame2$Sentences)

# Add the mapped original sentences to the double filtered data frame
DoubleFilter$MappedOriginal <- mapped_sentences
TripleFilter<-DoubleFilter

DoubleFilter<-DoubleFilter%>%mutate(Count,ifelse(Count==3,NA,Count))
DoubleFilter<-na.omit(DoubleFilter)
options(width=2000)
count_words <- function(sentence) {
  words <- strsplit(sentence, "\\s+")[[1]]
  return(length(words))
}

# Filter out sentences with fewer than 4 words
DoubleFilter<- DoubleFilter%>%
  filter(sapply(Sentence, count_words) > 4)

DoubleFilter<-DoubleFilter%>%
  arrange(desc(Count))


DoubleFilter$MappedOriginal <- sapply(DoubleFilter$MappedOriginal, function(x) paste(x, collapse = " "))
FinalDoubleDone<-subset(DoubleFilter,select=-Sentence)
FinalDoubleDone<-na.omit(FinalDoubleDone)


write.csv(FinalDoubleDone, "filtered_sentences.csv", row.names=FALSE)
