libraries <- c("stringr", "ggplot2", "cowplot", "quanteda", "quanteda.corpora", 
               "quanteda.textplots", "quanteda.textstats", "stopwords", "dplyr", 
               "rio", "DescTools", "tidyr", "tibble", "haven", "gridExtra", 
               "reshape2", "tidytext", "wordcloud", "RColorBrewer")

install_missing <- function(libraries) {
  for (lib in libraries) {
    if (!require(lib, character.only = TRUE)) {
      install.packages(lib, dependencies = TRUE)
      library(lib, character.only = TRUE)
    }
  }
}

install_missing(libraries)

# Function to read documents
read_documents <- function(directory) {
  file_list <- list.files(directory, full.names = TRUE)
  documents <- data.frame("filepath" = file_list,
                          "name" = basename(file_list),
                          "grant" = sapply(file_list, function(file) {
                            raw_text <- tryCatch(readLines(file, warn = FALSE), error = function(e) NA)
                            paste(raw_text, collapse = " ")
                          }))
  return(documents)
}

# Read in documents
documents <- read_documents("./SMART GRANT")

# Create corpus
my_corp <- corpus(documents$grant, docnames = documents$name, meta = list(source = "8 Smart Grants"))

# Tokenize the corpus
my_tokens <- tokens(my_corp, what = "word", remove_punct = TRUE, remove_symbols = TRUE, 
                    remove_numbers = TRUE, remove_url = TRUE, remove_separators = TRUE)

# Create Document-Feature Matrix (DFM)
doc_term_matrix <- dfm(my_tokens, tolower = TRUE)
dfm_nostopwords <- dfm_remove(doc_term_matrix, stopwords("english"))
dfm_stemmed <- dfm_wordstem(dfm_nostopwords)

# Find top terms
top_terms <- data.frame("Rank" = 1:12,
                        "Defaults" = names(topfeatures(doc_term_matrix, 12)),
                        "Processing" = names(topfeatures(dfm_stemmed, 12)))

# Plot term frequency
freq_weight <- textstat_frequency(dfm_nostopwords, n = 30)
ggplot(data = freq_weight, aes(x = nrow(freq_weight):1, y = frequency)) +
  geom_point() +
  coord_flip() +
  scale_x_continuous(breaks = nrow(freq_weight):1, labels = freq_weight$feature) +
  labs(x = NULL, y = "Relative frequency")

# Convert DFM to tidy format for further analysis
tidy_dfm <- tidy(dfm_nostopwords)

# Create long format for word frequency
word_freq <- tidy_dfm %>%
  group_by(term) %>%
  summarize(count = sum(count), .groups = 'drop') %>%
  arrange(desc(count))

# Create Grant Data
Grant_Data <- data.frame(Name = documents$name, Text = documents$grant)

# Tokenize data for term frequency calculation
tokenized_data <- tidy_dfm %>%
  group_by(document, term) %>%
  summarize(n = sum(count), .groups = 'drop') %>%
  rename(Name = document)

# Calculate Inverse Document Frequency (IDF)
total_documents <- n_distinct(tokenized_data$Name)
idf <- tokenized_data %>%
  group_by(term) %>%
  summarize(idf = log(total_documents / n()), .groups = 'drop')

# Calculate TF-IDF
tf_idf <- tokenized_data %>%
  inner_join(idf, by = "term") %>%
  mutate(tf_idf = n * idf) %>%
  arrange(desc(tf_idf))

# Create TF-IDF Matrix
tf_idf_matrix <- tf_idf %>%
  select(Name, term, tf_idf) %>%
  pivot_wider(names_from = term, values_from = tf_idf, values_fill = 0)

# Print TF-IDF Matrix
print(tf_idf_matrix)

# Convert TF-IDF Matrix to long format for heatmap plotting
tf_idf_long <- tf_idf_matrix %>%
  pivot_longer(cols = -Name, names_to = "term", values_to = "tf_idf")

# Plot heatmap
ggplot(tf_idf_long, aes(x = term, y = as.factor(Name), fill = tf_idf)) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Words", y = "Documents", fill = "TF-IDF")
