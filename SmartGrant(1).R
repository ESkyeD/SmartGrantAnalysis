library(stringr)
library(ggplot2)
library(cowplot)
library(quanteda)
library(quanteda.corpora)
library(quanteda.textplots)
library(quanteda.textstats)
library(stopwords)
library (dplyr)
library(rio)
library(ggplot2)
library(DescTools)
library(tidyr)
library(tibble)
library(haven)
library(gridExtra)
library(reshape2)
###### 2. Reading in documents and creating a corpus. ###### 

# There are a few of different standard ways that you may save your data (i.e., documents) in:
# 1. as a data frame, where the text is in a column and associated information or metadata is in the neighboring columns;
# 2. as a collection of separate text documents that are housed in the same folder. 
# First, let's grab the list of ile names that we need to read in.
grant.list = list.files("./SMART GRANT")
grant.list = paste("./SMART GRANT/",grant.list,sep="")
# Next let's read in the documents. 
documents = data.frame("filepath" = rep(NA,length(grant.list)),
                       "name" = rep(NA,length(grant.list)),
                       "grant" = rep(NA,length(grant.list)))

for(i in 1:length(grant.list)){
  
  cat("currently working on bill:",i,"\n")
  
  # log the file path 
  documents$filepath[i] = grant.list[i]
  #add a file name
  documents$name[i]<-documents$filepath[i]
  documents$name[i]=sub("./SMART GRANT/","",documents$name[i])
  documents$name[i]=sub(".txt","",documents$name[i])
  # get the text of the bill
  raw_text = readLines(grant.list[i])
  # collapse it together into a string
  if(length(raw_text)>1){
    raw_text = paste0(raw_text,collapse = " ")
  }
  # store it in the data frame
  documents$grant[i] = raw_text
}

# We can create a quanteda corpus object out of this character vector:
my_corp <- corpus(x = documents$grant,
                  docnames = documents$name,
                  meta = list(source = "8 Smart Grants"))
summary(my_corp)

#### Finding Key Words in Context based off of project demands is very easy kwic(my_tokens_defaults,pattern=phrase("whatever words or sentence you want to put in here"),window=(some number between 1-20)
my_tokens_defaults<-tokens(my_corp)

my_tokens <- tokens(my_corp,
                    what = "word",
                    remove_punct = TRUE, # removes punctuation
                    remove_symbols = TRUE, # removes unicode symbols
                    remove_numbers = TRUE, # removes numbers but not words starting with digits
                    remove_url = TRUE, # eliminates URLs beginning with http/https
                    remove_separators = TRUE, # removes separators as categorized in unicode
                    include_docvars = FALSE) # whether to pass on the docvars to the tokens object
dfm_nostopwords  <- dfm_remove(doc_term_matrix,
                               stopwords("english"))

doc_term_matrix <- dfm(my_tokens,
                       tolower = TRUE)
dfm_stemmed <- dfm_wordstem(dfm_nostopwords)

top_terms = data.frame("Rank" = 1:12,
                       "Defaults" = names(topfeatures(doc_term_matrix,12)),
                       "Processing" = names(topfeatures(dfm_stemmed,12)))

freq_weight = textstat_frequency(dfm_nostopwords, 
                                 n = 30)
ggplot(data = freq_weight, 
       aes(x = nrow(freq_weight):1, 
           y = frequency)) +
  geom_point() +
  coord_flip() +
  scale_x_continuous(breaks = nrow(freq_weight):1,
                     labels = freq_weight$feature) +
  labs(x = NULL, y = "Relative frequency")

cleaned_text<-as.data.frame(dfm_nostopwords)

word_freq<-dfm_nostopwords%>%

library(wordcloud)
library(RColorBrewer)


Grant_Data<-data.frame(Name=documents$name,Text=documents$grant)




word_freq <- cleaned_text %>%
  count(word, sort = TRUE)



####So Basically from this we kind of just know the KWIC used by each document, We're now going to make a Term Frequency Inverse Matrix, to determine if these winners in the Sensor and Smart Traffic Category are all applying under the same objective or have variance.
install.packages("tidytext")
library(tidytext)


term_frequency<-tokenized_data%>%count(Name,word,sort=TRUE)




# Calculate Inverse Document Frequency (IDF)
total_documents <- n_distinct(term_frequency$Name)
idf <- term_frequency %>%
  group_by(word) %>%
  summarize(idf = log(total_documents / n()))
tf_idf <- term_frequency %>%
  inner_join(idf, by = "word") %>%
  mutate(tf_idf = n * idf) %>%
  arrange(desc(tf_idf))

tf_idf_matrix <- tf_idf %>%
  select(Name, word, tf_idf) %>%
  spread(key = word, value = tf_idf, fill = 0)

# Print the TF-IDF Matrix
print(tf_idf_matrix)

tf_idf_long <- tf_idf_matrix %>%
  gather(key = "word", value = "tf_idf", -Name)

# Plot a heatmap
ggplot(tf_idf_long, aes(x = word, y = as.factor(Name), fill = tf_idf)) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Words", y = "Documents", fill = "TF-IDF")
 
