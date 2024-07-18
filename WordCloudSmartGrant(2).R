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

tokenized_text <- documents %>%
  unnest_tokens(word, grant)

# Remove stop words
data("stop_words")
cleaned_text <- tokenized_text %>%
  anti_join(stop_words)
cleaned_text <- cleaned_text %>%
  filter(!grepl("^[0.0-9.0]+$", word))
cleaned_text <- cleaned_text %>%
  filter(!grepl("[0-9]", word))
word_freq <- cleaned_text %>%
  count(word, sort = TRUE)

#### Text Cloud Metrics, Which show us that duh Projects the #1 word lol, 
set.seed(1234) # for reproducibility
wordcloud(words = word_freq$word, 
          freq = word_freq$n, 
          min.freq = 2, 
          max.words = 200, 
          random.order = FALSE, 
          rot.per = 0.35, 
          colors = brewer.pal(8, "Dark2"))

###What is universally true though?
term_frequency <- cleaned_text %>%
  count(name, word, sort = TRUE)

# Calculate TF-IDF
tf_idf <- term_frequency %>%
  bind_tf_idf(word, name, n)

# Filter out words that appear in only one document
filtered_tf_idf <- tf_idf %>%
  filter(idf != log(nrow(documents)))

set.seed(1234) # for reproducibility
wordcloud(words = filtered_tf_idf$word, 
          freq = filtered_tf_idf$tf_idf, 
          scale = c(4, 0.5),
          min.freq = 0.1, 
          max.words = 500, 
          random.order = FALSE, 
          rot.per = 0.35, 
          colors = brewer.pal(8, "Dark2"))
