#Loading libraries
libraries <- c("tm", "tidytext", "ggplot2", "wordcloud", "syuzhet", "dplyr", "tibble", "textstem", "textdata", "tidyr")

install.packages(libraries)# Comment out after first execution

for (lib in libraries) {
  library(lib, character.only=TRUE) #Library takes function names without quotes, character only must be used in a loop of this kind.
}

#Import the csv file
filepath <- "C:\\Users\\bryan\\Downloads\\Restaurant reviews.csv" # Define file path. requires df to be replaced by \\./ works on Mac (apparently). as_tibble(read.csv(filepath, stringsAsFactors = FALSE)) # Since we have text data we do not want this read as a factor
df <- as_tibble(read.csv(filepath, stringsAsFactors = FALSE)) # Since we have text data we do not want this read as a factor


#Inspecting Data
summary(df)

#Selecting Data
df <- df[,1:4] # Select first 4 columns
df <- na.omit (df) # Removes all rows containing null values
df$Review_no <- 1:nrow (df) # Adds identifier column to reviews


#Sampling Data
set.seed(1) # Set random seed for repeatability
# Take sample of 5 restaurants
sample_index <- sample (length (unique (df $Restaurant)), 5) #Sample (size of population, size of sample), returns index for sample
sampled_restaurants <- unique (df$Restaurant) [sample_index] # Take restaurants at index defined
previously
df <- df %>%
  filter (Restaurant %in% sampled_restaurants) # Select only rows where restaurant is one of sampled_restaurants
print(summary (df))
head(df)

#Tokenizing
word_tokenized_data <- df %>%
  unnest_tokens (output = word, input = "Review", token = "words", to_lower = TRUE) # Token
bigram_tokenized_data <- df %>%
  unnest_tokens (output = bigram, input = "Review", token = "ngrams", n=2, to_lower = TRUE)

#Creating a Plot
word_counts <- word_tokenized_data %>%
  count (word, sort = TRUE) # Counts the occurences of each word and sorts.
ggplot(word_counts [1:10, ], aes (x = reorder (word, n), y = n)) + # Plots first 10 rows axis and n on the y axis
  geom_col (fill = "blue") + # Sets colours of bars to blue
  labs (x = "words", y = "Frequency") + # Defines x and y labels
  coord_flip() + # Flips coordinates so words go on the y axis (for readability)
  theme_minimal() # Sets theme of visualisation

#Creating a word cloud
set.seed(1)
wordcloud (words = word_counts$word, freq = word_counts$n, min.freq = 10,
           random.order=FALSE, random.color=FALSE, colors = sample (colors(), size = 10))
# words = vector of words, freq = vector of frequencies, min.freq = minimum frequency to plot, random.order=FALSE means words are plotted in order of n, random.color=FALSE colors according to frequency and colors key word specifies colors to use


#Data Cleaning
clean_tokens <- word_tokenized_data %>%
  anti_join(stop_words, by = "word") # Removes stop words
clean_tokens $word <- gsub ("[^a-zA-Z]", "", clean_tokens $word) %>% # Remove special characters and numbers
  na_if("") %>% # Replaces empty strings with NA 
  lemmatize_words() # Lemmatizes text
clean_tokens <- na.omit(clean_tokens) # Removes null values

#Creating a Grouped Plot
# Grouped words
top_words <- top_n (word_counts, 10,n) $word # Gets a vector of top 10 words
# Groups clean_tokens by restaurant and counts the number of occurences of each word, and filters to only the top 10 words. 
grouped_count <- group_by(clean_tokens, Restaurant) %>%
count (word) %>%
  filter (word %in% top_words)
grouped_count$word <- factor (grouped_count$word, levels = top_words[length(top_words):1]) # orders the top words according to overall frequency
ggplot (data = grouped_count, aes (x = word, yn, fill = Restaurant)) + # Fill keyword allows groupings
  geom_col (position = "dodge") + # position = dodge creates grouped bar chart
  labs (x = "words", y = "Fill", fill = "Restaurant") +
  coord_flip() +
  theme_minimal()


#Applying Bing Lexicon
# Create dataset containing only words with associated sentiment & adds sentiment column.
sentiment_data <- clean_tokens %>%
  inner_join(get_sentiments("bing"), by = "word") # Joins lexicon to dataset using only words that are in both.
# Calculate Sentiment scores for each review
sentiment_score <- sentiment_data %>%
  group_by (Review_no) %>%
  summarize (bing_sentiment = sum(sentiment == "positive") - sum (sentiment =="negative")) # Calculates sentiment score as sum of number of positive and negative sentiments
# Merge with df
df_with_sentiment = df %>%
  inner_join (sentiment_score, by = "Review_no")


#Inspecting Reviews
worst_review = df_with_sentiment [order(df_with_sentiment$bing_sentiment) [1], "Review"] 
print(worst_review)
best_review = df_with_sentiment [order(df_with_sentiment$bing_sentiment, decreasing = TRUE) [1], "Review"]
print(best_review)

#Sentiment Histogram
ggplot(df_with_sentiment, aes (x = bing_sentiment)) + geom_histogram (binwidth = 1)


#Average Sentiment by Restaurant
# Average Sentiment by Restaurant
restaurant_sentiment <- df_with_sentiment %>%
  group_by (Restaurant) %>%
  summarize (Average_Bing_Sentimet = mean (bing_sentiment))
ggplot(restaurant_sentiment, aes (x = reorder (Restaurant, Average_Bing_Sentimet), y =
                                    Average_Bing_Sentimet, fill = Restaurant)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs (title = "Average Sentiment Score by Restaurant", x = "Restaurant", y = "Average Sentiment Score")
  

#Box Plot (Sentiment vs. Rating)
# Box Plot of Sentiment against rating
ggplot(df_with_sentiment, aes (x = Rating, y = bing_sentiment)) +
  geom_boxplot() +
  labs (title = "Box Plot of Bing Sentiment Score vs. Rating",
        x = "Rating",
        y = "Sentiment Score")


#Applying AFFIN Lexicon
# Create dataset containing only words with associated sentiment & adds sentiment column.
sentiment_data <- clean_tokens %>%
  inner_join (get_sentiments ("afinn"), by = "word")
# Calculate Sentiment scores for each review
sentiment_score <- sentiment_data %>%
  group_by (Review_no) %>% summarize (afinn_sentiment = sum(value))
# Merge with df
df_with_sentiment = df_with_sentiment %>% inner_join(sentiment_score, by = "Review_no")


# Scatter Plot of Bing vs. AFINN Sentiment
ggplot(df_with_sentiment, aes (x = bing_sentiment, y = afinn_sentiment)) +
  geom_point() +
  labs (title = "Scatter Plot of Bing vs. AFINN Sentiment Scores",
        x = "Bing Sentiment Score",
        y = "AFINN Sentiment Score")
        
#Applying NRC Lexicon
# Create dataset containing only words with associated sentiment & adds sentiment column.
emotion_data <- clean_tokens %>% 
inner_join (get_sentiments ("nrc"), by = "word")
# Calculate Sentiment scores for each review
emotion_count <- emotion_data %>%
group_by (Review_no) %>%
count (sentiment)
# Pivots data so that there is a column associated with each emotion
wide_emotion_data <- emotion_count %>%
pivot_wider (names_from = sentiment, values_from = n, values_fill = list(n = 0))
# Merge with df
df_with_sentiment = df_with_sentiment %>%
inner_join (wide_emotion_data, by = "Review_no")
        

#Heatmap of Emotion by Restaurant
long_df <- df_with_sentiment %>%
  pivot_longer(cols = c("joy", "positive", "trust", "anticipation", "surprise", "sadness", "negative", "anger", "disgust", "fear"),
              names_to = "Emotion",
              values_to = "Intensity")
emotion_scores <- long_df %>% 
  group_by (Restaurant, Emotion) %>%
  summarize (avg_intensity = mean(Intensity))
ggplot(emotion_scores, aes (x = Restaurant, y = Emotion, fill = avg_intensity)) +
  geom_tile() + # Creates the heatmap tiles
  scale_fill_gradient2 (low = "blue", high = "red") + # Adjust colors
  labs (x = "Restaurant", y = "Emotion", fill = "Intensity") +
  theme (axis.text.x = element_text(angle = 30, hjust=1)) # Rotates text to 30 degrees

#Topic Modelling
libraries <- c("tm", "tidytext", "ggplot2", "wordcloud", "syuzhet", "dplyr", "tibble", "textstem", "textdata", "tidyr", "Matrix", "topicmodels", "stringr", "reshape2", "LDAvis", "jsonlite")
#install.packages (libraries) # Comment out after first execution
for (lib in libraries) {
library(lib, character.only=TRUE) #Library takes function names without quotes, character only must be used in a loop of this kind
}
install.packages('stringr')
library(stringr)


filepath <- 'C:\\Users\\bryan\\Downloads\\wiki_movie_plots_deduped.csv' # Define file path. Windows requires \ to be replaced by \\. / Works on Mac (apparently). 
df <- as_tibble(read.csv(filepath, stringsAsFactors = FALSE)) # Since we have text data we do not want this read as a factor
# Inspect summary and first few rows of data

print(summary(df))
print(head(df))

#Data Selection and Sampling

# Select Columns
df <- df %>%
  select(c("Release.Year", "Title", "Genre", "Plot")) %>% 
  filter(str_count(Plot) >= 200 & str_count(Plot) <= 400)



# Replace values of "unknown in Genre with NA 
df$Genre <- na_if(df$Genre, "unknown")

df <- na.omit(df) # Removes all rows containing null values

df$Movie_no <- 1:nrow (df)
if (nrow (df) > 1000) {
          set.seed(1) # for reproducibility
          df <- sample_n(df, 1000)
}
          
#Creating a Term-Document Matrix
          
# Convert text column to corpus
corpus <- VCorpus(VectorSource(df$Plot))
          
# Apply cleaning
corpus <- tm_map (corpus, content_transformer(tolower)) %>%
  tm_map(content_transformer(function(x) gsub ("[^a-zA-Z]", "",x))) %>% 
  tm_map (removeWords, stopwords ("en")) %>%
  tm_map(stemDocument)



# Convert to a term document matrix
tdm <- TermDocumentMatrix (corpus, control = list(wordLengths = c(0, 100)))
tdm_matrix <- as.matrix(tdm)


#Visualisation of Term Frequencies
term_frequencies <- rowSums(tdm_matrix)

# Create a data frame for plotting
term_frequency_df <- data.frame(term = names(term_frequencies), frequency = term_frequencies)

# Sort the data frame by frequency in descending order and select the top 10
top_terms <- term_frequency_df %>%
  arrange (desc (frequency)) %>%
  head (10)
# Display the top 10 terms 
print(top_terms)

# Create the histogram
ggplot(term_frequency_df, aes (x = frequency)) +
  geom_histogram(binwidth = 1) + labs (title = "Histogram of Term Frequencies",
                                       x = "Term Frequency",
                                       y = "Number of Terms") +
  theme_minimal()


#Selecting Terms to Remove

# Find terms that appear in more than 10% of documents
frequent_terms <- findFreqTerms (tdm, lowfreq = 0.1 * ncol (tdm_matrix))
# Find terms that appear in less than 1% of documents
rare_terms <- findFreqTerms (tdm, highfreq = 0.01 * ncol (tdm_matrix))
print("Frequent Terms") 
print (frequent_terms)
print("First 20 Infrequent Terms")
print(rare_terms [1:20])
# Edit list of frequent words to keep useful ones
to_keep <- c("famili", "love", "murder")
to_remove <- frequent_terms [!frequent_terms %in% to_keep]
                             

#Removing Terms
filtered_tdm_matrix <- tdm_matrix[!rownames (tdm_matrix) %in% to_remove, ] 
filtered_tdm_matrix <- filtered_tdm_matrix[!rownames (filtered_tdm_matrix) 
%in% rare_terms, ]

# Remove O sum columns from tdm.
# Calculate column sums
column_sums <- colSums (filtered_tdm_matrix)

# Identify columns that are all zeros
zero_columns <- which(column_sums == 0)

# Remove these columns
if (length (zero_columns) > 0) {
  # Remove these columns
  filtered_tdm_matrix <- filtered_tdm_matrix[, -zero_columns]
} else {
  # If no columns are all zeros, just use the original matrix 
  print("No zero columns in TDM matrix")
}

#Applying LDA
dtm <- t(filtered_tdm_matrix) Ida_model <- LDA (dtm, k = 5)

    
#Visualise Distribution of Top Words in Topics

topics <- tidy (lda_model, matrix = "beta")
topics
top_terms <- topics %>%
  group_by(topic) %>%
  top_n (10, beta) %>%
  ungroup () %>%
  rrange (topic, -beta)
top_terms %>%
  ggplot (aes (x = reorder (term, beta), y = beta, fill =
                 factor (topic))) +
  geom_col (show.legend = FALSE) +
  facet_wrap (~ topic, scales = "free") +
  coord_flip()
    

#Perplexity Plot

range_k <- seq(2, 10, by = 1) # Adjust the range as needed
perplexities <- sapply (range_k, function (k) {
  model <- LDA (dtm, k = k, control = list(seed = 1))
  perplexity (model)
})
# Plotting perplexities
plot(range_k, perplexities, type = "b", xlab = "Number of Topics", ylab = "Perplexity")
  
  
#LDAvis Code

set.seed (1)
lda_model <- LDA (dtm, k = 10)
Ida_vis_data <- createJSON (phi = posterior (lda_model)$terms,
                            theta = posterior(lda_model)$topics,
                            doc.length = rowSums (as.matrix(dtm)), vocab = colnames(as.matrix(dtm)),
                            term.frequency =
                              colsums (as.matrix(dtm)))
servis(lda_vis_data)
  
#Further exploration
#Implementation of TF-IDF
tdm <- TermDocumentMatrix(corpus, weighting <- weightTfIdf)

#BERTopic
library(NMF)
tdm <- TermDocumentMatrix(corpus) result <- nmf(tdm, rank = 5, .options = 'NMF') # 'rank' is the number of
topics
library(stm)
dtm <- t (TermDocumentMatrix(corpus))
meta_data <- your_metadata # Document metadata in dataframe with docs as rows stm_out <- stm (documents = dtm, data = meta_data, K = 5)
stm_out <- stm(documents = dtm, data = meta_data, K = 5)

#Dependency Parsing
install.packages ("spacyr") # Run on first execution library("spacyr")
spacy_install() # Run on first execution
spacy_initialize(model = "en_core_web_sm")


``{r using spacy}
corpus <- c("Apple was founded by Steve Jobs in California.",
            "Microsoft was founded by Bill Gates and Paul Allen.",
            "The Eiffel Tower is located in Paris.")
# Perform Named Entity Recognition
entities <- spacy_parse(corpus, dependency = TRUE)
# View the entities
print(entities)

