# Applied Data Science @ Columbia
## Fall 2023
## Project 1: What made you happy today?

![image](figs/title.jpeg)
# Load necessary libraries
library(tidyverse)
library(tidytext)
library(SnowballC)

happy_data <- read.csv("cleaned_hm.csv")

str(happy_data)

# Check for missing values
summary(is.na(happy_data))

# Perform text preprocessing
happy_data_clean <- happy_data %>%
  mutate(cleaned_text = tolower(cleaned_hm)) %>%
  unnest_tokens(word, cleaned_text) %>%
  anti_join(stop_words) %>%
  mutate(word_stem = wordStem(word))

# Conduct sentiment analysis
sentiment_scores <- happy_data_clean %>%
  inner_join(get_sentiments("afinn"), by = "word_stem") %>%
  group_by(hmid) %>%
  summarize(sentiment_score = sum(value)) %>%
  ungroup()

# Visualize sentiment distribution
ggplot(sentiment_scores, aes(x = sentiment_score)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Sentiment Distribution of Happy Moments")

# Topic modeling using LDA
happy_text <- happy_data_clean %>%
  select(hmid, word_stem) %>%
  filter(!is.na(word_stem)) %>%
  group_by(hmid) %>%
  summarize(happy_text = paste(word_stem, collapse = " "))

# Perform topic modeling

# Write your blog post in R Notebook documenting your analysis

