# QTM2000 Final Project
# Professor Mathaisel
# Project title: "Exploratory Analysis and fake news classification on Buzzfeed News".

# Data dictionary: Refer to the Exam 2 documentation.
# There are two datsets of Buzzfeed news: one dataset of fake news 
# and another dataset of real news. 

# References:
# https://www.kaggle.com/clmentbisaillon/fake-and-real-news-dataset
# https://www.kaggle.com/omkarkonduskar/fake-news-classification-using-rf-99-8-accuracy?select=True.csv
# https://www.kaggle.com/michaleczuszek/fake-news-analysis
#################################################################
# Installing Packages: Only need to install once on your hard disk
install.packages("readr")
install.packages("dplyr")
install.packages("stringr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("tm")
install.packages("textstem")
install.packages("tidytext")
install.packages("wordcloud2")
install.packages("pROC")
install.packages("ROCR")
install.packages("randomForest")
install.packages("naivebayes")
install.packages("caret")

library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(tm)          # For text mining
library(textstem)    # For stemming / lemmatization
library(tidytext)
library(wordcloud2)
library(pROC)
library(ROCR)
library(naivebayes)
library(caret)

#################################################################
# Loading the datset and checking its structure.
fake <- read.csv("C:/Docs/Babson/QTM2000/Data/Fake.csv")
true <- read.csv("C:/Docs/Babson/QTM2000/Data/True.csv")
str(fake)
str(true)

# But, these two datasets are large. So, let's randomly sample 500 observations (rows) from this large dataset.
# If the Logistic Regression model takes too long to process the data, you may change the 500 to a smaller
# sample size, like 100, to shorten the computation time for Logistic. If you do change the sample size,
# make sure you note that in your Executive Summary.
set.seed(123)
fake <- fake[sample(1:nrow(fake), size= 500, replace = TRUE),]
true <- true[sample(1:nrow(fake), size= 500, replace = TRUE),]
str(fake)
str(true)

# Assign "0" to fake as a category variable and assign "1" to true as a category variable
fake$category <- 0
true$category <- 1
# Now, what do we have?
glimpse(fake)
glimpse(true)

# Merge the two (fake and true) datasets into one for the classification modelling.
# Remember: category will be "0" or "1" in the merged dataset for fake and true, respectifully.
news <- bind_rows(fake, true)

# Let's look at what we have so far:
news %>%
  sample_n(10)
glimpse(news)

# Change the data type for category to factor.
news$category <- as.factor(news$category)
str(news)

# Check for missing values
summary(is.na(news))

#################################################################
# Exploratory Data Analysis (EDA)

# How many fake and true news observations do we have? 
ggplot(news, aes(x = category, fill = category)) + 
  geom_bar() +
  theme_classic() +
  theme(axis.title = element_text(face = 'bold', size = 15),
        axis.text = element_text(size = 13)) +
  theme(legend.position = 'none')

# Change data type of subject to factor for the following visualizations
news$subject <- as.factor(news$subject)

# News count grouped by Subject
news %>%
  group_by(subject) %>%
  count() %>%
  arrange(desc(n))

# Visualize news count grouped by Subject and Category
ggplot(news, aes(x = subject, fill = category)) +
  geom_bar(position = 'dodge', alpha = 0.6) +
  theme_classic() +
  theme(axis.title = element_text(face = 'bold', size = 15),
        axis.text = element_text(size = 13, angle = 10))

#################################################################
# Combine title and text column
news <- news %>% 
  select(title, text, category) %>%
  unite(col = text ,title, text, sep = ' ')  %>%  # Combine 'title' & 'text' column
  mutate(ID = as.character(1:nrow(news)))    # Uniqe row ID for furt

#################################################################
# A corpus (plural corpora) or text corpus is a language resource consisting of a large and structured set of text.
# Create a corpus:
doc <- VCorpus(VectorSource(news$text))

#################################################################
# Natural Language Processing means that we have to clean/tidy human language text
# because it contains a lot of punctuation, symbols and unwanted text, like numbers and stopwords.

# Data Cleaning

# Convert text to lower case
doc <- tm_map(doc, content_transformer(tolower))

# Remove numbers
doc <- tm_map(doc, removeNumbers)

# Remove Punctuation
doc <- tm_map(doc, removePunctuation)

# Remove Stopwords - This takes a lot of computation time!
doc <- tm_map(doc, removeWords, stopwords('english'))

# Remove Whitespace
doc <- tm_map(doc, stripWhitespace)

# Inspect some of the results of data cleaning the corpus
writeLines(as.character(doc[[45]]))

# Make sure all the punctuation is removed.
doc <- tm_map(doc, content_transformer(str_remove_all), "[[:punct:]]")

# Again, inspect some of the results of data cleaning the corpus
writeLines(as.character(doc[[45]]))
writeLines(as.character(doc[[50]]))

#################################################################
# Stemming / Lemmatization - What is this?
# Lemmatization usually refers to doing things properly with the use of a vocabulary and 
# morphological analysis of words, normally aiming to remove inflectional endings only and 
# to return the base or dictionary form of a word, which is known as the lemma.
# If confronted with the token word "saw", stemming might return just s, whereas lemmatization 
# would attempt to return either see or saw depending on whether the use of the token was as a verb or a noun.

# Lemmatization - This takes a lot of computation time! Please wait for the > prompt in the console!
doc <- tm_map(doc, content_transformer(lemmatize_strings))

# Create a Document Term Matrix
dtm <- DocumentTermMatrix(doc)
inspect(dtm)

# Remove all terms whose sparsity is greater than some threshold value.
# A term-document matrix where those terms from x are removed which have at least a sparse 
# percentage of empty (i.e., terms occurring 0 times in a document) elements. 
# I.e., the resulting matrix contains only terms with a sparse factor of less than sparse.
dtm.clean <- removeSparseTerms(dtm, sparse = 0.99)
inspect(dtm.clean)

# Create Tidy data
df.tidy <- tidy(dtm.clean)
df.word<- df.tidy %>% 
  select(-document) %>%
  group_by(term) %>%
  summarize(freq = sum(count)) %>%
  arrange(desc(freq))

#################################################################
# Word Clouds
# Word Clouds (also known as wordle, word collage or tag cloud) are visual representations 
# of words that give greater prominence to words that appear more frequently.

# Word cloud for the fake news
set.seed(1234)
df.tidy %>% 
  inner_join(news, by = c('document' = 'ID')) %>% 
  select(-text) %>%
  group_by(term, category) %>%
  summarize(freq = sum(count)) %>%
  filter(category == 0) %>%
  select(-category) %>%
  arrange(desc(freq)) %>%
  wordcloud2(size = 1.4,  color='random-dark')

# Word cloud for the true news
set.seed(1234)
df.tidy %>% 
  inner_join(news, by = c('document' = 'ID')) %>% 
  select(-text) %>%
  group_by(term, category) %>%
  summarize(freq = sum(count)) %>%
  filter(category == 1) %>%
  select(-category) %>%
  arrange(desc(freq)) %>%
  wordcloud2(size = 1.4,  color='random-dark')

# Convert dtm to matrix
dtm.mat <- as.matrix(dtm.clean)
dim(dtm.mat)
dtm.df <- as.data.frame(dtm.mat)
dtm.mat <- cbind(dtm.mat, category = news$category)

# Showing that the count of 1's is equivalent to count of fake news, 
# and count of 2's are equivalent to count of true news.
as.data.frame(dtm.mat) %>% count(category)

# Convert matrix to data frame
dtm.df <- as.data.frame(dtm.mat)

# Replace values in category by original values (1 by 0 & 2 by 1)
dtm.df$category <- ifelse(dtm.df$category == 2, 1, 0)
dtm.df$category <- as.factor(dtm.df$category)
table(dtm.df$category)

#################################################################
# Modeling
# Splitting Data into Train & Test Sets using a 75/25 rule.
# Create 75:25 split
set.seed(1234)
index <- sample(nrow(dtm.df), nrow(dtm.df)*0.75, replace = FALSE)

train_news <- dtm.df[index,]
test_news <- dtm.df[-index,]
#glimpse(train_news)
#glimpse(test_news)

# Make column names to follow R's variable naming convention
names(train_news) <- make.names(names(train_news))
names(test_news) <- make.names(names(test_news))

# Are both Train & Test sets balanced?
table(train_news$category)
table(test_news$category)

# Naive Bayes Model
mdl_nb <- naive_bayes(category ~ ., data = train_news)

# Model Summary
summary(mdl_nb)

# Logistic Regression Model - This takes a lot of computation time!!! 
# Wait for the > prompt (or stop sign to disappear). Go take a short break! About 10-15 minutes. So, be patient.
# Also: Ignore any warning messages.
mdl_lr <- glm(formula = category ~.,
              data = train_news,
              family = 'binomial')

#################################################################
# Model Evaluation
# Predicted values
train_news$pred_nb <- predict(mdl_nb, type = 'class')
train_news$pred_lr <- predict(mdl_lr, type = 'response')

# Predicted Values for test set
test_news$pred_nb <- predict(mdl_nb, newdata = test_news)
test_news$pred_lr <- predict(mdl_lr, newdata = test_news, type = 'response')

# Plot ROC Curve for test set
prediction(as.numeric(test_news$pred_nb), as.numeric(test_news$category)) %>%
  performance('tpr', 'fpr') %>%
  plot(col = 'red', lwd = 2)
prediction(as.numeric(test_news$pred_lr), as.numeric(test_news$category)) %>%
  performance('tpr', 'fpr') %>%
  plot(add = TRUE, col = 'blue', lwd = 2)
legend(0.8, 0.2, legend=c("Naive Bayes", "Logistic"),
       col=c("red", "blue", 'green'), lty = 1, cex = 1.2, box.lty = 0)

# Set Threshold for Logistic Regression Model
roc(test_news$category, test_news$pred_lr) %>% coords()
test_news$pred_lr <- ifelse(test_news$pred_lr > 0.5, 1, 0)
test_news$pred_lr <- as.factor(test_news$pred_lr)

# Classification Matrix
conf_nb <- caret::confusionMatrix(test_news$category, test_news$pred_nb)
conf_lr <- caret::confusionMatrix(test_news$category, test_news$pred_lr)

# Present the Classification Matrix as a "Heat Map".
# A heat Map is a data visualization technique that shows magnitude of a phenomenon as color in two dimensions.
# The variation in color may be by hue or intensity, giving obvious visual cues about how the 
# clasification prediction matches against what actually occurred. 
bind_rows(as.data.frame(conf_nb$table), as.data.frame(conf_lr$table)) %>% 
  mutate(Model = rep(c('Naive Bayes', 'Logistic Regression'), each = 4)) %>%
  ggplot(aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  labs(x = 'Actual', y = 'Predicted') +
  scale_fill_gradient(low = "#CCE5FF", high = "#000099") +
  scale_x_discrete(limits = c('1', '0'), labels = c('1' = 'Not Fake', '0' = 'Fake')) +
  scale_y_discrete(labels = c('1' = 'Not Fake', '0' = 'Fake')) +
  facet_grid(. ~ Model) +
  geom_text(aes(label = Freq), fontface = 'bold') +
  theme(panel.background = element_blank(),
        legend.position = 'none',
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 14, face = 'bold'),
        axis.text = element_text(size = 11, face = 'bold'),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = 'bold'))

# Evaluate the prediction accuracy of the models.
acc <- c(nb = conf_nb[['overall']]['Accuracy'], 
         lr = conf_lr[['overall']]['Accuracy'])

# Precision is the number of True Positives divided by the number of True Positives and False Positives. 
# Put another way, it is the number of positive predictions divided by the total number of 
# positive class values predicted. It is also called the Positive Predictive Value (PPV).
precision <- c(nb = conf_nb[['byClass']]['Pos Pred Value'], 
               lr = conf_lr[['byClass']]['Pos Pred Value'])

# Recall is the number of True Positives divided by the number of True Positives and the number of False Negatives. 
# Put another way it is the number of positive predictions divided by the number of positive class values 
# in the test data. It is also called Sensitivity or the True Positive Rate.
recall <- c(nb = conf_nb[['byClass']]['Sensitivity'], 
            lr = conf_lr[['byClass']]['Sensitivity'])

# F1_score conveys the balance between the precision and the recall.
data.frame(Model = c('Naive Bayes', 'Logistic'),
           Accuracy = acc,
           F1_Score = (2 * precision * recall) / (precision + recall),
           row.names = NULL)

#################################################################
#################################################################