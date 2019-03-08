#Set working directory
setwd("C:\\Users\\ericv\\OneDrive\\Documents\\MGSC410")

# Necessary Libraries
library(ggplot2)
library(RTextTools)
library(tidytext)
library(quanteda)
library(RColorBrewer)

#Read in csv
Tweets <- read.csv("Tweets.csv", header = TRUE, na.strings=c("", "NA"))

#find structre of Tweets
str(Tweets)

#Summary statistics
summary(Tweets)

#####################################
######EXPLORATORY DATA ANALYSIS######
#####################################
a <- sort(Tweets$tweet_created, decreasing = TRUE)
a
#Airline Sentiment Plot
air_sent_plot <- ggplot(Tweets, aes(airline_sentiment)) + geom_bar(fill = c("red", "green", "blue"), color = "black") + ylim(c(0,10000)) + ggtitle("Count of Sentiments") + xlab("Airline Sentiment")
air_sent_plot

#Airline Sentiment Counts
air_sent_counts <- table(Tweets$airline_sentiment)
air_sent_counts

#cross tabulation of airline vs sentiment
air_sent_xtab <- xtabs(~Tweets$airline + Tweets$airline_sentiment, data = Tweets)
air_sent_xtab

#percentage of each airline being negative, neutral, or positive
perc_per_airline <- prop.table(table(Tweets$airline, Tweets$airline_sentiment),1)
perc_per_airline

#Airline and airline sentiment
air_vs_air_sent <- ggplot(Tweets, aes(airline,fill = airline_sentiment))
air_vs_air_sent <- air_vs_air_sent + geom_histogram(stat="count", color = "black")
air_vs_air_sent

#negative reason plot
negative_reason <- ggplot(Tweets, aes(negativereason)) + geom_bar(color = "black", fill = "purple") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Negative Reason Frequency")
negative_reason

table(Tweets$negativereason)

#cross tabulation of airline vs negative reason
air_neg_reason_xtab <- xtabs(~Tweets$airline + Tweets$negativereason, data = Tweets)
air_neg_reason_xtab

#percentage of each airline's negative reasons
neg_reason_per_airline <- prop.table(table(Tweets$airline, Tweets$negativereason),1)
neg_reason_per_airline

#Airline and Negative Reason
air_vs_neg_reason <- ggplot(Tweets, aes(airline, fill = negativereason))
air_vs_neg_reason <- air_vs_neg_reason + geom_histogram(stat="count", color = "black")
air_vs_neg_reason

# Most Occurring Words
# not really relevant
twitter_words <- tolower(Tweets$text)
twitter_words_list<-strsplit(twitter_words, "\\W+", perl=TRUE)
twitter_words_vector <- unlist(twitter_words_list)
twitter_words_freq <-table(twitter_words_vector)
twitter_words_sorted <- sort(twitter_words_freq, decreasing = TRUE)
head(twitter_words_sorted, 10)


# plotting tweets based on timezone
timezone <- table(Tweets$user_timezone)
timezone <- sort(timezone, decreasing = TRUE)
timezone <- head(timezone, 10)
timezone
barplot(timezone,
        col = ("thistle3"),
        ylim = c(0, 4000),
        main = "Most Frequent Tweets per 'Timezone'",
        xlab = "General Location",
        ylab = "Count")

# Most Frequent Twitter Users
users <- table(Tweets$name)
users <- sort(users, decreasing = TRUE)
head(users, 10)

#See Tweets and sentiments from return users above
Tweets[Tweets$name == "kbosspotter",]$text
Tweets[Tweets$name == "kbosspotter",]$airline_sentiment

#Find the 15 biggest haters (negative airline_sentiment of 15 or more)
name_vs_airline_sentiment <- table(Tweets$name, Tweets$airline_sentiment)
name_vs_airline_sentiment <- as.data.frame(name_vs_airline_sentiment)
names(name_vs_airline_sentiment) <- c("name", "airline_sentiment", "Frequency")

haters <- name_vs_airline_sentiment[(name_vs_airline_sentiment$airline_sentiment=="negative"),]
haters <- haters[order(haters$Frequency, decreasing = TRUE),]

#subset into a dataframe
haters_top <- haters[1:15,]
haters_top$name <- factor(haters_top$name, levels = haters_top$name[order(haters_top$Frequency, decreasing = TRUE)])
haters_top

# plot the top 15 haters
ggplot(haters_top, aes(name,Frequency)) + geom_bar(stat = "identity", fill = "red", color = "black") + ggtitle("Most Negative Sentiment Tweets by User") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Find the 15 biggest fans (positive airline_sentiment of 15 or more)
fans <- name_vs_airline_sentiment[(name_vs_airline_sentiment$airline_sentiment=="positive"),]
fans <- fans[order(fans$Frequency, decreasing = TRUE),]

#subset into a dataframe
fans_top <- fans[1:15,]
fans_top$name <- factor(fans_top$name, levels = fans_top$name[order(fans_top$Frequency, decreasing = TRUE)])
fans_top

ggplot(fans_top, aes(name,Frequency)) + geom_bar(stat = "identity", fill = "green", color = "black") + ggtitle("Most Negative Sentiment Tweets by User") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Create Word Clouds
#https://www.r-bloggers.com/text-message-classification/


# Create Word Cloud of entire data set

#Vectorize Text column
text <- as.vector(Tweets$text)

# Create a corpus object
text_corpus <- corpus(text)

# Attaching class labels to the corpus message text
docvars(text_corpus) <- text

# Build wordplot using corpus object
text_plot<-dfm(text_corpus, 
               tolower = TRUE, 
               remove_punct = TRUE, 
               remove_twitter = TRUE, 
               remove_numbers = TRUE, 
               remove=stopwords("SMART"))

# assign coloring
text_col <- brewer.pal(10, "BrBG")  

# Project Wordcloud
textplot_wordcloud(text_plot, min.freq = 100, color = text_col)  
title("Tweet Wordcloud (Word Frequency of 100)", col.main = "grey14")

# Create Negative Word Cloud

#Vectorize Text column
negative_sentiment <- as.vector(Tweets$text[Tweets$airline_sentiment == "negative"])

# Create a corpus object
negative_sentiment_corpus <- corpus(negative_sentiment)

# Attaching class labels to the corpus message text
docvars(negative_sentiment_corpus) <- negative_sentiment

# Build wordplot using corpus object
negative_sentiment_plot<-dfm(negative_sentiment_corpus, 
                             tolower = TRUE, 
                             remove_punct = TRUE, 
                             remove_twitter = TRUE, 
                             remove_numbers = TRUE, 
                             remove=stopwords("SMART"))

# assign coloring
text_col <- brewer.pal(10, "BrBG")  

# Project Wordcloud
textplot_wordcloud(negative_sentiment_plot, min.freq = 50, color = text_col)  
title("Negative Tweet Wordcloud (Word Frequency of 50)", col.main = "grey14")

# Create positive word cloud

#Vectorize Text column
positive_sentiment <- as.vector(Tweets$text[Tweets$airline_sentiment == "positive"])

# Create a corpus object
positive_sentiment_corpus <- corpus(positive_sentiment)

# Attaching class labels to the corpus message text
docvars(positive_sentiment_corpus) <- positive_sentiment

# Build wordplot using corpus object
positive_sentiment_plot<-dfm(positive_sentiment_corpus, 
                             tolower = TRUE,
                             remove_punct = TRUE, 
                             remove_twitter = TRUE, 
                             remove_numbers = TRUE, 
                             remove=stopwords("SMART"))

# assign coloring
text_col <- brewer.pal(10, "BrBG")  

# Project Wordcloud
textplot_wordcloud(positive_sentiment_plot, min.freq = 50, color = text_col)  
title("Positive Tweet Wordcloud (Word Frequency of 50)", col.main = "grey14")

############MODEL BUILDING###########
#####################################
# https://morianodotnet.wordpress.com/2016/08/14/classifying-text-with-r-and-rtexttools/

# Sentiment dummy column
# - 1 for negative values, 0 for neutral, 1 for positive
Tweets$sentiment_value <- ifelse(TweetsSVM$airline_sentiment == "negative", -1, 
                                 ifelse(TweetsSVM$airline_sentiment == "positive", 1, 
                                        0))

# Training and Testing partition
base_data <- Tweets[1:nrow(Tweets)*0.7, ]
cross_validation_data <- Tweets[as.integer((nrow(Tweets)*0.7)+1):nrow(Tweets)-1, ]

# Document Term Matrix
dtMatrix <- create_matrix(Tweets$text)
dtMatrix

# Configure the training data
# Data is pre randomized for sampling
container <- create_container(dtMatrix, 
                              Tweets$sentiment_value, 
                              trainSize=1:as.integer(nrow(base_data)*0.7), 
                              testSize=as.integer((nrow(base_data)*0.7)+1):(nrow(Tweets)-1), 
                              virgin=FALSE)

# train a SVM Model
model <- train_model(container, "SVM", kernel="linear", cost=1)

# perform predictions
classify <- classify_model(container, model)
  
analytics <- create_analytics(container, classify)

summary(analytics)
# 
# dtMatrix2 <- create_matrix(cross_validation_data$text, 
#                                 originalMatrix=dtMatrix,
#                                 language="english", 
#                                 removeNumbers=TRUE,
#                                 stemWords=TRUE, 
#                                 removeSparseTerms=0.998
# )
# 
# 
# # Now again, lets create a container and perform predictions on it
# container2 <- create_container(dtMatrix2, 
#                                  cross_validation_data$sentiment_value,
#                                  trainSize=NULL,
#                                  testSize=1:(nrow(cross_validation_data)),
#                                  virgin=TRUE)
# 
# 
# 
# classify2 <- classify_models(container2, model)
# analytics2 <- create_analytics(container2, classify2)
# 
# predictions_raw <- analytics_cv@document_summary
# 
# # Create a simple dataframe for predictions
# predictions <- data.frame(real_label=cross_validation_data$Class, 
#                           predicted_label=predictions_raw$CONSENSUS_CODE,
#                           algorithms_agree=predictions_raw$CONSENSUS_AGREE)
# 
# min_algs_agreeing <- 2 # We will require at least two algorightms to agree on the class to be classified
# 
# 
# # Change our predictions dataframe, if we have at least X algorithms agreeing on the results, then 
# # we will keep the prediction, if not, we will set the prediction as "N/A"
# predictions$predicted_label <- ifelse(predictions$algorithms_agree >= min_algs_agreeing, predictions$predicted_label, "N/A")
# 
# 
# # Lets print out some final conclusions
# print(paste("Total number of documents to classify", nrow(cross_validation_data)))
# print(paste("Total number of documents classified", nrow(subset(predictions, predicted_label!="N/A"))))
# print(paste("Total number of correctly classified documents", nrow(subset(predictions, predicted_label == real_label))))
# print(paste("Total number of incorrectly classified documents", nrow(subset(predictions, predicted_label != real_label))))
# print(paste("Total number of documents which were not classified", nrow(subset(predictions, predicted_label == "N/A"))))

