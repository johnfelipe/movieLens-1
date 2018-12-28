## Script to develop prediction
library(tidyverse)
library(dslabs)
library(caret)

data("movielens")
movielens <- select(movielens, -year) # to match edx set

# Create train set and test set
set.seed(1)
test_index <- createDataPartition(y = movielens$rating, p=0.2, list = FALSE)
trainSet <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
testSet <- temp %>% 
  semi_join(trainSet, by = "movieId") %>%
  semi_join(trainSet, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, testSet)
trainSet <- rbind(trainSet, removed)
rm(test_index, temp, movielens, removed)

# A function to calculate accuracy in terms of % correct
accuracy <- function(true_ratings, predicted_ratings) {
  correct <- sum(true_ratings == predicted_ratings)
  pctAc <- correct / length(true_ratings)
  
  pctAc
}

# A function to discretize ratings to nearest valid 0.5
flixStar <- function(rating) {
  rating <- round(rating*2)/2
  rating <- replace(rating, rating <= 0, 0.5)
  rating <- replace(rating, rating > 5, 5)

  rating
}

# Follow the textbook approach
# overall averages for the whole set
mu_hat <- mean(trainSet$rating)
median_hat <- median(trainSet$rating)

native_acc <- accuracy(testSet$rating, flixStar(mu_hat))
median_acc <- accuracy(testSet$rating, flixStar(median_hat))
acc_results <- data_frame(method = "mean", acc = native_acc)
acc_results <- bind_rows(acc_results, data_frame(method="median", acc=median_acc))

# movie effects
mu <- mean(trainSet$rating)
movie_avgs <- trainSet %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

predicted_ratings <- testSet %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i + mu
model_1_acc <- accuracy(testSet$rating, predicted_ratings)
acc_results <- bind_rows(acc_results, data_frame(method="mean movie", acc=model_1_acc))

#using median
med <- median(trainSet$rating)
movie_avgs <- trainSet %>%
  group_by(movieId) %>%
  summarize(b_i = median(rating - med))

predicted_ratings <- testSet %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i + med
model_1_acc <- accuracy(testSet$rating, predicted_ratings)
acc_results <- bind_rows(acc_results, data_frame(method="median movie", acc=model_1_acc))

acc_results %>% knitr::kable()
