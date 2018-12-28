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
flixStar <- function(ratings) {
  ratings <- round(ratings*2)/2
  ratings <- replace(ratings, ratings <= 0, 0.5)
  ratings <- replace(ratings, ratings > 5, 5)

  ratings
}

# Follow the textbook approach
# overall averages for the whole set
mu <- mean(trainSet$rating)

native_acc <- accuracy(testSet$rating, flixStar(mu))
acc_results <- data_frame(method = "mean", acc = native_acc)

# movie effects
movie_avgs <- trainSet %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

predicted_ratings <- testSet %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i + mu
model_1_acc <- accuracy(testSet$rating, flixStar(predicted_ratings))
acc_results <- bind_rows(acc_results, data_frame(method="movie", acc=model_1_acc))

# user effects
user_avgs <- trainSet %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- testSet %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_2_acc <- accuracy(testSet$rating, flixStar(predicted_ratings))
acc_results <- bind_rows(acc_results, data_frame(method="movie+user", acc=model_2_acc))


acc_results %>% knitr::kable()
