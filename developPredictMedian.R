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

# A general function to discretize a vector ratings with optional Whole flag for integers only
flixStar <- function(ratings, whole = FALSE) {
  ratings <- replace(ratings, ratings <= 0.5, 0.51) # zero not allowed, IEEE rounding
  ratings <- replace(ratings, ratings > 5, 5) # 5 is max rating
  
  data.frame(ratings, whole) %>% 
    rowwise() %>% 
    mutate( new = if (whole) round(ratings + 0.01) else round(ratings*2)/2 ) %>% # IEEE rounding
    .$new
}
# Follow the textbook approach
# overall averages for the whole set
med <- median(trainSet$rating)

median_acc <- accuracy(testSet$rating, flixStar(med))
acc_results <- data_frame(method="median", acc=median_acc)

# movie effects
# using median
movie_avgs <- trainSet %>%
  group_by(movieId) %>%
  summarize(b_i = median(rating - med))

predicted_ratings <- testSet %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i + med
model_1_acc <- accuracy(testSet$rating, flixStar(predicted_ratings))
acc_results <- bind_rows(acc_results, data_frame(method="median movie", acc=model_1_acc))

# user effects
# using median
user_avgs <- trainSet %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = median(rating - med - b_i))

predicted_ratings <- testSet %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = med + b_i + b_u) %>%
  .$pred

model_2_acc <- accuracy(testSet$rating, flixStar(predicted_ratings))
acc_results <- bind_rows(acc_results, data_frame(method="movie+user", acc=model_2_acc))

# regularized movie effect
# using median
lambda <- 2.33
movie_reg_avgs <- trainSet %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - med)/(n()+lambda), n_i = n())

predicted_ratings <- testSet %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  mutate(pred = med + b_i) %>%
  .$pred

model_3_acc <- accuracy(testSet$rating, flixStar(predicted_ratings))
acc_results <- bind_rows(acc_results, data_frame(method="movie reg", acc=model_3_acc))

# genre effect n > minGenre
# using median

minGenre <- 220 # determined best between 10 and 1000

genre_avgs <- trainSet %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(n = n(), b_g = median(rating - med - b_i - b_u)) %>%
  filter(n >= minGenre) %>%
  select(-n)

predicted_ratings <- testSet %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(b_g = replace_na(b_g, 0)) %>%
  mutate(pred = med + b_i + b_u + b_g) %>%
  .$pred

model_4_acc <- accuracy(testSet$rating, flixStar(predicted_ratings))
acc_results <- bind_rows(acc_results, data_frame(method="movie+user+genre", acc=model_4_acc))

## Try whole number rounding for users so inclined
usersWhoWhole <- trainSet %>% group_by(userId) %>%
  summarize(total = length(rating), 
            wholes = sum(rating %% 1 == 0), 
            wholepct = wholes/total) %>%
  filter(wholepct >= 0.75) %>%
  .$userId

predicted_ratings <- testSet %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(b_g = replace_na(b_g, 0)) %>%
  mutate(pred = med + b_i + b_u + b_g) %>%
  mutate(userWhoWholes = userId %in% usersWhoWhole) %>%
  mutate(roundPred = flixStar(pred, userWhoWholes))


model_4_5_acc <- accuracy(testSet$rating, flixStar(predicted_ratings$roundPred))
acc_results <- bind_rows(acc_results, data_frame(method="movie+user+genre user round", acc=model_4_5_acc))


# regularized everything
l <- 0.9

b_i <- trainSet %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - med)/(n()+l)) %>%
  mutate(b_i = flixStar(med + b_i) - med)

b_u <- trainSet %>%
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - med)/(n()+l)) %>%
  mutate(b_u = flixStar(med + b_u) - med)

  
b_g <- trainSet %>%
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  group_by(genres) %>%
  summarize(n = n(), b_g = sum(rating - med - b_i - b_u)/(n() + l)) %>%
  filter(n >= minGenre) %>%
  select(-n) %>%
  mutate(b_g = flixStar(med + b_g) - med)

  

predicted_ratings <- testSet %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by="genres") %>%
  mutate(b_g = replace_na(b_g, 0)) %>%
  mutate(pred = med + b_i + b_u + b_g) %>%
  .$pred

model_5_acc <- accuracy(testSet$rating, flixStar(predicted_ratings))
acc_results <- bind_rows(acc_results, data_frame(method="reg movie+user+genre", acc=model_5_acc))

## Try whole number rounding for users so inclined

predicted_ratings <- testSet %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by="genres") %>%
  mutate(b_g = replace_na(b_g, 0)) %>%
  mutate(pred = med + b_i + b_u + b_g) %>%
  mutate(userWhoWholes = userId %in% usersWhoWhole) %>%
  mutate(roundPred = flixStar(pred, userWhoWholes))

model_6_acc <- accuracy(testSet$rating, predicted_ratings$roundPred)
acc_results <- bind_rows(acc_results, data_frame(method="reg m+u+g user round", acc=model_6_acc))


acc_results %>% knitr::kable()
