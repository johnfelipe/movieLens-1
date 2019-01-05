## Script to develop prediction
library(tidyverse)
library(dslabs)
library(caret)

data("movielens")
movielens <- select(movielens, -year) # to match edx set which doesn't have year

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

# A general function to discretize ratings vector with optional Whole flag vector for integers only
flixStar <- function(ratings, whole = FALSE) {
  map2_dbl(ratings, whole, function (a, b) {
    if (a <= 0.5) a <- 0.51 else if (a > 5) a <- 5
    if (b) round(a + 0.01) else round(a*2)/2  # IEEE rounding
  })
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

# regularized movie effect
lambda <- 2.33
movie_reg_avgs <- trainSet %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n())

predicted_ratings <- testSet %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  .$pred

model_3_acc <- accuracy(testSet$rating, flixStar(predicted_ratings))
acc_results <- bind_rows(acc_results, data_frame(method="movie reg", acc=model_3_acc))

# genre effect n > minGenre

minGenre <- 220 # determined best between 10 and 1000

genre_avgs <- trainSet %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(n = n(), b_g = mean(rating - mu - b_i - b_u)) %>%
  filter(n >= minGenre) %>%
  select(-n)
  
predicted_ratings <- testSet %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(b_g = replace_na(b_g, 0)) %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  .$pred
  
model_4_acc <- accuracy(testSet$rating, flixStar(predicted_ratings))
acc_results <- bind_rows(acc_results, data_frame(method="movie+user+genre", acc=model_4_acc))


# regularized everything
l <- 0.9

  b_i <- trainSet %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))

  b_u <- trainSet %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))

  b_g <- trainSet %>%
    left_join(movie_avgs, by='movieId') %>%
    left_join(user_avgs, by='userId') %>%
    group_by(genres) %>%
    summarize(n = n(), b_g = sum(rating - mu - b_i - b_u)/(n() + l)) %>%
    filter(n >= minGenre) %>%
    select(-n)

  predicted_ratings <- testSet %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by="genres") %>%
    mutate(b_g = replace_na(b_g, 0)) %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    .$pred

  model_5_acc <- accuracy(testSet$rating, flixStar(predicted_ratings))
  acc_results <- bind_rows(acc_results, data_frame(method="reg movie+user+genre", acc=model_5_acc))
  
## Try whole number rounding for users so inclined
usersWhoWhole <- trainSet %>% group_by(userId) %>%
  summarize(total = length(rating), 
            wholes = sum(rating %% 1 == 0), 
            wholepct = wholes/total) %>%
  filter(wholepct >= 0.55) %>%
  .$userId
 
predicted_ratings <- testSet %>%
   left_join(b_i, by = "movieId") %>%
   left_join(b_u, by = "userId") %>%
   left_join(b_g, by="genres") %>%
   mutate(b_g = replace_na(b_g, 0)) %>%
   mutate(pred = mu + b_i + b_u + b_g) %>%
   mutate(userWhoWholes = userId %in% usersWhoWhole) %>%
   mutate(roundPred = flixStar(pred, userWhoWholes))

model_6_acc <- accuracy(testSet$rating, predicted_ratings$roundPred)
acc_results <- bind_rows(acc_results, data_frame(method="reg m+u+g user round", acc=model_6_acc))


acc_results %>% knitr::kable()

# #Determine the best lambda.... 0.9
# lambdas <- seq(0.5, 1.5, 0.1)
# 
# accs <- sapply(lambdas, function(l){
#   
#   b_i <- trainSet %>% 
#     group_by(movieId) %>%
#     summarize(b_i = sum(rating - mu)/(n()+l))
#   
#   b_u <- trainSet %>% 
#     left_join(b_i, by="movieId") %>%
#     group_by(userId) %>%
#     summarize(b_u = sum(rating - b_i - mu)/(n()+l))
#   
#   b_g <- trainSet %>%
#     left_join(movie_avgs, by='movieId') %>%
#     left_join(user_avgs, by='userId') %>%
#     group_by(genres) %>%
#     summarize(n = n(), b_g = sum(rating - mu - b_i - b_u)/(n() + l)) %>%
#     filter(n >= minGenre) %>%
#     select(-n)
#   
#   predicted_ratings <- testSet %>% 
#     left_join(b_i, by = "movieId") %>%
#     left_join(b_u, by = "userId") %>%
#     left_join(b_g, by="genres") %>%
#     mutate(b_g = replace_na(b_g, 0)) %>%
#     mutate(pred = mu + b_i + b_u + b_g) %>%
#     .$pred
#   return(accuracy(testSet$rating, flixStar(predicted_ratings)))
# })
# qplot(lambdas, accs)
# lambdas[which.max(accs)]

# #Determine the best num genres
# numgen <- seq(10, 1000, 10)
# 
# accs <- sapply(numgen, function(minGenre){
#   genre_avgs <- trainSet %>% 
#     left_join(movie_avgs, by='movieId') %>%
#     left_join(user_avgs, by='userId') %>%
#     group_by(genres) %>%
#     summarize(n = n(), b_g = mean(rating - mu - b_i - b_u)) %>%
#     filter(n >= minGenre) %>%
#     select(-n)
#   
#   predicted_ratings <- testSet %>% 
#     left_join(movie_avgs, by='movieId') %>%
#     left_join(user_avgs, by='userId') %>%
#     left_join(genre_avgs, by='genres') %>%
#     mutate(b_g = replace_na(b_g, 0)) %>%
#     mutate(pred = mu + b_i + b_u + b_g) %>%
#     .$pred
#   
#   return(accuracy(testSet$rating, flixStar(predicted_ratings)))
# })
# qplot(numgen, accs)
# numgen[which.max(accs)] # 220
