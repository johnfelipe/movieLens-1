library(tidyverse)
library(caret)

# For temporary use to save time
if (!("edx" %in% ls())) load("createDataSets.RData")

# For submission
# if (!("edx" %in% ls())) source("createDataSets.R")

## Helper functions

# A function to calculate accuracy in terms of % correct
accuracy <- function(true_ratings, predicted_ratings) {
  correct <- sum(true_ratings == predicted_ratings)
  return(correct / length(true_ratings))
}

# A general function to discretize ratings vector with optional Whole flag vector for integers only
flixStar <- function(ratings, whole = FALSE) {
    map2_dbl(ratings, whole, function (a, b) {
      if (a <= 0.5) a <- 0.51 else if (a > 5) a <- 5
      if (b) round(a + 0.01) else round(a*2)/2  # IEEE rounding
    })
}


## Split edx into training and test data using same strategy as course split

# Create train set and test set
set.seed(1)
test_index <- createDataPartition(y = edx$rating, p=0.2, list = FALSE)
trainSet <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in validation set are also in edx set
testSet <- temp %>% 
  semi_join(trainSet, by = "movieId") %>%
  semi_join(trainSet, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, testSet)
trainSet <- rbind(trainSet, removed)
rm(test_index, temp, removed)


## Data exploration of training set
# explore ratings
trainSet %>% 
  ggplot() +
  aes(rating) +
  geom_histogram(binwidth = 0.5)
# proportion of whole number ratings
wholes <- sum(trainSet$rating %% 1 == 0) / length(trainSet$rating)

# proportion of whole number ratings per user
trainSet %>% group_by(userId) %>%
  summarize(total = length(rating), 
            wholes = sum(rating %% 1 == 0), 
            wholepct = wholes/total) %>%
  ggplot(aes(wholepct)) + geom_histogram(binwidth = 0.1)
# So, most users only use whole # ratings, and a smaller group use them ~50% of the time

# We want to tag certain users as always or nearly always assigning whole number ratings
usersWhoWhole <- trainSet %>% group_by(userId) %>%
  summarize(total = length(rating), 
            wholes = sum(rating %% 1 == 0), 
            wholepct = wholes/total) %>%
  filter(wholepct >= .75) %>%
  .$userId

# Follow the textbook approach
# overall median for the whole set
med <- median(trainSet$rating)

median_acc <- accuracy(testSet$rating, flixStar(med))
median_RMSE <- RMSE(testSet$rating, med)
med_results <- data_frame(Method="Median", Accuracy = median_acc, RMSE = median_RMSE)

# center on user/movie median ratings
movie_avgs <- trainSet %>%
  group_by(movieId) %>%
  summarize(b_i = median(rating - med))

user_avgs <- trainSet %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = median(rating - med - b_i))

predicted_ratings <- testSet %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = med + b_i + b_u) %>%
  mutate(roundPred = flixStar(pred, userId %in% usersWhoWhole))

median_um_acc <- accuracy(testSet$rating, flixStar(predicted_ratings$pred))
median_um_accWholes <- accuracy(testSet$rating, predicted_ratings$roundPred)
median_um_RMSE <- RMSE(testSet$rating, predicted_ratings$pred)

med_results <- bind_rows(med_results, data_frame(Method="Movie+User", 
                                                 Accuracy = median_um_acc,
                                                 RMSE = median_um_RMSE,
                                                 AccuracyWhole = median_um_accWholes))



# Next: regularization
# Optimize lambda
lambdas <- seq(0, 2, 0.1)

laccs <- sapply(lambdas, function(l){

  b_i <- trainSet %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - med)/(n()+l))

  b_u <- trainSet %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - med)/(n()+l))

  predicted_ratings <- testSet %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = med + b_i + b_u) %>%
    mutate(roundPred = flixStar(pred, userId %in% usersWhoWhole))
  return(accuracy(testSet$rating, predicted_ratings$roundPred))
})
qplot(lambdas, laccs)
lambda <- lambdas[which.max(laccs)]

# now calculate the regularized accuracy
b_i <- trainSet %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - med)/(n()+lambda)) 

b_u <- trainSet %>%
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - med)/(n()+lambda)) 

predicted_ratings <- testSet %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = med + b_i + b_u) %>%
  mutate(roundPred = flixStar(pred, userId %in% usersWhoWhole))

median_umR_acc <- accuracy(testSet$rating, flixStar(predicted_ratings$pred))
median_umR_accWholes <- accuracy(testSet$rating, predicted_ratings$roundPred)
median_umR_RMSE <- RMSE(testSet$rating, predicted_ratings$pred)

med_results <- bind_rows(med_results, data_frame(Method="Reg Movie+User", 
                                                 Accuracy = median_umR_acc,
                                                 RMSE = median_umR_RMSE,
                                                 AccuracyWhole = median_umR_accWholes))


med_results %>% knitr::kable()


# tune the % of the time a user gives integer ratings
wholes <- seq(0.5,1,0.05)

waccs <- sapply(wholes, function(w) {

  usersWhoWhole <- trainSet %>% group_by(userId) %>%
    summarize(total = length(rating), 
              wholes = sum(rating %% 1 == 0), 
              wholepct = wholes/total) %>%
    filter(wholepct >= w) %>%
    .$userId
  
  predicted_ratings <- testSet %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = med + b_i + b_u) %>%
    mutate(roundPred = flixStar(pred, userId %in% usersWhoWhole))
  
  return(accuracy(testSet$rating, predicted_ratings$roundPred))
})
qplot(wholes, waccs)
wholes[which.max(waccs)]
