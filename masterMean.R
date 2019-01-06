library(tidyverse)
library(caret)

# If the data set isn't loaded yet, call the script
if (!("edx" %in% ls())) source("createDataSets.R")

## Helper functions
# A function to calculate accuracy in terms of % exactly correct
accuracy <- function(true_ratings, predicted_ratings) {
  correct <- sum(true_ratings == predicted_ratings)
  return(correct / length(true_ratings))
}

# A general function to discretize ratings vector with optional Whole flag vector for integers only
# The extra 0.01 additions are due to IEEE rounding, so we can be sure 0.5 always rounds up
flixStar <- function(ratings, whole = FALSE) {
    map2_dbl(ratings, whole, function (a, b) {
      if (a <= 0.5) a <- 0.51 else if (a > 5) a <- 5
      if (b) round(a + 0.01) else round(a*2)/2
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
# explore ratings distribution
fig1 <- trainSet %>% 
  ggplot() +
  aes(rating) +
  geom_histogram(binwidth = 0.5) +
  xlab("Rating") + ylab("# Ratings") + ggtitle("Ratings Histogram")
fig1

# proportion of whole number ratings, which is quite high
wholes <- sum(trainSet$rating %% 1 == 0) / length(trainSet$rating)

# proportion of whole number ratings per user
fig2 <- trainSet %>% group_by(userId) %>%
  summarize(total = length(rating), 
            wholes = sum(rating %% 1 == 0), 
            wholepct = wholes/total) %>%
  ggplot(aes(wholepct)) + geom_histogram(binwidth = 0.1) +
  xlab("% Integer Ratings") + ylab("# Users") + ggtitle("Integer Ratings Per User")
fig2
# So, most users only use whole # ratings, and a smaller group use them ~50% of the time

# We want to tag certain users as always or nearly always assigning whole number ratings
# We will tune the "nearly always" cutoff later.  75% for now
wholeCutoff <- 0.75
usersWhoWhole <- trainSet %>% group_by(userId) %>%
  summarize(total = length(rating), 
            wholes = sum(rating %% 1 == 0), 
            wholepct = wholes/total) %>%
  filter(wholepct >= wholeCutoff) %>%
  .$userId

# Some users don't move their ratings around much
fig3 <- trainSet %>% group_by(userId) %>% summarize(spread = max(rating) - min(rating)) %>% 
  ggplot(aes(spread)) + geom_histogram(binwidth = 0.5) +
  xlab("Ratings Spread") + ylab("# Users") + ggtitle("Min/Max Ratings Spread")
fig3

# How many different ratings does each user give
fig4 <- trainSet %>% group_by(userId, rating) %>% summarize(num = n()) %>%
  group_by(userId) %>% summarize(distinct = n_distinct(rating)) %>%
  ggplot() + aes(distinct) + geom_histogram(binwidth=1) +
  xlab("# Distinct Ratings") + ylab("# Users") + ggtitle("Distinct Ratings Per User")
fig4

# some users may not use the whole range. Most go up to 5 but fewer go down to 1
userMinMax <- trainSet %>% group_by(userId) %>% summarize(min = min(rating), max=max(rating))

fig5 <- userMinMax %>% ggplot() + 
  geom_histogram(aes(min), binwidth=0.5, fill="blue", alpha=0.5) +
  geom_histogram(aes(max), binwidth=0.5, fill="red", alpha = 0.5) +
  xlab("Rating") + ylab("# Users") + ggtitle("Min (Blue) and Max (Red) Ratings Per User") 
fig5

## Develop prediction algorithm
# Follow the textbook approach
# overall mean for the whole set
mu <- mean(trainSet$rating)
mean_acc <- accuracy(testSet$rating, flixStar(mu))
mean_RMSE <- RMSE(testSet$rating, mu)
mean_results <- data_frame(Method="Mean", Accuracy = mean_acc, RMSE = mean_RMSE)

# overall median for the whole set
med <- median(trainSet$rating)
median_acc <- accuracy(testSet$rating, flixStar(med))
median_RMSE <- RMSE(testSet$rating, med)
mean_results <- bind_rows(mean_results, data_frame(Method="Median",
                                                       Accuracy = median_acc,
                                                       RMSE = median_RMSE))

# center on user/movie mean ratings
movie_avgs <- trainSet %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

user_avgs <- trainSet %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- testSet %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  mutate(roundPred = flixStar(pred, userId %in% usersWhoWhole))

mean_um_acc <- accuracy(testSet$rating, flixStar(predicted_ratings$pred))
mean_um_accWholes <- accuracy(testSet$rating, predicted_ratings$roundPred)
mean_um_RMSE <- RMSE(testSet$rating, predicted_ratings$pred)

mean_results <- bind_rows(mean_results, data_frame(Method="Movie+User", 
                                                 Accuracy = mean_um_acc,
                                                 RMSE = mean_um_RMSE,
                                                 AccuracyWhole = mean_um_accWholes))

# Center on user/movie/genre-combo
genre_avgs <- trainSet %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u))
  
predicted_ratings <- testSet %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  mutate(roundPred = flixStar(pred, userId %in% usersWhoWhole))

mean_umg_acc <- accuracy(testSet$rating, flixStar(predicted_ratings$pred))
mean_umg_accWholes <- accuracy(testSet$rating, predicted_ratings$roundPred)
mean_umg_RMSE <- RMSE(testSet$rating, predicted_ratings$pred)

mean_results <- bind_rows(mean_results, data_frame(Method="Movie+User+Genre", 
                                                   Accuracy = mean_umg_acc,
                                                   RMSE = mean_umg_RMSE,
                                                   AccuracyWhole = mean_umg_accWholes))

# Regularization
# Optimize lambda by minimizing RMSE
lambdas <- seq(4, 5.5, 0.1)
lRMSEs <- sapply(lambdas, function(l){

  b_i <- trainSet %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))

  b_u <- trainSet %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_g <- trainSet %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_i - b_u - mu)/(n()+l))

  predicted_ratings <- testSet %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by="genres") %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    mutate(roundPred = flixStar(pred))
  return(RMSE(testSet$rating, predicted_ratings$pred))
})
fig6 <- qplot(lambdas, lRMSEs)
fig6
lambda <- lambdas[which.min(lRMSEs)]

# now calculate the regularized accuracy with the best lambda
b_i <- trainSet %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda)) 

b_u <- trainSet %>%
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda)) 

b_g <- trainSet %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - b_i - b_u - mu)/(n()+lambda))

predicted_ratings <- testSet %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  mutate(roundPred = flixStar(pred, userId %in% usersWhoWhole))

mean_umgR_acc <- accuracy(testSet$rating, flixStar(predicted_ratings$pred))
mean_umgR_accWholes <- accuracy(testSet$rating, predicted_ratings$roundPred)
mean_umgR_RMSE <- RMSE(testSet$rating, predicted_ratings$pred)

mean_results <- bind_rows(mean_results, data_frame(Method="Reg Movie+User+Genre", 
                                                 Accuracy = mean_umgR_acc,
                                                 RMSE = mean_umgR_RMSE,
                                                 AccuracyWhole = mean_umgR_accWholes))

## Tune our ratings rounding strategy
# tune the % of the time a user gives integer ratings
wholes <- seq(0.4,1,0.05)
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
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    mutate(roundPred = flixStar(pred, userId %in% usersWhoWhole))
  
  return(accuracy(testSet$rating, predicted_ratings$roundPred))
})
fig7 <- qplot(wholes, waccs)
fig7
wholeCutoff <- wholes[which.max(waccs)]

mean_results <- bind_rows(mean_results, data_frame(Method="Reg Movie+User+Genre Integer Tune", 
                                                   AccuracyWhole = max(waccs)))

mean_results %>% knitr::kable()

# retabulate integer-happy users with optimized cutoff
usersWhoWhole <- trainSet %>% group_by(userId) %>%
  summarize(total = length(rating), 
            wholes = sum(rating %% 1 == 0), 
            wholepct = wholes/total) %>%
  filter(wholepct >= wholeCutoff) %>%
  .$userId



## That's as good as we're going to do on the model.
# Cool, cool.
# Now predict the validation set.
predicted_ratings <- validation %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  mutate(roundPred = flixStar(pred, userId %in% usersWhoWhole))

write.csv(predicted_ratings %>% select(userId, movieId, rating = roundPred),
          "submission.csv", na = "", row.names=FALSE)

fig8 <- predicted_ratings %>% ggplot() + 
  geom_histogram(aes(roundPred), binwidth = 0.5) +
  xlab("Predicted Rating") + ylab("# Ratings") + ggtitle('Predicted Rating Histogram')
fig8 
