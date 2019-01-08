## Script to develop prediction
library(tidyverse)
library(recommenderlab)

# A function to calculate accuracy in terms of % correct
accuracy <- function(true_ratings, predicted_ratings) {
  correct <- sum(true_ratings == predicted_ratings)
  pctAc <- correct / length(true_ratings)
  
  pctAc
}

# A general function to discretize a vector ratings with optional Whole flag for integers only
flixStar <- function(ratings, whole = FALSE) {
  ratings <- replace(ratings, ratings <= 0.5, 0.51) # zero not allowed, nudge for IEEE rounding
  ratings <- replace(ratings, ratings > 5, 5) # 5 is max rating
  
  data.frame(ratings, whole) %>% 
    rowwise() %>% 
    mutate( new = if (whole) round(ratings + 0.01) else round(ratings*2)/2 ) %>% # IEEE rounding
    .$new
}

edxMatrix <- edx %>% select(userId, movieId, rating) %>% as("realRatingMatrix")

e <- evaluationScheme(edxMatrix, method="split", train=0.9, given = -10)

r1 <- Recommender(getData(e, "train"), method = "IBCF", parameter=list(k=10))
