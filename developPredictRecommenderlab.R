## Script to develop prediction
library(tidyverse)
library(dslabs)

data("movielens")
movielens <- select(movielens, -year) # to match edx set which doesn't have year


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

# prepare the Recommenderlab matrix
library(recommenderlab)
rm <- movielens %>% select(userId, movieId, rating) %>% as("realRatingMatrix")

e <- evaluationScheme(rm, method="split", train=0.9, given = -10)

r1 <- Recommender(getData(e, "train"), method = "UBCF", parameter=list(nn=10))


p1 <- predict(r1, getData(e, "known"), type="ratings") # fills in ratings for unknowns, knowns are NA
error <-  calcPredictionAccuracy(p1, getData(e, "unknown")) # versus the actual "unknown" ratings

unknown <- as(getData(e, "unknown"), "data.frame") %>% as.tibble()
unknown_predict <- p1 %>% as("data.frame") %>% as.tibble()

compare <- inner_join(unknown, unknown_predict, by = c("user", "item"), suffix = c("Actual", "Predicted"))

accuracy(compare$ratingActual, flixStar(compare$ratingPredicted))

# explore ratings
# proportion of whole number ratings
sum(getRatings(rm) %% 1 == 0) / length(getRatings(rm))

# proportion of whole number ratings per user
movielens %>% group_by(userId) %>%
  summarize(total = length(rating), 
            wholes = sum(rating %% 1 == 0), 
            wholepct = wholes/total) %>%
  ggplot(aes(wholepct)) + geom_histogram(binwidth = 0.1)



# We want to tag certain users as always or nearly always assigning whole number ratings
# Explore the "nearly" cutoff
wholeCutoff <- seq(0,1,0.05)  # % of time this user assigns a whole number

# Assess how many half ratings we leave on the table this cutoff
halfsSet <- sapply(wholeCutoff, function(cutoff) {
  usersWhoWhole <- movielens %>% group_by(userId) %>%
    summarize(total = length(rating), 
              wholes = sum(rating %% 1 == 0), 
              wholepct = wholes/total) %>%
    filter(wholepct >= cutoff) %>%
    .$userId
  
  total <- movielens %>% group_by(userId) %>% filter(userId %in% usersWhoWhole) %>%
    summarize(wholes = sum(rating %% 1 == 0),
              total = length(rating),
              halfs = total-wholes) %>%
    summarize(totalWhole = sum(wholes), totalHalf = sum(halfs))
  
  total$totalHalf
    
})

qplot(wholeCutoff, halfsSet / length(getRatings(rm)))
# Set it at 0.75, this miscategorizes about 1% of all ratings


usersWhoWhole <- movielens %>% group_by(userId) %>%
  summarize(total = length(rating), 
            wholes = sum(rating %% 1 == 0), 
            wholepct = wholes/total) %>%
  filter(wholepct > 0.75) %>%
  .$userId

movielens %>% group_by(userId) %>% filter(userId %in% usersWhoWhole) %>%
  ggplot() + aes(rating) + geom_histogram(binwidth = 0.5)

length(usersWhoWhole)
length(usersWhoWhole) / movielens %>% group_by(userId) %>% summarize() %>% nrow()

compare <- compare %>% mutate(userWhoWholes = user %in% usersWhoWhole)
accuracy(compare$ratingActual, flixStar(compare$ratingPredicted, whole = compare$userWhoWholes))

# algorithms <- list(
#    "random items" = list(name="RANDOM", param=NULL),
#    "popular items" = list(name="POPULAR", param=NULL),
#    "user-based CF" = list(name="UBCF", param=list(nn=25)),
#    "item-based CF" = list(name="IBCF", param=list(k=10)),
#    "SVD approximation" = list(name="SVD", param=list(k = 50))
# )
# 
# results <- evaluate(e, algorithms, type = "ratings")
# 

