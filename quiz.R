library(tidyverse)

## Quiz: MovieLens dataset

## Q1
# How many rows and columns are there in the edx dataset?

rows <- dim(edx)[1]
columns <- dim(edx)[2]

## Q2
# How many zeros were given as ratings in the edx dataset?
# How many threes were given as ratings in the edx dataset?

zeros <- sum(edx$rating == 0)
threes <- sum(edx$rating == 3)

ratingCount <- lapply(seq(0,5,0.5), function(n)
  sum(edx$rating == n)
  ) %>% 
  setNames(as.character(seq(0,5,0.5)))

edx %>% filter(rating == 0) %>% tally()
edx %>% filter(rating == 3) %>% tally()


# Q3
# How many different movies are in the edx dataset?

uniqueMovies <- length(unique(edx$movieId))
n_distinct(edx$movieId)

# Q4
# How many different users are in the edx dataset?

uniqueUsers <- n_distinct(edx$userId)


# Q5
# How many movie ratings are in each of the 
# following genres in the edx dataset?

genreList <- c('Drama', 'Comedy', 'Thriller', 'Romance')
genreCounts <- sapply(genreList, function(g){
  edx %>% filter(str_detect(genres, g)) %>% tally()
})

edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n())

# Q6
# Which movie has the greatest number of ratings?

numRatings <- edx %>% group_by(movieId) %>% 
  summarize(numRatings = n(), movieTitle = first(title)) %>%
  arrange(desc(numRatings)) %>%
  top_n(10, numRatings)
               

# Q7
# What are the five most given ratings in order from most to least?

numRatings <- edx %>% group_by(rating) %>% 
  summarize(number = n())

numRatings %>% top_n(5) %>% arrange(desc(number))

# Q8
# True or False: In general, half star ratings are less common than whole star
# ratings (e.g., there are fewer ratings of 3.5 than there are ratings of 3 or
# 4, etc.).
numRatings %>%
  mutate(halfStar = rating %% 1 == 0.5) %>%
  group_by(halfStar) %>%
  summarize(number = sum(number))

edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()
