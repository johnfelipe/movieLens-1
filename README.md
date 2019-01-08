# movieLens
Intro to Data Science project

For this project, you will be creating a movie recommendation system using the MovieLens dataset. The version of movielens included in the dslabs package (which was used for some of the exercises in PH125.8x: Data Science: Machine Learning) is just a small subset of a much larger dataset with millions of ratings. You can find the entire latest MovieLens dataset here. You will be creating your own recommendation system using all the tools we have shown you throughout the courses in this series. We will use the 10M version of the MovieLens dataset to make the computation a little easier.

You will download the MovieLens data and run code we will provide to generate your datasets.

First, there will be a short quiz on the MovieLens data. You can view this quiz as an opportunity to familiarize yourself with the data in order to prepare for your project submission.

Second, you will train a machine learning algorithm using the inputs in one subset to predict movie ratings in the validation set. You will submit your predicted movie ratings in a submission file. Your project itself will be assessed by peer grading, and part of the peer grading will involve running an automated grading script.

## Files included:
createDataSets.R is as supplied by the course.  It downloads the movielens dataset and splits it into edx and validation.  This will be called if these objects don't exist yet.

report.Rmd includes the actual prediction script that develops the model and applies it to the validation list, along with the report text and figures.  It is self-contained and will call createDataSets.R if needed.

report.pdf is the final report.

submission.csv is the predicted ratings for the validation set, ready for grading.

The remaining files were used in development and testing and are only saved for reference.