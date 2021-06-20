
##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Exploratory Data Analysis

# Loading required packages

requiredPackages <- c("tidyverse","knitr","lubridate","caret")
lapply(requiredPackages, library, character.only = TRUE)

# Column summary values for training set
summary(edx)

#Dimensions of dataset
cat("The edx dataset has", nrow(edx), "rows and", ncol(edx), "columns.\n")
cat("There are", n_distinct(edx$userId), "different users and", n_distinct(edx$movieId), "different movies in the edx dataset.")

mu <- mean(edx$rating)
cat("The average rating is", mu)


# Plot distribution of ratings
edx %>% ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.2, color = "light blue", fill="steel blue") +
  scale_y_continuous(breaks = c(1000000, 2000000), labels = c("1", "2")) +
  labs(x = "Rating", y = "Count (in millions)")

# Users usually give a full rating rather than a rating and a half, as in they would rather give a 4 or a 5 rather
#than a 4.5

#Ratings per movies 
edx %>% count(movieId) %>% ggplot(aes(n))+
  geom_histogram(bins=30,color = "dark grey" , fill= "grey")+
  scale_x_log10()+
  ggtitle("Ratings per Movie")

#There are Movies with more ratings than others 

# Ratings per user
edx %>% count(userId) %>% ggplot(aes(n))+
  geom_histogram(bins=30, color = "light blue" , fill= "steel blue")+
  scale_x_log10()+
  ggtitle("Ratings Per User")

#There are users that rate more movies than others

# Average rating per user
  edx %>% group_by(userId) %>%
  summarise(ave_rating = sum(rating)/n()) %>%
  ggplot(aes(ave_rating)) +
  geom_histogram(bins=30, color = "dark grey", fill= "grey") +
  labs(x = "Average rating", y = "Number of users")+
  ggtitle(" Average Rating Per User")
  
#The plot shows that the average rating per user is between 3.5 and 4
  
# Group and list top 10 movie titles based on number of ratings
  edx %>% group_by(title) %>%
  summarise(n = n(), avg= mean(rating)) %>% slice_max(n, n=10)
  
#This table shows that Pulp Fiction is the most reviewed movie, but the top 10 indicates that blockbusters are
#the most rated movies
  
#Remove timestamp and change to date
   
   edx_n <- edx %>% mutate(review_year = year(as_datetime(timestamp)))%>%
   select(-timestamp,-genres,-userId)
   head(edx_n)
   

#Separate year from Movie title
  
   edx_n1 <- edx_n %>% 
   mutate(release_year = as.numeric(str_extract(str_extract(title, "[/(]\\d{4}[/)]$"), regex("\\d{4}"))),title = str_remove(title, "[/(]\\d{4}[/)]$")) 
   
     
   head(edx_n1)
   
#By executing some data wrangling we are able to gain some further insights on the data set
   
#Reviews per review year
   
   edx_n1 %>% select(review_year ) %>% 
   group_by(review_year) %>% 
   summarise(count = n())%>% 
   arrange(desc(count))
   
   
   edx_n1 %>% select(review_year ) %>% 
   group_by(review_year) %>% 
   summarise(count = n())%>% ggplot(aes(review_year,count))+
   geom_bar(stat="identity", fill="steel blue")+
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
   ggtitle(" Reviews per Review Year")
   
#The number of reviews per year fluctuates, and is not incremental per year
   
  
# Reviewed Movies per release year 
   
    edx_n1 %>% select(movieId, release_year) %>% 
    group_by(release_year) %>% 
    summarise(count = n())  %>% 
    arrange(desc(count)) %>% top_n(10)
    
    edx_n1 %>% select(movieId, release_year) %>% 
      group_by(release_year) %>% 
      summarise(count = n())  %>%  ggplot(aes(release_year,count))+
      geom_point()+
      geom_line()+
      ggtitle(" Reviews per Release Year")
    
# The most reviewed movies were released in 1990's with the trend in reviews trending downward for movies
#released in later years. 
   
     
#RMSE Model
     
#We add a parameter to account for the average rating of each movie
     
     mu <- mean(edx$rating)
     mu
     
     rmse1 <- RMSE(validation$rating, mu)
     rmse1
     
#Takes into account movie effects by mu + b_i
#b_i is the mean difference of the average rating mu from each movie rating
     
     movie_avgs <- edx %>%
     group_by(movieId) %>%
     summarise(b_i = mean(rating - mu))
     
     head(movie_avgs)
     
# The predicted ratings in y_hat_b_i are based on the mean rating and movie-dependant parameters b_i
     
     y_hat_b_i <- validation %>%
     left_join(movie_avgs, by = "movieId") %>%
     mutate(pred = mu + b_i) %>%
     pull(pred)
     
     rmse2 <- RMSE(validation$rating, y_hat_b_i)
     rmse2
     
#Takes into account movie effects and user effects by mu + b_i +b_u
# The mean difference of the average rating mu and movie parameter b_i from each user rating
     
     user_avgs <- edx %>% left_join(movie_avgs, by = "movieId") %>%
     group_by(userId) %>%
     summarize(b_u = mean(rating - mu - b_i))
     
     head(user_avgs)
     
# The predicted ratings in y_hat_b_i_b_u are based on the mean rating, movie-dependant parameters b_i, 
#and user dependant parameter b_u
     
     y_hat_b_i_b_u <- validation %>% 
     left_join(movie_avgs, by='movieId') %>%
     left_join(user_avgs, by='userId') %>%
     mutate(pred = mu + b_i + b_u) %>%
     pull(pred)
     
     rmse3 <- RMSE(validation$rating, y_hat_b_i_b_u)
     rmse3
     
     
# We use regularization to take into account the number of ratings per movie
# to diminish the b_i effect of movies with a small number of ratings
     
     lambdas <- seq(0, 10, 0.25)
     
     
     rmses <- sapply(lambdas, function(l){
       
     mu <- mean(edx$rating)
       
     b_i <- edx %>%
     group_by(movieId) %>%
     summarise(b_i = sum(rating - mu)/(n()+l))
       
     b_u <- edx %>%
     left_join(b_i, by="movieId") %>%
     group_by(userId) %>%
     summarise(b_u = sum(rating - b_i - mu)/(n()+l))
       
     predicted_ratings <- validation %>%
     left_join(b_i, by = "movieId") %>%
     left_join(b_u, by = "userId") %>%
     mutate(pred = mu + b_i + b_u) %>%
     pull(pred)
       
     return(RMSE(predicted_ratings, validation$rating))
       
     })
     
     rmse_regularisation <- min(rmses)
     rmse_regularisation
     
## Plot RMSE against Lambdas to find optimal lambda
     qplot(lambdas, rmses)
     lambda <- lambdas[which.min(rmses)]
     lambda