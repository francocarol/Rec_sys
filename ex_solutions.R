
# Hands-On Exercise: 
# Implementing a Basic Recommender Engine for Movies
#
# Instructor: Dr. Ekpe Okorafor
# August 14th, 2018
# DataTrieste18


install.packages("data.table")
install.packages("ggplot2")
install.packages("recommenderlab")

# Exercise 1
movies = read.csv("/Users/ekpe/recommender/ml-latest-small/movies.csv") #load movies.csv file
str(movies)/#list the structure of movies
  
  ratings = read.csv("/Users/ekpe/recommender/ml-latest-small/ratings.csv") #load ratings.csv file
str(ratings) #list the structure of ratings

# Exercise 2
library(ggplot2)
plot <- ggplot(ratings, aes(x = rating)) + geom_histogram()
plot

# Exercise 3
library(data.table)
movgen <- as.data.frame(movies$genres, stringsAsFactors=FALSE)
movgen2 <- as.data.frame(tstrsplit(movgen[,1], '[|]', type.convert = TRUE), stringsAsFactors = FALSE)
colnames(movgen2) <- c(1:7)
head(movgen2, n=4)

# Exercise 4
movgen_list <- c("Action", "Adventure", "Animation", "Children", "Comedy", "Crime","Documentary", "Drama", "Fantasy","Film-Noir", "Horror", "Musical", "Mystery","Romance","Sci-Fi", "Thriller", "War", "Western")
movgen_matrix <- matrix(0,9126,18) #empty matrix
movgen_matrix[1,] <- movgen_list #set first row to genre list
colnames(movgen_matrix) <- movgen_list #set column names to genre list

#iterate through matrix
for (i in 1:nrow(movgen2)) {
  for (c in 1:ncol(movgen2)) {
    genmat_col = which(movgen_matrix[1,] == movgen2[i,c])
    movgen_matrix[i+1,genmat_col] <- 1
  }
}

#convert into dataframe
movgen_matrix2 <- as.data.frame(movgen_matrix[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list
for (c in 1:ncol(movgen_matrix2)) {
  movgen_matrix2[,c] <- as.integer(movgen_matrix2[,c])
} #convert from characters to integers

# Exercise 5
binary_ratings <- ratings
for (i in 1:nrow(binary_ratings)){
  if (binary_ratings[i,3] > 3){
    binary_ratings[i,3] <- 1
  }
  else{
    binary_ratings[i,3] <- -1
  }
}
head(binary_ratings, n=7)

binary_ratings2 <- dcast(binary_ratings, movieId~userId, value.var = "rating", na.rm=FALSE)
for (i in 1:ncol(binary_ratings2)){
  binary_ratings2[which(is.na(binary_ratings2[,i]) == TRUE),i] <- 0
}
binary_ratings2 = binary_ratings2[,-1] #remove movieIds col. Rows are movieIds, cols are userIds
dim(binary_ratings2)

#Remove rows that are not rated from movies dataset
unique_movieIds <- (unique(movies$movieId)) #9125
unique_ratings <- (unique(ratings$movieId)) #9066
movies2 <- movies[-which((unique_movieIds %in% unique_ratings) == FALSE),]
rownames(movies2) <- NULL
dim(movies2)

#Remove rows that are not rated from movgen_matrix2
movgen_matrix3 <- movgen_matrix2[-which((unique_movieIds %in% unique_ratings) == FALSE),]
rownames(movgen_matrix3) <- NULL
dim(movgen_matrix3)

#Calculate dot product for User Profiles
result = matrix(0,18,671)
for (c in 1:ncol(binary_ratings2)){
  for (i in 1:ncol(movgen_matrix3)){
    result[i,c] <- sum((movgen_matrix3[,i]) * (binary_ratings2[,c]))
  }
}
#Convert to Binary scale
for (i in 1:nrow(result)){
  for (j in 1:ncol(result)) {
    if (result[i,j] < 0){
      result[i,j] <- 0
    }
    else {
      result[i,j] <- 1
    }
  }
}

# Exercise 6
result2 <- result[1,] #First user's profile
sim_mat <- rbind.data.frame(result2, movgen_matrix3)
sim_mat <- data.frame(lapply(sim_mat,function(x){as.integer(x)})) #convert data to type integer

#Calculate Jaccard distance between user profile and all movies
library(proxy)
sim_results <- dist(sim_mat, method = "Jaccard")
sim_results <- as.data.frame(as.matrix(sim_results[1:9066]))
rows <- which(sim_results == min(sim_results))
#Recommended movies
movies[rows,]

#Exercise 7
library(recommenderlab)
recommender_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommender_models) # Display models
lapply(recommender_models, "[[", "description") # Describe models
recommender_models$UBCF_realRatingMatrix$parameters # List parameters of model

library(reshape2)
#Create ratings matrix. Rows = userId, Columns = movieId
ratingmat <- dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingmat <- as.matrix(ratingmat[,-1]) #remove userIds

#Convert rating matrix into a recommenderlab sparse matrix
ratingmat <- as(ratingmat, "realRatingMatrix")

#Normalize the data
ratingmat_norm <- normalize(ratingmat)

#Create Recommender Model. "UBCF" stands for User-Based Collaborative Filtering
recommender_model <- Recommender(ratingmat_norm, method = "UBCF", param=list(method="Cosine",nn=30))
recom <- predict(recommender_model, ratingmat[1], n=10) #Obtain top 10 recommendations for 1st user in dataset
recom_list <- as(recom, "list") #convert recommenderlab object to readable list

#Obtain recommendations
recom_result <- matrix(0,10)
for (i in c(1:10)){
  recom_result[i] <- movies[as.integer(recom_list[[1]][i]),1]
}
recom_result

