#Hands-on excercises
#install.packages("data.table")
#install.packages("recommenderlab")

library(data.table)
library(recommenderlab)

# movies
movies <- read.csv("~/Desktop/Rec_sys/ml-latest-small/movies.csv")
str(movies)
head(movies, n = 10)

# ratings
ratings <- read.csv("~/Desktop/Rec_sys/ml-latest-small/ratings.csv")
str(ratings)
head(ratings, n=10)

#Visualization
library(ggplot2)
library(hrbrthemes)
ggplot(data = ratings, aes(x =rating)) + 
  geom_histogram(binwidth = 0.6, fill = "cyan") + 
  theme_ipsum_tw()

ratings$timehours = ratings$timestamp/(60*60*24)

ggplot(data = ratings, aes(x =rating)) + 
  geom_histogram(binwidth = 0.6, fill = "cyan") + 
  #geom_density(aes(y = ..density..)) + 
  theme_ipsum_tw()

#Exercise 3
movies$genres
?tstrsplit

#Method 1
movgen <- data.table(movies$genres)
A <- movgen[, c("c1","c2","c3","c4","c5","c6","c7","c8","c9","c10") := tstrsplit(V1, "|", fixed=TRUE)]
B <- A[,c("c1","c2","c3","c4","c5","c6","c7","c8","c9","c10")]

#Method 2
movgen <- as.data.frame(movies$genres, stringAsFactors=FALSE)
movgen2 <- as.data.frame(tstrsplit(movgen[,1], '[|]', type.convert = TRUE), stringsAsFactors = FALSE)
  colnames(movgen2) <- c(1:10)
head(movgen2, n=10)
head(B, n=10)

# Excercise 4
movgen_list <- c ("Action", "Adventure", "Animation",
                  "Children", "Comedy", "Crime", "Documentary", "Drama",
                  "Fantasy", "Film-Noir", "Horror", "Musical", "Mystery",
                  "Romance", "Sci-Fi", "Thriller", "War", "Western")

movgen_matrix <- matrix(nrow = 9125, ncol = 18)
colnames(movgen_matrix) <- movgen_list

for (i in 1:nrow(movgen2)){
  a <- movgen2[i,]
  for (j in 1:length(movgen_list)){
    b<-movgen_list[j]
    if (b %in% a){
      movgen_matrix[i,j] = 1
    } else {
      movgen_matrix[i,j] = 0
    }
  }
}

movgen_matrix2 <- data.frame(movgen_matrix)

for (i in 1:ncol(movgen_matrix2)) {
  movgen_matrix2[,i] = lapply(movgen_matrix2[,i], as.integer)
}

# Exercise 5

ratings <- read.csv("~/Desktop/Rec_sys/ml-latest-small/ratings.csv")

ratings$rating <- as.integer(ratings$rating)
  
index <- ratings$rating %in% c(1,2,3) 
ratings$rating[index] <- -1

index <- ratings$rating %in% c(4,5) 
ratings$rating[index] <- 1

binary_ratings <- ratings

binary_ratings2 <- dcast(binary_ratings, movieId~userId, value.var = "rating", na.rm=FALSE)

binary_ratings2[is.na(binary_ratings2)] <- 0

####### ----
#### From here it is CF

# 5.
binary_ratings2 <- dcast(binary_ratings, movieId~userId, value.var = "rating", na.rm=FALSE)
for (i in 1:ncol(binary_ratings2)){
  binary_ratings2[which(is.na(binary_ratings2[,i]) == TRUE),i] <- 0
}
binary_ratings2 = binary_ratings2[,-1] #remove movieIds col. Rows are movieIds, cols are userIds

binary_ratings2

# 6.
#Remove rows that are not rated from movies dataset
unique_movieIds <- unique(movies$movieId) #9125
unique_ratings <- unique(ratings$movieId) #9066 #likes and dislikes
movies2 <- movies[-which((unique_movieIds %in% unique_ratings) == FALSE),]
rownames(movies2) <- NULL
#Remove rows that are not rated from movgen_matrix2
movgen_matrix3 <- movgen_matrix2[-which((unique_movieIds %in% unique_ratings)
                                        == FALSE),]
rownames(movgen_matrix3) <- NULL

# dot product: insight about preference
# as.matrix(movgen_matrix3) %*% as.matrix(binary_ratings2)
library(pracma)

#matrix product: dot <- as.matrix(movgen_matrix3) %*% as.matrix(binary_ratings2)
dot2 <- as.matrix(movgen_matrix3) %*% as.matrix(t(binary_ratings2))



result = matrix(0,18,672)
for (c in 1:ncol(binary_ratings2)){
  for (i in 1:ncol(movgen_matrix3)){
    result[i,c] <- sum(t(movgen_matrix3[,i])) * (binary_ratings[,c])
  }
}

# Convert to binary

for (i in 1:nrow(result)){
  for(j in 1:ncol(result)){
    if (result[i,j]<0){
      result[i,j] <- sum(t(movgen_matrix3[,i])*(binary_ratings2[,c]))
    }
  }
}

  # Ex 6: Creation of the user recommendation

#
#
#

# Calculate Jaccard distance

# Make recommendation

############# ----
# Using Recommender LAb (code from lectures on pad.carpentries)

library(recommenderlab)
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
  recom_result[i] <- movies[as.integer(recom_list[[1]][i]),2]
}

#