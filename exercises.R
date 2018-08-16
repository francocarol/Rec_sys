# Recomender Systems hands-on Exercises
#CODATA_RDA Summer School
#Caroline Franco

# install.packages(“data.table”)
# install.packages(“ggplot2”)
# install.packages(“recommenderlab”)

# Ex 1: ----

movies = read.csv("~/Desktop/Rec_sys/ml-latest-small/movies.csv") #load movies.csv file

str(movies) #list the structure of movies
head(movies)

ratings = read.csv("~/Desktop/Rec_sys/ml-latest-small/ratings.csv")
str(ratings)
head(ratings)

# Ex 2: Visualization ----

library(ggplot2)

g = ggplot(data=ratings, aes(x=rating))

g + geom_histogram()

mov_and_rat = cbind.data.frame()
g + geom_histogram(aes(fill=factor(rating)), binwidth = 0.5)+
  geom_histogram(aes(y=..density..), binwidth=0.5) +
  #geom_histogram(data=ratings[ratings$movieId==1,], ) +
  theme_light() #+
#facet_wrap(~ decade)

g + #geom_histogram(aes(fill=factor(rating)), binwidth = 0.5)+
  geom_histogram(aes(y=..density..), binwidth=0.5) +
  #geom_histogram(data=ratings[ratings$movieId==1,], ) +
  theme_light() #+
#facet_wrap(~ decade)

# how do add a density linwe?

# Ex 3: ----

library(data.table)
??tstrsplit

movgen <- as.data.frame(movies$genres, stringsAsFactors=F)

#movGen = data.table(tstrsplit(movies$genres, "|", fixed=T))
#(movGen)

movGen <- data.table(tstrsplit(movgen[,1], '[|]', type.convert=T), stringsAsFactors=F)
colnames(movGen) <- c(1:7)

head(movGen, n=1)

# Another version:
#
#
#

# Ex 4: ----

# 18 movie genres
movgen_list <- c ("Action", "Adventure", "Animation",
                  "Children", "Comedy", "Crime", "Documentary", "Drama",
                  "Fantasy", "Film-Noir", "Horror", "Musical", "Mystery",
                  "Romance", "Sci-Fi", "Thriller", "War", "Western")

n <- nrow(movGen)
movgen_matrix <- matrix(nrow = n+1, ncol = 18)
movgen_matrix[1,] <- movgen_list
colnames(movgen_matrix) <- movgen_list

#trying func apply
##for (i in n+1) {
#movgen_matrix <- as.integer( lapply((movGen, movgen_list)) )
#movgen_matrix <-  #sapply(
#  apply(movGen, 1, '==', movgen_list)#, as.integer
  #) 
##}
#head(movgen_matrix)

#iterate through matrix
for (i in 1:nrow(movGen)){
  for (c in 1:ncol(movGen)) {
    genmat_col = which(movgen_matrix[1,] == movGen[i,c])
    movgen_matrix[i+1,genmat_col] <- 1
  }
}

# convert into dataframe
movgen_matrix2 <- as.data.frame(movgen_matrix[-1,], stringsAsFactors = F) # remove first row, which was the genre list
for (c in 1:col(movgen_matrix2)) {
  movgen_matrix2[,c] <- as.integer(movgen_matrix2[,c])
} # convert from characters to integers

## DOESNT WORK
#
#
#

# Ex 5: User matrix

for (i in nrow(binary_ratings)) {
  if (binary_ratings[i,3]>3){
    binary_ratings[i,3] <- 1
  }
}else{
  binary_ratings[i,3] <-1
}

binary_ratings2 <- dcast(binary_ratings, movieId~userId, value.var = "rating", na.rm=FALSE)
for (i in 1:ncol(binary_ratings2)){
  binary_ratings2[which(is.na(binary_ratings2[,i]) == TRUE),i] <- 0
}
binary_ratings2 = binary_ratings2[,-1] #remove movieIds col. Rows are movieIds, cols are userIds

# dcast() function:
