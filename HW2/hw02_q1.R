#############################
# Denys Liubyvyi, UNI: dvl2110
# STAT W4240 
# Homework 2 , Problem 1
# Homework Due September30
#
#############################

#################
# Setup
#################
require(graphics)
require(fields)
#change working directory
setwd("/Users/dennis/github/W4240/HW2/")

#######################################################################################
# a. (2 Points) Load hw02 q1 p1 fall14.csv. Find the column means and the row means for
# the data. What do these values tell us about this data set?
#######################################################################################

#load file and clean the data
data1 <- read.csv("hw02_q1_p1_fall14.csv")
data1 <- na.omit(data1)

#see the data
pairs(data1)

#calcuate means of rows
rows.means <- apply(data1,1,mean)
cat('Means of rows summary:')
print(as.numeric(rows.means))
summary(rows.means)
#hist(rows.means,breaks = 50)

#calcuate means of columns
cols.means <- apply(data1,2,mean)
cat('Means of columns summary:')
print(as.numeric(cols.means))
summary(cols.means)
#hist(cols.means,breaks = 50)

cat('It seems like the data is symmetric and unimodal')

#######################################################################################
# b. (3 Points) Center the data and and find the empirical covariance matrix, Σˆ. 
# This should be a 5-by-5 matrix. What do the diagonal values of the covariance matrix 
# tell us about this data set? What do the off diagonal elements tell us about this 
# data set?
#######################################################################################

#let's center the data!
data1.centered <- scale(data1,center=TRUE,scale=FALSE)

#let's find the empirical covariance matrix
data1.cov <- cov(data1.centered)
cat('Our covariance matrix:')
print(data1.cov)


# check the dimensions
cat('Dimenstions of our covariance matrix:',nrow(data1.cov),'x',ncol(data1.cov))

# What do the diagonal values of the covariance matrix tell us about this data set?
cat('Covariance between X1 and X2 in the original data:',cov(data1[,1],data1[,2]))
cat('Diagonal X1<->X2 of the covariance matrix:',data1.cov[1,2],data1.cov[2,1])
cat('As we see, the entries on the diagonal of the covariance matrix are the 
variances of each element of the initial matrix.')

#######################################################################################
# c. (5 Points) Give the eigenvalues and associated eigenvectors of Σˆ. 
# Why does this matrix have the same left eigenvectors as right eigenvectors?
#######################################################################################

# get the right eigenvectors
data1.eigen.right <- eigen(data1.cov)
cat('The right eigenvectors:')
print(data1.eigen.right)

# get the left eigenvectors
data1.eigen.left <- eigen(t(data1.cov))  
cat('The left eigenvectors:')
print(data1.eigen.left)

cat("Since our initial covariance matrix is symmetric, transpose operation 
doesn't change it so our right eighten vectors and left eighten vectors are equal.")

#######################################################################################
# d. (5 Points) Give all of the loadings and all of the scores for the data.
#######################################################################################

#find the loadings
ppc1 <- prcomp(data1.centered)
data1.loadings <- ppc1$rotation
cat('Data loadings:')
print(data1.loadings)

#find the scores
data1.scores <- ppc1$x
cat('Data scores:')
print(data1.scores)

#######################################################################################
# e. (5 Points) Plot the proportion of variance captured against the number of 
# components included. How many components should we include and why?
#######################################################################################
var.data.1 <- ppc1$sdev^2/sum(ppc1$sdev^2)
barplot(var.data.1, names.arg =colnames(data1.scores))

cat('Defenitely we should include to our analysis two first components because they 
covers most of the variance in the data')

#######################################################################################
# f. (5 Points) Load hw02 q1 p2 fall14.csv. This has 5 new observations in the 
# original coordinates. Give their scores.
#######################################################################################

#load file with new observations and clean the data
f.data <- as.matrix(read.csv("hw02_q1_p2_fall14.csv"))
f.data <- na.omit(f.data)
f.cols.means <- colMeans(f.data)
f.data.centered <- t(apply(f.data,1,function(x) {x-cols.means}))

#compare old and new means
cat('Compare means:')
cat('Old dataset mean:')
print(cols.means)
cat('New dataset mean:')
print(f.cols.means)

cat('Dimenstions of our data:')
print(dim(f.data))

#give their scores
f.scores <- f.data.centered %*% (data1.loadings)
cat('Scores of our new data:')
print(f.scores)

#######################################################################################
# g. (5 Points) Now use only the first two scores to represent the observations 
# from the previous part. What are the coordinates of the projections in the original 
# space, x′? What is their Euclidean distance from the original data points?
#######################################################################################

#aplly 2 pc to the data 
f.data.back <- f.data.centered + f.scores[,1:2] %*% t(data1.loadings[,1:2])

cat("The coordinates of the projections in the original space")
#if we uncomment this we can check what happens when we use all the principal components
print(f.data.back)

#calculate distances
dist <- rdist(f.data.back,f.data)
cat('Euclidian distance from the original data points:',diag(dist))


#######################################################################################
# h. (5 Points) Define the error of a point as
# d(x′, x) = x′ − x,
# which is a 5-dimensional vector of errors. In what direction is d(x′,x) 
# for the 5 new points? Why do you think this is?
#######################################################################################

#calculate errors
errors <- as.matrix(f.data.back-f.data)
cat('Error of a point:')
print(errors)
cat("We loose part of information when we project the data to two-dimensional 
space and then project it back. We generally keep the % of information 
only associated with two first principal components. That's why we have this error.")

