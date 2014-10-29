#############################
# Denys Liubyvyi, UNI: dvl2110
# STAT W4240 
# Homework 3, Problem 4
# Due October 14
#
# The following code loads the eigenfaces data and
# performs a set of simple loading and plotting functions
#############################

#################
# Setup
#################

# make sure R is in the proper working directory
# note that this will be a different path for every machine
setwd("/Users/dennis/github/W4240/HW2/")



# first include the relevant libraries
# note that a loading error might mean that you have to
# install the package into your R distribution.  
library(pixmap)

#################
# Problem 4a
#################

views_4a = c('P00A+000E+00', 'P00A+005E+10', 'P00A+005E-10', 'P00A+010E+00' )

# load the data and save it as a matrix with the name face_matrix_4a

#----- START YOUR CODE BLOCK HERE -----#

dir_list_1 = dir(path="CroppedYale/",all.files=FALSE)
dir_list_2 = dir(path="CroppedYale/",all.files=FALSE,recursive=TRUE)

# the list of pictures (note the absence of 14 means that 31 corresponds to yaleB32)
pic_list = 1:length(dir_list_1)
view_list = views_4a

# find lengths
len_dl1 = length(dir_list_1)
len_dl2 = length(dir_list_2)

#store original photo dimentions
photo.dim <- c(192,168)

# Pre-allocate a matrix with dimensions (number of pictures) x (number of pixels per picture)
n_rows <- length(pic_list)*length(view_list)
n_cols <- photo.dim[1]* photo.dim[2]
face_matrix_4a <- matrix(NA,nrow = n_rows,ncol = n_cols)
subj_view <- data.frame(subject=character(),view=character())

cat('Dimenstions of photo matrix:',dim(face_matrix_4a))

# Load all of the pictures using some code from homework 1
rownames.list <- vector()
for (i in 1:length(pic_list)) {
    for (j in 1:length(view_list)) {
        filename <- sprintf("CroppedYale/%s/%s_%s.pgm", dir_list_1[pic_list[i]] , dir_list_1[pic_list[i]] , view_list[j])
        filename.short <- sprintf("%s_%s.pgm", dir_list_1[pic_list[i]] , view_list[j])
        photo <- as.vector(getChannels(read.pnm(filename)))
        face_matrix_4a[(i-1)*length(view_list)+j,] <- photo
        subj_view <- rbind(subj_view,data.frame(subject=dir_list_1[pic_list[i]],view=view_list[j]))
        rownames.list <- c(rownames.list,filename.short)
    }
}
rownames(face_matrix_4a) <- rownames.list
rownames(subj_view)<-rownames.list
#----- END YOUR CODE BLOCK HERE -----#

# Get the size of the matrix for use later
fm_4a_size = dim(face_matrix_4a)
# Use 4/5 of the data for training, 1/5 for testing
ntrain_4a = floor(fm_4a_size[1]*4/5) # Number of training obs
ntest_4a = fm_4a_size[1]-ntrain_4a # Number of testing obs
set.seed(1) # Set pseudo-random numbers so everyone gets the same output
ind_train_4a = sample(1:fm_4a_size[1],ntrain_4a) # Training indices
ind_test_4a = c(1:fm_4a_size[1])[-ind_train_4a] # Testing indices

#----- START YOUR CODE BLOCK HERE -----#
train_4a <- face_matrix_4a[ind_train_4a,]
names.train_4a <- as.vector(subj_view[ind_train_4a,1])
photo.names.train_4a <- as.vector(rownames.list[ind_train_4a])

test_4a <- face_matrix_4a[ind_test_4a,]
names.test_4a <- as.vector(subj_view[ind_test_4a,1])
photo.names.test_4a <- as.vector(rownames.list[ind_test_4a])





#What are the first 5 fles in the training set?
print(rownames(train_4a[1:5,]))
#What are the first 5 fles in the training set?
print(rownames(test_4a[1:5,]))
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 4b
#################

#----- START YOUR CODE BLOCK HERE -----#

#train_4a
mean.face.train_4a <- apply(train_4a,2,mean)
train_4a.centered <- t(apply(train_4a,1,function(x) {x-mean.face.train_4a}))

#Display the mean face
mean.face.train_4a.matrix=mean.face.train_4a
dim(mean.face.train_4a.matrix) <- photo.dim
mean.face.plot <- pixmapGrey(mean.face.train_4a.matrix)
plot(mean.face.plot,main='mean face train set')

#run PCA with prcomp 
train_4a.ppc <- prcomp(train_4a.centered)

# Project your testing data onto the first 25 loadings so that 
# it is also represented by the first 25 scores.
#test_4a.centered <- scale(test_4a,center = TRUE,scale=FALSE)
test_4a.centered <- t(apply(test_4a,1,function(x) {x-mean.face.train_4a}))
test_4a.25scores <- test_4a.centered %*% train_4a.ppc$rotation[,1:25]

# Use 1NN classification in the space of the first 25 scores to identify 
# the subject for each testing observation.
library(class)

knn.pred <- knn(train_4a.ppc$x[,1:25],test_4a.25scores,names.train_4a,k=1)

cat(sum((knn.pred==names.test_4a),na.rm=TRUE),'from',length(knn.pred),'identified correctly')

#----- END YOUR CODE BLOCK HERE -----#


#################
# Problem 4c
#################

# Use different lighting conditions

views_4c = c('P00A-035E+15', 'P00A-050E+00', 'P00A+035E+15', 'P00A+050E+00')

# load your data and save the images as face_matrix_4c

#----- START YOUR CODE BLOCK HERE -----#

view_list = views_4c

# Pre-allocate a matrix with dimensions (number of pictures) x (number of pixels per picture)
n_rows <- length(pic_list)*length(view_list)
n_cols <- photo.dim[1]* photo.dim[2]
face_matrix_4c <- matrix(NA,nrow = n_rows,ncol = n_cols)
subj_view <- data.frame(subject=character(),view=character())
cat('Dimenstions of photo matrix:',dim(face_matrix_4c))

# Load all of the pictures using some code from homework 1
rownames.list <- vector()
for (i in 1:length(pic_list)) {
    for (j in 1:length(view_list)) {
        filename <- sprintf("CroppedYale/%s/%s_%s.pgm", dir_list_1[pic_list[i]] , dir_list_1[pic_list[i]] , view_list[j])
        filename.short <- sprintf("%s_%s.pgm", dir_list_1[pic_list[i]] , view_list[j])
        photo <- as.vector(getChannels(read.pnm(filename)))
        face_matrix_4c[(i-1)*length(view_list)+j,] <- photo
        subj_view <- rbind(subj_view,data.frame(subject=dir_list_1[pic_list[i]],view=view_list[j]))
        rownames.list <- c(rownames.list,filename.short)
    }
}
rownames(face_matrix_4c) <- rownames.list



#----- END YOUR CODE BLOCK HERE -----#

fm_4c_size = dim(face_matrix_4c)
# Use 4/5 of the data for training, 1/5 for testing
ntrain_4c = floor(fm_4c_size[1]*4/5)
ntest_4c = fm_4c_size[1]-ntrain_4c
set.seed(2) # Set pseudo-random numbers
# You are resetting so that if you have used a random number in between the last use of sample(), you will still get the same output

ind_train_4c = sample(1:fm_4c_size[1],ntrain_4c)
#ind_train_4c = 1:152

ind_test_4c = c(1:fm_4c_size[1])[-ind_train_4c]
#ind_test_4c = 122:152

#----- START YOUR CODE BLOCK HERE -----#
train_4c <- face_matrix_4c[ind_train_4c,]
rownames(train_4c) <- rownames.list[ind_train_4c]
names.train_4c <- as.vector(subj_view[ind_train_4c,1])
photo.names.train_4c <- as.vector(rownames.list[ind_train_4c])

test_4c <- face_matrix_4c[ind_test_4c,]
rownames(test_4c) <- rownames.list[ind_test_4c]
names.test_4c <- as.vector(subj_view[ind_test_4c,1])
photo.names.test_4c <- as.vector(rownames.list[ind_test_4c])


#What are the first 5 fles in the training set?
print(rownames(train_4c[1:5,]))
#What are the first 5 fles in the training set?
print(rownames(test_4c[1:5,]))

#train_4c
mean.face.train_4c <- apply(train_4c,2,mean)
#train_4c.centered <- scale(train_4c,center = TRUE,scale=FALSE)
train_4c.centered <- t(apply(train_4c,1,function(x) {x-mean.face.train_4c}))

#Display the mean face
mean.face.train_4c.matrix=mean.face.train_4c
dim(mean.face.train_4c.matrix) <- photo.dim
mean.face.plot <- pixmapGrey(mean.face.train_4c.matrix)
par(mfrow=c(1,1))
plot(mean.face.plot,main='4c mean face')

#run PCA with prcomp 
train_4c.ppc <- prcomp(train_4c.centered)

# Project your testing data onto the first 25 loadings so that 
# it is also represented by the first 25 scores.
test_4c.centered <- t(apply(test_4c,1,function(x) {x-mean.face.train_4c}))
rownames(test_4c.centered) <- rownames.list[ind_test_4c]
#test_4c.centered <- scale(test_4c,center = TRUE,scale=FALSE)
test_4c.25scores <- test_4c.centered %*% train_4c.ppc$rotation[,1:25]
train_4c.25scores <- train_4c.centered %*% train_4c.ppc$rotation[,1:25]

# Use 1NN classification in the space of the first 25 scores to identify 
# the subject for each testing observation.

knn.pred <- knn(train_4c.25scores,test_4c.25scores,names.train_4c,k=1)
cat(sum((knn.pred==names.test_4c),na.rm=TRUE),'from',length(knn.pred),'identified correctly')


#chech the one's identified incorrectly
test_4c_initial_filenames_not_identified_correctly <- photo.names.test_4c[!(knn.pred==names.test_4c)]

#which photo condition is this? 
test_4c_condition_of_photos_not_identified_correctly <- subj_view[which(rownames.list %in% test_4c_initial_filenames_not_identified_correctly),2]

#what are wrong guesses?
test_4c_knn_wrong_names <- knn.pred[!(knn.pred==names.test_4c)]
test_4c_knn_wrong_names_filenames <- paste(test_4c_knn_wrong_names,'_',test_4c_condition_of_photos_not_identified_correctly,'.pgm',sep='')

face.plot <- function(photo_vector) {
    dim(photo_vector) <- photo.dim
    face_plot <- pixmapGrey(photo_vector)
    plot(face_plot)    
}


par(mfrow=c(2,2))
for (i in 1:min(length(test_4c_knn_wrong_names),8)) {
photo_vector1 <- face_matrix_4c[test_4c_initial_filenames_not_identified_correctly[i],]
dim(photo_vector1) <- photo.dim
face_plot1 <- pixmapGrey(photo_vector1)
plot(face_plot1)    


photo_vector2 <- face_matrix_4c[test_4c_knn_wrong_names_filenames[i],]
dim(photo_vector2) <- photo.dim
face_plot2 <- pixmapGrey(photo_vector2)
plot(face_plot2)    

}



#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 4d
#################

#----- START YOUR CODE BLOCK HERE -----#

for (i in 1:10) {
    fm_4d_size = dim(face_matrix_4c)
    
    # Use 4/5 of the data for training, 1/5 for testing
    ntrain_4d = floor(fm_4d_size[1]*4/5)
    ntest_4d = fm_4d_size[1]-ntrain_4d
    ind_train_4d = sample(1:fm_4d_size[1],ntrain_4d)
    ind_test_4d = c(1:fm_4d_size[1])[-ind_train_4d]
    
    
    train_4d <- face_matrix_4c[ind_train_4d,]
    rownames(train_4d) <- rownames.list[ind_train_4d]
    names.train_4d <- as.vector(subj_view[ind_train_4d,1])
    photo.names.train_4d <- as.vector(rownames.list[ind_train_4d])
    
    test_4d <- face_matrix_4c[ind_test_4d,]
    rownames(test_4d) <- rownames.list[ind_test_4d]
    names.test_4d <- as.vector(subj_view[ind_test_4d,1])
    photo.names.test_4d <- as.vector(rownames.list[ind_test_4d])
    
    #train_4d
    
    mean.face.train_4d <- apply(train_4d,2,mean)
    train_4d.centered <- t(apply(train_4d,1,function(x) {x-mean.face.train_4d}))
    rownames(train_4d.centered) <- rownames.list[ind_train_4d]
    
    #run PCA with prcomp 
    train_4d.ppc <- prcomp(train_4d.centered)
    
    # Project your testing data onto the first 25 loadings so that 
    # it is also represented by the first 25 scores.
    
    test_4d.centered <- t(apply(test_4d,1,function(x) {x-mean.face.train_4d}))
    rownames(test_4d.centered) <- rownames.list[ind_test_4d]
    
    test_4d.25scores <- test_4d.centered %*% train_4d.ppc$rotation[,1:25]
    train_4d.25scores <- train_4d.centered %*% train_4d.ppc$rotation[,1:25]
    
    # Use 1NN classification in the space of the first 25 scores to identify 
    # the subject for each testing observation.
    
    knn.pred <- knn(train_4d.25scores,test_4d.25scores,names.train_4d,k=1)
    score <- sum((knn.pred==names.test_4d),na.rm=TRUE)
    cat('loop',i,':',score,'identified correctly / ', length(knn.pred)-score,'identified incorrectly','\n')
    
}

#----- END YOUR CODE BLOCK HERE -----#

#################
# End of Script
#################


