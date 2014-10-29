#############################
# Denys Liubyvyi, UNI: dvl2110
# STAT W4240 
# Homework 3 , Problem 4
#
#############################

#clean the workspace
rm(list=ls())

#change working directory
setwd("/Users/dennis/github/W4240/HW2/")

#load libraries
library(pixmap)

# get directory structure
dir_list_1 = dir(path="CroppedYale/",all.files=FALSE)
dir_list_2 = dir(path="CroppedYale/",all.files=FALSE,recursive=TRUE)

# the list of pictures (note the absence of 14 means that 31 corresponds to yaleB32)
pic_list = 1:length(dir_list_1)
view_list = c('P00A+000E+00', 'P00A+005E+10', 'P00A+005E-10', 'P00A+010E+00')

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

fm_4a_size = dim(face_matrix_4a)
# Use 4/5 of the data for training, 1/5 for testing
ntrain_4a = floor(fm_4a_size[1]*4/5)
ntest_4a = fm_4a_size[1]-ntrain_4a
set.seed(1)
ind_train_4a = sample(1:fm_4a_size[1],ntrain_4a)
ind_test_4a = c(1:fm_4a_size[1])[-ind_train_4a]

train_4a <- face_matrix_4a[ind_train_4a,]
test_4a <- face_matrix_4a[ind_test_4a,]
    
#What are the first 5 fles in the training set?
print(rownames(train_4a[1:5,]))
#What are the first 5 fles in the training set?
print(rownames(test_4a[1:5,]))

mean.face.train_4a <- apply(train_4a,2,mean)

#Display the mean face
dim(mean.face.train_4a) <- photo.dim
mean.face.plot <- pixmapGrey(mean.face.train_4a)
plot(mean.face.plot)

# get centered train matrix
train_4a.centered <- apply(train_4a,1,function(x) {x-mean.face.train_4a})

#run PCA with prcomp 
train_4a.ppc <- prcomp(train_4a.centered)

# Project your testing data onto the first 25 loadings so that 
# it is also represented by the first 25 scores.
train_4a.25scores <- faces.ppc$x[,1:25]

# Use 1NN classification in the space of the first 25 scores to identify 
# the subject for each testing observation.
library(class)
knn.pred(train_4a.25scores,test.X,train.Direction ,k=1)