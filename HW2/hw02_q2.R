#############################
# Denys Liubyvyi, UNI: dvl2110
# STAT W4240 
# Homework 2 , Problem 2
# Homework Due September30
#
# The following code loads the eigenfaces data and
# performs a set of simple loading and plotting functions
#############################

#################
# Setup
#################

# NOTE: It is acceptable to use code from previous solutions;
# just give it credit or load previous file via source()



# NOTE: we partly use code from the previous homework

# make sure R is in the proper working directory
# note that this will be a different path for every machine
#setwd("~/Documents/academic/teaching/STAT_W4240_2014_SPRG/hw/hw01")

#change working directory
setwd("/Users/dennis/github/W4240/HW2/")

# first include the relevant libraries
# note that a loading error might mean that you have to
# install the package into your R distribution.  From the
# command line, type install.packages("pixmap")
library(pixmap)

# the list of pictures (note the absence of 14 means that 31 corresponds to yaleB32)
pic_list = 1:38
view_list = c(  'P00A+000E+00', 'P00A+005E+10' , 'P00A+005E-10' , 'P00A+010E+00')

# get directory structure
dir_list_1 = dir(path="CroppedYale/",all.files=FALSE)
dir_list_2 = dir(path="CroppedYale/",all.files=FALSE,recursive=TRUE)

# find lengths
len_dl1 = length(dir_list_1)
len_dl2 = length(dir_list_2)


#############################################################################################
# a. (10 Points) Load the views P00A+000E+00, P00A+005E+10, P00A+005E-10, and P00A+010E+00 
# for all subjects. Convert each photo to a vector; store the collection as a matrix where 
# each row is a photo. What is the size of this matrix?
#############################################################################################

# Find the total number of pixels in a picture
filename <- sprintf("CroppedYale/%s/%s_%s.pgm", dir_list_1[pic_list[1]] , dir_list_1[pic_list[1]] , view_list[1])
photo <- getChannels(read.pnm(filename))
photo.dim <- dim(photo)
cat('The total number of pixels in a picture: ',length(as.vector(photo)))

# Pre-allocate a matrix with dimensions (number of pictures) x (number of pixels per picture)
photo.matrix <- matrix(NA,nrow = length(pic_list)*length(view_list),ncol = length(photo))
cat('Dimenstions of photo matrix:',dim(photo.matrix))

# Load all of the pictures using some code from homework 1
rownames.list <- vector()
for (i in 1:length(pic_list)) {
    for (j in 1:length(view_list)) {
        filename <- sprintf("CroppedYale/%s/%s_%s.pgm", dir_list_1[pic_list[i]] , dir_list_1[pic_list[i]] , view_list[j])
        filename.short <- sprintf("%s_%s.pgm", dir_list_1[pic_list[i]] , view_list[j])
        photo <- as.vector(getChannels(read.pnm(filename)))
        photo.matrix[(i-1)*length(view_list)+j,] <- photo
        rownames.list <- c(rownames.list,filename.short)
    }
}
rownames(photo.matrix) <- rownames.list



#################
# # Problem 2b
#################
# Use colMeans() on your matrix to get "mean face" vector
# Convert back to original size using dim()

mean.face <- apply(photo.matrix,2,mean)

#Display the mean face
dim(mean.face) <- photo.dim
mean.face.plot <- pixmapGrey(mean.face)
plot(mean.face.plot)

# give it a nice title
title('The mean face 192 x 168')
#save photo
filename = 'mean_face.png'
dev.copy(device=png, file=filename, height=photo.dim[1], width=photo.dim[2])
dev.off()

#################
# # Problem 2c
#################
# Run prcomp() on your centered face matrix

#Center the matrix
photo.matrix.centered <- scale(photo.matrix,center = TRUE, scale = FALSE)

#run PCA with prcomp 
faces.ppc <- prcomp(photo.matrix.centered)
var.data <- faces.ppc$sdev^2/sum(faces.ppc$sdev^2)
names(var.data) <- colnames(faces.ppc$rotation)
barplot(var.data[1:20])

#screeplot(faces.ppc,type="lines")

#################
# # Problem 2d
#################
# Build your eigenface grid like the face grid in homework 1

str(faces.ppc$rotation)
dim(faces.ppc$rotation)
eigen.faces <- faces.ppc$rotation

# checking the idea that scorings are just pre-calculated projections of original data to princilal onto 
# the principal components
# this one should be bunch of zeros:
# round(photo.matrix.centered %*% faces.ppc$rotation - faces.ppc$x,digits=3)
# and yes! it is! we learned something
    
    
eigen.face <- eigen.faces[,1]
dim(eigen.face) <- photo.dim
eigen.face.pix = pixmapGrey(eigen.face)
plot(eigen.face.pix)

faces_matrix = vector()

for (i in 1:3) {
    for (j in 1:3) {
        eigen.face <- eigen.faces[,(i-1)*3+j]
        dim(eigen.face) <- photo.dim
        if (j==1) pnm_row <- eigen.face else pnm_row <- cbind(pnm_row,eigen.face)
    }
    if (i==1) faces_matrix <- pnm_row else faces_matrix <- rbind(faces_matrix,pnm_row)  
}

# now faces_matrix has been built properly.  plot it
faces = pixmapGrey(faces_matrix)
plot(faces)

# give it a nice title
title('3x3 grid of eigenfaces')

#################
# # Problem 2e
#################
# Find the index of face yaleB01_P00A+010E+00.pgm
# Often, reading in the names and storing them as a list is a good idea
# Use the scores and loadings you found in 2c to reconstruct a face 
# by adding in 1 (or 5) bases at a time

e.index <- which(rownames.list == "yaleB01_P00A+010E+00.pgm")
e.matrix <- photo.matrix[e.index,]
dim(e.matrix) <- photo.dim
e.face.pix = pixmapGrey(e.matrix)
plot(e.face.pix)
title('yaleB01_P00A+010E+00.pgm')

e.faces_matrix <- vector()
for (i in 1:5) {
    for (j in 1:5) {
        
        e.face.reconstructed <- as.vector(mean.face) + faces.ppc$x[e.index,0:((i-1)*3+j-1)] %*% t(eigen.faces[,0:((i-1)*3+j-1)])
        dim(e.face.reconstructed) <- photo.dim
    
        if (j==1) pnm_row <- e.face.reconstructed else pnm_row <- cbind(pnm_row,e.face.reconstructed)
    }
    if (i==1) faces_matrix <- pnm_row else faces_matrix <- rbind(faces_matrix,pnm_row)  
}

# now faces_matrix has been built properly.  plot it
faces = pixmapGrey(faces_matrix)
plot(faces)

# give it a nice title
title('5x5 grid of applied eigenfaces for yaleB01_P00A+010E+00.pgm')



#################
# # Problem 2f
#################
# Find the index of the faces to remove
# Find the index of face yaleB05_P00A+010E+00.pgm
# Remove pictures from matrix; run prcomp()

f.index.to.remove <- grep("yaleB05",rownames(photo.matrix))
cat('Index to remove:',f.index.to.remove)

f.photo.matrix <- photo.matrix[-f.index.to.remove,]
dim(f.photo.matrix)

#create new mean face
f.mean.face <- apply(f.photo.matrix,2,mean)
dim(f.mean.face) <- photo.dim

#Center the matrix
f.photo.matrix.centered <- scale(f.photo.matrix,scale=FALSE,center = TRUE)

#run PCA with prcomp 
f.faces.ppc <- prcomp(f.photo.matrix.centered)
screeplot(f.faces.ppc,type="lines")
f.eigen.faces <- f.faces.ppc$rotation

f.index <- which(rownames(photo.matrix) == "yaleB05_P00A+010E+00.pgm")
f.matrix <- photo.matrix[f.index,]
dim(f.matrix) <- photo.dim
f.face.pix = pixmapGrey(f.matrix)
plot(f.face.pix)
title('yaleB05 P00A+010E+00.pgm')

#apply new eigen vectors to the photo yaleB05_P00A+010E+00.pgm
f.face.reconstructed <- as.vector(f.mean.face) + as.vector(f.matrix) %*% f.eigen.faces %*% t(f.eigen.faces)
dim(f.face.reconstructed) <- photo.dim
f.face.reconstructed <- f.face.reconstructed + f.mean.face
f.face = pixmapGrey(f.face.reconstructed)
plot(f.face)

#################
# # End of Script
#################