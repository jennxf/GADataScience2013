library(stats)
library(ggplot2)
set.seed(1)

d.train <- read.csv("~/Desktop/imageRecog/train.csv", stringsAsFactors=F)
#import training data into R and dont convert strings to numeric
str(d.train) #check data type
im.train <- d.train$Image 
# remove the Image column which contains pixel intensity and save it as another variable as it is too long
d.train$Image <- NULL
as.integer(unlist(strsplit(im.train[1], " "))) # split the pixel intensity of image 1 into strings, simplify it into vectors then convert it to integers.
install.packages('doMC') # in order to automate the conversion to integer process of 7000 or more images, we need to use a multi core approach using doMC package.

library(DoMC)
registerDoMC()

im.train <- foreach(im = im.train, .combine=rbind) %dopar% {
as.integer(unlist(strsplit(im, " ")))}  

#The foreach loop will evaluate the inner command for each row in im.train, 
#and combine the results with rbind (combine by rows). %dopar% instructs R 
#to do all evaluations in parallel.

str(im.train)  #im.train is now a matrix with 7049 rows (one for each image) and 9216 columns (one for each pixel)
im <- matrix(data=rev(im.train[1,]), nrow=96, ncol=96) # lets visialize the pixels into an image by using a 96x96 matrix 
image(1:96, 1:96, im, col=heat.colors(30)) # Visualize the image using a heatmap 


d.train2<-na.omit(d.train) # remove any NA for all 30 columns (excluding the Image column)
cols2<-d.train2[,c(1,3)] #compare x coordinate of left eye and x coordinate of right eye for kmeans analysis

cols2$row.names<-NULL # remove the row.names column
 head(cols2)
 ktest<-kmeans(cols2,2, nstart=25)  #For a more accurate determination of the center of the clusters, lets randomly start with 25 points 
 kdata <- cbind(d.train2, cluster=as.factor(ktest$cluster)) # combine the cluster column (as a factor) into the d.train2 dataset
 plot<-ggplot(kdata, aes(x=left_eye_center_x, y=right_eye_center_x)) +
geom_point(aes(colour=cluster)) + ggtitle('clustering results')          # plot the kmeans cluster analysis
 
 plot
 
 #the cluster analysis seems a little weird because we would assume the x coordinates of both the left and right eye centers to be the same level but it appears not to be.
 
 
