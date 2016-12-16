library(data.table)
library(Matrix)
library(foreach)
library(reshape2)
library(doParallel)
cl <- makeCluster(8)
registerDoParallel(cl)

setwd("~/projects/kaggle/recog")
train.file <- paste0('training.csv')
test.file  <- paste0('test.csv')

load("data.Rd")

im <- matrix(data=rev(im.train[1,]), nrow=96, ncol=96)

image(1:96, 1:96, im, col=gray((0:255)/255))


for(i in 1:nrow(d.train)) {
      points(96-d.train$left_eye_center_x[i], 96-d.train$left_eye_center_y[i], col="red")
}


























































