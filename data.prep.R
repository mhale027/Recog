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

features <- names(d.train)
index <- data.table(feature = gsub("_x", "", features[grep("_x", features)]),
                    x = seq(1, 29, 2),
                    y = seq(2, 30, 2))



l.eye.center <- 1:2
r.eye.center <- 3:4
l.eye.inner <- 5:6
r.eye.inner <- 7:8
l.eye.outer <- 9:10
r.eye.outer <- 11:12
l.brow.inner <- 13:14
r.brow.inner <- 15:16
l.brow.outer <- 17:18
r.brow.outer <- 19:20
nose.center <- 21:22
l.mouth.corner <- 23:24
r.mouth.corner <- 25:26
top.lip.center <- 27:28
bottom.lip.center <- 29:30


get.image <- function(id) {
      im.id <- matrix(data=rev(im.train[id,]), nrow=96, ncol=96)
      eval(image(1:sqrt(length(im.id)), 1:sqrt(length(im.id)), 
                 im.id, col=gray((0:255)/255)), parent.frame())
      
           for ( i in 1:(ncol(d.train)/2)) {
                 eval(points(96-d.train[id,(i*2-1)], 96-d.train[id,(i*2)], col = "red"), parent.frame())
           }
}

plot.mean <- function(feature, id) {
           eval(get.image(id), parent.frame())
           feat <- grep(as.character(feature), names(d.train))
           eval( for (i in 1:nrow(d.train)) {
                 points(96-d.train[i,feat[1]],
                        96-d.train[i,feat[2]],
                       col = "red")
                  } , parent.frame()
                 )
}

extract.patch <- function(feat, size, id, env = parent.fram()) {
      
      coord_x <- paste(feat, "x", sep="_")
      coord_y <- paste(feat, "y", sep="_")
      
      im  <- matrix(data = im.train[id,], nrow=96, ncol=96)
      
      x   <- d.train[id, coord_x]
      y   <- d.train[id, coord_y]
      x1  <- (x-size)
      x2  <- (x+size)
      y1  <- (y-size)
      y2  <- (y+size)
      if ( (!is.na(x)) && (!is.na(y)) && (x1>=1) && (x2<=96) && (y1>=1) && (y2<=96) ){
            mat <- as.vector(im[x1:x2, y1:y2])
            # eval(image(1:nrow(mat), 1:ncol(mat), mat, col=gray((0:255)/255)), parent.frame())
            mat
      } else { NULL }
}



mean.patch <- function(ft = as.character("left_eye_center"), sz, env = parent.frame()) {
      coord_x <- paste(ft, "x", sep="_")
      coord_y <- paste(ft, "y", sep="_")
      
      patches <- for (i in 1:7049) {
            im  <- matrix(data = im.train[i,], nrow=96, ncol=96)
            x   <- d.train[i, coord_x]
            y   <- d.train[i, coord_y]
            x1  <- (x-sz)
            x2  <- (x+sz)
            y1  <- (y-sz)
            y2  <- (y+sz)
            if ( (!is.na(x)) && (!is.na(y)) && (x1>=1) && (x2<=96) && (y1>=1) && (y2<=96) )
            {
                  as.vector(im[x1:x2, y1:y2])
            }
            else
            {
                  NULL
            }
      }
      mean.patch.i <- matrix(data = colMeans(patches), nrow=2*patch_size+1, ncol=2*patch_size+1) 
}







mean.patch <- function(ft = as.character("left_eye_center"), sz, im.tr = im.train, env = parent.frame()) {
      
      im  <- matrix(data = im.tr[i,], nrow=96, ncol=96)
      patches <- data.frame()
      for (i in 1:nrow(d.train)) {
            patches[i,] <- extract.patch(ft, sz, i)
      }
      mean.patch <- matrix(data = colMeans(patches), nrow=2*patch_size+1, ncol=2*patch_size+1) 
}




















